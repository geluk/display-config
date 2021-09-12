//! XRandR wrapper. Queries an X11 server for available outputs.

use std::{cmp, collections::HashMap, convert::TryFrom};

use anyhow::*;
use log::*;
use sha2::{Digest, Sha256};
use x11rb::{
    protocol::{
        randr::{self, ConnectionExt, GetScreenResourcesReply, ModeFlag, ModeInfo},
        render::SubPixel,
        xproto::{self, Timestamp, Window},
    },
    rust_connection::RustConnection,
};

use crate::xorg::X11Wrapper;

const EDID_NAME: &str = "EDID";
const EDID_LENGTH_BYTES: u32 = 256;

/// An Xorg XID
pub type Xid = u32;
/// Refresh rate in Hz
pub type RefreshRate = f32;
/// Flags indicating the rotation/reflection of a CRTC
pub type Rotation = u16;

/// Represents the states an Xrandr output may be in.
#[derive(Debug, Clone)]
pub enum Output {
    Connected(ConnectedOutput),
    Disconnected(UnknownOutput),
    Unknown(UnknownOutput),
}

/// An output with an available monitor, connected to the display adapter.
#[derive(Debug, Clone)]
pub struct ConnectedOutput {
    /// The XID of the output.
    pub id: Xid,
    /// Name of the output adapter, e.g. DP-1.
    pub name: String,
    /// Physical dimensions of the monitor connected to the output.
    pub dimensions: Dimensions,
    /// The CRTC this output is bound to, if any.
    pub crtc: Option<Crtc>,
    /// Subpixel order of the output, if known.
    pub subpixel_order: SubPixel,
    /// An output may not have a preferred mode.
    pub edid: Option<Vec<u8>>,
    /// A SHA-256 hash of the edid bytes.
    pub edid_sha256: Option<String>,
    pub preferred_mode: Option<Mode>,
    /// Modes supported by this output. An output can only be connected to a
    /// CRTC if it supports the mode that is currently being used by the CRTC.
    pub supported_modes: Vec<Mode>,
}
impl ConnectedOutput {
    /// Returns whether the output is currently bound to a CRTC.
    pub fn is_active(&self) -> bool {
        self.crtc.is_some()
    }

    /// Returns the maximum supported resolution. Resolutions are compared by
    /// comparing their areas. Returns [`None`] if the output has no supported
    /// modes.
    pub fn max_resolution(&self) -> Option<&Resolution> {
        self.supported_modes.iter().map(|m| &m.resolution).max()
    }

    /// Returns the best supported resolution, which is defined to be the
    /// resolution of this output's preferred mode. If the output has no
    /// preferred mode, the maximum supported resolution is returned instead.
    /// Returns [`None`] if the output has no supported modes.
    pub fn best_resolution(&self) -> Option<&Resolution> {
        self.preferred_mode
            .as_ref()
            .map(|r| &r.resolution)
            .or_else(|| self.max_resolution())
    }

    /// Returns the highest supported refresh rate of the output's best
    /// resolution (as defined by [`Self::best_resolution()`]).
    pub fn best_refresh_rate(&self) -> Option<RefreshRate> {
        // We don't just take the highest possible refresh rate, because
        // monitors often support higher refresh rates at low-resolution modes.
        // So, firt we determine the best supported resolution for this monitor.
        let best_resolution = self.best_resolution()?;

        // Now, we can find all modes with this resolution,
        // and take the highest refresh rate.
        self.supported_modes
            .iter()
            .filter(|m| &m.resolution == best_resolution)
            .filter_map(|m| m.refresh_rate)
            .reduce(f32::max)
    }
}

/// A display mode, encoding information about the resolution and sync rates of
/// an ouput.
#[derive(Debug, Clone)]
pub struct Mode {
    pub id: Xid,
    pub resolution: Resolution,
    pub refresh_rate: Option<RefreshRate>,
    pub hsync: Option<RefreshRate>,
    flags: ModeFlag,
}
impl Mode {
    /// Returns true if the given output is currently set to this mode.
    pub fn is_active_on(&self, output: &ConnectedOutput) -> bool {
        output
            .crtc
            .as_ref()
            .and_then(|c| c.mode.as_ref())
            .map_or(false, |m| m == self)
    }

    /// Returns true if this mode is the preferred mode of the given output.
    pub fn is_preferred_by(&self, output: &ConnectedOutput) -> bool {
        output.preferred_mode.as_ref().map_or(false, |m| m == self)
    }
}
impl Eq for Mode {}
impl PartialEq for Mode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

/// An output that is either disconnected or in an unknown state.
#[derive(Debug, Clone)]
pub struct UnknownOutput {
    pub id: Xid,
    pub name: String,
}

/// A rectangle on the screen, which is directly visible on or more outputs.
/// A CRTC defines a mode, which all connected outputs must support.
#[derive(Debug, Clone)]
pub struct Crtc {
    id: Xid,
    dimensions: Rectangle,
    mode: Option<Mode>,
    rotation: Rotation,
    rotations: Rotation,
}

/// Physical dimensions, in millimetres.
#[derive(Debug, Clone)]
pub struct Dimensions {
    pub mm_width: u32,
    pub mm_height: u32,
}
impl Dimensions {
    fn new(mm_width: u32, mm_height: u32) -> Self {
        Self {
            mm_width,
            mm_height,
        }
    }
}

/// Resolution in pixels.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolution {
    pub width: u16,
    pub height: u16,
}
impl Resolution {
    fn new(width: u16, height: u16) -> Self {
        Self { width, height }
    }
    fn area(&self) -> u32 {
        self.width as u32 * self.height as u32
    }
}
impl PartialOrd for Resolution {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.area().cmp(&other.area()))
    }
}

impl Ord for Resolution {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.area().cmp(&other.area())
    }
}

/// A rectangle, defined by its position, width, and height, in pixels.
#[derive(Debug, Clone)]
pub struct Rectangle {
    pub x: i16,
    pub y: i16,
    pub width: u16,
    pub height: u16,
}
impl Rectangle {
    fn new(x: i16, y: i16, width: u16, height: u16) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }
}

/// Xrandr wrapper for fetching monitor settings.
pub struct Xrandr<'conn> {
    conn: &'conn RustConnection,
    root_wnd: Window,
}
impl<'conn> Xrandr<'conn> {
    pub fn new(wrapper: &'conn X11Wrapper) -> Self {
        Self {
            conn: &wrapper.connection,
            root_wnd: wrapper.root.root,
        }
    }

    pub fn get_connected_outputs(&self) -> Result<Vec<ConnectedOutput>> {
        Ok(self
            .get_all_outputs()?
            .into_iter()
            .filter_map(|o| match o {
                Output::Connected(out) => Some(out),
                _ => None,
            })
            .collect())
    }

    pub fn get_all_outputs(&self) -> Result<Vec<Output>> {
        debug!("Querying outputs");
        let resources = self.get_screen_resources()?;

        let modes = resources
            .modes
            .iter()
            .map(|i| self.create_mode(i).map(|m| (m.id, m)))
            .collect::<Result<_>>()?;

        let crtcs = resources
            .crtcs
            .iter()
            .map(|i| {
                self.create_crtc(*i, &modes, resources.timestamp)
                    .map(|c| (c.id, c))
            })
            .collect::<Result<_>>()?;

        let outputs = resources
            .outputs
            .iter()
            .map(|o| self.create_output(*o, &modes, &crtcs, resources.timestamp))
            .collect::<Result<Vec<_>>>()?;
        Ok(outputs)
    }

    fn get_screen_resources(&self) -> Result<GetScreenResourcesReply> {
        let cookie = self.conn.randr_get_screen_resources(self.root_wnd)?;
        let reply = cookie.reply().context("Failed to get screen resources")?;
        Ok(reply)
    }

    pub(crate) fn create_crtc(
        &self,
        id: Xid,
        modes: &HashMap<Xid, Mode>,
        timestamp: Timestamp,
    ) -> Result<Crtc> {
        let cookie = self.conn.randr_get_crtc_info(id, timestamp)?;
        let reply = cookie.reply().context("Failed to get CRTC")?;

        let mode = match reply.mode {
            0 => None,
            xid => Some(
                modes
                    .get(&xid)
                    .cloned()
                    .ok_or_else(|| anyhow!("Could not find active mode"))?,
            ),
        };

        Ok(Crtc {
            id,
            dimensions: Rectangle::new(reply.x, reply.y, reply.width, reply.height),
            mode,
            rotation: reply.rotation,
            rotations: reply.rotations,
        })
    }

    fn create_mode(&self, mode: &ModeInfo) -> Result<Mode> {
        if mode.htotal == 0 || mode.vtotal == 0 {
            bail!("Did not expect htotal or vtotal to be zero");
        }
        // If the dot clock is zero, then all of the timing
        // parameters and flags are not used, and must be zero as this
        // indicates that the timings are unknown or otherwise unused.
        // (randrproto.txt:2322)
        let sync_rates = (mode.dot_clock != 0).then(|| {
            let mut vtotal = mode.vtotal as RefreshRate;
            if mode.mode_flags & u32::from(ModeFlag::DOUBLE_SCAN) != 0 {
                // doublescan doubles the number of lines (xrandr.c:582)
                vtotal *= 2.;
            }
            if mode.mode_flags & u32::from(ModeFlag::INTERLACE) != 0 {
                // interlace splits the frame into two fields
                // the field rate is what is typically reported by monitors
                // (xrandr.c:587)
                vtotal /= 2.;
            }
            let vsync = mode.dot_clock as RefreshRate
                / (mode.htotal as RefreshRate * vtotal as RefreshRate);
            let hsync = mode.dot_clock as RefreshRate / mode.htotal as RefreshRate;
            (vsync, hsync)
        });
        let flags = u16::try_from(mode.mode_flags).context("Failed to convert mode flags")?;

        Ok(Mode {
            id: mode.id,
            resolution: Resolution::new(mode.width, mode.height),
            refresh_rate: sync_rates.map(|i| i.0),
            hsync: sync_rates.map(|i| i.1),
            flags: ModeFlag::from(flags),
        })
    }

    fn create_output(
        &self,
        id: randr::Output,
        modes: &HashMap<Xid, Mode>,
        crtcs: &HashMap<Xid, Crtc>,
        timestamp: Timestamp,
    ) -> Result<Output> {
        let cookie = self.conn.randr_get_output_info(id, timestamp)?;
        let reply = cookie.reply().context("Failed to get output info")?;

        let name = String::from_utf8(reply.name.clone())?;

        // println!("CONNECTED: {:#?}", reply)
        match reply.connection {
            randr::Connection::CONNECTED => (),
            randr::Connection::DISCONNECTED => {
                return Ok(Output::Disconnected(UnknownOutput { id, name }));
            }
            _ => return Ok(Output::Unknown(UnknownOutput { id, name })),
        }

        let dimensions = Dimensions::new(reply.mm_width, reply.mm_height);

        let crtc = match &reply.crtc {
            0 => None,
            xid => Some(
                crtcs
                    .get(xid)
                    .ok_or_else(|| anyhow!("Could not find CRTC {} for output {}", reply.crtc, id))?
                    .clone(),
            ),
        };

        let preferred_mode = match reply.num_preferred {
            // The preferred mode of a monitor is 0 if it has no preferred mode
            0 => None,
            idx => Some(
                // If it does have a preferred mode, it's a 1-indexed array
                // index of the monitor's supported modes, so we subtract 1.
                modes
                    .get(&reply.modes[idx as usize - 1])
                    .cloned()
                    .ok_or_else(|| anyhow!("Could not find preferred mode for output {}", id))?,
            ),
        };

        let supported_modes = reply
            .modes
            .iter()
            .map(|m| {
                modes
                    .get(m)
                    .cloned()
                    .ok_or_else(|| anyhow!("Could not find supported mode: {}", m))
            })
            .collect::<Result<_>>()?;

        let (edid, edid_sha256) = match self.create_edid(id)? {
            Some((e, s)) => (Some(e), Some(s)),
            None => (None, None),
        };

        Ok(Output::Connected(ConnectedOutput {
            id,
            name,
            dimensions,
            crtc,
            subpixel_order: reply.subpixel_order,
            edid,
            edid_sha256,
            preferred_mode,
            supported_modes,
        }))
    }

    fn create_edid(&self, id: randr::Output) -> Result<Option<(Vec<u8>, String)>> {
        let properties = self.conn.randr_list_output_properties(id)?.reply()?;
        for atom in &properties.atoms {
            let name = String::from_utf8(
                xproto::ConnectionExt::get_atom_name(self.conn, *atom)?
                    .reply()?
                    .name,
            )?;

            if name == EDID_NAME {
                let prop = self
                    .conn
                    .randr_get_output_property::<u32>(
                        id,
                        *atom,
                        0,
                        0,
                        // Xorg property length represents the number of 32-bit
                        // integers that fit in it (not the number of bytes)
                        // so we divide by four here.
                        EDID_LENGTH_BYTES / 4,
                        false,
                        false,
                    )?
                    .reply()?;

                if prop.bytes_after > 0 {
                    // This should be downgraded to a warning once we support
                    // generating warnings.
                    bail!(
                        "Edid length of {} bytes greater than expected ({} bytes)",
                        // See above for the significance of multiplying by four.
                        prop.length * 4 + prop.bytes_after,
                        EDID_LENGTH_BYTES
                    );
                }
                let hash = create_hash(&prop.data);
                return Ok(Some((prop.data, hash)));
            }
        }

        Ok(None)
    }
}

/// Convenience function for creating a base64-encoded SHA256 hash.
fn create_hash(data: impl AsRef<[u8]>) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    let result = hasher.finalize();
    base64::encode(result)
}
