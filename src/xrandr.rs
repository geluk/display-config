use std::{collections::HashMap, convert::TryFrom};

use anyhow::{anyhow, bail, Context, Result};
use x11rb::{
    protocol::{
        randr::{self, ConnectionExt, GetScreenResourcesReply, ModeFlag, ModeInfo},
        render::SubPixel,
        xproto::{self, Window},
    },
    rust_connection::RustConnection,
};

use crate::xorg::X11Wrapper;

/// An Xorg XID
pub type Xid = u32;
/// Refresh rate in Hz
pub type RefreshRate = f32;
/// Flags indicating the rotation/reflection of a CRTC
pub type Rotation = u16;

pub type Timestamp = xproto::Timestamp;

pub struct Xrandr<'conn> {
    conn: &'conn RustConnection,
    root_wnd: Window,
}

#[derive(Debug, Clone)]
pub struct Crtc {
    id: Xid,
    dimensions: Rectangle,
    mode: Option<Mode>,
    rotation: Rotation,
    rotations: Rotation,
}

#[derive(Debug, Clone)]
pub struct Output {
    pub id: Xid,
    pub name: String,
    pub dimensions: Option<Dimensions>,
    pub connection: Connection,
    pub active: bool,
    pub crtc: Option<Crtc>,
    pub subpixel_order: SubPixel,
    pub preferred_mode: Option<Mode>,
    pub supported_modes: Vec<Mode>,
}

impl Output {
    pub fn is_connected(&self) -> bool {
        matches!(self.connection, Connection::Connected)
    }
}

#[derive(Debug, Clone)]
pub struct Dimensions {
    pub mm_width: u32,
    pub mm_height: u32,
}

#[derive(Debug, Clone)]
pub struct Resolution {
    pub width: u16,
    pub height: u16,
}

#[derive(Debug, Clone)]
pub struct Rectangle {
    pub x: i16,
    pub y: i16,
    pub width: u16,
    pub height: u16,
}

#[derive(Debug, Clone)]
pub enum Connection {
    Connected,
    Disconnected,
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Mode {
    pub id: Xid,
    pub resolution: Resolution,
    pub refresh_rate: Option<RefreshRate>,
    pub hsync: Option<RefreshRate>,
    flags: ModeFlag,
}

impl Mode {
    pub fn is_active_on(&self, output: &Output) -> bool {
        output
            .crtc
            .as_ref()
            .and_then(|c| c.mode.as_ref())
            .map_or(false, |m| m == self)
    }

    pub fn is_preferred_by(&self, output: &Output) -> bool {
        output.preferred_mode.as_ref().map_or(false, |m| m == self)
    }
}

impl Eq for Mode {}
impl PartialEq for Mode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'conn> Xrandr<'conn> {
    pub fn new(wrapper: &'conn X11Wrapper) -> Self {
        Self {
            conn: &wrapper.connection,
            root_wnd: wrapper.root.root,
        }
    }

    pub fn get_configuration(&self) -> Result<Vec<Output>> {
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

        let connection = match reply.connection {
            randr::Connection::DISCONNECTED => Connection::Disconnected,
            randr::Connection::CONNECTED => Connection::Connected,
            randr::Connection::UNKNOWN => Connection::Unknown,
            other => bail!("Invalid connection status: {:?}", other),
        };

        let dimensions = match connection {
            Connection::Disconnected => None,
            _ => Some(Dimensions::new(reply.mm_width, reply.mm_height)),
        };

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

        Ok(Output {
            id,
            name: String::from_utf8(reply.name)?,
            dimensions,
            connection,
            active: reply.crtc != 0,
            crtc,
            subpixel_order: reply.subpixel_order,
            preferred_mode,
            supported_modes,
        })
    }
}

impl Dimensions {
    fn new(mm_width: u32, mm_height: u32) -> Self {
        Self {
            mm_width,
            mm_height,
        }
    }
}
impl Resolution {
    fn new(width: u16, height: u16) -> Self {
        Self { width, height }
    }
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
