//! XRandR wrapper. Queries an X11 server for available outputs.

use std::collections::HashMap;

use anyhow::*;
use log::*;
use sha2::{Digest, Sha256};
use x11rb::{
    protocol::{
        randr::{self, ConnectionExt, GetScreenResourcesReply, ModeFlag, ModeInfo},
        xproto::{self, Timestamp, Window},
    },
    rust_connection::RustConnection,
};

use super::*;

use crate::xorg::X11Wrapper;

const EDID_NAME: &str = "EDID";
const EDID_LENGTH_BYTES: u32 = 256;

/// Xrandr wrapper for fetching monitor settings.
pub struct Xrandr {
    conn: RustConnection,
    root_wnd: Window,
}
impl OutputProvider for Xrandr {
    fn get_connected_outputs(&self) -> Result<Vec<ConnectedOutput>> {
        Ok(self
            .get_all_outputs()?
            .into_iter()
            .filter_map(|o| match o {
                Output::Connected(out) => Some(out),
                _ => None,
            })
            .collect())
    }

    fn get_all_outputs(&self) -> Result<Vec<Output>> {
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
            .map(|&i| {
                self.create_crtc(i, &modes, resources.timestamp)
                    .map(|c| (c.id, c))
            })
            .collect::<Result<_>>()?;

        let outputs = resources
            .outputs
            .iter()
            .map(|&o| self.create_output(o, &modes, &crtcs, resources.timestamp))
            .collect::<Result<Vec<_>>>()?;
        Ok(outputs)
    }
}

impl Xrandr {
    pub fn new(wrapper: X11Wrapper) -> Self {
        Self {
            conn: wrapper.connection,
            root_wnd: wrapper.root.root,
        }
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
            dimensions: Rectangle::new(
                reply.x as i32,
                reply.y as i32,
                reply.width as u32,
                reply.height as u32,
            ),
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

        Ok(Mode {
            id: mode.id,
            resolution: Resolution::new(mode.width as u32, mode.height as u32),
            refresh_rate: sync_rates.map(|i| i.0),
            hsync: sync_rates.map(|i| i.1),
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

        dbg!(&edid);

        Ok(Output::Connected(ConnectedOutput {
            id,
            name,
            description: None, // Not available in xrandr
            dimensions,
            crtc,
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
                xproto::ConnectionExt::get_atom_name(&self.conn, *atom)?
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
                    warn!(
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
