use std::cmp;

use anyhow::Result;

pub mod hyprland;
pub mod xrandr;

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
#[allow(unused)]
pub struct ConnectedOutput {
    /// The XID of the output.
    pub id: Xid,
    /// Name of the output adapter, e.g. DP-1.
    pub name: String,
    /// Brand and model, as reported by the monitor.
    pub description: Option<String>,
    /// Physical dimensions of the monitor connected to the output.
    pub dimensions: Dimensions,
    /// The CRTC this output is bound to, if any.
    pub crtc: Option<Crtc>,
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
#[allow(unused)]
pub struct Mode {
    pub id: Xid,
    pub resolution: Resolution,
    pub refresh_rate: Option<RefreshRate>,
    pub hsync: Option<RefreshRate>,
}
impl Mode {
    /// Returns true if the given output is currently set to this mode.
    pub fn is_active_on(&self, output: &ConnectedOutput) -> bool {
        output.crtc.as_ref().and_then(|c| c.mode.as_ref()) == Some(self)
    }

    /// Returns true if this mode is the preferred mode of the given output.
    pub fn is_preferred_by(&self, output: &ConnectedOutput) -> bool {
        output.preferred_mode.as_ref() == Some(self)
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
#[allow(unused)]
pub struct UnknownOutput {
    pub id: Xid,
    pub name: String,
}

/// A rectangle on the screen, which is directly visible on or more outputs.
/// A CRTC defines a mode, which all connected outputs must support.
#[derive(Debug, Clone)]
#[allow(unused)]
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
    pub width: u32,
    pub height: u32,
}
impl Resolution {
    fn new(width: u32, height: u32) -> Self {
        Self { width, height }
    }
    fn area(&self) -> u32 {
        self.width * self.height
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
#[allow(unused)]
pub struct Rectangle {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32,
}
impl Rectangle {
    fn new(x: i32, y: i32, width: u32, height: u32) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }
}

pub trait OutputProvider {
    fn get_connected_outputs(&self) -> Result<Vec<ConnectedOutput>>;

    fn get_all_outputs(&self) -> Result<Vec<Output>>;
}
