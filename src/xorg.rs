use anyhow::{bail, Result};
use log::debug;
use x11rb::{connection::Connection, protocol::xproto::Screen, rust_connection::RustConnection};

pub struct X11Wrapper {
    pub connection: RustConnection,
    pub vendor: String,
    pub screen: usize,
    pub root: Screen,
}

pub fn connect() -> Result<X11Wrapper> {
    // Passing None will use the DISPLAY environment variable.
    let (connection, screen) = x11rb::connect(None)?;
    let setup = connection.setup();
    let vendor = String::from_utf8(setup.vendor.clone())?;

    let roots = &setup.roots;

    if roots.is_empty() {
        bail!("No X11 roots found");
    }
    if roots.len() > 1 {
        bail!("Using multiple X11 roots is not supported yet")
    }
    let root = (&roots[0]).clone();

    debug!("Connected to X11 - screen: {}, vendor: {}", screen, vendor);
    debug!(
        "Root window is {} × {}",
        root.width_in_pixels, root.height_in_pixels
    );
    Ok(X11Wrapper {
        connection,
        vendor,
        screen,
        root,
    })
}
