# Maintained by: Johan Geluk <johan@gelukio s/io/.io/>
pkgname=display-config
pkgrel=1
pkgver=0.1.0.r11.ge2af879
pkgdesc='Configure you desktop and run commands based on connected displays'
arch=(x86_64)
url='https://github.com/geluk/display-config'
makedepends=(cargo)
source=("$pkgname::git+https://github.com/geluk/display-config")
sha1sums=('SKIP')

pkgver() {
    cd $pkgname
    jgit describe --tags | sed 's/^v//;s/\([^-]*-g\)/r\1/;s/-/./g'
}

prepare() {
    cd $pkgname
    cargo fetch --locked --target "$CARCH-unknown-linux-gnu"
}

build() {
    cd $pkgname
    export RUSTUP_TOOLCHAIN=stable
    export CARGO_TARGET_DIR=target
    cargo build --release --locked
}

check() {
    cd $pkgname
    cargo test --frozen
}

package() {
    cd $pkgname
    install -Dm0755 -t "$pkgdir/usr/bin/" "target/release/display-config"
}
