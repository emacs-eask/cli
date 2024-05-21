pkgname='eask'
pkgver=$(git tag --sort=-creatordate | head -n1).$(git rev-parse --short HEAD)
pkgrel=1
pkgdesc='CLI for building, running, testing, and managing your Emacs Lisp dependencies'
arch=('x86_64')
makedepends=('npm')
options=('!strip') # node puts scripts into debug section, so we can't strip it
url='https://github.com/emacs-eask/cli'
license=('GPL-3.0')

prepare() {
    npm i
}

build() {
    npm run pkg-linux-x64
}

package() {
    mkdir -p "$pkgdir/usr/bin"
    # ATM eask only works when `lisp` dir is installed together with the binary
    cp -r "${startdir}/lisp" "${startdir}/dist/eask" "$pkgdir/usr/bin"
}
