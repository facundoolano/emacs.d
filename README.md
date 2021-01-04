My emacs configuration. Previously based on prelude.

![](emacs.png)

``` shell
brew install emacs --with-cocoa --with-gnutls --with-dbus --with-librsvg --with-imagemagick@6 --with-mailutils --devel

npm install -g livedown
npm install -g mocha
npm install -g eslint

brew install candid82/brew/joker

brew install aspell

pip3 install --user isort
pip3 install --user autopep8

rustup component add rustfmt
rustup toolchain add nightly
rustup component add rust-src
cargo +nightly install racer
rustup component add clippy --toolchain=nightly
```

run from spotlight:

- open /usr/local/Cellar/emacs/26.0.91/
- make alias
- drag to dock
