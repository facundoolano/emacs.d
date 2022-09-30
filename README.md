My emacs configuration. Previously based on prelude.

![](emacs.png)

``` shell
alias emacs="emacsclient -c -a emacs"
export EDITOR=emacs

brew install --cask emacs-mac
brew install ag

npm install -g livedown
npm install -g mocha
npm install -g eslint

brew install candid82/brew/joker

brew install aspell

pip3 install --user isort
pip3 install --user autopep8

rustup toolchain install nightly
rustup component add rls
rustup component add rust-src
rustup component add rustfmt
rustup component add clippy --toolchain=nightly

git clone https://github.com/rust-analyzer/rust-analyzer.git
cd rust-analyzer
cargo xtask install --server

brew install coreutils
brew install plantuml

M-x all-the-icons-install-fonts
```
