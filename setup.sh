#!/bin/sh

# Takes program as argument.
checkProgram() {
  if hash $1 2> /dev/null; then
    echo "$1 is installed!"
  else
    echo "$1 is not installed!"
    exit 1
  fi
}

# Takes title as argument.
writeBanner() {
  echo "== $1 =="
}

# OS specific.
case "$(uname -s)" in
  Darwin)
    writeBanner "Homebrew"
    checkProgram brew
    brew install ripgrep the_silver_searcher cppcheck shellcheck composer aspell
    ;;
esac

writeBanner "clangd (LSP)"
checkProgram clangd

writeBanner "Python"
checkProgram pip3
pip3 install python-language-server flake8 bandit

writeBanner "Rust"
checkProgram rustup
checkProgram rustc
checkProgram cargo
rustup component add rustfmt rls rust-analysis rust-src
