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
    brew install curl git ripgrep the_silver_searcher cppcheck shellcheck composer aspell
    ;;
esac

writeBanner "Git"
checkProgram git

writeBanner "LLVM / Clang"
checkProgram clang-format
checkProgram clangd # LSP

writeBanner "Python"
checkProgram pip3
pip3 install python-lsp-server flake8 bandit

writeBanner "Rust"
checkProgram rustup
checkProgram rustc
checkProgram cargo
rustup component add rustfmt rls rust-analysis rust-src
