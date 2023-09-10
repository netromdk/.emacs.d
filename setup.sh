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
    writeBanner "Fonts"
    cp -v font/*.ttf ~/Library/Fonts/

    writeBanner "Homebrew"
    checkProgram brew
    brew install curl git ripgrep the_silver_searcher cppcheck shellcheck aspell zstd
    ;;

  Linux)
    # Dist specific.
    if grep -q Fedora /etc/system-release; then
      sudo dnf install git ripgrep cppcheck shellcheck aspell aspell-da aspell-en zstd
    fi
    ;;
esac

writeBanner "Git"
checkProgram git

writeBanner "LLVM / Clang"
checkProgram clang-format
checkProgram clangd # LSP

writeBanner "Python"
checkProgram pip3
pip3 install --user python-lsp-server flake8 bandit

writeBanner "Rust"
checkProgram rustup
checkProgram rustc
checkProgram cargo
rustup component add rustfmt rls rust-analysis rust-src

writeBanner "Zstd"
checkProgram zstd
