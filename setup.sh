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
    brew install curl git ripgrep the_silver_searcher cppcheck shellcheck aspell
    ;;

  Linux)
    writeBanner "Fonts"
    mkdir -p ~/.local/share/fonts
    cp -v font/*.ttf ~/.local/share/fonts
    fc-cache -f -v ~/.local/share/fonts

    if [ -f /etc/lsb-release -o -d /etc/lsb-release.d ]; then
      DISTRO=$(lsb_release -i | cut -d: -f2 | sed s/'^\t'//)
    else
      DISTRO=$(ls -d /etc/[A-Za-z]*[_-][rv]e[lr]* | grep -v "lsb" | cut -d'/' -f3 | cut -d'-' -f1 | cut -d'_' -f1)
    fi

    if [ "${DISTRO}" = "Ubuntu" ] || [ "${DISTRO}" = "Debian"]; then
        writeBanner "APT"
        checkProgram apt
        sudo apt install curl git ripgrep silversearcher-ag cppcheck shellcheck aspell aspell-en aspell-da
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
pip3 install python-lsp-server flake8 bandit

writeBanner "Rust"
checkProgram rustup
checkProgram rustc
checkProgram cargo
rustup component add rustfmt rust-analysis rust-src
