#!/bin/sh

# OS specific
case "$(uname -s)" in
  Darwin)
    brew install cquery the_silver_searcher cppcheck shellcheck composer

    # Rust related.
    rustup component add rustfmt-preview rls-preview rust-analysis rust-src
    ;;
esac

# General
pip install python-language-server flake8 bandit
