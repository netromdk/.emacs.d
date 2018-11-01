#!/bin/sh

# OS specific
case "$(uname -s)" in
  Darwin)
    brew install cquery the_silver_searcher cppcheck shellcheck
    ;;
esac

# General
pip install python-language-server flake8 bandit
