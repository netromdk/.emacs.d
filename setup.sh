#!/bin/sh

# OS specific
OS=$(uname -s)
case "${OS}" in
  Darwin)
    brew install cquery the_silver_searcher cppcheck shellcheck
    ;;

  *)
    echo "Unknown how to setup deps for '${OS}'!"
    ;;
esac

# General
pip install python-language-server flake8 bandit
