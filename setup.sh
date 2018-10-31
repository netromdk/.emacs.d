#!/bin/sh

OS=$(uname -s)
case "${OS}" in
  Darwin)
    brew install cquery
    brew install the_silver_searcher
    brew install cppcheck
    brew install shellcheck
    pip install python-language-server
    ;;

  *)
    echo "Unknown how to setup deps for '${OS}'!"
    ;;
esac
