@echo off
:: Check for admin rights
net session >nul 2>&1
if %errorlevel% neq 0 (
  powershell -Command "Start-Process '%~f0' -Verb RunAs"
  exit /b
)

:: llvm also contains clangd and clang-format.
choco install -y ripgrep shellcheck hunspell.portable llvm
pip3 install python-lsp-server flake8 bandit
