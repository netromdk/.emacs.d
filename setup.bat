@echo off
:: Check for admin rights
net session >nul 2>&1
if %errorlevel% neq 0 (
  powershell -Command "Start-Process '%~f0' -Verb RunAs"
  exit /b
)


:: Copy Hunspell dicts.
mkdir "C:\Hunspell"
xcopy "%~dp0\dicts\*.aff" "C:\Hunspell\" /Y
xcopy "%~dp0\dicts\*.dic" "C:\Hunspell\" /Y

:: llvm also contains clangd and clang-format.
choco install -y ripgrep ag shellcheck cppcheck hunspell.portable llvm
pip3 install python-lsp-server flake8 bandit
rustup component add rustfmt rust-analysis rust-src
