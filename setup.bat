@echo off

:: Check for admin rights
net session >nul 2>&1
if %errorlevel% neq 0 (
  powershell -Command "Start-Process '%~f0' -Verb RunAs"
  exit /b
)

setlocal

:: Check for essential programs.
call :checkProgram powershell
call :checkProgram git
call :checkProgram choco
call :checkProgram pip3
call :checkProgram rustup
call :checkProgram rustc
call :checkProgram cargo

:: Copy Hunspell dicts.
mkdir "C:\Hunspell"
xcopy "%~dp0\dicts\*.aff" "C:\Hunspell\" /Y
xcopy "%~dp0\dicts\*.dic" "C:\Hunspell\" /Y

:: Install dependencies.
:: llvm also contains clangd and clang-format.
choco install -y ripgrep ag shellcheck cppcheck hunspell.portable llvm
pip3 install python-lsp-server flake8 bandit
rustup component add rustfmt rust-analysis rust-src

pause
exit /b

:checkProgram
where %1 >nul 2>&1
if %errorlevel%==0 (
  echo %1 is installed!
) else (
  echo %1 is not installed!
  pause
  exit 1
)
