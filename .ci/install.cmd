curl -L https://github.com/QB64Official/qb64/releases/download/v2.1/qb64_dev_2022-09-08-07-14-00_47f5044_win-x64.7z -o qb64.7z
curl -L https://www.7-zip.org/a/7zr.exe -o 7zr.exe

7zr.exe x qb64.7z

@REM cd qb64
@REM call setup_win.cmd