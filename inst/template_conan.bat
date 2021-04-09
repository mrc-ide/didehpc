@echo off
ECHO generated on host: {{hostname}}
ECHO generated on date: {{date}}
ECHO didehpc version: {{didehpc_version}}
ECHO conan version: {{conan_version}}
ECHO running on: %COMPUTERNAME%

call setr64_{{r_version}}.bat

{{network_shares_create}}

ECHO Using Rtools at {{rtools$rtools_root}}
set PATH={{rtools$gcc_path}};{{rtools$make_path}};%PATH%
set BINPREF={{rtools$binpref}}/

set CONAN_PATH_BOOTSTRAP={{conan_path_bootstrap}}
set CONAN_PATH_CACHE={{context_root}}\conan\cache

set CONAN_ID={{{{conan_id}}}}

set CONAN_LOGFILE={{context_root}}\conan\log\%CONAN_ID%
ECHO logfile: %CONAN_LOGFILE%

{{context_workdrive}}
cd \{{context_workdir}}
ECHO working directory: %CD%

ECHO on
Rscript "{{context_root}}\conan\bin\%CONAN_ID%" "{{r_libs_user}}" > "%CONAN_LOGFILE%" 2>&1

@ECHO off
%SystemDrive%
set ErrorCode=%ERRORLEVEL%

{{network_shares_delete}}

set ERRORLEVEL=%ErrorCode%

if %ERRORLEVEL% neq 0 (
  ECHO Error running conan
  EXIT /b %ERRORLEVEL%
)

@ECHO Quitting
