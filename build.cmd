@echo off
echo %~dp0

PUSHD %~dp0
cls

IF exist Packages\Fake ( echo skipping FAKE download ) ELSE ( 
echo downloading FAKE
REM mklink .\.git\hooks\pre-commit .\pre-commit
"Packages\nuget\nuget.exe" "install" "FAKE" "-OutputDirectory" "Packages" "-ExcludeVersion" "-Prerelease"
"Packages\nuget\nuget.exe" "install" "FSharp.Formatting.CommandTool" "-OutputDirectory" "Packages" "-ExcludeVersion" "-Prerelease"
"Packages\nuget\nuget.exe" "install" "SourceLink.Fake" "-OutputDirectory" "Packages" "-ExcludeVersion"
"Packages\nuget\nuget.exe" "install" "NUnit.Runners" "-OutputDirectory" "Packages" "-ExcludeVersion"
)

SET TARGET="All"
IF NOT [%1]==[] (set TARGET="%1")

"Packages\FAKE\tools\Fake.exe" "build.fsx" "target=%TARGET%"

POPD

REM IF NOT [%1]==[] (set TARGET="%1")
REM "tools\FAKE\tools\Fake.exe" "build.fsx" "target=%TARGET%" %*