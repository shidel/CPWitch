#!/bin/sh

function build_flag () {
  local flag=$(grep -i "UNDEF BUILD_${1}" version.def 2>/dev/null)
  if [[ "${flag}" == "" ]] ; then
    echo "NOTICE: ${1} set in project information. ${2}"
	return 0
  fi
  return 1
}

# Prepare for release and cross compiling

build_flag PRERELEASE "Aborted" && exit 1

appver=$(grep 'APP_VERSION' version.pp | cut -d "'" -f 2)
appbld=$(grep 'APP_BUILD' version.pp | cut -d "'" -f 2)

plist=$(grep -i "<string>${appver}.${appbld}</string>" CPWitch.app/Contents/Info.plist 2>/dev/null)

if [[ "${plist}" == '' ]] ; then
  echo "macOS Application Bundle Requires Version information Updated"
  echo "To Version ${appver}.${appbld}"
  exit 1
fi

# cross compile file update
echo "Update Cross-compile source files"
cp -f *.lfm *.pas version.* icons.* xcompile/
cp -f CPWitch.lpr xcompile/CPWitch_Linux.lpr
cp -f CPWitch.lpr xcompile/CPWitch_Windows.lpr

# macOS Bundle
echo 'Update macOS application bundle'
cp -f CPWitch.app/Contents/Info.plist ../bin/macOS/CPWitch.app/Contents/
cp -f CPWitch  ../bin/macOS/CPWitch.app/Contents/MacOS/
cp -f codepages.ini  ../bin/macOS/CPWitch.app/Contents/Resources/
cp -f translations/*.nls ../bin/macOS/CPWitch.app/Contents/Resources/
cp -f 0816norm.uff ../bin/macOS/CPWitch.app/Contents/Resources/
cp -f dictionary.cpw ../bin/macOS/CPWitch.app/Contents/Resources/
cp -f TODO.md ../bin/macOS/

# Linux Release files excluding binary
echo 'Update Linux Package data files'
cp -f codepages.ini  ../bin/Linux/
cp -f translations/*.nls ../bin/Linux/
cp -f 0816norm.uff ../bin/Linux/
cp -f dictionary.cpw ../bin/Linux/
cp -f TODO.md ../bin/Linux/

# Windows Release files excluding binary
echo 'Update Windows Package data files'
cp -f codepages.ini  ../bin/Windows/
cp -f translations/*.nls ../bin/Windows/
cp -f 0816norm.uff ../bin/Windows/
cp -f dictionary.cpw ../bin/Windows/
cp -f TODO.md ../bin/Windows/

build_flag DEBUG
build_flag PRERELEASE
build_flag PATCHED
build_flag PRIVATE
build_flag SPECIAL
echo "Version ${appver}.${appbld}"

exit 0