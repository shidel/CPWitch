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

build_flag DEBUG
build_flag PRERELEASE
build_flag PATCHED
build_flag PRIVATE
build_flag SPECIAL

echo "Version ${appver}.${appbld}"
version="${appver}.${appbld}"

function create_downloads () {
	pushd ../bin >/dev/null || exit 1
	rm *.zip 2>&1 >/dev/null
	zipdir * || exit 1
	mkdir "../releases/v${version}" || exit 1
	mv Linux.zip ../releases/v${version}/CPWitch_Linux-${version}.zip || exit 1
	mv macOS.zip ../releases/v${version}/CPWitch_macOS-${version}.zip || exit 1
	mv Windows.zip ../releases/v${version}/CPWitch_Windows-${version}.zip || exit 1
	popd >/dev/null || exit 1
}

function make_notes () {
notes=$(while read -r line ; do
  [[ "${line:0:1}" == '#' ]] && continue
  [[ "${line}" == "" ]] && continue
  echo "	<li>${line/\* }</li>"
done<NOTES.md
);
echo "<h2>Preview ${version}</h2>"
echo
echo "<ul>"
echo "${notes}"
echo "</ul>"
echo

}

function create_notes () {
  make_notes >../releases/v${version}/CPWitch_Linux-${version}.notes
  make_notes >../releases/v${version}/CPWitch_macOS-${version}.notes
  make_notes >../releases/v${version}/CPWitch_Windows-${version}.notes
}

create_downloads || exit 1
create_notes || exit 1

exit 0
