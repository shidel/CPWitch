#!/bin/bash

#   Copyright (c) 2025-2026 Jerome Shidel
#   The Clear BSD License
#   All rights reserved.

#   This script is part of the MPLA application framework available at:
#   https://gitlab.com/mpla-oss/mpla/

# for macOS and Linux

[[ "$(uname)" == "Darwin" ]] && macOS=TRUE || unset macOS

if [[ ! -x mpla/version.sh ]] ; then
  echo 'Could not locate the MPLA Application Framework Library.'
  exit 1
fi

function get_attribute () {

  local atr="${1}"
  local line="${2}"
  local a

  while [[ ${#line} -gt 0 ]] ; do
    a="${line%%=*}"
    line="${line:$((${#a} + 1))}"
    a="${a// }"
    line="${line#*\"}"
    if [[ "${a}" == "${atr}" ]] ; then
      line="${line%%\"*}"
      break
    fi
    line="${line#*\"}"
  done

  [[ ${#line} -eq 0 ]] && return 1
  echo "${line}"
  return 0

}

function get_key() {

  if [[ "${1}" == '-e' ]] ; then
    local exact=true
    shift
  else
    local exact=
  fi

  local line ckey tkey
  local atr="${1##*/}"
  local key="${1%/*}"
  local xml=$(<${XML_FILE})
  local xml="${xml//$'\n'}"
  local x=''
  while [[ "${x}" != "${xml}" ]] ; do
    x="${xml}"
    xml="${xml// </<}"
  done
  xml="${xml//</}"
  xml="${xml//>/$'\n'}"

  while IFS=''; read -r line ; do
    [[ "${line:0:1}" == '?' ]] && continue;
    if [[ "${line:0:1}" == '/' ]] ; then
      if [[ "${ckey:$((${#ckey}-${#line}))}" != "${line}" ]] ; then
        echo "XML parsing error" >&2
        return 1
      fi
      ckey="${ckey:0:$((${#ckey}-${#line}))}"
      continue
    fi;
    ckey="${ckey}/${line%% *}"
    tkey=
    if [[ ${exact} ]] ; then
      [[ "${ckey}" == "${key}" ]] && tkey=true
    else
      [[ "${ckey:$((${#ckey}-${#key}))}" == "${key}" ]] && tkey=true
    fi
    if [[ $tkey ]] ; then
      get_attribute "${atr}" "${line#* }"
      return $?
    fi
    if [[ "${line:$((${#line}-1))}" == "/" ]] ; then
      ckey="${ckey%\/*}"
    fi;
  done<<<"${xml}"

}

function existcopy () {
  local f
  while [[ ${#1} -ne 0 ]] ; do
    f="${1}"
    shift
    [[ ! -e "${f}" ]] && continue
    # if [[ -e "${targetdir}${f##*/}" ]] ; then
    #      continue
    # fi
    cp -fa "${f}" "${targetdir}"
    if [[ $? -ne 0 ]] ; then
      echo "Failed to copy file to target directory."
      exit 1
    fi
  done
}

APP=''
for i in *.lpr *.LPR ; do
  [[ "${i}" == '*.lpr' ]] && continue
  [[ "${i}" == '*.LPR' ]] && continue
  APP="${i}"
  break
done

APP="${APP%.*}"

if [[ "${APP}" == '' ]] ; then
  echo 'Could not determine application'
  exit 1;
fi

if [[ -e "${APP}" ]] ; then
  rm "${APP}"
  if [[ $? -ne 0 ]] ; then
    echo "Failed to remove old compiled binary."
    exit 1
  fi
fi

./mpla/cleanup.sh || exit 1
./mpla/version.sh || exit 1
./mpla/icons.sh || exit 1

if [[ ${macOS} ]] ; then
  targetdir='./'
  target="${APP}"
  lazbuild "${APP}.lpi"
else
  if [[ -d xcompile ]] ; then
    cp -a *.pas *.pp *.lfm *.def *.lrs xcompile/
    pushd xcompile >/dev/null
    XML_FILE="${APP}_Linux.lpi"
    target=$(get_key Target/Filename/Value)
    if [[ "${target}" == '' ]] ; then
      target="${APP}"
    fi
    targetdir="${target%/*}"
    if [[ ! -d "${targetdir}" ]] ; then
      mkdir -p "${targetdir}"
      if [[ $? -ne 0 ]] ; then
        echo "Failed to create target directory for binary."
        exit 1
      fi
    fi
    [[ "${targetdir}" == '' ]] && targetdir='.'
    targetdir="${targetdir}/"
    if [[ -e "${target}" ]] ; then
      rm "${target}"
      if [[ $? -ne 0 ]] ; then
        echo "Failed to remove old compiled binary."
        exit 1
      fi
    fi
    lazbuild "${APP}_Linux.lpi"
    popd >/dev/null
    if [[ "${targetdir:0:3}" == '../' ]] ; then
      targetdir="${targetdir:3}"
    elif [[ "${targetdir:0:2}" == './' ]] ; then
      targetdir="xcompile/${targetdir:2}"
    elif [[ "${targetdir:0:2}" != '/' ]] ; then
      targetdir="xcompile/${targetdir}"
    fi
  else
    echo "Unable to compile ${APP} on this OS."
  fi
fi

if [[ ! -x "${targetdir}${APP}" ]] ; then
  echo 'Compile failed.'
  exit 1
fi

echo
echo "Compiled new binary to: ${targetdir}"

if [[ ! ${macOS} ]] && [[ "${targetdir}" != './' ]] ; then
  existcopy 0816norm.uff
  existcopy codepages.ini
  existcopy TODO.md
  existcopy NOTES.md
  existcopy translations/*.nls
  existcopy master.cpw
  existcopy ../manual
fi

echo 'Done.'
exit 0

