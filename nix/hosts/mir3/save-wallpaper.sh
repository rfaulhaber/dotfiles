#!/usr/bin/env bash
set -euo pipefail

match=$(egrep -o '/tmp/tmp\.\w+\/wallpaper' < ~/.fehbg)
id=$(uuidgen | tr -d '-')

cp $match ~/Pictures/wallpapers/$id
