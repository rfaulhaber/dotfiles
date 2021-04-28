#!/usr/bin/env bash
set -euo pipefail

KEY=$(pass wallpaper)
API="https://api.unsplash.com/photos/random/?client_id=$KEY&orientation=landscape&query=desktop-wallpapers"
LOG_FILE=~/.wallpaper-log.txt

dir=$(mktemp -d)

req=$(curl $API)
id=$(echo $req | jq '.id' | tr -d '"')
img_url=$(echo $req | jq '.urls.full')
desc=$(echo $req | jq '.description // .alt_description')

url=$(echo $img_url | tr -d '"')
timestamp=$(date '+%Y/%m/%d %H:%M:%S')
filename="$dir/$id.jpg"

curl -o $filename $url

tmp_log=$(mktemp)
jq --arg id "$id" --arg description "$desc" --arg timestamp "$timestamp" --arg url "$url" '. += [{id: $id, description: $description, timestamp: $timestamp, url: $url}]' $LOG_FILE > $tmp_log
mv $tmp_log $LOG_FILE

feh --bg-fill $filename
