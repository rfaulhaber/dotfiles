#!/usr/bin/env bash
set -euo pipefail

KEY=$(pass wallpaper)
API="https://api.unsplash.com/photos/random/?client_id=$KEY&orientation=landscape&query=desktop-wallpapers"
LOG_FILE=~/.wallpaper-log.txt

dir=$(mktemp -d)

req=$(curl $API)
id=$(echo $req | jq -r '.id')
img_url=$(echo $req | jq -r '.urls.full')
desc=$(echo $req | jq -r '.description // .alt_description')
html_link=$(echo $req | jq -r '.links.html')

download_url=$(echo $img_url)
timestamp=$(date '+%Y/%m/%d %H:%M:%S')
filename="$dir/$id.jpg"

curl -o $filename $download_url

tmp_log=$(mktemp)
jq --arg id "$id" \
	--arg description "$desc" \
	--arg timestamp "$timestamp" \
	--arg download_url "$download_url" \
	--arg html_link "$html_link" \
	'. += [{id: $id, description: $description, timestamp: $timestamp, download_url: $download_url, html_link: $html_link}]' $LOG_FILE >$tmp_log
mv $tmp_log $LOG_FILE

feh --bg-fill $filename
