#!/usr/bin/env bash
set -euo pipefail

KEY=$(pass wallpaper)
API="https://api.unsplash.com/photos/random/?client_id=$KEY&orientation=landscape&collections=wallpapers,nature,architecture,experimental,film,technology,travel,textures-patterns,history"

dir=$(mktemp -d)

req=$(curl $API | jq '.urls.full')

for i in $req
do
    curl -o wallpaper --output-dir $dir $(echo $i | tr -d '"')
done

feh --bg-fill $dir/wallpaper