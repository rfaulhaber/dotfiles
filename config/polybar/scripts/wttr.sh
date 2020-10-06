#!/usr/bin/env bash
set -euo pipefail

format="%C %t"
weather=$(curl -sG wttr.in/Cleveland --data-urlencode "format=$format")
echo -n $weather
