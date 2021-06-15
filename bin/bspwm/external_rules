#!/usr/bin/env bash

wid=$1
desktop=$4
title="$(xtitle $wid)"

if echo $title | grep "emacs-everywhere"; then
    echo "desktop=focused state=floating locked=off"
fi
