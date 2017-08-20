#!/usr/bin/env bash

if [ -f $HOME/.Xresources ]; then
	ln $HOME/.Xresources $HOME/Projects/dotfiles
else
	echo "No .Xresources found"
fi

if [ -f $HOME/.vimrc ]; then
	ln $HOME/.vimrc $HOME/Projects/dotfiles
else
	echo "No .vimrc found"
fi

if [ -f $HOME/.config/i3/config ]; then
	ln $HOME/.config/i3/config $HOME/Projects/dotfiles
else
	echo "No i3 config file found"
fi
