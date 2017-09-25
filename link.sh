#!/usr/bin/env bash

if [ -f $HOME/.Xresources ]; then
	ln $HOME/.Xresources $HOME/Projects/dotfiles
    echo "Linked .Xresources file"
else
	echo "No .Xresources found"
fi

if [ -f $HOME/.vimrc ]; then
	ln $HOME/.vimrc $HOME/Projects/dotfiles
    echo "linked .vimrc"
else
	echo "No .vimrc found"
fi

if [ -f $HOME/.config/i3/config ]; then
	ln $HOME/.config/i3/config $HOME/Projects/dotfiles
    echo "linked i3"
else
	echo "No i3 config file found"
fi

if [[ $OSTYPE == "darwin"* ]]; then
    if [ -d $HOME/Library/Application\ Support/Code ]; then
        ln $HOME/Library/Application\ Support/Code/User/settings.json $HOME/Projects/dotfiles
        echo "linked vscode for mac"
    else 
        echo "No vscode settings found on macOS"
    fi
elif [ $OSTYPE == "linux-gnu" ]; then
    if [ -d $HOME/.config/Code ]; then
        ln $HOME/.config/Code/User/settings.json $HOME/Projects/dotfiles
        echo "linked vscode for linux"
    else
        echo "No vscode settings found on Linux"
    fi    
else
    echo "This operating system, $OSTYPE, is not supported for linking vscode"
fi 
