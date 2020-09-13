##
# dotfiles setup

MAC_CONFIG_DIRS := alacritty nvim tmux
CWD := $(shell pwd)
TR_SPLIT := tr ' ' '\n'

linux: config emacs
	for dir in ./config/*; do ln -fs $(CWD)/config/$$(basename $$dir) $(HOME)/.config/$$(basename $$dir); done

mac: config emacs
	echo $(MAC_CONFIG_DIRS) | $(TR_SPLIT) |  xargs -I {} ln -s $(CWD)/config/{} $(HOME)/.config/{}

emacs:
	ln -fs $(CWD)/doom.d $(HOME)/.doom.d

config:
	mkdir -p $(HOME)/.config


.PHONY: emacs config

# end
