##
# dotfiles setup

MAC_CONFIG_DIRS := alacritty nvim tmux
CWD := $(shell pwd)
TR_SPLIT := tr ' ' '\n'

linux: nix config emacs
	for dir in ./config/*; do ln -fs $(CWD)/config/$$(basename $$dir) $(HOME)/.config/$$(basename $$dir); done

mac: config emacs
	echo $(MAC_CONFIG_DIRS) | $(TR_SPLIT) |  xargs -I {} ln -s $(CWD)/config/{} $(HOME)/.config/{}

nix: 
	ln -sf $(CWD)/nix/hosts/mir3/nixpkgs/config.nix $(HOME)/.config/nixpkgs/config.nix
	ln -sf $(CWD)/nix/hosts/mir3/nixpkgs/home.nix $(HOME)/.config/nixpkgs/home.nix

emacs:
	ln -fs $(CWD)/doom.d $(HOME)/.doom.d

config:
	mkdir -p $(HOME)/.config


.PHONY: nix emacs config

# end
