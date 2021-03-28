##
# dotfiles setup

CONFIG=$(HOME)/.config
LINUX_IGNORE=hammerspoon
MAC_IGNORE=bspwn|polybar|sxhkd

linux: config emacs
	stow -n -t $(CONFIG) --ignore="$(LINUX_IGNORE)" config

mac: config emacs
	stow -n -t $(CONFIG) --ignore="$(MAC_IGNORE)" config

# nix: 
# 	cd ./nix/hosts/mir3
# 	stow -n -t $(CONFIG)/nixpkgs nixpkgs

emacs:
	mkdir -p $(HOME)/.doom.d
	stow -n -t $(HOME)/.doom.d doom.d

config:
	mkdir -p $(HOME)/.config
	stow -n -t $(HOME)/.config config

.PHONY: nix emacs config

# end
