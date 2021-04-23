##
# dotfiles setup
# TODO adjust XDG_CONFIG_HOME variable to make this simpler?

CONFIG=$(HOME)/.config-test
DOOM=$(HOME)/.doom.d
NIXPKGS_HOST=$(PWD)/nix/hosts/mir3/nixpkgs
NIXPKGS_TARGET=$(CONFIG)/nixpkgs
LINUX_IGNORE=hammerspoon
MAC_IGNORE=bspwm|polybar|sxhkd

linux: config nix emacs
	stow -n -t $(CONFIG) --ignore="$(LINUX_IGNORE)" config

mac: config emacs
	stow -n -t $(CONFIG) --ignore="$(MAC_IGNORE)" config

nix: 
	mkdir -p $(NIXPKGS_TARGET)
	stow -n -d $(NIXPKGS_HOST) -t $(NIXPKGS_TARGET) 
	ln -s $(NIXPKGS_HOST)/configuration.nix /etc/nixos/configuration.nix

emacs:
	mkdir -p $(DOOM)
	stow -n -t $(DOOM) doom.d

config:
	mkdir -p $(CONFIG)
	stow -t $(CONFIG) --ignore="$(LINUX_IGNORE)" config

.PHONY: nix emacs config

# end
