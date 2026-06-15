# zoxide and carapace are sourced at the top level of the generated wrapper
# (nix/lib/configs/nushell.nix), not here: sourcing them from inside this
# host-matched file confines their `def`/`alias` definitions to the block.
