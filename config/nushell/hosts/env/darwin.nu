# add zoxide
if (which zoxide | length) > 0 {
    zoxide init nushell | save -f ~/.zoxide.nu
} else {
    print 'zoxide not installed'
}

# should be low in the PATH
$env.PATH = (
          $env.PATH
          | split row (char esep)
          | append '/usr/local/bin'     # /usr/local/bin doesn't exist by default on macOS, but I need it
          | prepend '/opt/homebrew/bin' # add homebrew to PATH
          | prepend $'($env.HOME)/.nix-profile/bin' # add nix profile to PATH
)

# this is a port of the nix-daemon.sh script that's supposed to get autoloaded
let nix_link = $"/nix/var/nix/profiles/default"
$env.NIX_LINK = $nix_link
$env.NIX_LINK_NEW = $nix_link

$env.NIX_PROFILES = $"/nix/var/nix/profiles/default ($nix_link)"

$env.NIX_SSL_CERT_FILE = "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"

$env.PATH = ($env.PATH | split row (char esep) | append $"($nix_link)/bin")

## set up carapace

if (which carapace | length) > 0 {
  mkdir ~/.cache/carapace
  carapace _carapace nushell | save --force ~/.cache/carapace/init.nu
} else {
  print "carapace not installed"
}




