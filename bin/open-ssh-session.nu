#!/usr/bin/env nu

const ssh_config = "~/.ssh/config"

# Only stanzas that set a User are login targets; the git forges in the same
# file carry key settings alone and would just print a banner and disconnect.
def login-hosts []: nothing -> list<string> {
  $ssh_config
  | path expand
  | open --raw $in
  | split row --regex '(?m)^(?=(?i:host|match)\b)'
  | where { $in =~ '(?i)^host\s' }
  | where { $in =~ '(?im)^\s*user[\s=]' }
  | each { lines | first | str replace --regex '(?i)^host[\s=]+' '' | split row --regex '\s+' }
  | flatten
  | where { $in !~ '[*?!]' }
  | uniq
  | sort
}

def main []: nothing -> nothing {
  let selection = login-hosts
    | to text --no-newline
    | ^noctalia dmenu --prompt "SSH: "
    | default ""

  if $selection != "" {
    # The trailing command only runs when the session is created, so picking
    # a host again re-attaches to the existing connection. Sessions resurrected
    # after a reboot reconnect straight away instead of waiting for ENTER.
    ^ghostty -e zellij attach --create --force-run-commands $"ssh-($selection)" -- ssh $selection
  }
}
