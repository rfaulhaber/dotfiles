#!/usr/bin/env nu

# Front end for on-demand NetworkManager VPN profiles. NetworkManager's default
# polkit policy lets a local user activate an existing system connection, so
# nothing here needs sudo. Profiles are addressed by connection id; the one a
# bare `up`, `down` or `toggle` acts on comes from $env.VPN_PROFILE.

def default-profile []: nothing -> string {
    $env.VPN_PROFILE? | default "airvpn"
}

# Every VPN profile NetworkManager knows about. Terse output is one colon
# separated row per connection; an inactive profile has empty STATE and DEVICE
# fields.
def profiles []: nothing -> table {
    ^nmcli -t -f NAME,TYPE,STATE,DEVICE connection show
        | lines
        | parse "{name}:{type}:{state}:{device}"
        | where type in ["wireguard" "vpn"]
        | update state {|row| if ($row.state | is-empty) { "down" } else { $row.state } }
}

def is-active [name: string]: nothing -> bool {
    (^nmcli -g GENERAL.STATE connection show $name | str trim) == "activated"
}

def main []: nothing -> table {
    main status
}

# List every VPN profile with its state.
def "main status" []: nothing -> table {
    profiles | select name state device
}

# Bring a profile up; the default profile when no name is given.
def "main up" [name?: string] {
    ^nmcli connection up ($name | default (default-profile))
}

# Take a profile down; the default profile when no name is given.
def "main down" [name?: string] {
    ^nmcli connection down ($name | default (default-profile))
}

# Flip a profile between up and down.
def "main toggle" [name?: string] {
    let profile = $name | default (default-profile)
    if (is-active $profile) {
        main down $profile
    } else {
        main up $profile
    }
}
