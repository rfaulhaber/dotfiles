#!/usr/bin/env nu

# adapted from https://raw.githubusercontent.com/mullvad/mullvad-wg.sh/main/mullvad-wg.sh
def main [--out-dir: path, --account-number: string, --private-key: string] {
  let locations = http get https://api.mullvad.net/public/relays/wireguard/v1/
    | get countries
    | each { |country| $country.cities
        | each { |city| $city.relays
            | each { |relay|
                {
                    country_name: $country.name,
                    country_code: $country.code,
                    city_name: $city.name,
                    city_code: $city.code,
                }
                | merge $relay
            }
        }
    }
    | flatten
    | flatten

  mkdir $out_dir

  let dns = "10.64.0.1"
  let public_key = $private_key | wg pubkey | str trim

  let config = http post --full --allow-errors --content-type application/x-www-form-urlencoded https://api.mullvad.net/wg { account: $account_number, pubkey: $public_key }

  if ($config.status > 299) {
    print --stderr $"Mullvad API returned ($config.status): ($config.body)"
    exit 1
  }

  let ip = $config.body | split row "," | first

  $locations
    | par-each { |location|
        $"[Interface]
PrivateKey = ($private_key)
Address = ($ip)
DNS = ($dns)

[Peer]
PublicKey = ($location.public_key)
Endpoint = ($location.ipv4_addr_in):51820
AllowedIPs = 0.0.0.0/0, ::0"
        | save -f $"($out_dir)/($location.hostname).conf"
    }
}
