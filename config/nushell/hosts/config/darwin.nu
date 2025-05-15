if ("~/.cache/carapace/init.nu" | path exists) {
  source ~/.cache/carapace/init.nu
}
# the following is adapted from the home-manager implementation of direnv support for nushell
# which seems to be better than the standard hook
# see https://github.com/nix-community/home-manager/blob/098e365dd83311cc8236f83ea6be42abb49a6c76/modules/programs/direnv.nix#L177-L206
# ideally I would just use home-manager for this on macOS, but, alas...
$env.config.hooks.pre_prompt = (
    $env.config.hooks.pre_prompt?
    | default []
    | append {||
        ^direnv export json
        | from json --strict
        | default {}
        | items {|key, value|
            let value = do (
                {
                  "path": {
                    from_string: {|s| $s | split row (char esep) | path expand --no-symlink }
                    to_string: {|v| $v | path expand --no-symlink | str join (char esep) }
                  }
                }
                | merge ($env.ENV_CONVERSIONS? | default {})
                | get -i $key
                | get -i from_string
                | default {|x| $x}
            ) $value
            return [ $key $value ]
        }
        | into record
        | load-env
    }
)
