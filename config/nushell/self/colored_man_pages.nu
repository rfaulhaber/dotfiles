export def main [
    --theme: record, # Theme you want to color the manpage with. Should be a record with attributes mb, md, me, so, se, us, ue, which correspond to LESS_TERMCAP variables
    ...args # Arguments passed to `man`
] {

    # we create a private extern for `man` so we can pass it a list of strings later
    extern man [...args: string]

    let colors = if $theme != null {
        $theme
    } else {
        {
            mb: (ansi red_bold),
            md: (ansi red_bold),
            me: (ansi reset)
            so: (ansi { fg: 'yellow_bold', bg: 'blue', attr: 'b' })
            se: (ansi reset),
            us: (ansi green_bold),
            ue: (ansi reset)
        }
    }

    let less_env = ($colors | columns | reduce -f {} { |c, all| $all | insert $"LESS_TERMCAP_($c)" ($colors | get $c)  })

    with-env $less_env { ^man $args }
}
