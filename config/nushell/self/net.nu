# Uses `ss` to list all actively used ports.
export def list-ports [] {
    ^ss -tulnpH
    | lines
    | where { |line| ($line | str trim) != "" }
    | each { |line|
        let cols = ($line | split row -r '\s+')
        let proto = ($cols | get 0 | str downcase)
        let local = ($cols | get 4)

        # Port is everything after the last ':'.
        let parts = ($local | split row ":")
        let port = ($parts | last | into int)

        # Address is everything before the last ':'.
        let addr = ($parts | drop 1 | str join ":")
        let address = if $addr == "*" or $addr == "0.0.0.0" or $addr == "[::]" or $addr == "" {
            "all"
        } else {
            $addr
        }

        # Process info lives in column 6 when present, formatted as:
        #   users:(("process_name",pid=NNN,fd=N))
        # Extract just the process name from inside the first set of quotes.
        let process = if ($cols | length) > 6 {
            let raw = ($cols | get 6)
            if ($raw | str contains '("') {
                $raw
                | parse --regex '\("(?P<name>[^"]+)"'
                | get 0?.name?
                | default "unknown"
            } else {
                "unknown"
            }
        } else {
            "unknown"
        }

        { protocol: $proto, address: $address, port: $port, process: $process }
    }
    | uniq-by protocol address port process
    | sort-by protocol address port
    | group-by protocol
    | transpose protocol entries
    | each { |row|
        {
            protocol: $row.protocol,
            ports: ($row.entries | select address port process)
        }
    }
}
