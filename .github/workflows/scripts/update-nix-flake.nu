nix flake metadata --json | save -f before.json
nix flake update 
nix flake metadata --json | save -f after.json

let root_nodes = open before.json | get locks.nodes.root.inputs | values
let before = open before.json | get locks.nodes
let after = open after.json | get locks.nodes

mut results = []

for root_node in $root_nodes {
    let before_node = $before | get $root_node
    let after_node = $after | get $root_node

    if $before_node.locked.rev != $after_node.locked.rev {
        $results = $results | append {
            "input": $"**($root_node)**"
            "previous rev": $"($before_node.locked.rev) \(($before_node.locked.lastModified | into datetime -f '%s' | format date '%Y-%m-%d %H:%M:%S%:z')\)" 
            "current rev": $"($after_node.locked.rev) \(($after_node.locked.lastModified | into datetime -f '%s' | format date '%Y-%m-%d %H:%M:%S%:z')\)" 
        }
    }
}

$results | to md | save -f output
