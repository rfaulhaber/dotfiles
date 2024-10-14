let keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILay6kXYKlXA8p+8fvtKS12TMibEhpQII06GCYjRBRcP ryf@sent.as"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAZQ6dhGnjyJ+SBMeN5IRHcpV6ERR+a/WPmvD7o2TM90 ryan@hyperion"
    ];
in {
  "samba.age".publicKeys = keys;
}
