let keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILay6kXYKlXA8p+8fvtKS12TMibEhpQII06GCYjRBRcP"
    ];
in {
  "passwd.age".publicKeys = keys;
  "samba.age".publicKeys = keys;
}
