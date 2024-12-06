let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlCcs8h2PrT3GcOVs6K0IGozqV8yuR945ZDr8eYhqfj ryan@hyperion"
  ];
in {
  "unsplash.age".publicKeys = keys;
  # "samba.age".publicKeys = keys;
}
