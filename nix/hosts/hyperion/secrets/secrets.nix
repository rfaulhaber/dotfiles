let
  users = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlCcs8h2PrT3GcOVs6K0IGozqV8yuR945ZDr8eYhqfj ryan@hyperion"
  ];
  hosts = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINxjBe2CxWh5EZ+Ai+zsoVMnUrCdUn+/zyTK2j3uvMw9 host key"
  ];
  keys = users ++ hosts;
in {
  "unsplash.age".publicKeys = keys;
}
