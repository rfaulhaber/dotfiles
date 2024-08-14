let
  hyperion = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGl7rrcCvWXaq4yDf8YbpYNYszZX8YQr/Yftr8EdxLbd ryan@hyperion";
in {
  "passwd.age".publicKeys = [hyperion];
}
