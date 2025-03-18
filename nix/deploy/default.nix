localFlake: {
  config,
  inputs,
  ...
}: {
  sshUser = "ryan";
  autoRollback = true;
  magicRollback = true;
  nodes.atlas = import ../hosts/atlas/deploy.nix {
    inherit (inputs) deploy-rs;
    inherit (config) nixosConfigurations;
  };
  nodes.pallas = import ../hosts/pallas/deploy.nix {
    inherit (inputs) deploy-rs;
    inherit (config) nixosConfigurations;
  };
}
