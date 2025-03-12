{
  deploy-rs,
  nixosConfigurations,
}: {
  pallas = {
    hostname = "pallas";
    profiles.system = {
      user = "root";
      path =
        deploy-rs.lib.aarch64-linux.activate.nixos
        nixosConfigurations.pallas;
    };
  };
}
