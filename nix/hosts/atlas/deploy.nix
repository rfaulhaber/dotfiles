{
  deploy-rs,
  nixosConfigurations,
}: {
  atlas = {
    hostname = "atlas";
    profiles.system = {
      user = "root";
      path =
        deploy-rs.lib.x86_64-linux.activate.nixos
        nixosConfigurations.atlas;
    };
  };
}
