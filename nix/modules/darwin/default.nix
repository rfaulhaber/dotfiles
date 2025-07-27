{
  config,
  lib,
  pkgs,
  ...
}: {
  config = {
    system.primaryUser = config.user.name;
  };
}
