# TODO refactor again
# I'm pretty dissatisfied with this module
# Ideally, I'd like to specify systemd modules as a list
# however this leads to a lot of performance and evaluation problems
# so for now it's entirely declarative
{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./modules];
}
