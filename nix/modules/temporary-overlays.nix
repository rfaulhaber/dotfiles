# Reports every overlay built with lib.my.mkTemporaryOverlay that this host
# applies, whether still needed or due for removal. `warnings` is forced once
# per host evaluation, whatever the overlay patches and whoever consumes it.
{
  lib,
  pkgs,
  ...
}: {
  warnings = lib.my.temporaryOverlayWarnings pkgs;
}
