{lib}:
with lib; {
  # Builds the standard `image.*` option set for an OCI service. Each service
  # passes its defaults; users override with services.<svc>.image.{repository,
  # version,digest}. Returns a nested option attrset, NOT a submodule — placed
  # under `image = ociLib.mkImageOptions { ... };` it produces cfg.image.repository,
  # cfg.image.version, cfg.image.digest.
  mkImageOptions = {
    repository,
    version,
    digest ? null,
  }: {
    repository = mkOption {
      type = types.str;
      default = repository;
      description = "Container image repository (registry path), without tag or digest.";
    };
    version = mkOption {
      type = types.str;
      default = version;
      description = ''
        Image tag — typically the upstream version (e.g. "13.0.1", "latest",
        "nightly"). Read by the auto-update tooling to detect newer upstream
        releases. For non-semver compounds (e.g. "14-vectorchord0.4.3-pgvectors0.2.0")
        store the whole tag here; auto-update flags it via digest drift.
      '';
    };
    digest = mkOption {
      type = types.nullOr types.str;
      default = digest;
      example = "sha256:abc123...";
      description = ''
        Optional immutable digest pin appended after the tag as repo:tag@digest.
        Set on top of a moving tag (:nightly, :develop, :main) to snapshot it,
        or alongside a semver pin for defense-in-depth. Podman accepts the
        combined form and pulls by digest, ignoring the tag for resolution.
      '';
    };
  };

  # Render a structured image attrset (cfg.image) to the canonical podman image
  # string. Format: repo:tag[@digest]. Including the tag alongside the digest
  # improves readability of `podman inspect` output without weakening the pin —
  # the digest is what podman resolves against.
  renderImage = img:
    "${img.repository}:${img.version}"
    + optionalString (img.digest != null) "@${img.digest}";

  # Render an image whose GPU variant is expressed as a tag suffix rather than
  # a separate repository (immich's release-cuda / release-openvino scheme).
  # The suffix belongs between version and digest — the digest pins the
  # already-suffixed manifest, so appending it after the digest would be wrong.
  # `gpu` is nullable for CPU-only, and the null has to be short-circuited: an
  # `or` fallback catches a missing attribute, not a null attribute selector.
  mkGpuImage = {
    image,
    gpu,
    suffixes ? {
      nvidia = "-cuda";
      intel = "-openvino";
    },
  }: let
    suffix = optionalString (gpu != null) (suffixes.${gpu} or "");
  in
    "${image.repository}:${image.version}${suffix}"
    + optionalString (image.digest != null) "@${image.digest}";

  # Build a list of `--label=key=value` flags for `virtualisation.oci-containers.
  # containers.<name>.extraOptions`. Surfaces the version we *declared* alongside
  # the module path that produced the container, so `podman ps --filter
  # label=dev.dotfiles.oci.module=immich.postgres` is a one-liner. The standard
  # `org.opencontainers.image.version` label overrides whatever the upstream
  # image self-reported, since that often lies (e.g. lscr.io's `:latest` tags
  # report `version=latest` while pinning a build hash).
  mkImageLabels = {
    module,
    image,
  }:
    [
      "--label=org.opencontainers.image.version=${image.version}"
      "--label=dev.dotfiles.oci.module=${module}"
    ]
    ++ optional (image.digest != null) "--label=dev.dotfiles.oci.digest=${image.digest}";
}
