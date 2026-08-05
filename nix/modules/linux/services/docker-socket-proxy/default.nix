{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.docker-socket-proxy;

  runtimeDir = "docker-socket-proxy";
  proxySocket = "/run/${runtimeDir}/docker.sock";

  # Match a Docker API path section with or without the /vX.Y prefix the
  # moby SDK prepends after version negotiation. url_dec first so a
  # percent-encoded path can't slip past the regex.
  sectionAcl = section: "{ path,url_dec -m reg -i ^(/v[\\d.]+)?/${section} }";

  haproxyConfig = pkgs.writeText "docker-socket-proxy.cfg" ''
    global
      log stdout format raw daemon
      maxconn 32

    defaults
      mode http
      log global
      option httplog
      timeout connect 5s
      timeout client 10m
      timeout server 10m
      timeout http-request 10s

    # haproxy >= 3.3 rejects a frontend and backend sharing a name — keep
    # these distinct.
    frontend docker-api
      # Mode 660 (owner/group are the DynamicUser) blocks unprivileged host
      # users; rootful containers that get this socket bind-mounted connect
      # via CAP_DAC_OVERRIDE.
      bind unix@${proxySocket} mode 660
      # GET/HEAD only (METH_GET matches both) — write access to the Docker
      # API is root on this host, and making that unreachable is the reason
      # this proxy exists. There is deliberately no option to open it up.
      http-request deny unless METH_GET
      # Handshake endpoints every Docker client needs before its first real
      # call; neither exposes container state.
      http-request allow if ${sectionAcl "_ping"}
      http-request allow if ${sectionAcl "version"}
      ${concatMapStringsSep "\n  " (s: "http-request allow if ${sectionAcl s}") cfg.allowedApiSections}
      http-request deny
      ${optionalString (elem "events" cfg.allowedApiSections) ''use_backend podman-events if ${sectionAcl "events"}''}
      default_backend podman

    backend podman
      server socket unix@${cfg.backendSocket}

    ${optionalString (elem "events" cfg.allowedApiSections) ''
      backend podman-events
        # /events is an unbounded stream; the defaults-level server timeout
        # would cut it every 10m and put clients in a reconnect loop. The
        # zero value draws a "missing timeouts" warning at startup — that is
        # expected, infinite is the intent.
        timeout server 0
        server socket unix@${cfg.backendSocket}
    ''}
  '';
in {
  options.modules.services.docker-socket-proxy = {
    enable = mkEnableOption ''
      read-only filtering proxy in front of the rootful podman socket.

      Runs nixpkgs haproxy as a hardened host service listening on a unix
      socket (see socketPath) that can be bind-mounted into containers
      needing Docker API *visibility* — listing containers, watching events —
      without handing them the real socket, which is root-equivalent
      regardless of a :ro mount flag. Only GET/HEAD passes, and only for the
      path sections listed in allowedApiSections.

      Callers should treat the proxy as optional at runtime: it is an
      availability dependency only for the container-listing feature, so
      order After= it but never Requires= it
    '';

    backendSocket = mkOption {
      description = "Rootful podman socket the proxy fronts.";
      type = types.str;
      default = "/run/podman/podman.sock";
    };

    allowedApiSections = mkOption {
      description = ''
        First path components of the Docker API to allow through, matched
        with or without the /vX.Y version prefix (e.g. "containers" admits
        both /containers/json and /v1.51/containers/{id}/json). _ping and
        version are always allowed. Note "containers" includes per-container
        inspect, whose Config.Env carries the container's environment —
        secrets included — so consumers of this proxy read every container's
        credentials: keep the socket's exposure narrow.
      '';
      type = types.listOf types.str;
      default = [];
      example = ["containers" "events"];
    };

    socketPath = mkOption {
      description = "Unix socket the proxy listens on (read-only).";
      type = types.str;
      default = proxySocket;
      readOnly = true;
    };
  };

  config = mkIf cfg.enable {
    systemd.sockets."podman".enable = true;

    systemd.services."docker-socket-proxy" = {
      description = "Read-only filtering proxy for the podman Docker API socket";
      after = ["podman.socket"];
      requires = ["podman.socket"];
      wantedBy = ["multi-user.target"];

      serviceConfig = {
        Type = "notify";
        ExecStart = "${pkgs.haproxy}/bin/haproxy -Ws -f ${haproxyConfig}";
        Restart = "always";
        RestartSec = 2;

        # The proxy is a security boundary: it holds a root-equivalent
        # socket while being exposed to semi-trusted clients, so it gets
        # the full least-privilege treatment. Group podman grants the
        # backend connect (socket is root:podman 0660). The INET families
        # must stay allowed even though every bind here is unix: haproxy
        # 3.4's QUIC self-test (quic_test_socketopts) opens an IP socket
        # unconditionally at startup and treats EAFNOSUPPORT as fatal —
        # AF_UNIX-only kills the daemon before it reads the config.
        # IPAddressDeny provides the no-network property instead, one
        # layer down: socket() succeeds, every packet is dropped.
        DynamicUser = true;
        SupplementaryGroups = ["podman"];
        RuntimeDirectory = runtimeDir;
        UMask = "0077";
        CapabilityBoundingSet = "";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        # connect() needs MAY_WRITE on the backend socket's inode, which
        # ProtectSystem=strict's read-only mount would refuse. Whitelist the
        # directory, not the socket file: podman.socket recreates the inode
        # on restart, and a file-level bind mount would pin the stale one.
        ReadWritePaths = [(dirOf cfg.backendSocket)];
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHostname = true;
        ProtectProc = "invisible";
        RestrictAddressFamilies = ["AF_UNIX" "AF_INET" "AF_INET6"];
        IPAddressDeny = "any";
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = ["@system-service" "~@privileged"];
      };
    };
  };
}
