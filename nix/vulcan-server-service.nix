# NixOS module for configuring Vulcan Server service.
{ config, lib, pkgs, ... }:
let
  cfg = config.services.vulcan-server;
in
with lib; {
  options.services.vulcan-server = with types; {
    enable = mkEnableOption "Vulcan Smart Contracts server";

    package = mkOption {
      description = "Vulcan Smart Contracts package";
      type = package;
      default = pkgs.vulcan-server;
    };
  };

  config = mkIf cfg.enable {
    assertions = [];

    users.users.vulcan = {
      isSystemUser = true;
      group = "vulcan";
    };
    users.groups.vulcan = { };

    systemd.services.vulcan-server = {
      enable = true;
      after = [];
      wantedBy = [ "multi-user.target" ];

      script = escapeShellArgs (concatLists [
        [ "${cfg.package}/bin/vulcan-server" ]
      ]);

      serviceConfig = {
        User = "vulcan";
        Group = "vulcan";
        # Security
        UMask = "0077";
        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        ProcSubset = "pid";
        ProtectProc = "invisible";
        NoNewPrivileges = true;
        DevicePolicy = "closed";
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        PrivateUsers = true;
        ProtectHostname = true;
        ProtectClock = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" ];
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;
        PrivateMounts = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [ "~@cpu-emulation @debug @keyring @mount @obsolete @privileged @setuid @resources" ];
        MemoryDenyWriteExecute = true;
      };
    };
  };
}