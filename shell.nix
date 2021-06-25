{ pkgs ? import (fetchTarball "https://nixos.org/channels/nixos-21.05/nixexprs.tar.xz") {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.nodejs
    pkgs.yarn
  ];

  shellHook = ''
    echo "Starting node dev environment"
  '';
}
