with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "acc";

  buildInputs = [
    pkgs.ocaml
    pkgs.opam
    pkgs.ocamlPackages.utop
    pkgs.pkg-config
    pkgs.sqlite
  ];

  shellHook = ''
    export PGDATA=~/.pgsql/data
  '';
}
