with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "acc";

  buildInputs = [
    pkgs.ocaml
    pkgs.opam
    pkgs.ocamlPackages.utop
  ];

  shellHook = ''
    export PGDATA=~/.pgsql/data
  '';
}
