with import <nixpkgs> {};

pkgsStatic.stdenv.mkDerivation rec {
  name = "acc";

  buildInputs = [
    pkgs.ocaml
    pkgs.opam
    pkgs.ocamlPackages.utop
    pkgs.pkg-config
    pkgsStatic.sqlite.dev
  ];

  shellHook = ''
    export PGDATA=~/.pgsql/data
  '';
}
