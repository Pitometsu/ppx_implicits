#
#  Development Environment Sandbox
#

with import <nixpkgs> {};

let
  sandbox-exec = stdenv.mkDerivation {
    name = "sandbox-exec";
    src = /usr/bin/sandbox-exec;
    unpackPhase = "true";
    buildPhase = "true";
    installPhase = "mkdir -p $out/bin && ln -s $src $out/bin/sandbox-exec";
  };
in

with ocaml-ng.ocamlPackages_4_06; mkShell {
  pname = "sandbox";
  version = "0.0.1";
  src = if lib.inNixShell then null else ./.;

  buildInputs = [
    opam
    cacert
    ocaml
    dune
    utop
    merlin
    coreutils
    less
    ncurses
    which
    gitMinimal
    m4
    perl
  ] ++ stdenv.lib.optional stdenv.isDarwin sandbox-exec;
}
