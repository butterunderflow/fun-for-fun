let
  pkgs = import <nixpkgs> { };
  stdenv = pkgs.stdenv;
in {
  shell = pkgs.mkShell {
    name = "fun-for-fun";
    args = [ "build" ];
    src = [ ./lib ./dune-project ];
    system = builtins.currentSystem;
    buildInputs = with pkgs; [ opam cmake ];
    # fixme: I failed to install package by package manager outside nix, maybe it's bad practice. 
    # shellHooks = ''
    #   opam install . --deps-only --with-test --yes
    #   opam install merlin --yes
    # '';
  };
}
