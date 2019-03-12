{ pkgs ? import <nixpkgs> {} }:
let
  easy-purescript-nix = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "993f63359b64db080061b274e4688e3b80c4f68e";
    sha256 = "18b7fmmxkg38y1av9kfgcv2rikdlji51ya5b9p7sy3aml2hprmi5";
  });
in
  pkgs.mkShell
    {
      inputsFrom = [easy-purescript-nix];
      buildInputs =
        [ pkgs.nodejs-10_x
          pkgs.openjdk10
          pkgs.sbt
          pkgs.yarn
        ];
    }
