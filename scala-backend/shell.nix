{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell
    {
      buildInputs =
        [ pkgs.openjdk10
          pkgs.sbt
        ];
    }
