#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.aeson ])"

main = do
  putStrLn "Hello world from a distributable Haskell script!"
