#!/bin/sh
nix-shell --pure \
   -p python \
   -p gnumake which \
   -p 'pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [ optparse-applicative regex-posix ])'
