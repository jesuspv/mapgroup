#!/run/current-system/sw/bin/bash
nix-shell --pure \
   -p gnumake which \
   -p 'pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [ optparse-applicative regex-posix ])'
