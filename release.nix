let 
  pkgs = import <nixpkgs> { };
in
  { lantis = pkgs.haskellPackages.callPackage ./default.nix { };
}
