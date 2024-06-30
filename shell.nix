{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "dev";
  packages = with pkgs; [
    ansible
  ];
}
