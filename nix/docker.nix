{ pkgs, app }:
pkgs.dockerTools.buildImage {
  name = "compiler";
  tag = "latest";
  copyToRoot = pkgs.buildEnv {
    name = "compiler";
    paths = [ app ];
    pathsToLink = [ "/bin" ];
  };
  config = {
    Entrypoint = [ "compiler" "-" ];
    WorkingDir = "${app}";
  };
}