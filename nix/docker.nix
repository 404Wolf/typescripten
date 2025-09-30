{ dockerTools, buildEnv, app, lib }:
dockerTools.buildImage {
  name = "compiler";
  tag = "latest";
  copyToRoot = buildEnv {
    name = "compiler";
    paths = [ app ];
    pathsToLink = [ "/bin" ];
  };
  config = {
    Entrypoint = [ (lib.getExe app) "-" ];
    WorkingDir = "${app}";
  };
}
