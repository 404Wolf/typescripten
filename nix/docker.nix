{ dockerTools, buildEnv, app, lib, static ? false }:
if static then (dockerTools.buildImage {
  name = "compiler-static";
  tag = "latest";
  copyToRoot = "${app}/bin";
  config = {
    Entrypoint = [ "/${app.meta.mainProgram}" "-" ];
    WorkingDir = "/";
  };
}) else (dockerTools.buildImage {
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
})
