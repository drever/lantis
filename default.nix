{ mkDerivation, aeson, base, blaze-html, blaze-markup, bytestring
, directory, either, exceptions, generic-aeson, mtl, pandoc, parsec
, QuickCheck, servant, servant-blaze, servant-js, servant-server
, split, stdenv, text, time, transformers, utf8-string, wai, warp
, yaml
}:
mkDerivation {
  pname = "lantis";
  version = "0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring directory either
    exceptions generic-aeson mtl pandoc parsec QuickCheck servant
    servant-blaze servant-js servant-server split text time
    transformers utf8-string wai warp yaml
  ];
  description = "A bug tracking system";
  license = stdenv.lib.licenses.bsd3;
}
