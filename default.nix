with import <nixpkgs> {};
let gitignoreSrc = pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      # put the latest commit sha of gitignore Nix library here:
      rev = "c4662e662462e7bf3c2a968483478a665d00e717";
      # use what nix suggests in the mismatch message here:
      sha256 = "sha256:1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
    };
    inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
in
ocamlPackages.buildDunePackage rec {
  pname = "LuaOCaml";
  useDune2 = true;
  version = "0.0";
  src = gitignoreSource ./.;
  minimumOCamlVersion = "4.10";
  buildInputs = with ocamlPackages; [
    utop
    ppx_deriving
    menhir
    sedlex_2
    nodePackages.ocaml-language-server
    merlin
  ];

  buildPhase = "dune build -p ${pname}";

  doCheck = true;
}
