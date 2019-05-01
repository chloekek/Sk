{lib, stdenv, haskellPackages}:
let
    Ghc = haskellPackages.ghcWithPackages (p: [
        p.base
        p.bytestring
        p.cereal
        p.free
        p.lens
        p.pipes
        p.vector
    ]);
in
stdenv.mkDerivation {
    name = "Sk";
    src = lib.cleanSource ./..;
    buildInputs = [Ghc];
    phases = ["unpackPhase" "buildPhase" "installPhase" "fixupPhase"];
    buildPhase = ''
        GhcFlags=(
            -O
            -Wall -Wincomplete-record-updates -Wincomplete-uni-patterns -Werror
        )
        HaskellSources=(
            Support/GArrow.hs
            Support/FreeCategory.hs
            Support/FreeGArrow.hs
            Sk/Sample.hs
            Sk/Funnel/Parse.hs
            Sk/Funnel/Parse/Cereal.hs
            Sk/Funnel.hs
            Sk/Funnel/Pipes.hs
            Sk/System.hs
        )
        for f in "''${HaskellSources[@]}"; do
            ( set -x && ghc -c "''${GhcFlags[@]}" "$f" )
        done
    '';
}
