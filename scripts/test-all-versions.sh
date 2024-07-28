#/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

CLAIRNOTE=sn ./test.py -o stable-sn -l "$(nix build ..#lilypond-2-24 -v --no-link --print-out-paths)/bin/lilypond"
CLAIRNOTE=dn ./test.py -o stable-dn -l "$(nix build ..#lilypond-2-24 -v --no-link --print-out-paths)/bin/lilypond"

CLAIRNOTE=sn ./test.py -o unstable-sn -l "$(nix build ..#lilypond-2-25 -v --no-link --print-out-paths)/bin/lilypond"
CLAIRNOTE=dn ./test.py -o unstable-dn -l "$(nix build ..#lilypond-2-25 -v --no-link --print-out-paths)/bin/lilypond"
