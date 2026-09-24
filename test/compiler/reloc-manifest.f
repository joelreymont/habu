\ reloc-manifest.f - Execute relocation vectors against shipped instruction sequences.
\ Actual image save/relaunch has separate native tests.

require lib/test.f
require test/compiler/reloc-cases.f

package RELOC-MANIFEST-TEST
public

: RUN ( -- )
   T-RESET
   RELOC-CASES:HABU-SIDE
   T-REPORT ;

;package

RELOC-MANIFEST-TEST:RUN
