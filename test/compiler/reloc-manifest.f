\ Execute relocation vectors against the shipped instruction sequences.
\ test/compiler/reloc-proof.f separately checks the shared rows and statements
\ against the Rocq model. Actual image save/relaunch has its own native tests.

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
