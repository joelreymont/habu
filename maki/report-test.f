\ maki/report-test.f - checked tests for the Model CAD report schema (cad-0a).

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/report.f

package MAKI

\ ---- render-output containment helper --------------------------------------
variable RVA  variable RVU
: RSAVE ( ptr u8 n -- )  RVU ! RVA ! ;
: HAS ( ptr u8 n -- )  RVA @ RVU @ 2swap CONTAINS? TTRUE ;
: HASNT ( ptr u8 n -- )  RVA @ RVU @ 2swap CONTAINS? TFALSE ;

\ ---- fail-closed probes (top-level cannot push quotations) ------------------
: BAD-ROOF     ( -- )  REPORT:NEW 99 REPORT:ROOFLINE! drop ;
: BAD-COAL     ( -- )  REPORT:NEW s" x" 9 REPORT:HOT+ drop ;
: BAD-GATE     ( -- )  REPORT:NEW s" r" V-PASS 9 REPORT:GATE! drop ;
: BAD-VERDICT  ( -- )  REPORT:NEW s" r" 9 G-CERTIFY REPORT:GATE! drop ;
: BAD-SPLIT-IX ( -- )  REPORT:NEW s" a" REPORT:SPLIT+ 5 REPORT:SPLIT@ 2drop ;
: BAD-SELECT   ( -- )  REPORT:NEW s" a" REPORT:CAND+ 4 REPORT:SELECT! drop ;
: FULL-SPLIT   ( -- )  REPORT:NEW 20 0 ?do s" x" REPORT:SPLIT+ loop drop ;

T-RESET

\ ---- fresh report: honest defaults -----------------------------------------
REPORT:NEW dup REPORT:OPS-BEFORE@ 0 T=
REPORT:NEW dup REPORT:REGIONS@ 0 T=
REPORT:NEW dup REPORT:BYTES-KNOWN? TFALSE
REPORT:NEW dup REPORT:ROOFLINE@ RC-UNKNOWN T=
REPORT:NEW dup REPORT:SELECT@ -1 T=
REPORT:NEW dup REPORT:SPLIT-COUNT 0 T=
REPORT:NEW dup REPORT:WARN-COUNT 0 T=
REPORT:NEW dup REPORT:CAND-COUNT 0 T=
REPORT:NEW dup REPORT:HOT-COUNT 0 T=
REPORT:NEW dup REPORT:PROF-COUNT 0 T=
REPORT:NEW dup G-GOLDEN REPORT:GATE-TAG@ V-NOTRUN T=
drop drop drop drop drop drop drop drop drop drop drop

\ ---- key setters / getters -------------------------------------------------
REPORT:NEW
   s" FFN"       REPORT:MODEL!
   s" b1s128d64" REPORT:SHAPE!
   s" f32"       REPORT:DTYPE!
   s" row-major" REPORT:LAYOUT!
   s" sm_87"     REPORT:TARGET!
   s" ffn|sm_87" REPORT:CACHE!
dup REPORT:MODEL$  s" FFN"       T$=
dup REPORT:SHAPE$  s" b1s128d64" T$=
dup REPORT:DTYPE$  s" f32"       T$=
dup REPORT:LAYOUT$ s" row-major" T$=
dup REPORT:TARGET$ s" sm_87"     T$=
dup REPORT:CACHE$  s" ffn|sm_87" T$=
drop

\ unset key reads back empty
REPORT:NEW dup REPORT:MODEL$ nip 0 T= drop

\ ---- fusion-plan scalars ---------------------------------------------------
REPORT:NEW  6 3 REPORT:OPS!  3 REPORT:REGIONS!  3 REPORT:MATERIALIZED!
dup REPORT:OPS-BEFORE@  6 T=
dup REPORT:OPS-AFTER@   3 T=
dup REPORT:REGIONS@     3 T=
dup REPORT:MATERIALIZED@ 3 T=
drop

\ ---- memory bytes + known flag ---------------------------------------------
REPORT:NEW  1024 512 REPORT:BYTES!
dup REPORT:BYTES-KNOWN?  TTRUE
dup REPORT:BYTES-BEFORE@ 1024 T=
dup REPORT:BYTES-AFTER@  512  T=
drop

\ ---- roofline (+ fail closed) ----------------------------------------------
REPORT:NEW RC-MEMORY REPORT:ROOFLINE! dup REPORT:ROOFLINE@ RC-MEMORY T= drop
' BAD-ROOF E-RPT-ROOF TTHROWS

\ ---- split reasons (+ index fail closed) -----------------------------------
REPORT:NEW s" fuse-barrier" REPORT:SPLIT+ s" reg-pressure" REPORT:SPLIT+
dup REPORT:SPLIT-COUNT 2 T=
dup 0 REPORT:SPLIT@ s" fuse-barrier" T$=
dup 1 REPORT:SPLIT@ s" reg-pressure" T$=
drop
' BAD-SPLIT-IX E-RPT-IDX TTHROWS
' FULL-SPLIT   E-RPT-FULL TTHROWS

\ ---- warnings --------------------------------------------------------------
REPORT:NEW s" phase0-estimates" REPORT:WARN+
dup REPORT:WARN-COUNT 1 T=
dup 0 REPORT:WARN@ s" phase0-estimates" T$=
drop

\ ---- schedule candidates + selection (+ fail closed) -----------------------
REPORT:NEW s" elementwise-v1" REPORT:CAND+ s" gemm-tf32-v1" REPORT:CAND+  1 REPORT:SELECT!
dup REPORT:CAND-COUNT 2 T=
dup 0 REPORT:CAND@ s" elementwise-v1" T$=
dup 1 REPORT:CAND@ s" gemm-tf32-v1"   T$=
dup REPORT:SELECT@ 1 T=
drop
' BAD-SELECT E-RPT-IDX TTHROWS

\ ---- hot tensors + coalescing (+ fail closed) ------------------------------
REPORT:NEW s" x" CO-COALESCED REPORT:HOT+ s" w" CO-STRIDED REPORT:HOT+
dup REPORT:HOT-COUNT 2 T=
dup 0 REPORT:HOT-NAME@ s" x" T$=
dup 0 REPORT:HOT-STATUS@ CO-COALESCED T=
dup 1 REPORT:HOT-STATUS@ CO-STRIDED   T=
drop
' BAD-COAL E-RPT-COAL TTHROWS

\ ---- profile rows ----------------------------------------------------------
REPORT:NEW s" saxpy" 42 REPORT:PROF+
dup REPORT:PROF-COUNT 1 T=
dup 0 REPORT:PROF-NAME@ s" saxpy" T$=
dup 0 REPORT:PROF-US@ 42 T=
drop

\ ---- gate verdicts (+ fail closed) -----------------------------------------
REPORT:NEW
   s" "         V-PASS   G-CERTIFY   REPORT:GATE!
   s" no-device" V-NOTRUN G-GOLDEN   REPORT:GATE!
dup G-CERTIFY REPORT:GATE-TAG@ V-PASS   T=
dup G-GOLDEN  REPORT:GATE-TAG@ V-NOTRUN T=
dup G-GOLDEN  REPORT:GATE-REASON@ s" no-device" T$=
drop
' BAD-GATE    E-RPT-GATE    TTHROWS
' BAD-VERDICT E-RPT-VERDICT TTHROWS

\ ---- machine render: line-oriented key:value -------------------------------
REPORT:NEW
   s" FFN"   REPORT:MODEL!
   s" sm_87" REPORT:TARGET!
   6 3 REPORT:OPS!  3 REPORT:REGIONS!  3 REPORT:MATERIALIZED!
   s" fuse-barrier" REPORT:SPLIT+
   s" x" CO-COALESCED REPORT:HOT+
   s" elementwise-v1" REPORT:CAND+  0 REPORT:SELECT!
   RC-MEMORY REPORT:ROOFLINE!
   s" ffn|sm_87" REPORT:CACHE!
   s" phase0-estimates" REPORT:WARN+
   s" " V-PASS G-CERTIFY REPORT:GATE!
   s" no-device" V-NOTRUN G-GOLDEN REPORT:GATE!
dup REPORT:RENDER RSAVE
s" report.model: FFN"            HAS
s" report.shape: unset"          HAS
s" fusion.ops-before: 6"         HAS
s" fusion.ops-after: 3"          HAS
s" fusion.regions: 3"            HAS
s" fusion.split.0: fuse-barrier" HAS
s" memory.bytes-before: unknown" HAS
s" memory.materialized: 3"       HAS
s" coalesce.tensor.0: x"         HAS
s" coalesce.status.0: coalesced" HAS
s" schedule.candidate.0: elementwise-v1" HAS
s" schedule.selected: 0"         HAS
s" gate.certify.verdict: pass"   HAS
s" gate.golden.verdict: not-run" HAS
s" gate.golden.reason: no-device" HAS
s" roofline.class: memory-bound" HAS
s" cache.key: ffn|sm_87"         HAS
s" warning.0: phase0-estimates"  HAS
drop

\ known bytes render as the number, not "unknown"
REPORT:NEW 1024 512 REPORT:BYTES! dup REPORT:RENDER RSAVE
s" memory.bytes-before: 1024" HAS
s" memory.bytes-before: unknown" HASNT
drop

\ ---- human render ----------------------------------------------------------
REPORT:NEW
   s" FFN" REPORT:MODEL!  s" sm_87" REPORT:TARGET!  6 3 REPORT:OPS!  3 REPORT:REGIONS!
   RC-MEMORY REPORT:ROOFLINE!
   s" " V-PASS G-CERTIFY REPORT:GATE!
   s" no-device" V-NOTRUN G-GOLDEN REPORT:GATE!
   s" phase0-estimates" REPORT:WARN+
dup REPORT:RENDER-HUMAN RSAVE
s" Model CAD report" HAS
s" model:"           HAS
s" certify:"         HAS
s" pass"             HAS
s" golden:"          HAS
s" not-run (no-device)" HAS
s" warnings:"        HAS
drop

\ ---- failure packets: one line per non-pass gate, none for a pass -----------
REPORT:NEW
   s" FFN" REPORT:MODEL!
   s" " V-PASS G-CERTIFY REPORT:GATE!
   s" no-device" V-NOTRUN G-GOLDEN REPORT:GATE!
dup REPORT:RENDER-PACKETS RSAVE
s" packet.certify"                    HASNT
s" packet.golden: class=not-run"      HAS
s" expected=pass actual=not-run"      HAS
s" reason=no-device"                  HAS
s" repair=run-device-golden"          HAS
s" repro=model:FFN"                   HAS
drop

end-package

\ ---- roofline swapped-role negatives (dot habu-cad-adt-swap; capability S1) --
\ F-ROOFLINE stores a real `roofline` ENUM behind the unchanged RC-* n accessors;
\ the slot is reachable only as `ptr roofline` (F-ROOFLINE-AT, REPORT-private, so
\ the checks run in a reopened REPORT block). rtalien is a test-owned foreign
\ family proving family identity (not just n-vs-enum) is enforced at the slot.
package REPORT

ENUM rtalien aa bb ;ENUM

\ positive controls: the typed slot accepts its own family; the alien type resolves
s" RT-ROOF-OK ( roofline -- ) F-ROOFLINE-AT !"    CHECK-QUIET-CANDIDATE! -1 T=
s" RT-ALIEN-ID ( rtalien -- rtalien )"            CHECK-QUIET-CANDIDATE! -1 T=
\ n->enum laundering (diag: "at '!' expected: roofline<> ptr roofline<> actual: n ptr roofline<>")
s" RT-ROOF-NIN ( n -- ) F-ROOFLINE-AT !"          CHECK-QUIET-CANDIDATE! 0 T=
\ enum->n laundering (diag: "at '@' expected: n actual: roofline<>")
s" RT-ROOF-NOUT ( -- n ) F-ROOFLINE-AT @"         CHECK-QUIET-CANDIDATE! 0 T=
\ wrong family into the roofline slot (diag: "at '!' expected: roofline<> ptr
\ roofline<> actual: rtalien<> ptr roofline<>")
s" RT-ROOF-MIX ( rtalien -- ) F-ROOFLINE-AT !"    CHECK-QUIET-CANDIDATE! 0 T=

\ ---- verdict swapped-role negatives (G-TAG stores a `verdict` ENUM) ----------
\ positive control: the typed gate slot accepts its own family
s" RT-GATE-OK ( verdict n -- ) G-TAG-AT !"        CHECK-QUIET-CANDIDATE! -1 T=
\ n->enum / enum->n laundering at the gate slot
s" RT-GATE-NIN ( n n -- ) G-TAG-AT !"             CHECK-QUIET-CANDIDATE! 0 T=
s" RT-GATE-NOUT ( n -- n ) G-TAG-AT @"            CHECK-QUIET-CANDIDATE! 0 T=
\ the REAL cross-column swaps, both directions (diags: "at '!' expected:
\ verdict<> ptr verdict<> actual: roofline<> ptr verdict<>" and "at '!'
\ expected: roofline<> ptr roofline<> actual: verdict<> ptr roofline<>")
s" RT-GATE-ROOF ( roofline n -- ) G-TAG-AT !"     CHECK-QUIET-CANDIDATE! 0 T=
s" RT-ROOF-VERD ( verdict -- ) F-ROOFLINE-AT !"   CHECK-QUIET-CANDIDATE! 0 T=

end-package

T-REPORT
