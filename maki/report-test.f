\ maki/report-test.f - checked tests for the Model CAD report schema (cad-0a).

require lib/test.f
require lib/string.f
require maki/report.f

package MAKI

\ ---- render-output containment helper --------------------------------------
variable RVA  variable RVU
: RSAVE ( ptr u8 n -- )  RVU ! RVA ! ;
: HAS ( ptr u8 n -- )  RVA @ RVU @ 2swap CONTAINS? TTRUE ;
: HASNT ( ptr u8 n -- )  RVA @ RVU @ 2swap CONTAINS? TFALSE ;

\ ---- fail-closed probes (top-level cannot push quotations) ------------------
: BAD-ROOF     ( -- )  RPT-NEW 99 RPT-ROOFLINE! drop ;
: BAD-COAL     ( -- )  RPT-NEW s" x" 9 RPT-HOT+ drop ;
: BAD-GATE     ( -- )  RPT-NEW s" r" V-PASS 9 RPT-GATE! drop ;
: BAD-VERDICT  ( -- )  RPT-NEW s" r" 9 G-CERTIFY RPT-GATE! drop ;
: BAD-SPLIT-IX ( -- )  RPT-NEW s" a" RPT-SPLIT+ 5 RPT-SPLIT@ 2drop ;
: BAD-SELECT   ( -- )  RPT-NEW s" a" RPT-CAND+ 4 RPT-SELECT! drop ;
: FULL-SPLIT   ( -- )  RPT-NEW 20 0 ?do s" x" RPT-SPLIT+ loop drop ;

T-RESET

\ ---- fresh report: honest defaults -----------------------------------------
RPT-NEW dup RPT-OPS-BEFORE@ 0 T=
RPT-NEW dup RPT-REGIONS@ 0 T=
RPT-NEW dup RPT-BYTES-KNOWN? TFALSE
RPT-NEW dup RPT-ROOFLINE@ RC-UNKNOWN T=
RPT-NEW dup RPT-SELECT@ -1 T=
RPT-NEW dup RPT-SPLIT-COUNT 0 T=
RPT-NEW dup RPT-WARN-COUNT 0 T=
RPT-NEW dup RPT-CAND-COUNT 0 T=
RPT-NEW dup RPT-HOT-COUNT 0 T=
RPT-NEW dup RPT-PROF-COUNT 0 T=
RPT-NEW dup G-GOLDEN RPT-GATE-TAG@ V-NOTRUN T=
drop drop drop drop drop drop drop drop drop drop drop

\ ---- key setters / getters -------------------------------------------------
RPT-NEW
   s" FFN"       RPT-MODEL!
   s" b1s128d64" RPT-SHAPE!
   s" f32"       RPT-DTYPE!
   s" row-major" RPT-LAYOUT!
   s" sm_87"     RPT-TARGET!
   s" ffn|sm_87" RPT-CACHE!
dup RPT-MODEL$  s" FFN"       T$=
dup RPT-SHAPE$  s" b1s128d64" T$=
dup RPT-DTYPE$  s" f32"       T$=
dup RPT-LAYOUT$ s" row-major" T$=
dup RPT-TARGET$ s" sm_87"     T$=
dup RPT-CACHE$  s" ffn|sm_87" T$=
drop

\ unset key reads back empty
RPT-NEW dup RPT-MODEL$ nip 0 T= drop

\ ---- fusion-plan scalars ---------------------------------------------------
RPT-NEW  6 3 RPT-OPS!  3 RPT-REGIONS!  3 RPT-MATERIALIZED!
dup RPT-OPS-BEFORE@  6 T=
dup RPT-OPS-AFTER@   3 T=
dup RPT-REGIONS@     3 T=
dup RPT-MATERIALIZED@ 3 T=
drop

\ ---- memory bytes + known flag ---------------------------------------------
RPT-NEW  1024 512 RPT-BYTES!
dup RPT-BYTES-KNOWN?  TTRUE
dup RPT-BYTES-BEFORE@ 1024 T=
dup RPT-BYTES-AFTER@  512  T=
drop

\ ---- roofline (+ fail closed) ----------------------------------------------
RPT-NEW RC-MEMORY RPT-ROOFLINE! dup RPT-ROOFLINE@ RC-MEMORY T= drop
' BAD-ROOF E-RPT-ROOF TTHROWS

\ ---- split reasons (+ index fail closed) -----------------------------------
RPT-NEW s" fuse-barrier" RPT-SPLIT+ s" reg-pressure" RPT-SPLIT+
dup RPT-SPLIT-COUNT 2 T=
dup 0 RPT-SPLIT@ s" fuse-barrier" T$=
dup 1 RPT-SPLIT@ s" reg-pressure" T$=
drop
' BAD-SPLIT-IX E-RPT-IDX TTHROWS
' FULL-SPLIT   E-RPT-FULL TTHROWS

\ ---- warnings --------------------------------------------------------------
RPT-NEW s" phase0-estimates" RPT-WARN+
dup RPT-WARN-COUNT 1 T=
dup 0 RPT-WARN@ s" phase0-estimates" T$=
drop

\ ---- schedule candidates + selection (+ fail closed) -----------------------
RPT-NEW s" elementwise-v1" RPT-CAND+ s" gemm-tf32-v1" RPT-CAND+  1 RPT-SELECT!
dup RPT-CAND-COUNT 2 T=
dup 0 RPT-CAND@ s" elementwise-v1" T$=
dup 1 RPT-CAND@ s" gemm-tf32-v1"   T$=
dup RPT-SELECT@ 1 T=
drop
' BAD-SELECT E-RPT-IDX TTHROWS

\ ---- hot tensors + coalescing (+ fail closed) ------------------------------
RPT-NEW s" x" CO-COALESCED RPT-HOT+ s" w" CO-STRIDED RPT-HOT+
dup RPT-HOT-COUNT 2 T=
dup 0 RPT-HOT-NAME@ s" x" T$=
dup 0 RPT-HOT-STATUS@ CO-COALESCED T=
dup 1 RPT-HOT-STATUS@ CO-STRIDED   T=
drop
' BAD-COAL E-RPT-COAL TTHROWS

\ ---- profile rows ----------------------------------------------------------
RPT-NEW s" saxpy" 42 RPT-PROF+
dup RPT-PROF-COUNT 1 T=
dup 0 RPT-PROF-NAME@ s" saxpy" T$=
dup 0 RPT-PROF-US@ 42 T=
drop

\ ---- gate verdicts (+ fail closed) -----------------------------------------
RPT-NEW
   s" "         V-PASS   G-CERTIFY   RPT-GATE!
   s" no-device" V-NOTRUN G-GOLDEN   RPT-GATE!
dup G-CERTIFY RPT-GATE-TAG@ V-PASS   T=
dup G-GOLDEN  RPT-GATE-TAG@ V-NOTRUN T=
dup G-GOLDEN  RPT-GATE-REASON@ s" no-device" T$=
drop
' BAD-GATE    E-RPT-GATE    TTHROWS
' BAD-VERDICT E-RPT-VERDICT TTHROWS

\ ---- machine render: line-oriented key:value -------------------------------
RPT-NEW
   s" FFN"   RPT-MODEL!
   s" sm_87" RPT-TARGET!
   6 3 RPT-OPS!  3 RPT-REGIONS!  3 RPT-MATERIALIZED!
   s" fuse-barrier" RPT-SPLIT+
   s" x" CO-COALESCED RPT-HOT+
   s" elementwise-v1" RPT-CAND+  0 RPT-SELECT!
   RC-MEMORY RPT-ROOFLINE!
   s" ffn|sm_87" RPT-CACHE!
   s" phase0-estimates" RPT-WARN+
   s" " V-PASS G-CERTIFY RPT-GATE!
   s" no-device" V-NOTRUN G-GOLDEN RPT-GATE!
dup RPT-RENDER RSAVE
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
RPT-NEW 1024 512 RPT-BYTES! dup RPT-RENDER RSAVE
s" memory.bytes-before: 1024" HAS
s" memory.bytes-before: unknown" HASNT
drop

\ ---- human render ----------------------------------------------------------
RPT-NEW
   s" FFN" RPT-MODEL!  s" sm_87" RPT-TARGET!  6 3 RPT-OPS!  3 RPT-REGIONS!
   RC-MEMORY RPT-ROOFLINE!
   s" " V-PASS G-CERTIFY RPT-GATE!
   s" no-device" V-NOTRUN G-GOLDEN RPT-GATE!
   s" phase0-estimates" RPT-WARN+
dup RPT-RENDER-HUMAN RSAVE
s" Model CAD report" HAS
s" model:"           HAS
s" certify:"         HAS
s" pass"             HAS
s" golden:"          HAS
s" not-run (no-device)" HAS
s" warnings:"        HAS
drop

\ ---- failure packets: one line per non-pass gate, none for a pass -----------
RPT-NEW
   s" FFN" RPT-MODEL!
   s" " V-PASS G-CERTIFY RPT-GATE!
   s" no-device" V-NOTRUN G-GOLDEN RPT-GATE!
dup RPT-RENDER-PACKETS RSAVE
s" packet.certify"                    HASNT
s" packet.golden: class=not-run"      HAS
s" expected=pass actual=not-run"      HAS
s" reason=no-device"                  HAS
s" repair=run-device-golden"          HAS
s" repro=model:FFN"                   HAS
drop

T-REPORT

end-package
