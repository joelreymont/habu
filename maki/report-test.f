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

\ ---- byte-golden builder: append "<line>\n" fragments into a scratch buffer ---
\ Its own buffer (not SB) so REPORT:RENDER's internal SB use cannot clobber it.
$1000 constant GL-CAP
create GL-BUF GL-CAP allot  variable GL-U
: GL-RESET ( -- )  0 GL-U ! ;
: GL+ ( ptr u8 n -- )  {: a:ptr u:n :}   \ append line then newline
   a GL-BUF GL-U @ + u BYTE-COPY  GL-U @ u + GL-U !
   $0A GL-BUF GL-U @ + c!  GL-U @ 1+ GL-U ! ;
: GL$ ( -- ptr u8 n )  GL-BUF GL-U @ ;

\ ---- fail-closed probes (top-level cannot push quotations) ------------------
: BAD-ROOF     ( -- )  REPORT:NEW 99 REPORT:ROOFLINE! drop ;
: BAD-COAL     ( -- )  REPORT:NEW s" x" 9 REPORT:HOT+ drop ;
: BAD-GATE     ( -- )  REPORT:NEW 9 REPORT:GATE-TAG@ drop ;   \ reader validates raw gid
: BAD-VERDICT  ( -- )  9 REPORT:>VERDICT drop ;               \ verdict parse boundary fail-closes
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
   s" "          MAKI-VERDICT:PASS    MAKI-GATE:CERTIFY  REPORT:VERDICT!
   s" no-device" MAKI-VERDICT:NOT-RUN MAKI-GATE:GOLDEN   REPORT:VERDICT!
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
   s" " MAKI-VERDICT:PASS MAKI-GATE:CERTIFY REPORT:VERDICT!
   s" no-device" MAKI-VERDICT:NOT-RUN MAKI-GATE:GOLDEN REPORT:VERDICT!
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
   s" " MAKI-VERDICT:PASS MAKI-GATE:CERTIFY REPORT:VERDICT!
   s" no-device" MAKI-VERDICT:NOT-RUN MAKI-GATE:GOLDEN REPORT:VERDICT!
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
   s" " MAKI-VERDICT:PASS MAKI-GATE:CERTIFY REPORT:VERDICT!
   s" no-device" MAKI-VERDICT:NOT-RUN MAKI-GATE:GOLDEN REPORT:VERDICT!
dup REPORT:RENDER-PACKETS RSAVE
s" packet.certify"                    HASNT
s" packet.golden: class=not-run"      HAS
s" expected=pass actual=not-run"      HAS
s" reason=no-device"                  HAS
s" repair=run-device-golden"          HAS
s" repro=model:FFN"                   HAS
drop

\ ---- BYTE-GOLDEN: canonical machine render is byte-identical on a green path --
\ Demotion regression: routing the gate verdicts through the typed REPORT:VERDICT!
\ (in place of the retired raw REPORT:GATE!) must leave the rendered bytes exactly
\ as captured on the pre-change tree. The report exercises every render section
\ plus a passing and a not-run gate with a reason.
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
   s" " MAKI-VERDICT:PASS MAKI-GATE:CERTIFY REPORT:VERDICT!
   s" no-device" MAKI-VERDICT:NOT-RUN MAKI-GATE:GOLDEN REPORT:VERDICT!
dup REPORT:RENDER RSAVE
GL-RESET
s" report.model: FFN"              GL+
s" report.shape: unset"            GL+
s" report.dtype: unset"            GL+
s" report.layout: unset"           GL+
s" report.target: sm_87"           GL+
s" fusion.ops-before: 6"           GL+
s" fusion.ops-after: 3"            GL+
s" fusion.regions: 3"              GL+
s" fusion.split.0: fuse-barrier"   GL+
s" memory.bytes-before: unknown"   GL+
s" memory.bytes-after: unknown"    GL+
s" memory.materialized: 3"         GL+
s" coalesce.tensor.0: x"           GL+
s" coalesce.status.0: coalesced"   GL+
s" schedule.candidate.0: elementwise-v1" GL+
s" schedule.selected: 0"           GL+
s" gate.certify.verdict: pass"     GL+
s" gate.golden.verdict: not-run"   GL+
s" gate.golden.reason: no-device"  GL+
s" gate.gradcheck.verdict: not-run" GL+
s" gate.profile.verdict: not-run"  GL+
s" roofline.class: memory-bound"   GL+
s" cache.key: ffn|sm_87"           GL+
s" warning.0: phase0-estimates"    GL+
RVA @ RVU @  GL$  T$=
drop

;package

\ ---- roofline swapped-role negatives (dot habu-cad-adt-swap; capability S1) --
\ F-ROOFLINE stores a real `roofline` ENUM behind the unchanged RC-* n accessors;
\ its generative buffer exposes only `ptr roofline` through F-ROOFLINE-AT.
\ rtalien proves family identity, not merely n-vs-enum, is enforced at the slot.
package REPORT

ENUM rtalien aa bb ;ENUM

: RT-ROOF-LOW ( -- )  -1 F-ROOFLINE drop ;
: RT-ROOF-HIGH ( -- )  1 F-ROOFLINE drop ;
: RT-HOT-LOW ( -- )  -1 HOT-ST-AT drop ;
: RT-HOT-HIGH ( -- )  RPT-LCAP HOT-ST-AT drop ;
: RT-GATE-LOW ( -- )  -1 G-TAG-AT drop ;
: RT-GATE-HIGH ( -- )  MAKI:G-N G-TAG-AT drop ;

: RT-HOT-FULL ( -- )
   NEW RPT-LCAP 0 ?do s" x" MAKI:CO-COALESCED HOT+ loop
   dup HOT-COUNT RPT-LCAP T= drop ;

: RT-HOT-OVER ( -- )
   NEW RPT-LCAP 1+ 0 ?do s" x" MAKI:CO-COALESCED HOT+ loop drop ;

' RT-ROOF-LOW E-LAYOUT-BOUNDS TTHROWS
' RT-ROOF-HIGH E-LAYOUT-BOUNDS TTHROWS
' RT-HOT-LOW E-LAYOUT-BOUNDS TTHROWS
' RT-HOT-HIGH E-LAYOUT-BOUNDS TTHROWS
' RT-GATE-LOW E-LAYOUT-BOUNDS TTHROWS
' RT-GATE-HIGH E-LAYOUT-BOUNDS TTHROWS
RT-HOT-FULL
' RT-HOT-OVER E-RPT-FULL TTHROWS

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

\ ---- coalescing-status swapped-role negatives (typed L-HOT-ST column) -------
\ positive control: the typed hot-status slot accepts its own family
s" RT-HOT-OK ( costatus n -- ) HOT-ST-AT !"       CHECK-QUIET-CANDIDATE! -1 T=
\ n->enum / enum->n laundering at the hot-status slot
s" RT-HOT-NIN ( n n -- ) HOT-ST-AT !"             CHECK-QUIET-CANDIDATE! 0 T=
s" RT-HOT-NOUT ( n -- n ) HOT-ST-AT @"            CHECK-QUIET-CANDIDATE! 0 T=
\ cross-column swap: a verdict can never land in a hot-status cell
s" RT-HOT-VERD ( verdict n -- ) HOT-ST-AT !"      CHECK-QUIET-CANDIDATE! 0 T=

\ ---- TS-OLD-GATE: the raw (verdict, gate-id) writer is retired (demotion pin) --
\ Report demoted to a RENDER of typed evidence (MODEL-CAD-V2-PLAN.md:1827-1830,
\ sub-dot 5 :1941-1948): the raw REPORT:GATE! ( report ptr u8 n n n -- report )
\ left the public surface, folded into the typed REPORT:VERDICT! (verdict ENUM +
\ gate family). A candidate using the old raw writer is UNRESOLVABLE (verdict 1),
\ not merely a type reject -- the word is GONE, so census probe 1's slot swap has
\ no writer to certify. The retire outcome is PINNED (not the retype alternative,
\ which would keep a public gate-write word). Proven non-vacuous by the paired
\ positive control on the typed replacement, which resolves and certifies (-1).
s" TS-NEW-VERDICT ( report ptr u8 n verdict gate -- report ) REPORT:VERDICT!"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TS-OLD-GATE ( report ptr u8 n n n -- report ) REPORT:GATE!"
   CHECK-QUIET-CANDIDATE! 1 T=

;package

T-REPORT
