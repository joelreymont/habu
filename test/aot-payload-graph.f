\ Supported graph metadata survives file restoration and source-effect destruction.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-id.f
require lib/engine-candidate.f

package PAYLOAD-GRAPH-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
create ART FS-PATH-CAP allot
variable ART-U

: ART$ ( -- ptr u8 n ) ART ART-U @ ;
: SETUP ( -- )
   s" habu-payload-graph" TMPDIR-MKDIR
   2dup CLEANUP-TREE+ s" metadata.aot" ART JOIN-PATH ART-U ! ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   s" test/aot-payload-graph-child.f" ARG
   s" src/core/declaration-transaction.f" ARG
   s" src/core/generated-declaration.f" ARG
   s" src/core/decl-event.f" ARG
   s" src/core/structure-make.f" ARG
   s" src/core/structure-decl.f" ARG
   s" src/core/enum-decl.f" ARG
   s" src/core/structures.f" ARG
   s" src/core/bytes.f" ARG
   s" src/core/dynamic-storage.f" ARG
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" ARG s" src/os/linux/layout.f" ARG
   else
      s" src/os/macos/target.f" ARG s" src/os/macos/layout.f" ARG
   then
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/core/sha256.f" ARG
   s" lib/prelude.f" ARG
   PROC-ENV-RESET
   s" HABU_PAYLOAD_TEST_ARTIFACT" >LEN ART$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: CASE-RUN ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n diagnostic:ptr diagnosticu:n :}
   ARGS
   s" HABU_PAYLOAD_TEST_MODE" >LEN mode modeu >LEN PROC-ENV-SET
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   modeu 0= if
      rc 0 <> OUT outu LEN>N s" graph metadata file roundtrip: ok" CONTAINS? 0= or
      if OUT outu LEN>N type ERR erru LEN>N type cr then
      rc 0 T=
      OUT outu LEN>N s" graph metadata file roundtrip: ok" CONTAINS? TTRUE
      OUT outu LEN>N S\" window: 0\n" CONTAINS? TTRUE
   else
      rc 76 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
      rc 76 T=
      OUT outu LEN>N s" graph corruption applied: " CONTAINS? TTRUE
      OUT outu LEN>N mode modeu CONTAINS? TTRUE
      OUT outu LEN>N diagnostic diagnosticu CONTAINS?
      ERR erru LEN>N diagnostic diagnosticu CONTAINS? or TTRUE
   then ;

: BAD-GRAPH ( ptr u8 n -- ) s" checker: invalid captured effect graph" CASE-RUN ;

: RUN ( -- )
   T-RESET SETUP
   s" " s" " CASE-RUN
   s" length" BAD-GRAPH
   s" cycle" BAD-GRAPH
   s" tag" BAD-GRAPH
   s" variables" BAD-GRAPH
   s" family" s" tfam: a seeded effect has the wrong family arity" CASE-RUN
   s" exception" s" checker: exceptional quotation rows are not portable" CASE-RUN
   CLEANUP-RUN T-REPORT ;

RUN
;package
