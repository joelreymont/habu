\ build-fixpoint.f - checked self-rebuild orchestration.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/process.f, and lib/build.f.

262144 constant BF-SOURCE-CAP
32768 constant BF-CMP-CAP
4 constant BF-MAX-GENS
10 constant BF-LF
64 constant BF-USAGE-RC
74 constant BF-BUILD-RC

create BF-SOURCE-BUF BF-SOURCE-CAP allot
create BF-CMD-PATH FS-PATH-CAP allot
create BF-ART-PATH FS-PATH-CAP allot
create BF-OUT-PATH FS-PATH-CAP allot
create BF-A-PATH FS-PATH-CAP allot
create BF-B-PATH FS-PATH-CAP allot
create BF-CMP-A BF-CMP-CAP allot
create BF-CMP-B BF-CMP-CAP allot
create BF-LF-BUF 1 allot
BF-LF BF-LF-BUF c!

variable BF-SOURCE-LEN
variable BF-A-LEN
variable BF-B-LEN
variable BF-FDA
variable BF-FDB
variable BF-RA
variable BF-RB
variable BF-GEN
variable BF-FOUND

: BF-TRUE ( -- bool )
   0 0= ;

: BF-FALSE ( -- bool )
   0 0= 0= ;

: BF-TMP> ( ptr u8 n ptr u8 -- n ) {: name:ptr nameu dst:ptr :}
   name nameu TMP-PATH {: path:ptr pathu :}
   pathu FS-PATH-CAP > if E-BUILD-PATH throw then
   path dst pathu BYTE-COPY
   pathu ;

: BF-CMD$ ( ptr u8 n -- ptr u8 n )
   BF-CMD-PATH BF-TMP> BF-CMD-PATH swap ;

: BF-ART$ ( ptr u8 n -- ptr u8 n )
   BF-ART-PATH BF-TMP> BF-ART-PATH swap ;

: BF-OUT$ ( ptr u8 n -- ptr u8 n )
   BF-OUT-PATH BF-TMP> BF-OUT-PATH swap ;

: BF-RUN ( ptr u8 n ptr u8 n -- ) {: cmd:ptr cmdu art:ptr artu :}
   cmd cmdu BF-CMD$ art artu BF-ART$ BUILD-RUN drop ;

: BF-EXPECT ( ptr u8 n -- )
   BF-ART$ BUILD-EXPECT ;

: BF-RESET-OUT ( ptr u8 n -- )
   BF-OUT$ BF-SOURCE-BUF 0 WRITE-ALL ;

: BF-APPEND-BYTES ( ptr u8 n ptr u8 n -- ) {: out:ptr outu a:ptr u :}
   out outu BF-OUT$ a u APPEND-FILE ;

: BF-APPEND-LF ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-OUT$ BF-LF-BUF 1 APPEND-FILE ;

: BF-APPEND-LINE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu a:ptr u :}
   out outu a u BF-APPEND-BYTES
   out outu BF-APPEND-LF ;

: BF-APPEND-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu src:ptr srcu :}
   src srcu BF-SOURCE-BUF BF-SOURCE-CAP READ-ALL BF-SOURCE-LEN !
   out outu BF-OUT$ BF-SOURCE-BUF BF-SOURCE-LEN @ APPEND-FILE
   out outu BF-APPEND-LF ;

: BF-APPEND-HOOK ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" : HOOK CHECK ; ' HOOK set-check" BF-APPEND-LINE ;

: BF-APPEND-COMMON ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/core/util.f" BF-APPEND-SOURCE
   out outu s" src/core/checker.f" BF-APPEND-SOURCE
   out outu s" src/core/render.f" BF-APPEND-SOURCE
   out outu BF-APPEND-HOOK
   out outu s" src/core/sha256.f" BF-APPEND-SOURCE
   out outu s" src/core/combinators.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/asm.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/icode.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/mnem.f" BF-APPEND-SOURCE
   out outu s" src/os/macos/sys.f" BF-APPEND-SOURCE
   out outu s" src/os/macos/env.f" BF-APPEND-SOURCE
   out outu s" src/habu/treeshake.f" BF-APPEND-SOURCE
   out outu s" src/habu/rt.f" BF-APPEND-SOURCE
   out outu s" src/habu/crash.f" BF-APPEND-SOURCE
   out outu s" src/os/macos/macho.f" BF-APPEND-SOURCE
   out outu s" src/os/macos/sign2.f" BF-APPEND-SOURCE
   out outu s" src/habu/habu1.f" BF-APPEND-SOURCE
   out outu s" src/habu/prof.f" BF-APPEND-SOURCE
   out outu s" src/habu/regalloc.f" BF-APPEND-SOURCE
   out outu s" src/habu/jit.f" BF-APPEND-SOURCE
   out outu s" src/habu/habu2.f" BF-APPEND-SOURCE ;

: BF-EMIT-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-COMMON
   out outu driver driveru BF-APPEND-SOURCE ;

: BF-CLOSE-CMP ( -- )
   BF-FDA @ dup 0 >= if close else drop then
   BF-FDB @ dup 0 >= if close else drop then
   -1 BF-FDA !
   -1 BF-FDB ! ;

: BF-OPEN-CMP ( ptr u8 n ptr u8 n -- ) {: a:ptr au b:ptr bu :}
   -1 BF-FDA !
   -1 BF-FDB !
   a au FS-PATHZ open-rd BF-FDA !
   BF-FDA @ 0 < if E-BUILD-PATH throw then
   b bu FS-PATHZ open-rd BF-FDB !
   BF-FDB @ 0 < if
      BF-FDA @ close -1 BF-FDA ! E-BUILD-PATH throw
   then ;

: BF-READ-A ( -- n )
   BF-FDA @ BF-CMP-A BF-CMP-CAP read BF-RA !
   BF-RA @ 0 < if BF-CLOSE-CMP E-FS-IO throw then
   BF-RA @ BF-CMP-CAP > if BF-CLOSE-CMP E-FS-IO throw then
   BF-RA @ ;

: BF-READ-B ( -- n )
   BF-FDB @ BF-CMP-B BF-CMP-CAP read BF-RB !
   BF-RB @ 0 < if BF-CLOSE-CMP E-FS-IO throw then
   BF-RB @ BF-CMP-CAP > if BF-CLOSE-CMP E-FS-IO throw then
   BF-RB @ ;

: BF-FILE= ( ptr u8 n ptr u8 n -- bool )
   BF-OPEN-CMP
   begin
      BF-READ-A BF-RA !
      BF-READ-B BF-RB !
      BF-RA @ BF-RB @ <> if BF-CLOSE-CMP BF-FALSE exit then
      BF-RA @ 0= if BF-CLOSE-CMP BF-TRUE exit then
      BF-CMP-A BF-RA @ BF-CMP-B BF-RB @ STR= 0= if
         BF-CLOSE-CMP BF-FALSE exit
      then
   again ;

: BF-TMP-FILE= ( ptr u8 n ptr u8 n -- bool ) {: an:ptr anu bn:ptr bnu :}
   an anu BF-A-PATH BF-TMP> BF-A-LEN !
   bn bnu BF-B-PATH BF-TMP> BF-B-LEN !
   BF-A-PATH BF-A-LEN @ BF-B-PATH BF-B-LEN @ BF-FILE= ;

: BF-STAGE2-SOURCE ( -- )
   s" stage2-src" s" src/habu/stage2.f" BF-EMIT-SOURCE ;

: BF-STDIN-SOURCE ( -- )
   s" stage2-src" s" src/habu/stdin.f" BF-EMIT-SOURCE ;

: BF-SNAP-SOURCE ( -- )
   s" hb-snap-src" s" src/habu/snap.f" BF-EMIT-SOURCE ;

: BF-BOOTSTRAP-STAGE ( -- )
   s" build-bootstrap-stage" s" hb-stage" BF-RUN ;

: BF-RUN-STAGE ( -- )
   s" build-run-stage" s" stage2-got" BF-RUN ;

: BF-PROMOTE-STAGE ( -- )
   s" build-promote-stage" s" hb-stage" BF-RUN ;

: BF-VERIFY-STAGE ( -- )
   s" build-verify-stage" s" hb-stage" BF-RUN ;

: BF-STAGE-MATCH? ( -- bool )
   s" hb-stage" s" stage2-got" BF-TMP-FILE= ;

: BF-STAGE-FIXPOINT ( -- )
   BF-STAGE2-SOURCE
   BF-BOOTSTRAP-STAGE
   0 BF-GEN !
   0 BF-FOUND !
   begin BF-GEN @ BF-MAX-GENS < while
      BF-RUN-STAGE
      BF-STAGE-MATCH? if
         BF-VERIFY-STAGE
         -1 BF-FOUND !
         BF-MAX-GENS BF-GEN !
      else
         BF-PROMOTE-STAGE
         BF-GEN @ 1 + BF-GEN !
      then
   repeat
   BF-FOUND @ 0= if s" FIXPOINT BROKEN: no convergence after 4 generations" BF-BUILD-RC die then
   s" build OK: stage compiler fixpoint" type cr ;

: BF-BUILD-STDIN ( -- )
   BF-STDIN-SOURCE
   BF-RUN-STAGE
   s" build-promote-stdin-maker" s" hb-stdin-mk" BF-RUN
   s" build-run-stdin-maker" s" hb-stdin-got" BF-RUN
   s" build-promote-stdin-engine" s" hb-stdin" BF-RUN
   s" build-verify-stdin" s" hb-stdin" BF-RUN ;

: BF-BUILD-SNAP ( -- )
   BF-SNAP-SOURCE
   s" build-run-snap" s" hb-snap0" BF-RUN
   s" build-promote-snap" s" hb-new" BF-RUN
   s" hb-new" BF-EXPECT
   s" build OK: hb-new validated" type cr ;

: BF-BUILD-ALL ( -- )
   BF-STAGE-FIXPOINT
   BF-BUILD-STDIN
   BF-BUILD-SNAP ;

: BF-USAGE ( -- )
   s" usage: tools/build-fixpoint.f [all|stage|stdin|snap]" BF-USAGE-RC die ;

: BF-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: BF-MAIN ( -- )
   SCRIPT-ARGC 0= if BF-BUILD-ALL exit then
   SCRIPT-ARGC 1 <> if BF-USAGE then
   s" all" BF-ARG0= if BF-BUILD-ALL exit then
   s" stage" BF-ARG0= if BF-STAGE-FIXPOINT exit then
   s" stdin" BF-ARG0= if BF-BUILD-STDIN exit then
   s" snap" BF-ARG0= if BF-BUILD-SNAP exit then
   BF-USAGE ;

BF-MAIN
