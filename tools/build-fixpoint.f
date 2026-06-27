\ build-fixpoint.f - checked self-rebuild orchestration.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and lib/codesign.f.

262144 constant BF-SOURCE-CAP
32768 constant BF-CMP-CAP
4 constant BF-MAX-GENS
10 constant BF-LF
64 constant BF-USAGE-RC
74 constant BF-BUILD-RC

create BF-ART-PATH FS-PATH-CAP allot
create BF-OUT-PATH FS-PATH-CAP allot
create BF-A-PATH FS-PATH-CAP allot
create BF-B-PATH FS-PATH-CAP allot
create BF-LF-BUF 1 allot
BF-LF BF-LF-BUF c!

variable BF-SOURCE-BUF-A
variable BF-CMP-A-BUF-A
variable BF-CMP-B-BUF-A
variable BF-SOURCE-LEN
variable BF-A-LEN
variable BF-B-LEN
variable BF-FDA
variable BF-FDB
variable BF-RA
variable BF-RB
variable BF-GEN
variable BF-FOUND
variable BF-PID
variable BF-TMP-A
variable BF-TMP-U

: BF-TMP-A-FIELD ( -- ptr ptr u8 )
   BF-TMP-A 0 ptr-field ;

: BF-TMP-A@ ( -- ptr u8 )
   BF-TMP-A-FIELD @ ;

: BF-TMP-A! ( ptr u8 -- )
   BF-TMP-A-FIELD ! ;

: BF-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: BF-PTR-U8@ ( ptr a -- ptr u8 )
   BF-PTR-U8-FIELD @ ;

: BF-PTR-U8! ( ptr u8 ptr a -- )
   BF-PTR-U8-FIELD ! ;

: BF-ALLOC-BUF ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

: BF-BUF ( ptr a n -- ptr u8 ) {: slot:ptr cap :}
   slot @ 0= if cap BF-ALLOC-BUF slot BF-PTR-U8! then
   slot BF-PTR-U8@ ;

: BF-SOURCE-BUF ( -- ptr u8 )
   BF-SOURCE-BUF-A BF-SOURCE-CAP BF-BUF ;

: BF-CMP-A ( -- ptr u8 )
   BF-CMP-A-BUF-A BF-CMP-CAP BF-BUF ;

: BF-CMP-B ( -- ptr u8 )
   BF-CMP-B-BUF-A BF-CMP-CAP BF-BUF ;

: BF-TRUE ( -- bool )
   0 0= ;

: BF-FALSE ( -- bool )
   0 0= 0= ;

: BF-TMP! ( ptr u8 n -- )
   {: a:ptr u :}
   u BF-TMP-U !
   a BF-TMP-A! ;

: BF-TMP-OVERRIDE$ ( -- ptr u8 n )
   BF-TMP-A@ BF-TMP-U @ ;

: BF-TMP-RESET ( -- )
   0 BF-TMP-U ! ;

: BF-TMP$ ( -- ptr u8 n )
   BF-TMP-U @ 0 > if BF-TMP-OVERRIDE$ exit then
   s" HB_TMP" GETENV dup 0= if drop drop s" /tmp" then ;

: BF-TMP> ( ptr u8 n ptr u8 -- n ) {: name:ptr nameu dst:ptr :}
   BF-TMP$ {: root:ptr rootu :}
   rootu 0 <= if E-BUILD-PATH throw then
   rootu 1 + nameu + FS-PATH-CAP > if E-BUILD-PATH throw then
   root rootu name nameu dst JOIN-PATH ;

: BF-ART$ ( ptr u8 n -- ptr u8 n )
   BF-ART-PATH BF-TMP> BF-ART-PATH swap ;

: BF-OUT$ ( ptr u8 n -- ptr u8 n )
   BF-OUT-PATH BF-TMP> BF-OUT-PATH swap ;

: BF-A$ ( ptr u8 n -- ptr u8 n )
   BF-A-PATH BF-TMP> BF-A-LEN !
   BF-A-PATH BF-A-LEN @ ;

: BF-B$ ( ptr u8 n -- ptr u8 n )
   BF-B-PATH BF-TMP> BF-B-LEN !
   BF-B-PATH BF-B-LEN @ ;

: BF-EXPECT-PATH ( ptr u8 n -- ) {: path:ptr pathu :}
   pathu 0 <= if E-BUILD-PATH throw then
   path pathu FILE? 0= if E-BUILD-PATH throw then ;

: BF-EXPECT ( ptr u8 n -- )
   BF-ART$ BF-EXPECT-PATH ;

: BF-RC0 ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: BF-REMOVE-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

: BF-RENAME-TMP ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu BF-A$ dst dstu BF-B$ RENAME-FILE ;

: BF-CHMOD-X-TMP ( ptr u8 n -- )
   BF-A$ CHMOD-X ;

: BF-OPEN-INPUT ( ptr u8 n -- n )
   FS-PATHZ open-rd dup 0 < if E-BUILD-PATH throw then ;

: BF-PREPARE-ENV ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN BF-TMP$ >LEN PROC-ENV+ ;

: BF-FINISH-PID ( pid -- n ) {: pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid WAIT-RC RC>N ;

: BF-RUN-ENV-FDS ( ptr u8 n n n n -- n ) {: exe:ptr exeu infd outfd errfd :}
   BF-PREPARE-ENV
   exe exeu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE infd >FD outfd >FD errfd >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-RUN-ENV-INFD ( ptr u8 n n -- n ) {: exe:ptr exeu infd :}
   BF-PREPARE-ENV
   exe exeu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE infd >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   infd close
   pid BF-FINISH-PID ;

: BF-PREPARE-STAGE-ARGV ( ptr u8 n -- ptr u8 ptr a )
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+
   >LEN PROC-ARGV-PREPARE ;

: BF-RUN-STAGE-ENV-INFD ( ptr u8 n n -- n ) {: exe:ptr exeu infd :}
   BF-PREPARE-ENV
   exe exeu BF-PREPARE-STAGE-ARGV
   PROC-ENV-PREPARE infd >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW {: pid :}
   infd close
   pid BF-FINISH-PID ;

: BF-RUN-ENV-EXE ( ptr u8 n -- n )
   -1 -1 -1 BF-RUN-ENV-FDS ;

: BF-RUN-STAGE-ENV-EXE ( ptr u8 n -- n )
   BF-PREPARE-ENV
   BF-PREPARE-STAGE-ARGV
   PROC-ENV-PREPARE -1 >FD -1 >FD -1 >FD
   PROC-SPAWN-ARGV-ENV-RAW BF-FINISH-PID ;

: BF-RUN-ENV-PATH-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu src srcu BF-OPEN-INPUT BF-RUN-ENV-INFD ;

: BF-RUN-STAGE-PATH-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu src srcu BF-OPEN-INPUT BF-RUN-STAGE-ENV-INFD ;

: BF-RUN-ENV-TMP ( ptr u8 n -- n )
   BF-A$ BF-RUN-ENV-EXE ;

: BF-RUN-STAGE-TMP ( ptr u8 n -- n )
   BF-A$ BF-RUN-STAGE-ENV-EXE ;

: BF-RUN-ENV-TMP-INFILE ( ptr u8 n ptr u8 n -- n ) {: exe:ptr exeu src:ptr srcu :}
   exe exeu BF-A$ src srcu BF-B$ BF-OPEN-INPUT BF-RUN-ENV-INFD ;

: BF-CODESIGN-VERIFY-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ CODESIGN-VERIFY ;

: BF-CODESIGN-FORCE-TMP ( ptr u8 n -- ) {: a:ptr u :}
   a u BF-A$ CODESIGN-FORCE ;

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

: BF-READ-SOURCE ( ptr u8 n -- )
   BF-SOURCE-BUF BF-SOURCE-CAP READ-ALL BF-SOURCE-LEN ! ;

: BF-SOURCE-HAS? ( ptr u8 n -- bool )
   BF-SOURCE-BUF BF-SOURCE-LEN @ 2swap CONTAINS? ;

: BF-SOURCE-MUST-HAVE ( ptr u8 n -- )
   BF-SOURCE-HAS? 0= if s" build-fixpoint: native emitter shape missing" BF-BUILD-RC die then ;

: BF-SOURCE-MUST-LACK ( ptr u8 n -- )
   BF-SOURCE-HAS? if s" build-fixpoint: unsafe native emitter shape" BF-BUILD-RC die then ;

: BF-PREFLIGHT-HABU2 ( -- )
   s" src/habu/habu2.f" BF-READ-SOURCE
   s" variable CLOC-MAIN  variable CLOC-NOT" BF-SOURCE-MUST-HAVE
   s" variable CLOC-MEM   variable CLOC-QOK" BF-SOURCE-MUST-HAVE
   s" : C-LOCAL-REF-ARGS ( n n -- )" BF-SOURCE-MUST-HAVE
   s" : C-LOCAL-REF-LABELS ( -- )" BF-SOURCE-MUST-HAVE
   s" : EMIT-RESET-BUILDER ( ptr u8 n -- )" BF-SOURCE-MUST-HAVE
   s" {: lmainlbl notloc :}" BF-SOURCE-MUST-LACK
   s" LBL LBL {: lmem qlrefok :}" BF-SOURCE-MUST-LACK
   s" {: a:ptr u :}" BF-SOURCE-MUST-LACK
   s" CLOC-MAIN @ B," BF-SOURCE-MUST-HAVE
   s" CLOC-MAIN @ B ;" BF-SOURCE-MUST-LACK ;

: BF-PREFLIGHT-ICODE ( -- )
   s" src/arch/arm64/icode.f" BF-READ-SOURCE
   s" variable CODE-A" BF-SOURCE-MUST-HAVE
   s" : CODE ( -- ptr u8 )" BF-SOURCE-MUST-HAVE
   s" : ICODE-TABS ( -- ptr n )" BF-SOURCE-MUST-HAVE
   s" icode: code mmap failed" BF-SOURCE-MUST-HAVE
   s" icode: table mmap failed" BF-SOURCE-MUST-HAVE
   s" variable BYA" BF-SOURCE-MUST-HAVE
   s" variable BYU" BF-SOURCE-MUST-HAVE
   s" : BYA@ ( -- ptr u8 )" BF-SOURCE-MUST-HAVE
   s" : BYTES-ARGS ( ptr u8 n -- )" BF-SOURCE-MUST-HAVE
   s" : BYTES-CAP ( -- )" BF-SOURCE-MUST-HAVE
   s" : BYTES-COPY ( -- )" BF-SOURCE-MUST-HAVE
   s" : BYTES-PAD ( -- )" BF-SOURCE-MUST-HAVE
   s" : BYTES, ( ptr u8 n -- )" BF-SOURCE-MUST-HAVE
   s" create CODE CODE-CAP-BYTES allot" BF-SOURCE-MUST-LACK
   s" create LBLP LBL-CAP cells allot" BF-SOURCE-MUST-LACK
   s" create FXS 2048 cells allot" BF-SOURCE-MUST-LACK
   s" {: a:ptr u :}" BF-SOURCE-MUST-LACK ;

: BF-PREFLIGHT-HABU1 ( -- )
   s" src/habu/habu1.f" BF-READ-SOURCE
   s" variable PR-A  variable PR-U  variable PR-L  variable PR-E" BF-SOURCE-MUST-HAVE
   s" variable FP-A  variable FP-U  variable FP-XT" BF-SOURCE-MUST-HAVE
   s" variable SDA-FD  variable SDA-NEW  variable SDA-SKIP" BF-SOURCE-MUST-HAVE
   s" variable BSP-OK  variable BSP-DN  variable BSP-SAD" BF-SOURCE-MUST-HAVE
   s" variable SZA-I" BF-SOURCE-MUST-HAVE
   s" : REG-PRIM ( ptr u8 n n n -- )" BF-SOURCE-MUST-HAVE
   s" : FPRIM ( ptr u8 n n -- )" BF-SOURCE-MUST-HAVE
   s" : FPRIM-L ( ptr u8 n n -- )" BF-SOURCE-MUST-HAVE
   s" : PR-COPY-NAME ( -- )" BF-SOURCE-MUST-HAVE
   s" : BSP-LABELS3 ( -- )" BF-SOURCE-MUST-HAVE
   s" : FPRIM {: na:ptr nu xt :}" BF-SOURCE-MUST-LACK
   s" : FPRIM-L {: na:ptr nu xt :}" BF-SOURCE-MUST-LACK
   s" : REG-PRIM {: na:ptr nu lbl elbl :}" BF-SOURCE-MUST-LACK
   s" : ?PRIM-SPACE {: na:ptr nu :}" BF-SOURCE-MUST-LACK
   s" : SPAWN-DUP2-ACTION ( reg fd -- )" BF-SOURCE-MUST-HAVE
   s" : SPAWN-CHDIR-ACTION ( reg label -- )" BF-SOURCE-MUST-HAVE
   s" : SPAWN-DUP2-ACTION ( n n -- ) {: fdreg newfd :}" BF-SOURCE-MUST-LACK
   s" : SPAWN-CHDIR-ACTION ( n n -- ) {: cwdreg fail :}" BF-SOURCE-MUST-LACK
   s" 14 SP SPAWN-ADESC-OFF SZA-I @ + STR," BF-SOURCE-MUST-HAVE
   s" 14 SP SPAWN-ADESC-OFF + over + STR," BF-SOURCE-MUST-LACK
   s" LBL LBL LBL {: spok spdn sad :}" BF-SOURCE-MUST-LACK
   s" LBL LBL {: spok spdn :}" BF-SOURCE-MUST-LACK ;

: BF-PREFLIGHT ( -- )
   BF-PREFLIGHT-HABU2
   BF-PREFLIGHT-HABU1
   BF-PREFLIGHT-ICODE ;

: BF-APPEND-CHECK-OFF ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" 0 set-check" BF-APPEND-LINE ;

: BF-APPEND-CHECK-ON ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" ' HOOK set-check" BF-APPEND-LINE ;

: BF-APPEND-CHECKER-MODEL ( ptr u8 n -- ) {: out:ptr outu :}
   out outu BF-APPEND-CHECK-OFF
   out outu s" src/core/checker.f" BF-APPEND-SOURCE
   out outu s" src/core/render.f" BF-APPEND-SOURCE
   out outu s" src/core/check-hook.f" BF-APPEND-SOURCE
   out outu BF-APPEND-CHECK-ON ;

: BF-TARGET-UNKNOWN ( -- )
   s" build-fixpoint: unknown target" BF-BUILD-RC die ;

: BF-APPEND-TARGET-LAYOUT ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/layout.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/layout.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-SYS ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/sys.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/sys.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-TARGET-ENV ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/env.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/env.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-IMAGE-BYTES ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/os/image-bytes.f" BF-APPEND-SOURCE ;

: BF-APPEND-TARGET-IMAGE ( ptr u8 n -- ) {: out:ptr outu :}
   HB-TARGET-LINUX? if
      out outu s" src/os/linux/elf.f" BF-APPEND-SOURCE
      out outu s" src/os/linux/sign.f" BF-APPEND-SOURCE
      exit
   then
   HB-TARGET-MACOS? if
      out outu s" src/os/macos/macho.f" BF-APPEND-SOURCE
      out outu s" src/os/macos/sign2.f" BF-APPEND-SOURCE
      exit
   then
   BF-TARGET-UNKNOWN ;

: BF-APPEND-COMMON ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/arch/arm64/asm.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/icode.f" BF-APPEND-SOURCE
   out outu s" src/arch/arm64/mnem.f" BF-APPEND-SOURCE
   out outu BF-APPEND-TARGET-LAYOUT
   out outu BF-APPEND-TARGET-SYS
   out outu s" src/habu/layout.f" BF-APPEND-SOURCE
   out outu BF-APPEND-TARGET-ENV
   out outu s" src/core/sha256.f" BF-APPEND-SOURCE
   out outu s" src/core/roles.f" BF-APPEND-SOURCE
   out outu s" src/core/combinators.f" BF-APPEND-SOURCE
   out outu s" src/habu/treeshake.f" BF-APPEND-SOURCE
   out outu s" src/habu/rt.f" BF-APPEND-SOURCE
   out outu s" src/habu/crash.f" BF-APPEND-SOURCE
   out outu BF-APPEND-IMAGE-BYTES
   out outu BF-APPEND-TARGET-IMAGE
   out outu s" src/habu/habu1.f" BF-APPEND-SOURCE
   out outu s" src/habu/prof.f" BF-APPEND-SOURCE
   out outu s" src/habu/regalloc.f" BF-APPEND-SOURCE
   out outu s" src/habu/jit.f" BF-APPEND-SOURCE
   out outu s" src/habu/habu2.f" BF-APPEND-SOURCE
   out outu s" src/habu/xref.f" BF-APPEND-SOURCE ;

: BF-APPEND-DRIVER-IO ( ptr u8 n -- ) {: out:ptr outu :}
   out outu s" src/habu/driver-io.f" BF-APPEND-SOURCE ;

: BF-EMIT-SOURCE ( ptr u8 n ptr u8 n -- ) {: out:ptr outu driver:ptr driveru :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-CHECKER-MODEL
   out outu BF-APPEND-COMMON
   out outu BF-APPEND-DRIVER-IO
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
   s" stage2-got" BF-REMOVE-TMP
   s" hb-stage" BF-REMOVE-TMP
   s" bin/hb" s" stage2-src" BF-A$ BF-RUN-STAGE-PATH-INFILE BF-RC0
   s" stage2-got" BF-EXPECT
   s" stage2-got" s" hb-stage" BF-RENAME-TMP
   s" hb-stage" BF-CHMOD-X-TMP ;

: BF-RUN-STAGE ( -- )
   s" stage2-got" BF-REMOVE-TMP
   s" hb-stage" BF-RUN-STAGE-TMP BF-RC0
   s" stage2-got" BF-EXPECT ;

: BF-PROMOTE-STAGE ( -- )
   s" stage2-got" s" hb-stage" BF-RENAME-TMP
   s" hb-stage" BF-CHMOD-X-TMP ;

: BF-VERIFY-STAGE ( -- )
   s" hb-stage" BF-CODESIGN-VERIFY-TMP ;

: BF-STAGE-MATCH? ( -- bool )
   s" hb-stage" s" stage2-got" BF-TMP-FILE= ;

: BF-STAGE-FIXPOINT-FROM-SOURCE ( -- )
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
   s" bin/hb refresh OK: compiler fixpoint" type cr ;

: BF-STAGE-FIXPOINT ( -- )
   BF-PREFLIGHT
   BF-STAGE2-SOURCE
   BF-STAGE-FIXPOINT-FROM-SOURCE ;

: BF-BUILD-STDIN-FROM-STAGE ( -- )
   BF-STDIN-SOURCE
   BF-RUN-STAGE
   s" stage2-got" s" hb-stdin-mk" BF-RENAME-TMP
   s" hb-stdin-mk" BF-CHMOD-X-TMP
   s" hb-stdin-got" BF-REMOVE-TMP
   s" hb-stdin-mk" BF-RUN-ENV-TMP BF-RC0
   s" hb-stdin-got" BF-EXPECT
   s" hb-stdin-got" s" hb-stdin" BF-RENAME-TMP
   s" hb-stdin" BF-CHMOD-X-TMP
   s" hb-stdin" BF-CODESIGN-VERIFY-TMP ;

: BF-BUILD-STDIN ( -- )
   BF-PREFLIGHT
   BF-BUILD-STDIN-FROM-STAGE ;

: BF-BUILD-STDIN-FRESH ( -- )
   BF-STAGE-FIXPOINT
   BF-BUILD-STDIN-FROM-STAGE ;

: BF-BUILD-SNAP-FROM-STDIN ( -- )
   BF-SNAP-SOURCE
   s" hb-snap0" BF-REMOVE-TMP
   s" hb-new" BF-REMOVE-TMP
   s" hb-stdin" s" hb-snap-src" BF-RUN-ENV-TMP-INFILE BF-RC0
   s" hb-snap0" BF-EXPECT
   s" hb-snap0" s" hb-new" BF-RENAME-TMP
   s" hb-new" BF-CODESIGN-FORCE-TMP
   s" hb-new" BF-CHMOD-X-TMP
   s" hb-new" BF-EXPECT
   s" bin/hb refresh OK: candidate validated" type cr ;

: BF-BUILD-ALL ( -- )
   BF-STAGE-FIXPOINT
   BF-BUILD-STDIN-FROM-STAGE
   BF-BUILD-SNAP-FROM-STDIN ;

: BF-INSTALL-HB-NEW ( -- )
   s" hb-new" BF-A$ s" bin/hb" RENAME-FILE
   s" bin/hb" CHMOD-X ;

: BF-BIN-HB? ( ptr u8 n -- bool )
   s" bin/hb" STR= ;

: BF-REMOVE-BIN-OTHER ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE? if
      path pathu BF-BIN-HB? 0= if path pathu REMOVE-FILE then
   then ;

: BF-CLEAN-BIN ( -- )
   s" bin" [: BF-REMOVE-BIN-OTHER ;] WALK-FILES ;

: BF-INSTALL ( -- )
   BF-BUILD-ALL
   BF-INSTALL-HB-NEW
   BF-CLEAN-BIN
   s" bin/hb ready (checked engine, tty REPL + stdin)" type cr ;

: BF-USAGE ( -- )
   s" usage: tools/build-fixpoint.f [all|install|stage|stdin]" BF-USAGE-RC die ;

: BF-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: BF-MAIN ( -- )
   SCRIPT-ARGC 0= if BF-BUILD-ALL exit then
   SCRIPT-ARGC 1 <> if BF-USAGE then
   s" all" BF-ARG0= if BF-BUILD-ALL exit then
   s" install" BF-ARG0= if BF-INSTALL exit then
   s" stage" BF-ARG0= if BF-STAGE-FIXPOINT exit then
   s" stdin" BF-ARG0= if BF-BUILD-STDIN-FRESH exit then
   BF-USAGE ;
