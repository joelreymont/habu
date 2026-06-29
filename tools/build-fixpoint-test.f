\ build-fixpoint-test.f - checked fixture for tools/build-fixpoint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f
\ lib/process-argv.f lib/process-env.f lib/build.f lib/codesign.f
\ tools/build-fixpoint.f tools/build-fixpoint-test.f

8192 constant BFT-CAPTURE-CAP
$80000 constant BFT-READ-CAP
120000 constant BFT-TIMEOUT-MS
13 constant BFT-BUILD-ARGV#

variable BFT-ROOT-U
variable BFT-HB-NEW-U
variable BFT-HB-U
variable BFT-STAGE2-U
variable BFT-RUN-U
variable BFT-SNAP-U
variable BFT-BUILD-FILES
variable BFT-READ-A
variable BFT-PROF-I
variable BFT-REG-I
variable BFT-JIT-I
variable BFT-IMG-I
variable BFT-IMG-BUILD-I
variable BFT-HABU1-I

create BFT-ROOT-BUF FS-PATH-CAP allot
create BFT-HB-NEW-BUF FS-PATH-CAP allot
create BFT-HB-BUF FS-PATH-CAP allot
create BFT-STAGE2-BUF FS-PATH-CAP allot
create BFT-RUN-BUF FS-PATH-CAP allot
create BFT-SNAP-BUF FS-PATH-CAP allot
create BFT-OUT BFT-CAPTURE-CAP allot
create BFT-ERR BFT-CAPTURE-CAP allot
create BFT-CHECK-OFF-LINE
10 c, 48 c, 32 c, 115 c, 101 c, 116 c, 45 c,
99 c, 104 c, 101 c, 99 c, 107 c, 10 c,

: BFT-READ-FIELD ( -- ptr ptr u8 )
   BFT-READ-A 0 ptr-field ;

: BFT-READ-BUF! ( ptr u8 -- )
   BFT-READ-FIELD ! ;

: BFT-READ-BUF ( -- ptr u8 )
   BFT-READ-FIELD @ ;

: BFT-ALLOC-READ ( -- )
   BFT-READ-CAP MEM-ALLOC-BYTES drop BFT-READ-BUF! ;

: BFT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: BFT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: BFT-ROOT ( -- ptr u8 n )
   BFT-ROOT-BUF BFT-ROOT-U @ ;

: BFT-HB-NEW ( -- ptr u8 n )
   BFT-HB-NEW-BUF BFT-HB-NEW-U @ ;

: BFT-HB ( -- ptr u8 n )
   BFT-HB-BUF BFT-HB-U @ ;

: BFT-STAGE2 ( -- ptr u8 n )
   BFT-STAGE2-BUF BFT-STAGE2-U @ ;

: BFT-RUN ( -- ptr u8 n )
   BFT-RUN-BUF BFT-RUN-U @ ;

: BFT-SNAP ( -- ptr u8 n )
   BFT-SNAP-BUF BFT-SNAP-U @ ;

: BFT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: BFT-CHECK-OFF-LINE$ ( -- ptr u8 n )
   BFT-CHECK-OFF-LINE 13 ;

: BFT-ENV$ ( -- ptr u8 n )
   BFT-ROOT ;

: BFT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: BFT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-build-fixpoint" TMPDIR-MKDIR {: a:ptr u :}
   a u BFT-ROOT-BUF BFT-ROOT-U BFT-COPY!
   BFT-ALLOC-READ
   BFT-ROOT CLEANUP-TREE+
   BFT-ROOT s" hb-new" BFT-HB-NEW-BUF BFT-HB-NEW-U BFT-PATH!
   BFT-ROOT s" hb-stdin" BFT-HB-BUF BFT-HB-U BFT-PATH!
   BFT-ROOT s" stage2-src" BFT-STAGE2-BUF BFT-STAGE2-U BFT-PATH!
   BFT-ROOT s" stage2-run-src" BFT-RUN-BUF BFT-RUN-U BFT-PATH!
   BFT-ROOT s" hb-snap-src" BFT-SNAP-BUF BFT-SNAP-U BFT-PATH! ;

: BFT-ARGV-BUILD ( -- n )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN BFT-ENV$ >LEN PROC-ENV+
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f" BFT-ARG+
   s" lib/string.f" BFT-ARG+
   s" lib/memory.f" BFT-ARG+
   s" lib/fs.f" BFT-ARG+
   s" lib/fs-mutate.f" BFT-ARG+
   s" lib/process.f" BFT-ARG+
   s" lib/process-argv.f" BFT-ARG+
   s" lib/process-env.f" BFT-ARG+
   s" lib/build.f" BFT-ARG+
   s" lib/codesign.f" BFT-ARG+
   s" tools/build-fixpoint.f" BFT-ARG+
   s" tools/build-fixpoint-main.f" BFT-ARG+
   PROC-ARGV-N @ COUNT>N ;

: BFT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: BFT-RUN-BUILD ( -- n n n )
   BFT-ARGV-BUILD BFT-BUILD-ARGV# T=
   s" bin/hb" >LEN PROC-ARGV-CHECK-PATH
   BFT-CAPTURE-CAP >LEN BFT-CAPTURE-CAP >LEN PROC-CAPTURE-CHECK-CAPS
   s" bin/hb" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   BFT-TIMEOUT-MS >MS PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   BFT-OUT BFT-CAPTURE-CAP >LEN BFT-ERR BFT-CAPTURE-CAP >LEN PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC
   BFT-CAPTURE>N ;

: BFT-TEST-BUILD ( -- )
   BFT-RUN-BUILD 0 T=
   {: outu erru :}
   BFT-ERR erru BFT-EMPTY$ T$=
   BFT-OUT outu s" bin/hb refresh OK: compiler fixpoint" CONTAINS? TTRUE
   BFT-OUT outu s" snapshot image OK: candidate validated" CONTAINS? TFALSE
   BFT-HB FILE? TTRUE
   BFT-HB-NEW FILE? TFALSE ;

: BFT-READ ( ptr u8 n -- n )
   BFT-READ-BUF BFT-READ-CAP READ-ALL ;

: BFT-FIND-AFTER ( ptr u8 n n ptr u8 n -- n ) {: a:ptr u start needle:ptr nu :}
   start 0 < if -1 exit then
   start u >= if -1 exit then
   a start BYTE+ u start - needle nu FIND-SUB
   dup 0 < if exit then
   start + ;

: BFT-FOUND ( n -- )
   0 >= TTRUE ;

: BFT-NOT-FOUND ( n -- )
   0 < TTRUE ;

: BFT-TEST-STAGE2-SOURCE ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : HOOK ( ptr u8 n -- n ) CHECK! dup -1 <> if 70 throw then ; ' HOOK set-check" CONTAINS? TFALSE
   BFT-READ-BUF u s" variable SEQ" CONTAINS? TTRUE
   BFT-READ-BUF u s" : STR=" CONTAINS? TFALSE
   BFT-READ-BUF u s" : CORE-STR=" CONTAINS? TTRUE
   BFT-READ-BUF u s" include src/core/checker-registry.f" CONTAINS? TFALSE
   BFT-READ-BUF u s" checker-registry.f - typed checker effect store" CONTAINS? TTRUE
   BFT-READ-BUF u s" src/core/include.f" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-MMAP-PTR ( n -- ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-EVALUATE ( ptr u8 n -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : include ( -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" BFR-USIGS-RESET" CONTAINS? TTRUE
   BFT-READ-BUF u s" BFR-CHECK-OFF" CONTAINS? TTRUE
   BFT-READ-BUF u s" BFR-HIDE-DICT-FROM-EARLIEST" CONTAINS? TTRUE
   BFT-READ-BUF u s" : ATOMA-FIELD ( n -- ptr ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : ATOMA-FIELD" CONTAINS? TTRUE
   BFT-READ-BUF u s" 0 constant T-CON" CONTAINS? TTRUE
   BFT-READ-BUF u s" ' HOOK set-check" CONTAINS? TTRUE
   BFT-READ-BUF u s" STDIN-OUT" CONTAINS? TTRUE ;

: BFT-TEST-NO-STAGE2-RUN-SOURCE ( -- )
   BFT-RUN FILE? TFALSE ;

: BFT-TEST-CHECKED-REGALLOC ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : BPROF-ON" FIND-SUB dup BFT-FOUND BFT-PROF-I !
   BFT-READ-BUF u BFT-PROF-I @ s" : EMIT-VRINIT" BFT-FIND-AFTER dup BFT-FOUND BFT-REG-I !
   BFT-READ-BUF u BFT-REG-I @ s" : FOLD-ENTRY" BFT-FIND-AFTER dup BFT-FOUND BFT-JIT-I !
   BFT-PROF-I @ BFT-REG-I @ < TTRUE
   BFT-REG-I @ BFT-JIT-I @ < TTRUE ;

: BFT-TEST-CHECKED-TARGET-IMAGE ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : ASM-CODELEN!" FIND-SUB dup BFT-FOUND BFT-IMG-I !
   BFT-READ-BUF u BFT-IMG-I @ s" : BUILD-IMAGE" BFT-FIND-AFTER dup BFT-FOUND BFT-IMG-BUILD-I !
   BFT-READ-BUF u BFT-IMG-BUILD-I @ s" : RPD@" BFT-FIND-AFTER dup BFT-FOUND BFT-HABU1-I !
   BFT-READ-BUF u BFT-IMG-BUILD-I @ BFT-CHECK-OFF-LINE$ BFT-FIND-AFTER BFT-NOT-FOUND
   BFT-IMG-I @ BFT-IMG-BUILD-I @ < TTRUE
   BFT-IMG-BUILD-I @ BFT-HABU1-I @ < TTRUE ;

: BFT-TEST-STAGE2-SCRATCH ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" S2-SOURCE-CAP allot" CONTAINS? TFALSE
   BFT-READ-BUF u s" stage2: source mmap failed" CONTAINS? TTRUE ;

: BFT-TEST-SNAP-SOURCE ( -- )
   BFT-ROOT BF-TMP!
   BF-SNAP-SOURCE
   BFT-SNAP BFT-READ {: u :}
   BFT-READ-BUF u s" : ATOMA-FIELD ( n -- ptr ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-MMAP-PTR ( n -- ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-EVALUATE ( ptr u8 n -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : INCLUDE-READ-ALL ( ptr u8 n -- ptr u8 n )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : included ( ptr u8 n -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" SNAP-MAGIC" CONTAINS? TTRUE
   BF-TMP-RESET ;

: BFT-TEST-TMP-OVERRIDE ( -- )
   BFT-ROOT BF-TMP!
   BF-TMP$ BFT-ROOT T$=
   s" stage2-src" BF-A$ BFT-STAGE2 T$=
   BF-STAGE2-SOURCE
   BFT-STAGE2 FILE? TTRUE
   BFT-TEST-STAGE2-SCRATCH
   BF-TMP-RESET ;

: BFT-TEST-STAGE-ARGV-RESET ( -- )
   BFT-ROOT BF-TMP!
   PROC-ARGV-RESET
   s" stale" >LEN PROC-ARGV+
   s" bin/hb" BF-PREPARE-STAGE-ARGV 2drop
   PROC-ARGV-N @ COUNT>N 2 T=
   PROC-ARGV-RESET
   s" stale" >LEN PROC-ARGV+
   s" bin/hb" s" stage2-src" BF-A$ BF-PREPARE-LOAD-STAGE-ARGV 2drop
   PROC-ARGV-N @ COUNT>N 4 T=
   BF-TMP-RESET ;

: BFT-BUILD-FILE? ( ptr u8 n -- bool )
   BASENAME s" build-" STARTS-WITH? ;

: BFT-CHECK-BUILD-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u FILE? if
      a u BFT-BUILD-FILE? if
         BFT-BUILD-FILES @ 1 + BFT-BUILD-FILES !
      then
   then ;

: BFT-TEST-NO-BUILD-SHIMS ( -- )
   0 BFT-BUILD-FILES !
   BFT-ROOT [: BFT-CHECK-BUILD-FILE ;] WALK-FILES
   BFT-BUILD-FILES @ 0 T= ;

: BFT-MAIN ( -- )
   T-RESET
   BFT-PREPARE
   BFT-TEST-TMP-OVERRIDE
   BFT-TEST-STAGE-ARGV-RESET
   BFT-TEST-BUILD
   BFT-TEST-NO-BUILD-SHIMS
   BFT-TEST-STAGE2-SOURCE
   BFT-TEST-NO-STAGE2-RUN-SOURCE
   BFT-TEST-CHECKED-TARGET-IMAGE
   BFT-TEST-CHECKED-REGALLOC
   BFT-TEST-SNAP-SOURCE
   CLEANUP-RUN
   BFT-ROOT EXISTS? TFALSE
   T-REPORT
   s" build-fixpoint-test: ok" type cr ;

BFT-MAIN
