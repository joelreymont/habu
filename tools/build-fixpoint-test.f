\ build-fixpoint-test.f - checked fixture for tools/build-fixpoint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/build.f tools/build-fixpoint.f tools/build-fixpoint-test.f

8192 constant BFT-CAPTURE-CAP
$80000 constant BFT-READ-CAP
120000 constant BFT-TIMEOUT-MS

variable BFT-ROOT-U
variable BFT-HB-NEW-U
variable BFT-STAGE2-U
variable BFT-SNAP-U
variable BFT-BUILD-FILES

create BFT-ROOT-BUF FS-PATH-CAP allot
create BFT-HB-NEW-BUF FS-PATH-CAP allot
create BFT-STAGE2-BUF FS-PATH-CAP allot
create BFT-SNAP-BUF FS-PATH-CAP allot
create BFT-OUT BFT-CAPTURE-CAP allot
create BFT-ERR BFT-CAPTURE-CAP allot
create BFT-READ-BUF BFT-READ-CAP allot

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

: BFT-STAGE2 ( -- ptr u8 n )
   BFT-STAGE2-BUF BFT-STAGE2-U @ ;

: BFT-SNAP ( -- ptr u8 n )
   BFT-SNAP-BUF BFT-SNAP-U @ ;

: BFT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: BFT-ENV$ ( -- ptr u8 n )
   BFT-ROOT ;

: BFT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-build-fixpoint" TMPDIR-MKDIR {: a:ptr u :}
   a u BFT-ROOT-BUF BFT-ROOT-U BFT-COPY!
   BFT-ROOT CLEANUP-TREE+
   BFT-ROOT s" hb-new" BFT-HB-NEW-BUF BFT-HB-NEW-U BFT-PATH!
   BFT-ROOT s" stage2-src" BFT-STAGE2-BUF BFT-STAGE2-U BFT-PATH!
   BFT-ROOT s" hb-snap-src" BFT-SNAP-BUF BFT-SNAP-U BFT-PATH! ;

: BFT-ARGV-BUILD ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" BFT-ENV$ PROC-ENV+
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/process-env.f" PROC-ARGV+
   s" lib/build.f" PROC-ARGV+
   s" tools/build-fixpoint.f" PROC-ARGV+
   s" tools/build-fixpoint-main.f" PROC-ARGV+ ;

: BFT-RUN-BUILD ( -- n n n )
   BFT-ARGV-BUILD
   s" bin/hb" BFT-OUT BFT-CAPTURE-CAP BFT-ERR BFT-CAPTURE-CAP
   BFT-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE ;

: BFT-TEST-BUILD ( -- )
   BFT-RUN-BUILD 0 T=
   {: outu erru :}
   BFT-ERR erru BFT-EMPTY$ T$=
   BFT-OUT outu s" build OK: stage compiler fixpoint" CONTAINS? TTRUE
   BFT-OUT outu s" build OK: hb-new validated" CONTAINS? TTRUE
   BFT-HB-NEW FILE? TTRUE ;

: BFT-READ ( ptr u8 n -- n )
   BFT-READ-BUF BFT-READ-CAP READ-ALL ;

: BFT-TEST-STAGE2-SOURCE ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : HOOK CHECK ; ' HOOK set-check" CONTAINS? TTRUE
   BFT-READ-BUF u s" STDIN-OUT" CONTAINS? TTRUE ;

: BFT-TEST-SNAP-SOURCE ( -- )
   BFT-SNAP BFT-READ {: u :}
   BFT-READ-BUF u s" SNAP-MAGIC" CONTAINS? TTRUE ;

: BFT-TEST-TMP-OVERRIDE ( -- )
   BFT-ROOT BF-TMP!
   BF-TMP$ BFT-ROOT T$=
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
   BFT-TEST-BUILD
   BFT-TEST-NO-BUILD-SHIMS
   BFT-TEST-STAGE2-SOURCE
   BFT-TEST-SNAP-SOURCE
   CLEANUP-RUN
   BFT-ROOT EXISTS? TFALSE
   T-REPORT
   s" build-fixpoint-test: ok" type cr ;

BFT-MAIN
