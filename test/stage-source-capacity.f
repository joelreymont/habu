\ stage-source-capacity.f - stage2/maker synchronized source-capacity regression.
\ Run: bin/hb --load test/stage-source-capacity.f

require lib/errors.f
require lib/string.f
require lib/adt/option.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f

package STAGE-CAP

$100000 constant RETIRED-CAP
$200000 constant ACTIVE-CAP
$100001 constant ABOVE-RETIRED
$200001 constant ABOVE-ACTIVE
$10000 constant PAD-CAP
$4000 constant CAPTURE-CAP
120000 constant TIMEOUT-MS
$20 constant PAD-BYTE

create ROOT-BUF FS-PATH-CAP allot
create PAD-BUF PAD-CAP allot
create OUT-BUF CAPTURE-CAP allot
create ERR-BUF CAPTURE-CAP allot
variable ROOT-U

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb"
   then ;

: PAD-INIT ( -- )
   PAD-CAP 0 ?do PAD-BYTE PAD-BUF i + c! loop ;

: PAD-STEP ( ptr u8 n n -- ) {: path:ptr pathu:n left:n :}
   left PAD-CAP min {: size:n :}
   path pathu PAD-BUF size APPEND-FILE ;

: PAD-TO ( ptr u8 n n -- ) {: path:ptr pathu:n target:n :}
   path pathu FILE-SIZE target > if E-FS-CAPACITY throw then
   begin path pathu FILE-SIZE target < while
      path pathu target path pathu FILE-SIZE - PAD-STEP
   repeat ;

: RESET-TO ( ptr u8 n n -- ) {: path:ptr pathu:n target:n :}
   path pathu PAD-BUF 0 WRITE-ALL
   path pathu target PAD-TO ;

: STAGE-SOURCE ( -- ptr u8 n )
   BF-STAGE2-SOURCE
   s" stage2-src" BF-A$ ;

: MAKER-SOURCE ( -- ptr u8 n )
   \ A complete maker-driver engine source is valid maker input. The outer
   \ maker compiles it without executing its embedded MK-RUN, yielding the
   \ retained maker used by the overflow leg.
   s" hb-maker-src" s" src/habu/maker.f" BF-EMIT-SOURCE
   s" hb-maker-src" BF-A$ ;

: PREPARE-ARGV ( -- )
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+ ;

: PREPARE-BUILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   s" stage2-src" BF-A$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   BF-TMP$ >LEN PROC-ARGV+ ;

: RUN-DRIVER ( ptr u8 n -- len len outcome ) {: name:ptr nameu:n :}
   BF-PREPARE-ENV
   PREPARE-ARGV
   name nameu BF-A$ >LEN
   OUT-BUF CAPTURE-CAP >LEN
   ERR-BUF CAPTURE-CAP >LEN
   TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: RUN-BUILD ( -- len len outcome )
   BF-PREPARE-ENV
   PREPARE-BUILD-ARGV
   HB$ >LEN
   OUT-BUF CAPTURE-CAP >LEN
   ERR-BUF CAPTURE-CAP >LEN
   TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: ASSERT-EXIT ( outcome n -- ) {: want:n :}
   MATCH outcome
      exited OF want T= ENDOF
      signaled OF drop 1 0 T= ENDOF
      timeout OF 1 0 T= ENDOF
   ;MATCH ;

: ASSERT-SUCCESS-RESULT ( len len outcome -- )
   0 ASSERT-EXIT {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru LEN>N 0 T=
   s" stage2-got" BF-A$ FILE? TTRUE ;

: ASSERT-BUILD-SUCCESS ( -- )
   RUN-BUILD ASSERT-SUCCESS-RESULT ;

: RETAIN-DRIVER ( ptr u8 n -- ) {: name:ptr nameu:n :}
   s" stage2-got" name nameu BF-RENAME-TMP
   name nameu BF-CHMOD-X-TMP ;

: ASSERT-DIAG ( len ptr u8 n -- ) {: erru:len msg:ptr msgu:n :}
   ERR-BUF erru LEN>N msg msgu T$= ;

: ASSERT-OVERFLOW ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n msg:ptr msgu:n :}
   name nameu RUN-DRIVER 74 ASSERT-EXIT {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru msg msgu ASSERT-DIAG ;

: ASSERT-RETIRED-EDGE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu ABOVE-RETIRED PAD-TO
   path pathu FILE-SIZE RETIRED-CAP > TTRUE
   path pathu FILE-SIZE ACTIVE-CAP < TTRUE ;

: STAGE-POSITIVE ( -- )
   STAGE-SOURCE 2dup ASSERT-RETIRED-EDGE 2drop
   ASSERT-BUILD-SUCCESS
   s" cap-stage" RETAIN-DRIVER ;

: MAKER-POSITIVE ( -- )
   s" stage2-src" s" src/habu/maker.f" BF-EMIT-SOURCE
   MAKER-SOURCE 2dup ASSERT-RETIRED-EDGE 2drop
   ASSERT-BUILD-SUCCESS
   s" cap-maker" RETAIN-DRIVER ;

: STAGE-OVERFLOW ( -- )
   s" stage2-src" BF-A$ ABOVE-ACTIVE RESET-TO
   s" cap-stage" s" stage2: source exceeds buffer" ASSERT-OVERFLOW ;

: MAKER-OVERFLOW ( -- )
   s" hb-maker-src" BF-A$ ABOVE-ACTIVE RESET-TO
   s" cap-maker" s" maker: source exceeds buffer" ASSERT-OVERFLOW ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-stage-cap" TMPDIR-MKDIR ROOT!
   ROOT$ 2dup BF-TMP! CLEANUP-TREE+
   PAD-INIT ;

: RUN-ACT ( -- )
   PREPARE
   STAGE-POSITIVE
   MAKER-POSITIVE
   STAGE-OVERFLOW
   MAKER-OVERFLOW ;

public

: RUN ( -- )
   T-RESET
   [: RUN-ACT ;] catch {: rc:n :}
   BF-TMP-RESET
   CLEANUP-RUN
   rc 0 <> if rc throw then
   T-REPORT ;

;package

STAGE-CAP:RUN
