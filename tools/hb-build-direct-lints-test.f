\ hb-build-direct-lints-test.f - production-path direct lint hook fixtures.
\ Run: bin/hb --load tools/hb-build-direct-lints-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/outcome.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/engine-candidate.f
require tools/build-fixpoint.f
require tools/cli-run.f
require tools/object-image.f
require tools/hb-build-lib.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require lib/json-write.f
require tools/lint/source-lex.f
require tools/aot-lint-core.f
require tools/signature-lint-core.f

package HB-BUILD-DIRECT-LINTS-TEST
private

$7A01 constant AOT-SENTINEL-RC
$7A02 constant SIG-SENTINEL-RC

: AOT-SENTINEL ( -- )
   AOT-SENTINEL-RC throw ;

: SIG-SENTINEL ( -- )
   SIG-SENTINEL-RC throw ;

: INSTALL-SENTINELS ( -- )
   [: AOT-SENTINEL ;] is HBB-AOT-LINT-HOOK
   [: SIG-SENTINEL ;] is HBB-SIGNATURE-LINT-HOOK ;

INSTALL-SENTINELS

;package

require tools/hb-build-direct-lints.f

package HB-BUILD-DIRECT-LINTS-TEST
private

$4000 constant CAP
30000 constant TIMEOUT-MS

create ROOT-BUF FS-PATH-CAP allot
create GOOD-BUF FS-PATH-CAP allot
create AOT-BAD-BUF FS-PATH-CAP allot
create SIG-BAD-BUF FS-PATH-CAP allot
create MISSING-BUF FS-PATH-CAP allot
create OUT CAP allot
create ERR CAP allot

variable ROOT-U
variable GOOD-U
variable AOT-BAD-U
variable SIG-BAD-U
variable MISSING-U

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: GOOD$ ( -- ptr u8 n )
   GOOD-BUF GOOD-U @ ;

: AOT-BAD$ ( -- ptr u8 n )
   AOT-BAD-BUF AOT-BAD-U @ ;

: SIG-BAD$ ( -- ptr u8 n )
   SIG-BAD-BUF SIG-BAD-U @ ;

: MISSING$ ( -- ptr u8 n )
   MISSING-BUF MISSING-U @ ;

: GOOD-SRC$ ( -- ptr u8 n )
   S\" : HBBDL-GOOD ( -- ) ;\n" ;

: AOT-BAD-SRC$ ( -- ptr u8 n )
   S\" : HBBDL-AOT-BAD ( -- ) 0 0 patch32 ;\n" ;

: SIG-BAD-SRC$ ( -- ptr u8 n )
   S\" : HBBDL-SIG-BAD dup ;\n" ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-direct-lints" TMPDIR-MKDIR ROOT!
   ROOT$ CLEANUP-DIR+
   ROOT$ s" good.f" GOOD-BUF JOIN-PATH GOOD-U !
   ROOT$ s" aot-bad.f" AOT-BAD-BUF JOIN-PATH AOT-BAD-U !
   ROOT$ s" sig-bad.f" SIG-BAD-BUF JOIN-PATH SIG-BAD-U !
   ROOT$ s" missing-hb" MISSING-BUF JOIN-PATH MISSING-U !
   GOOD$ CLEANUP+
   AOT-BAD$ CLEANUP+
   SIG-BAD$ CLEANUP+
   GOOD$ GOOD-SRC$ WRITE-ALL
   AOT-BAD$ AOT-BAD-SRC$ WRITE-ALL
   SIG-BAD$ SIG-BAD-SRC$ WRITE-ALL ;

: SET-PATHS ( ptr u8 n -- )
   2dup HBB-PATHS! ;

: TEST-AOT-GOOD ( -- )
   HBB-RESET-OPTIONS
   GOOD$ SET-PATHS
   [: HBB-RUN-AOT-LINT ;] catch 0 T= ;

: TEST-SIG-GOOD ( -- )
   HBB-RESET-OPTIONS
   GOOD$ SET-PATHS
   HBB-STRICT-ON
   [: HBB-RUN-SIGNATURE-LINT ;] catch 0 T= ;

: TEST-DIRECT-GOOD ( -- )
   MISSING$ EXISTS? TFALSE
   MISSING$ MISSING$ CLI-TOOLS!
   s" direct AOT hook replaces child hook" T-LABEL
   TEST-AOT-GOOD
   s" direct signature hook replaces child hook" T-LABEL
   TEST-SIG-GOOD
   s" " s" " CLI-TOOLS! ;

: ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: RUN-CHILD ( ptr u8 n ptr u8 n -- len len outcome )
   {: mode:ptr modeu:n path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" --load" ARG+
   s" tools/hb-build-direct-lints-test.f" ARG+
   s" --" ARG+
   mode modeu ARG+
   path pathu ARG+
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN
   ERR CAP >LEN
   TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME ;

: EXPECT-FAIL ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n path:ptr pathu:n code:ptr codeu:n :}
   mode modeu path pathu RUN-CHILD
   1 T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   ERR erru code codeu CONTAINS? TTRUE ;

: TEST-AOT-BAD ( -- )
   s" aot-bad" AOT-BAD$ s" E-AOT-UNSUPPORTED" EXPECT-FAIL ;

: TEST-SIG-BAD ( -- )
   s" signature-bad" SIG-BAD$ s" E-MISSING-SIGNATURE" EXPECT-FAIL ;

: CHILD-PATHS ( -- )
   1 SCRIPT-ARGV$ SET-PATHS ;

: CHILD-AOT ( -- )
   HBB-RESET-OPTIONS
   CHILD-PATHS
   HBB-RUN-AOT-LINT
   s" " 0 die ;

: CHILD-SIG ( -- )
   HBB-RESET-OPTIONS
   CHILD-PATHS
   HBB-STRICT-ON
   HBB-RUN-SIGNATURE-LINT
   s" " 0 die ;

: AOT-MODE? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" aot-bad" STR= exit then
   0 0= 0= ;

: SIG-MODE? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" signature-bad" STR= exit then
   0 0= 0= ;

: CHILD-MODE? ( -- bool )
   AOT-MODE? SIG-MODE? or ;

: CHILD-MAIN ( -- )
   SCRIPT-ARGC 2 <> if E-TBL-BOUNDS throw then
   AOT-MODE? if CHILD-AOT then
   SIG-MODE? if CHILD-SIG then
   E-TBL-BOUNDS throw ;

: TEST-MAIN ( -- )
   T-RESET
   PREPARE
   TEST-DIRECT-GOOD
   s" direct AOT failure propagates" T-LABEL
   TEST-AOT-BAD
   s" direct signature failure propagates" T-LABEL
   TEST-SIG-BAD
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" hb-build-direct-lints-test: ok" type cr ;

: MAIN ( -- )
   CHILD-MODE? if CHILD-MAIN then
   TEST-MAIN ;

MAIN

;package
