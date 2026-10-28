\ A cold REPL builder reports its own deadline through hb-build, then a saved
\ builder works from another working directory, a CWD producer shadow forces a
\ rebuild, and so does the caller's tier. One program built at tier 0 and at
\ tier 1 runs, compiles from stdin and saves itself, and the saved image runs.
\ Run: bin/hb --load tools/hb-build-repl-image-test.f

require tools/hb-build-test-lib.f
require src/lib/string.f
require src/lib/process-cwd.f
require src/lib/tier.f
require test/app-builder-engine.f

package HB-BUILD-CLI

create HBT-IMAGE-HB-BUF FS-PATH-CAP allot
create HBT-IMAGE-TOOL-BUF FS-PATH-CAP allot
create HBT-IMAGE-SHADOW-BUF FS-PATH-CAP allot
create HBT-IMAGE-SOURCE-BUF FS-PATH-CAP allot
create HBT-IMAGE-TIER1-BUF FS-PATH-CAP allot
variable HBT-IMAGE-HB-U
variable HBT-IMAGE-TOOL-U
variable HBT-IMAGE-SHADOW-U
variable HBT-IMAGE-SOURCE-U
variable HBT-IMAGE-TIER1-U
variable HBT-IMAGE-TIER1
variable HBT-IMAGE-SIZE
variable HBT-IMAGE-SEC
variable HBT-IMAGE-NS
create HBT-IMAGE-RESAVED-BUF FS-PATH-CAP allot
variable HBT-IMAGE-RESAVED-U

: HBT-IMAGE-HB$ ( -- ptr u8 n ) HBT-IMAGE-HB-BUF HBT-IMAGE-HB-U @ ;
: HBT-IMAGE-TOOL$ ( -- ptr u8 n ) HBT-IMAGE-TOOL-BUF HBT-IMAGE-TOOL-U @ ;
: HBT-IMAGE-SHADOW$ ( -- ptr u8 n ) HBT-IMAGE-SHADOW-BUF HBT-IMAGE-SHADOW-U @ ;
: HBT-IMAGE-TIER1$ ( -- ptr u8 n ) HBT-IMAGE-TIER1-BUF HBT-IMAGE-TIER1-U @ ;
: HBT-IMAGE-RESAVED$ ( -- ptr u8 n ) HBT-IMAGE-RESAVED-BUF HBT-IMAGE-RESAVED-U @ ;

: HBT-IMAGE-PATHS ( -- )
   SOURCE-ROOT:ENGINE$ s" bin/hb" HBT-IMAGE-HB-BUF JOIN-PATH HBT-IMAGE-HB-U !
   SOURCE-ROOT:ENGINE$ s" tools/hb-build.f" HBT-IMAGE-TOOL-BUF JOIN-PATH HBT-IMAGE-TOOL-U !
   SOURCE-ROOT:ENGINE$ s" test/compiler/aot-mode.f"
      HBT-IMAGE-TIER1-BUF JOIN-PATH HBT-IMAGE-TIER1-U ! ;

: HBT-IMAGE-SHADOW-PATH ( -- )
   HBT-ROOT s" tools/app-build.f" HBT-IMAGE-SHADOW-BUF JOIN-PATH HBT-IMAGE-SHADOW-U ! ;

\ An empty root leaves HABU_OUT inherited: under the gate, the run's shared out
\ root, which holds the app-builder family image. A private out root keeps its
\ builder images private.
: HBT-IMAGE-ENV ( ptr u8 n ptr u8 n -- )
   {: limit:ptr limitu:n root:ptr rootu:n :}
   PROC-ENV-RESET
   s" HB_TMP" >LEN HBT-TMP >LEN PROC-ENV+
   rootu 0 > if s" HABU_OUT" >LEN root rootu >LEN PROC-ENV+ then
   s" HABU_FIXPOINT_ENGINE" >LEN HBT-IMAGE-HB$ >LEN PROC-ENV+
   s" HB_BUILD_TIMEOUT_MS" >LEN limit limitu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ The command runs at the CLI's default tier, or after test/compiler/aot-mode.f
\ selected tier 1 while HBT-IMAGE-TIER1 is set.
: HBT-IMAGE-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   HBT-IMAGE-TIER1 @ if HBT-IMAGE-TIER1$ >LEN PROC-ARGV+ then
   HBT-IMAGE-TOOL$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --repl" >LEN PROC-ARGV+ ;

: HBT-IMAGE-RUN-ON ( ptr u8 n -- n n n ) {: cwd:ptr cwdu:n :}
   HBT-IMAGE-HB$ >LEN cwd cwdu >LEN
   HBT-OUT HBT-CAPTURE-CAP >LEN HBT-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE HBT-CAPTURE>N ;

\ A cold attempt reports its deadline in text form and leaves no output image.
\ The cold deadline needs an out root with no builder: in a warm root the
\ deadline would time out the saved builder instead of the build.
: HBT-COLD-REPL-TIMEOUT-TEXT ( -- )
   s" 1" HBT-TMP HBT-IMAGE-ENV
   HBT-IMAGE-ARGS
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   SOURCE-ROOT:CWD$ HBT-IMAGE-RUN-ON {: outu:n erru:n rc:n :}
   rc HBB-UNCAUGHT-RC T=
   outu 0 T=
   HBT-ERR erru s" hb-build: code=E-PROC-TIMEOUT maker=repl" CONTAINS? TTRUE
   HBT-ERR erru HBT-REPL-SRC CONTAINS? TTRUE
   HBT-ERR erru s" timeout_ms=1" CONTAINS? TTRUE
   HBT-REPL-OUT EXISTS? TFALSE ;

: HBT-ROOTED-APP$ ( -- ptr u8 n )
   S\" require src/lib/json-read.f\n: MAIN ( -- ) s\q from-root\q type cr ;\n" ;

\ The rooted build uses the given out root; an empty one inherits the run's root
\ and its app-builder family image.
: HBT-ROOTED-BUILD ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   s" 600000" root rootu HBT-IMAGE-ENV
   HBT-IMAGE-ARGS
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   HBT-ROOT HBT-IMAGE-RUN-ON {: outu:n erru:n rc:n :}
   rc 0 <> if HBT-OUT outu type HBT-ERR erru type then
   rc 0 T=
   erru 0 T=
   HBT-REPL-OUT EXECUTABLE? TTRUE ;

: HBT-ROOTED-RUN ( -- )
   HBT-REPL-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
   HBT-RUN-ERR HBT-CAPTURE-CAP >LEN HBT-TIMEOUT-MS >MS
   RUN-CAPTURE HBT-CAPTURE>N {: outu:n erru:n rc:n :}
   rc 0 T=
   HBT-RUN-OUT outu s" from-root" CONTAINS? TTRUE
   HBT-RUN-ERR erru HBT-EMPTY$ T$= ;

: HBT-ROOTED-MARK ( -- )
   HBT-REPL-OUT FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   size HBT-IMAGE-SIZE ! sec HBT-IMAGE-SEC ! ns HBT-IMAGE-NS ! ;

: HBT-ROOTED-SAME? ( -- bool )
   HBT-REPL-OUT FILE-META {: size:n sec:n ns:n csec:n cns:n :}
   csec drop cns drop
   size HBT-IMAGE-SIZE @ = sec HBT-IMAGE-SEC @ = and
   ns HBT-IMAGE-NS @ = and ;

: HBT-ROOTED-COLD-WARM ( -- )
   HBT-REPL-SRC HBT-ROOTED-APP$ WRITE-ALL
   NULL$ HBT-ROOTED-BUILD
   HBT-ROOTED-RUN
   HBT-ROOTED-MARK
   NULL$ HBT-ROOTED-BUILD
   HBT-ROOTED-SAME? TTRUE
   HBT-ROOTED-RUN ;

\ A newly present CWD producer shadows the selected engine root's fallback
\ copy. The existing output must be rebuilt even though its old files match.
\ Editing that shadow after the warm no-op build rebuilds again. The builder
\ key folds the shadow's path under this run's fresh root, so these builders
\ can never hit again: they build in the row's private cache.
: HBT-ROOTED-PRODUCER-SHADOW ( -- )
   HBT-IMAGE-SHADOW-PATH
   HBT-IMAGE-SHADOW$ SOURCE-ROOT:DIRNAME MAKE-DIR
   SOURCE-ROOT:ENGINE$ s" tools/app-build.f"
      HBT-IMAGE-SOURCE-BUF JOIN-PATH HBT-IMAGE-SOURCE-U !
   HBT-IMAGE-SOURCE-BUF HBT-IMAGE-SOURCE-U @
      HBT-IMAGE-SHADOW$ COPY-FILE-STREAM
   HBT-TMP HBT-ROOTED-BUILD
   HBT-ROOTED-SAME? TFALSE
   HBT-ROOTED-RUN
   HBT-ROOTED-MARK
   HBT-TMP HBT-ROOTED-BUILD
   HBT-ROOTED-SAME? TTRUE
   HBT-IMAGE-SHADOW$ S\" \\ edited\n" APPEND-FILE
   HBT-TMP HBT-ROOTED-BUILD
   HBT-ROOTED-SAME? TFALSE
   HBT-ROOTED-RUN ;

\ The sidecar records the tier the application compiled at. The last build ran
\ at the CLI's default tier, 0 on ARM64, so the same build from a tier 1 caller
\ is not up to date. x86-64 has tier 1 only.
: HBT-ROOTED-TIER ( -- )
   HB-TARGET-LINUX-X86-64? if exit then
   HBT-ROOTED-MARK
   -1 HBT-IMAGE-TIER1 !
   HBT-TMP HBT-ROOTED-BUILD
   0 HBT-IMAGE-TIER1 !
   HBT-ROOTED-SAME? TFALSE
   HBT-ROOTED-RUN ;

\ Run an image with one stdin script: it exits 0 with nothing on stderr, and
\ leaves its stdout in HBT-RUN-OUT.
: HBT-IMAGE-STDIN-RUN ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n in:ptr inu:n :}
   PROC-ARGV-RESET
   exe exeu >LEN in inu >LEN
   HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE HBT-CAPTURE>N {: outu:n errn:n rc:n :}
   rc 0 <> if HBT-RUN-OUT outu type HBT-RUN-ERR errn type then
   rc 0 T=
   errn 0 T=
   outu ;


\ The first run's script: define and run X, then save the session.
: HBT-IMAGE-SAVE-SCRIPT$ ( -- ptr u8 n )
   SB-RESET
   S\" : X ( -- n ) 7 ; X .\ns\q " SB-APPEND
   HBT-IMAGE-RESAVED$ SB-APPEND
   S\" \q APP-IMAGE:SAVE\n" SB-APPEND
   SB$ ;


\ Leave in the string builder what an image of the subject loaded at TIER prints
\ for one stdin script that runs X: MAIN's text, then X's 7.
: HBT-REPL-TIER-OUT ( n -- )
   HBT-SUBJECT-OUT
   S\" 7\n" SB-APPEND ;


\ Build HBT-REPL-SRC --repl from a caller at TIER, then select the tier this row
\ ran at. The image runs MAIN, compiles X from stdin and saves itself; the saved
\ image runs MAIN again and still knows X. Each run prints exactly the text of
\ a subject loaded at TIER.
: HBT-REPL-TIER-IMAGE ( n -- )
   tier@ {: tier:n prior:n :}
   tier TIER:SELECT
   HBT-REPL-SRC HBT-REPL-OUT HBT-HBB-PREPARE-REPL
   HBT-HBB-BUILD-OUT
   prior TIER:SELECT
   HBT-IMAGE-RESAVED$ HBT-REMOVE-FILE?
   HBT-REPL-OUT HBT-IMAGE-SAVE-SCRIPT$ HBT-IMAGE-STDIN-RUN {: firstu:n :}
   tier HBT-REPL-TIER-OUT
   HBT-RUN-OUT firstu SB$ T$=
   HBT-IMAGE-RESAVED$ S\" X .\n" HBT-IMAGE-STDIN-RUN {: againu:n :}
   tier HBT-REPL-TIER-OUT
   HBT-RUN-OUT againu SB$ T$= ;


\ A REPL IMAGE OF TIER 0 CODE COMPILES, SAVES ITSELF AND RUNS LIKE ITS TIER 1
\ TWIN. test/tier0-product-subject.f is built --repl from a tier 1 caller, then
\ from a tier 0 caller into the same output. Each image and the image it saves
\ print the subject's text, the tier the subject loaded at - the compiler of
\ its definitions - and X's 7. The second build's tier line reads 0, so it
\ rebuilt the tier 1 image and loaded the subject at tier 0.
: HBT-REPL-TIER-PROGRAM ( -- )
   HB-TARGET-LINUX-X86-64? if exit then
   HBT-ROOT s" resaved" HBT-IMAGE-RESAVED-BUF HBT-IMAGE-RESAVED-U HBT-PATH!
   s" test/tier0-product-subject.f" SOURCE-ROOT:RESOLVE drop
   HBT-REPL-SRC COPY-FILE-STREAM
   1 HBT-REPL-TIER-IMAGE
   0 HBT-REPL-TIER-IMAGE ;

public
: HBT-REPL-IMAGE-MAIN ( -- )
   T-RESET
   APP-BUILDER-ENGINE:ENSURE
   HBT-PREPARE
   HBT-IMAGE-PATHS
   HBT-COLD-REPL-TIMEOUT-TEXT
   HBT-ROOTED-COLD-WARM
   HBT-ROOTED-PRODUCER-SHADOW
   HBT-ROOTED-TIER
   HBT-REPL-TIER-PROGRAM
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-repl-image-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-REPL-IMAGE-MAIN
