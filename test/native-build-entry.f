\ The production entry refuses a bad command line before it loads the build
\ closure (tools/native-build-args.f), so no refusal here but the last starts a
\ build. No output argument is refused by name; so is a bad second argument:
\ the class the build is being asked for is settled before the target load, so
\ a typo cannot quietly produce a product. So are a `--target` naming no target
\ and one whose machine this engine has no backend for. With that backend
\ loaded, the window for the other machine loads and is captured, and the build
\ stops before it loads a writer. The entry's accepted path, the closure
\ compiling under the native build guard, is every gate's whitebox engine build
\ (test/whitebox-engine.f runs this entry with `whitebox`).
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package NATIVE-BUILD-ENTRY-TEST

$4000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: ENTRY-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   s" tools/native-build.f" ARG ;

: DRIVE ( -- )
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !
   erru LEN>N ERR-U !
   rc RC ! ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

\ Every refusal here is the same shape: exit 74, nothing on stdout, and one
\ named line on stderr. Show the capture when the code is wrong, so a refusal
\ that moved is read from its own output instead of guessed at.
: REFUSED ( ptr u8 n -- ) {: want:ptr wantu:n :}
   RC @ 74 <> if
      OUT OUT-U @ type ERR$ type
   then
   RC @ 74 T=
   OUT-U @ 0 T=
   ERR$ want wantu T$= ;

: NO-OUTPUT-CASE ( -- )
   s" the entry refuses a missing output path before the build closure loads" T-LABEL
   ENTRY-ARGS
   DRIVE
   S\" native-build: one explicit output path is required, then an optional `whitebox`, then an optional `--target <target>`\n" REFUSED ;

\ The build class is an argument and not an inherited variable, so the argument
\ has exactly one other spelling and everything else is refused by name.
: BAD-CLASS-CASE ( -- )
   s" and refuses a second argument that is neither `whitebox` nor `--target`" T-LABEL
   ENTRY-ARGS
   s" --" ARG
   s" /dev/null/native-build-entry-test" ARG
   s" wightbox" ARG
   DRIVE
   S\" native-build: after the output path come only `whitebox`, then `--target <target>`\n" REFUSED ;

: TARGET-ARGS ( -- )
   ENTRY-ARGS
   s" --" ARG
   s" /dev/null/native-build-entry-test" ARG
   s" --target" ARG ;

: TARGET-MISSING-CASE ( -- )
   s" and refuses `--target` with no target after it" T-LABEL
   TARGET-ARGS
   DRIVE
   S\" native-build: after the output path come only `whitebox`, then `--target <target>`\n" REFUSED ;

: TARGET-UNKNOWN-CASE ( -- )
   s" and refuses a target name that is not one of the three" T-LABEL
   TARGET-ARGS
   s" linux-x86" ARG
   DRIVE
   S\" native-build: --target is linux-aarch64, macos-aarch64 or linux-x86-64\n" REFUSED ;

\ A product carries its own machine's backend and no other, so the target on the
\ other machine is the one this engine cannot emit for.
: FOREIGN$ ( -- ptr u8 n )
   HB-TARGET-LINUX-X86-64? if s" linux-aarch64" exit then
   s" linux-x86-64" ;

: TARGET-UNLOADED-CASE ( -- )
   s" and refuses a target whose machine has no backend loaded here" T-LABEL
   TARGET-ARGS
   FOREIGN$ ARG
   DRIVE
   S\" native-build: the --target machine has no backend loaded; load its backend module before tools/native-build.f\n" REFUSED ;

: FOREIGN-BACKEND$ ( -- ptr u8 n )
   HB-TARGET-LINUX-X86-64? if s" src/arch/arm64/backend.f" exit then
   s" src/arch/x86-64/backend.f" ;

\ The whole window loads and is captured for the other machine; then the
\ window's own compiler is the one a source-loaded writer would get
\ (tools/native-build-core.f WRITER-MACHINE-CK), so the build stops there.
\ Nothing is written: the refusal names the stop and the output path is under
\ /dev/null, which no write can reach.
: FOREIGN-WINDOW-CASE ( -- )
   s" and stops the other machine's window after its capture, before a writer loads" T-LABEL
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   FOREIGN-BACKEND$ ARG
   s" tools/native-build.f" ARG
   s" --" ARG
   s" /dev/null/native-build-entry-test" ARG
   s" --target" ARG
   FOREIGN$ ARG
   DRIVE
   S\" native-build: no writer is loaded for a --target on another machine; a writer loaded after the capture would compile for the window's machine\n" REFUSED ;

: RUN ( -- )
   T-RESET
   NO-OUTPUT-CASE
   BAD-CLASS-CASE
   TARGET-MISSING-CASE
   TARGET-UNKNOWN-CASE
   TARGET-UNLOADED-CASE
   FOREIGN-WINDOW-CASE
   T-REPORT ;

RUN
;package
