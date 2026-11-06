\ The production entry refuses a bad command line before it loads the build closure
\ (tools/native-build-args.f): no output, a bad class or --target profile, or a profile with no CORE ABI or backend here.
require src/lib/test.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package NATIVE-BUILD-ENTRY-TEST

$4000 constant CAP
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
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
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
\ has exactly one other spelling and everything else is refused by name. It is
\ settled before the target load, so a typo cannot quietly produce a product.
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

: TARGET-UNKNOWN-CASE ( -- )
   s" and refuses an unknown target profile" T-LABEL
   TARGET-ARGS
   s" linux-x86" ARG
   DRIVE
   S\" native-build: unknown --target profile\n" REFUSED ;

\ A product carries its own machine's backend and no other, so the target on the
\ other machine is the one this engine cannot emit for.
: FOREIGN$ ( -- ptr u8 n )
   HB-TARGET-LINUX-X86-64? if s" linux-aarch64" exit then
   s" linux-x86-64" ;

\ The profile resolves and has an architecture, but its native CORE ABI has
\ no implementation. This refusal precedes backend and target-window work.
: WINDOWS-CORE-CASE ( -- )
   s" and refuses the declared Windows profile before native compilation" T-LABEL
   TARGET-ARGS
   s" aarch64-pc-windows-msvc" ARG
   DRIVE
   S\" native-build: the --target profile has no native CORE ABI\n" REFUSED ;

: TARGET-UNLOADED-CASE ( -- )
   s" and refuses a target whose machine has no backend loaded here" T-LABEL
   TARGET-ARGS
   FOREIGN$ ARG
   DRIVE
   S\" native-build: the --target machine has no backend loaded; load its backend module before tools/native-build.f\n" REFUSED ;

: RUN ( -- )
   T-RESET
   NO-OUTPUT-CASE
   BAD-CLASS-CASE
   TARGET-UNKNOWN-CASE
   WINDOWS-CORE-CASE
   TARGET-UNLOADED-CASE
   T-REPORT ;

RUN
;package
