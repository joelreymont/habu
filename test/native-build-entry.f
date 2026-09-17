\ Load the production entry and its driver through the native build guard.
\ No output argument reaches the driver's own refusal after the callback and
\ dynamic source-load boundary have compiled; it does not start a full build.
\ Neither does a bad second argument: the class the build is being asked for is
\ settled before the target load, so a typo cannot quietly produce a product.
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
   s" the native driver reaches its missing-output refusal" T-LABEL
   ENTRY-ARGS
   DRIVE
   S\" native-build: one explicit output path is required, then an optional `whitebox`\n" REFUSED ;

\ The build class is an argument and not an inherited variable, so the argument
\ has exactly one other spelling and everything else is refused by name.
: BAD-CLASS-CASE ( -- )
   s" and refuses a second argument that is not `whitebox`" T-LABEL
   ENTRY-ARGS
   s" --" ARG
   s" /dev/null/native-build-entry-test" ARG
   s" wightbox" ARG
   DRIVE
   S\" native-build: the only second argument is `whitebox`\n" REFUSED ;

: RUN ( -- )
   T-RESET
   NO-OUTPUT-CASE
   BAD-CLASS-CASE
   T-REPORT ;

RUN
;package
