\ build-fixpoint-certify.f - the fixpoint's certify child: one generated engine
\ source, verified against the core prefix alone.
\
\ Run: <engine> --load tools/build-fixpoint-certify.f -- LABEL PATH [PHASE TARGET]
\
\ tools/build-fixpoint.f BF-CERTIFY-GENERATED-CORE spawns this on the engine
\ running the build for each generated source that redeclares the stdlib. The
\ top-level text at the end of this file rewinds this process to the end of its
\ core prefix (src/habu/prefix-rewind.f, top-level text because `seed-ndict!`
\ is a top-level boundary primitive no checked body names) and then runs
\ FINISH, ticked before the rewind took its name: the rewind removes records
\ and keeps code and data, so the xt and every word it calls outlive their
\ names. FINISH scans PATH's bytes under LABEL with VERIFY:SOURCE-BUF and, given
\ a nonempty PHASE, prints the census line for TARGET. It exits 0 for a certified
\ source, and 70 after printing the rejection and the diagnostic the scan
\ rendered.

require lib/source.f                     \ SOURCE:READ-WHOLE-FILE
require src/habu/verify-source.f         \ VERIFY:SOURCE-BUF
require tools/build-certify.f            \ the report and census lines

package BUILD-FIXPOINT-CERTIFY
private

$10000 constant DIAG-CAP
create DIAG DIAG-CAP allot
variable DIAG-U
DYNAMIC-BUFFER SRC u8                    \ PATH's bytes, read whole

: ROOM ( n -- ptr u8 )
   SRC-RESERVE 0 SRC ;

: USAGE ( -- )
   s" usage: build-fixpoint-certify.f -- LABEL PATH [PHASE TARGET]" 64 die ;

: ACT ( -- )
   SCRIPT-ARGC 2 <> SCRIPT-ARGC 4 <> and if USAGE then
   0 SCRIPT-ARGV$ DIAG-FILE!
   false DIAG-JSON!
   DIAG DIAG-CAP DIAG-BUFFER!
   1 SCRIPT-ARGV$ [: ROOM ;] SOURCE:READ-WHOLE-FILE {: u:n :}
   0 SRC u VERIFY:SOURCE-BUF
   SCRIPT-ARGC 4 = if
      2 SCRIPT-ARGV$ nip 0 > if
         2 SCRIPT-ARGV$ 3 SCRIPT-ARGV$ BUILD-CERTIFY:CENSUS
      then
   then ;

public

: FINISH ( -- )
   [: ACT ;] catch {: rc:n :}
   DIAG-BUFFER$ nip DIAG-U !
   DIAG-BUFFER-OFF
   rc 0<> if
      0 SCRIPT-ARGV$ rc DIAG DIAG-U @ BUILD-CERTIFY:REPORT
      s" " 70 die
   then
   s" " 0 die ;

;package

' BUILD-FIXPOINT-CERTIFY:FINISH
0 set-check
include src/habu/prefix-rewind.f
CHECKER-END-PACKAGE
execute
