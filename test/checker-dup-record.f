\ checker-dup-record.f - a duplicate the checker rejects in the caller's scope
\ (VERIFY:SOURCE-BUF-IN-SCOPE) leaves the record it collided with intact. CHECK
\ rejects before its record step's first write: a later-wins entry without the
\ first word's source authority fails its later callers E-CAP-TRUSTED (checker.f
\ CHECK-REC-ADMIT). A scan under no checker scope takes back its rows when it
\ ends.

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/habu/verify-source.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package CDR
private

TYPED-VARIABLE SRC-A ^u8
variable SRC-U

\ x86-64 rejects a quotation that reads a local, so the source span travels
\ through these two cells to the caught body.
: ACT ( -- )
   SRC-A @ SRC-U @ >str VERIFY:SOURCE-BUF-IN-SCOPE ;

create OUT $1000 allot
create ERR $1000 allot

\ A scan at top level, under no checker scope, then the scanned name defined:
\ the scan's rows must not outlive it. A child, since a leaked row ends the
\ load that holds it.
: TOP-SCAN$ ( -- str )
   SB-RESET
   S\" : CDR-NOP ( -- ) ;\n" SB-APPEND
   S\" s\\\" : CDR-Q ( -- ) parse-name drop ; immediate parses: CDR-Q 1\" ' CDR-NOP VERIFY:SOURCE-BUF-THEN-IN-SCOPE\n" SB-APPEND
   S\" : CDR-Q ( -- ) ; immediate\n" SB-APPEND
   SB$ ;

public

\ Verify one span in the caller's scope with the checker's diagnostics
\ collected; 0 when it certified, else the throw code.
: VERIFY-QUIET ( str -- n )
   {: s:str :}
   s STR:DATA SRC-A !
   s STR:LEN SRC-U !
   REJECTION:CAPTURE
   [: ACT ;] catch {: rc:n :}
   REJECTION:BUFFER-OFF
   rc ;

\ The exit code of a child engine that reads TOP-SCAN$ as its program.
: TOP-SCAN-RC ( -- n )
   TOP-SCAN$ {: src:str :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ src OUT $1000 >LEN ERR $1000 >LEN
   PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE
   PCAP-RESULT:>N {: outu:n erru:n rc:n :}
   ERR erru >str type
   rc ;

\ The same span in multi-error mode, where a rejection's record is kept instead
\ of thrown: the throw code, then the rejections counted.
: MULTI-QUIET ( str -- n n )
   {: s:str :}
   CHECK:MULTI-ERR-BEGIN
   s VERIFY-QUIET
   CHECK:MULTI-ERR-END ;

: DIAG$ ( -- str )
   REJECTION:BUFFER$ ;

;package

T-RESET

: CDR-ONE ( -- n ) 1 ;

s" a certified word redefined with a signature is rejected" T-LABEL
s" : CDR-ONE ( -- n ) 2 ;" CDR:VERIFY-QUIET 70 T=
s" a caller verified after it certifies" T-LABEL
s" : CDR-ONE-USE ( -- n ) CDR-ONE ;" CDR:VERIFY-QUIET 0 T=
CDR:DIAG$ STR:LEN 0 T=

: CDR-TWO ( -- n ) 2 ;

s" a certified word redefined without a signature is rejected" T-LABEL
s" : CDR-TWO 3 ;" CDR:VERIFY-QUIET 70 T=
s" a caller verified after it certifies" T-LABEL
s" : CDR-TWO-USE ( -- n ) CDR-TWO ;" CDR:VERIFY-QUIET 0 T=
CDR:DIAG$ STR:LEN 0 T=

\ REJECT-DUPLICATE (verify-source.f) leaves the body unchecked.
: CDR-THREE ( -- n ) 3 ;

s" a rejected body under a certified name is rejected as a duplicate" T-LABEL
s" : CDR-THREE ( -- n ) 1 2 ;" CDR:MULTI-QUIET 1 T= 0 T=
s" the duplicate is rejected at its name, its body unchecked" T-LABEL
CDR:DIAG$ s" error[E-DUPLICATE-DEFINITION]: `CDR-THREE`" STR:CONTAINS? TTRUE
CDR:DIAG$ s" E-MISMATCH" STR:CONTAINS? 0= TTRUE
s" a caller verified after it certifies" T-LABEL
s" : CDR-THREE-USE ( -- n ) CDR-THREE ;" CDR:VERIFY-QUIET 0 T=
CDR:DIAG$ STR:LEN 0 T=

\ Compiled by this load: each definition passes the checker hook or the load
\ dies with the hook's diagnostic.
: CDR-CALLS ( -- n n n )
   CDR-ONE CDR-TWO CDR-THREE ;

s" a caller compiled after the rejections runs the first definitions" T-LABEL
CDR-CALLS
3 T=
2 T=
1 T=

s" a top-level scan takes back its rows: its name defines after it" T-LABEL
CDR:TOP-SCAN-RC 0 T=

T-REPORT
s" checker-dup-record: ok" type cr
