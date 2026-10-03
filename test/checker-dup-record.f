\ checker-dup-record.f - a duplicate definition the checker refuses inside the
\ caller's own scope leaves the record it collided with as it was.
\
\ The engine's own walls refuse a duplicate before the checker sees it (the
\ REPL, `included` and `evaluate` all stop at habu2.f C-REJECT-DUP-DEF), so the
\ checker's guard is reached by the source verifier run in the caller's scope,
\ VERIFY:SOURCE-BUF-IN-SCOPE, and a caller that catches its throw goes on in
\ that scope. CHECK asks the guard before its record step's first write: the
\ control entry it appends is later-wins, and one appended for the refused
\ duplicate lacks the first word's source authority, so every later caller of
\ the first word would fail E-CAP-TRUSTED (src/core/checker.f CHECK-REC-ADMIT).
\ Each case redefines a certified word in scope - with a signature, without
\ one, and as a refused body in multi-error mode - and then proves the first
\ word kept its authority: a caller verified in scope certifies with nothing to
\ say, and a caller compiled by this load certifies and runs.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f
\   src/habu/verify-source.f test/checker-dup-record.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/verify-source.f

package CDR
private

TYPED-VARIABLE SRC-A ptr u8
variable SRC-U

\ A quotation cannot read the enclosing word's locals, so the source span
\ travels through these two cells to the caught body.
: ACT ( -- )
   SRC-A @ SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

create DIAG-BUF $1000 allot
variable DIAG-U

public

\ The duplicate's own code: habu2.f C-DUP-DEF-FAIL and checker.f
\ CHECKER-DUP-DEFINITION both throw it.
$4E constant DUP-RC

\ Verify one span in the caller's scope with the checker's diagnostics
\ collected; 0 when it certified, else the throw code.
: VERIFY-QUIET ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a SRC-A !
   u SRC-U !
   DIAG-BUF $1000 DIAG-BUFFER!
   [: ACT ;] catch {: rc:n :}
   DIAG-BUFFER$ nip DIAG-U !
   DIAG-BUFFER-OFF
   rc ;

\ The same span in multi-error mode, where a refused body is recorded with
\ source authority instead of thrown (src/core/checker.f CHECK).
: MULTI-QUIET ( ptr u8 n -- n )
   {: a:ptr u:n :}
   MULTI-ERR-BEGIN
   a u VERIFY-QUIET
   MULTI-ERR-END drop ;

\ The text the last verification collected.
: DIAG$ ( -- ptr u8 n )
   DIAG-BUF DIAG-U @ ;

;package

T-RESET

: CDR-ONE ( -- n ) 1 ;

s" a certified word redefined with a signature is refused" T-LABEL
s" : CDR-ONE ( -- n ) 2 ;" CDR:VERIFY-QUIET CDR:DUP-RC T=
s" a caller verified after it certifies" T-LABEL
s" : CDR-ONE-USE ( -- n ) CDR-ONE ;" CDR:VERIFY-QUIET 0 T=
CDR:DIAG$ nip 0 T=

: CDR-TWO ( -- n ) 2 ;

s" a certified word redefined without a signature is refused" T-LABEL
s" : CDR-TWO 3 ;" CDR:VERIFY-QUIET CDR:DUP-RC T=
s" a caller verified after it certifies" T-LABEL
s" : CDR-TWO-USE ( -- n ) CDR-TWO ;" CDR:VERIFY-QUIET 0 T=
CDR:DIAG$ nip 0 T=

: CDR-THREE ( -- n ) 3 ;

s" a refused body under a certified name is refused as a duplicate" T-LABEL
s" : CDR-THREE ( -- n ) 1 2 ;" CDR:MULTI-QUIET CDR:DUP-RC T=
CDR:DIAG$ s" cdr-three" CONTAINS? TTRUE
s" a caller verified after it certifies" T-LABEL
s" : CDR-THREE-USE ( -- n ) CDR-THREE ;" CDR:VERIFY-QUIET 0 T=
CDR:DIAG$ nip 0 T=

\ Compiled by this load: each definition passes the checker hook or the load
\ dies with the hook's diagnostic.
: CDR-CALLS ( -- n n n )
   CDR-ONE CDR-TWO CDR-THREE ;

s" a caller compiled after the refusals runs the first definitions" T-LABEL
CDR-CALLS
3 T=
2 T=
1 T=

T-REPORT
s" checker-dup-record: ok" type cr
