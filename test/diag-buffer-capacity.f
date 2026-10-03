\ diag-buffer-capacity.f - a diagnostic buffer with no room for the next record
\ refuses it by E-DIAG-CAPACITY and keeps every whole record before it.
\
\ The checker renders each refusal into the buffer its caller armed with
\ DIAG-BUFFER! (src/core/render.f). A record that does not fit throws out of
\ the check to the caller's catch, which reads the records that fit and goes on
\ checking. tools/check.f --all-errors meets the same refusal end to end when
\ its scratch fills (tools/check-test-lib.f TEST-SCRATCH-FULL), as a throw
\ record after the records that fit; this file sees which records the buffer
\ keeps and the checker go on in the same process. The sources are verified in
\ multi-error mode, where every refusal renders a record and the check goes on,
\ into a buffer sized from one measured record.
\
\ Run: bin/hb --load test/diag-buffer-capacity.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/verify-source.f

package DBC
private

$1000 constant CAP
create BUF CAP allot
variable DIAG-U
TYPED-VARIABLE SRC-A ptr u8
variable SRC-U

\ A quotation cannot read the enclosing word's locals, so the source span
\ travels through these two cells to the caught body.
: ACT ( -- )
   SRC-A @ SRC-U @ VERIFY:SOURCE-BUF ;

public

\ Verify one span in multi-error mode with its diagnostics collected in the
\ first given bytes of the buffer; 0 when it checked to the end, else the
\ throw code.
: VERIFY-INTO ( ptr u8 n n -- n )
   {: a:ptr u:n room:n :}
   a SRC-A !
   u SRC-U !
   MULTI-ERR-BEGIN
   BUF room DIAG-BUFFER!
   [: ACT ;] catch {: rc:n :}
   DIAG-BUFFER$ nip DIAG-U !
   DIAG-BUFFER-OFF
   MULTI-ERR-END drop
   rc ;

: VERIFY ( ptr u8 n -- n )
   CAP VERIFY-INTO ;

\ The text the last verification collected.
: DIAG$ ( -- ptr u8 n )
   BUF DIAG-U @ ;

;package

: DBC-ONE$ ( -- ptr u8 n )
   S\" : DBC-A ( -- n ) ;\n" ;

: DBC-TWO$ ( -- ptr u8 n )
   S\" : DBC-A ( -- n ) ;\n: DBC-B ( -- n ) ;\n" ;

: DBC-LAST ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u + 1 - c@ ;

T-RESET

s" one refusal renders one record" T-LABEL
DBC-ONE$ DBC:VERIFY 0 T=
DBC:DIAG$ s" dbc-a" CONTAINS? TTRUE
DBC:DIAG$ DBC-LAST 10 T=
DBC:DIAG$ nip constant DBC-RECORD

s" a record past the buffer is refused and the one before it kept whole" T-LABEL
DBC-TWO$ DBC-RECORD DBC-RECORD 2 / + DBC:VERIFY-INTO E-DIAG-CAPACITY T=
DBC:DIAG$ nip DBC-RECORD T=
DBC:DIAG$ s" dbc-a" CONTAINS? TTRUE
DBC:DIAG$ s" dbc-b" CONTAINS? TFALSE

s" a buffer with no room for the first record keeps nothing" T-LABEL
DBC-ONE$ DBC-RECORD 1 - DBC:VERIFY-INTO E-DIAG-CAPACITY T=
DBC:DIAG$ nip 0 T=

s" the checker goes on after the refusal" T-LABEL
s\" : DBC-C ( -- n ) 1 ;\n" DBC:VERIFY 0 T=
DBC:DIAG$ nip 0 T=
DBC-TWO$ DBC:VERIFY 0 T=
DBC:DIAG$ s" dbc-b" CONTAINS? TTRUE

T-REPORT
s" diag-buffer-capacity: ok" type cr
