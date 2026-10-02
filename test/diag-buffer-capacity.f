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
\ The renderer holds each record in its own buffer (RSBUF, 16 KiB) before it
\ reaches the caller's, and so does the recorder of each certified word's
\ effect. A diagnostic or an effect past that is refused by the same code with
\ the renderer left as a delivered record leaves it. tools/check-test-lib.f
\ TEST-RENDER-FULL and TEST-RENDER-FULL-EFFECT meet both through tools/check.f.
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

private

\ A name of WIDE-N backslashes is within the 7999 bytes `:` defines, and each
\ byte is two inside a JSON string. The refusal's record names its word and
\ echoes its source, so it passes the renderer's 16 KiB before any caller's
\ buffer sees it.
7900 constant WIDE-N

\ Six values of a type whose name is EFFECT-N bytes pass the same 16 KiB.
3000 constant EFFECT-N

\ Either source above is built here; TEXT-CAP holds the longer.
WIDE-N 32 + constant TEXT-CAP
create TEXT TEXT-CAP allot
variable TEXT-U

: TEXT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a TEXT TEXT-U @ + u BYTE-COPY
   TEXT-U @ u + TEXT-U ! ;

\ Append the given count of the given byte.
: TEXT-RUN ( n n -- )
   {: k:n c:n :}
   k 0 ?do
      c TEXT TEXT-U @ + c!
      TEXT-U @ 1 + TEXT-U !
   loop ;

public

\ One refused definition whose word is WIDE-N backslashes.
: WIDE$ ( -- ptr u8 n )
   0 TEXT-U !
   s" : " TEXT+
   WIDE-N 92 TEXT-RUN
   s\"  ( n -- n ) dup ;\n" TEXT+
   TEXT TEXT-U @ ;

\ USE declares no effect, so the checker records the one it infers through the
\ renderer: six values of a type whose name is EFFECT-N bytes.
: EFFECT$ ( -- ptr u8 n )
   0 TEXT-U !
   s" NEWTYPE " TEXT+
   EFFECT-N 97 TEXT-RUN
   s\"  0\nTRUSTED: MK ( -- " TEXT+
   EFFECT-N 97 TEXT-RUN
   s\"  ) 0 ;\n: USE MK MK MK MK MK MK ;\n" TEXT+
   TEXT TEXT-U @ ;

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

s" a record past the render buffer is refused and the next renders whole" T-LABEL
true DIAG-JSON!
DBC:WIDE$ DBC:VERIFY E-DIAG-CAPACITY T=
false DIAG-JSON!
DBC:DIAG$ nip 0 T=
DBC-ONE$ DBC:VERIFY 0 T=
DBC:DIAG$ nip DBC-RECORD T=
DBC:DIAG$ DBC-LAST 10 T=

s" an effect past the render buffer is refused and the next record renders whole" T-LABEL
DBC:EFFECT$ DBC:VERIFY E-DIAG-CAPACITY T=
DBC:DIAG$ nip 0 T=
DBC-ONE$ DBC:VERIFY 0 T=
DBC:DIAG$ nip DBC-RECORD T=
DBC:DIAG$ DBC-LAST 10 T=

T-REPORT
s" diag-buffer-capacity: ok" type cr
