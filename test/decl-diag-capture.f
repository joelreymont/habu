\ decl-diag-capture.f — shared declaration-diagnostic capture for the declaration
\ suites (package DECL-DIAG).
\
\ It captures through the SAME words tools/check-core.f uses: CHK-DECL-CAPTURE is
\ `CHK-JSON @ DIAG-JSON!` plus `CHK-ERR-BUF CHK-ERR-CAP DIAG-BUFFER!`, and
\ CHK-DECL-FLUSH is `DIAG-BUFFER$` plus `DIAG-BUFFER-OFF`. A suite that asserts
\ through this module is therefore reading the declaration packet out of the
\ production capture channel, in whichever of the two legs — prose or JSON — the
\ check tool would have selected, rather than out of a test-local copy of it.
\
\ That establishes the CHANNEL, not an end-to-end run of the check tool over a
\ unified declaration: check-core drives the legacy definers today and does not
\ scan STRUCTURE at all, so the end-to-end leg waits on the buffer-driven
\ registration entry.
\
\ Installing a capture buffer also silences the expected diagnostics a reject
\ suite provokes, exactly as test/type-decl-suite.f's TDIAG-BUF does.

package DECL-DIAG

8192 constant CAP
create BUF CAP allot

\ CAPTURE-ON routes declaration diagnostics into BUF, as JSON when its flag is
\ true; CAPTURE-OFF restores prose mode; CAPTURED$ returns the production buffer.
: CAPTURE-ON ( bool -- )
   DIAG-JSON!
   BUF CAP DIAG-BUFFER! ;
: CAPTURE-OFF ( -- )
   DIAG-BUFFER-OFF
   false DIAG-JSON! ;
: CAPTURED$ ( -- ptr u8 n ) DIAG-BUFFER$ ;

variable SI

: AT? ( ptr u8 n ptr u8 n -- bool )     \ haystack tail, needle: needle starts here?
   {: ha:ptr hu:n na:ptr nu:n :}
   nu hu > IF 0 0= 0= EXIT THEN
   0 BEGIN dup nu < WHILE
      dup na + c@  over ha + c@ <> IF drop 0 0= 0= EXIT THEN
      1 +
   REPEAT drop 0 0= ;

public

\ Start (or restart) a capture. Each call empties the buffer, so a suite asserts
\ about exactly one declaration at a time.
: PROSE ( -- ) false CAPTURE-ON ;
: JSON ( -- ) true CAPTURE-ON ;
: OFF ( -- ) CAPTURE-OFF ;

\ What the capture holds now.
: TEXT$ ( -- ptr u8 n ) CAPTURED$ ;
: LEN ( -- n ) CAPTURED$ nip ;
: SILENT? ( -- bool ) LEN 0= ;

\ Byte equality of two spans, for asserting a packet field against a literal
\ when the field is read back through DECL-REJECT's reflection rather than out
\ of a rendered line.
: SAME? ( ptr u8 n ptr u8 n -- bool ) {: aa:ptr au:n ba:ptr bu:n :}
   au bu <> IF 0 0= 0= EXIT THEN
   aa au ba bu AT? ;

\ Does the captured diagnostic contain this exact byte sequence?
: HAS? ( ptr u8 n -- bool ) {: na:ptr nu:n :}
   CAPTURED$ {: ha:ptr hu:n :}
   0 SI !
   BEGIN SI @ nu + hu <= WHILE
      ha SI @ +  hu SI @ -  na nu AT? IF 0 0= EXIT THEN
      SI @ 1 + SI !
   REPEAT 0 0= 0= ;

;package
