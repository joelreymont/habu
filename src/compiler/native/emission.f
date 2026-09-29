\ emission.f - one sealed emission as publication reads it: bytes, function
\ starts, call and address sites and the trailing return, in no machine's terms.
\
\ WHAT IT IS FOR. src/compiler/native/publish.f copies a routine into the code
\ region and records what a restore has to relocate. It needs the same facts of
\ every backend, and none of them is an instruction: where each function starts,
\ which sites call out or leave through a branch and where to, which sites carry
\ an absolute address, and how long the trailing return is. A backend decodes its
\ own instruction forms once, as its emission row seals, and states the answers
\ here; publication reads these rows and decodes nothing.
\
\ EVERY OFFSET IS IN BYTES from the first byte of the emission and lies inside
\ it; every target is an absolute address, measured from the placement.
\
\ LIFETIME. A backend OPENs the rows, adds them and SEALs them. They answer from
\ SEAL until CLEAR, which the backend's RETIRE row runs on the accepting and the
\ refusing path alike. A refusal between OPEN and SEAL leaves rows that no reader
\ can read, and OPEN over rows CLEAR has not retired is refused, so no row can
\ outlive the definition it describes.

require lib/prelude.f
require lib/errors.f

package NEMIT
private

0 constant ST-EMPTY
1 constant ST-OPEN
2 constant ST-SEALED
variable ST
ST-EMPTY ST !

PTR-VARIABLE CODE-AT                 \ the emission's first byte
variable CODE-N                      \ and how many bytes it is
variable RET-N                       \ how many of them are the trailing return
variable HAS-PLACE                   \ whether the emission was measured from a placement
variable PLACE-N                     \ and which

variable N-FUNS
DYNAMIC-BUFFER FUN-OFF-BUF n         \ where each function starts
variable N-CALLS
DYNAMIC-BUFFER CALL-OFF-BUF n        \ where a call or leaving branch sits
DYNAMIC-BUFFER CALL-KIND-BUF n       \ CALL or TAIL
DYNAMIC-BUFFER CALL-TGT-BUF n        \ the absolute address it goes to
variable N-ADDRS
DYNAMIC-BUFFER ADDR-OFF-BUF n        \ where an address chain starts
DYNAMIC-BUFFER ADDR-KIND-BUF n       \ which kind of address it carries

: SEAL-CK ( -- )
   ST @ ST-SEALED <> if E-NEMIT-STATE throw then ;

: OPEN-CK ( -- )
   ST @ ST-OPEN <> if E-NEMIT-STATE throw then ;

: ROW-CK ( n n -- n ) {: i:n count:n :}
   i 0 <  i count >= or if E-NEMIT-ROW throw then
   i ;

: OFFSET-CK ( n -- n )
   CODE-N @ ROW-CK ;

public

\ The kinds of a call site: a call that comes back, and a branch that leaves the
\ emission for good.
0 constant CALL
1 constant TAIL

: SIZE ( -- n )
   SEAL-CK CODE-N @ ;

: BYTES ( -- ptr u8 )
   SEAL-CK CODE-AT @ ;

\ Zero when the emission ends in no return, so its whole span is its record.
: RET-BYTES ( -- n )
   SEAL-CK RET-N @ ;

: FUNCTION-OFFSET@ ( n -- n )
   SEAL-CK N-FUNS @ ROW-CK FUN-OFF-BUF @ ;

: PLACED? ( -- bool )
   SEAL-CK HAS-PLACE @ 0<> ;

: PLACEMENT ( -- n )
   SEAL-CK
   HAS-PLACE @ 0= if E-NEMIT-STATE throw then
   PLACE-N @ ;

: CALL-SITES ( -- n )
   SEAL-CK N-CALLS @ ;

: CALL-SITE@ ( n -- n )
   SEAL-CK N-CALLS @ ROW-CK CALL-OFF-BUF @ ;

: CALL-KIND@ ( n -- n )
   SEAL-CK N-CALLS @ ROW-CK CALL-KIND-BUF @ ;

: CALL-TARGET@ ( n -- n )
   SEAL-CK N-CALLS @ ROW-CK CALL-TGT-BUF @ ;

: ADDR-SITES ( -- n )
   SEAL-CK N-ADDRS @ ;

: ADDR-SITE@ ( n -- n )
   SEAL-CK N-ADDRS @ ROW-CK ADDR-OFF-BUF @ ;

: ADDR-SITE-KIND@ ( n -- n )
   SEAL-CK N-ADDRS @ ROW-CK ADDR-KIND-BUF @ ;

\ ---- filling the rows, which only a backend's emission row does --------------
\ Nonthrowing, because a RETIRE row runs it on the refusing path.
: CLEAR ( -- )
   ST-EMPTY ST !
   NULL-PTR CODE-AT !
   0 CODE-N !  0 RET-N !
   0 HAS-PLACE !  0 PLACE-N !
   0 N-FUNS !  0 N-CALLS !  0 N-ADDRS ! ;

\ The bytes stay the backend's: they are read in place until CLEAR.
: OPEN ( ptr u8 n n -- ) {: p:ptr size:n ret:n :}
   ST @ ST-EMPTY <> if E-NEMIT-STATE throw then
   size 1 < if E-NEMIT-ROW throw then
   ret 0 <  ret size > or if E-NEMIT-ROW throw then
   p CODE-AT !  size CODE-N !  ret RET-N !
   ST-OPEN ST ! ;

: PLACE ( n -- ) {: at:n :}
   OPEN-CK
   at PLACE-N !  1 HAS-PLACE ! ;

: FUNCTION+ ( n -- ) {: off:n :}
   OPEN-CK off OFFSET-CK drop
   N-FUNS @ {: k:n :}
   k 1+ FUN-OFF-BUF-RESERVE
   off k FUN-OFF-BUF !
   k 1+ N-FUNS ! ;

: CALL-SITE+ ( n n n -- ) {: off:n kind:n target:n :}
   OPEN-CK off OFFSET-CK drop
   kind CALL <>  kind TAIL <> and if E-NEMIT-ROW throw then
   N-CALLS @ {: k:n :}
   k 1+ CALL-OFF-BUF-RESERVE
   k 1+ CALL-KIND-BUF-RESERVE
   k 1+ CALL-TGT-BUF-RESERVE
   off k CALL-OFF-BUF !
   kind k CALL-KIND-BUF !
   target k CALL-TGT-BUF !
   k 1+ N-CALLS ! ;

: ADDR-SITE+ ( n n -- ) {: off:n kind:n :}
   OPEN-CK off OFFSET-CK drop
   N-ADDRS @ {: k:n :}
   k 1+ ADDR-OFF-BUF-RESERVE
   k 1+ ADDR-KIND-BUF-RESERVE
   off k ADDR-OFF-BUF !
   kind k ADDR-KIND-BUF !
   k 1+ N-ADDRS ! ;

: SEAL ( -- )
   OPEN-CK ST-SEALED ST ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
