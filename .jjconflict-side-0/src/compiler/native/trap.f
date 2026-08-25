\ trap.f - the one routine a compiled trap branches to, and the table that says
\ what it is trapping on. One concern: turning a name into a number a trap site
\ can carry, and turning that number back into the message the process exits
\ with.
\
\
\ A trap site carries a NUMBER, not the bytes: the type-family string pool grows
\ by doubling and moves, so a pointer into it would dangle (habu2.f:7029).

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/core/engine-error.f

package NTRAP
private

\ ---- the table ---------------------------------------------------------------
\ The table has no reset: an ordinal is already compiled into published routines,
\ so registering is idempotent instead.
64 constant NAME-CAP                 \ the longest family name a row may hold
1024 constant ROWS-MAX               \ distinct families
$8000 constant ARENA-CAP             \ 32 KB of names

create ARENA ARENA-CAP allot
create R-OFF ROWS-MAX cells allot    \ each row's offset into the arena
create R-LEN ROWS-MAX cells allot    \ and its length
create R-KIND ROWS-MAX cells allot   \ and which of the two things it is about

\ Stored codes, not a flag: a row written with anything else decodes as neither.
0 constant KIND-TAG                  \ a scrutinee whose tag matches no arm
1 constant KIND-NORET                \ a call the certificate said never comes back

variable USED
variable ROWS
variable FOUND

: ROW-CK ( n -- n ) {: k:n :}
   k 0 < k ROWS @ >= or if E-NTRAP-ORD throw then
   k ;

: ROW-OFF ( n -- n ) {: k:n :}
   k cells R-OFF + @ ;

: ROW-LEN ( n -- n ) {: k:n :}
   k cells R-LEN + @ ;

: ROW-KIND ( n -- n ) {: k:n :}
   k cells R-KIND + @ ;

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   ARENA k ROW-OFF +  k ROW-LEN ;

\ The kind is half the key: a type family and a word may be spelled the same, and
\ answering one caller with the other's row would report the wrong subject.
: FIND ( ptr u8 n n -- n ) {: a:ptr u:n kind:n :}
   -1 FOUND !
   ROWS @ 0 ?do
      i ROW-KIND kind = if
         i ROW$ a u STR= if i FOUND ! leave then
      then
   loop
   FOUND @ ;

\ Every ceiling is checked before anything moves, so a refusal leaves no trace.
: ADD ( ptr u8 n n -- n ) {: a:ptr u:n kind:n :}
   u 0 <= if E-NTRAP-NAME throw then
   u NAME-CAP > if E-NTRAP-NAME throw then
   ROWS @ ROWS-MAX >= if E-NTRAP-CAP throw then
   USED @ u + ARENA-CAP > if E-NTRAP-CAP throw then
   a  ARENA USED @ +  u BYTE-COPY
   USED @ ROWS @ cells R-OFF + !
   u ROWS @ cells R-LEN + !
   kind ROWS @ cells R-KIND + !
   USED @ u + USED !
   ROWS @ 1+ ROWS !
   ROWS @ 1- ;

: INTERN ( ptr u8 n n -- n ) {: a:ptr u:n kind:n :}
   a u kind FIND {: k:n :}
   k 0 >= if k exit then
   a u kind ADD ;

\ ---- the message -------------------------------------------------------------
\ The tag form writes the exact bytes the engine's own inline trap writes, so a
\ compiled MATCH and an interpreted one end the process saying the same thing.
5 constant SFX-N                     \ " tag\n"
10 constant NSFX-N                   \ " returned\n"
8 NAME-CAP + SFX-N + constant TAG-MSG-CAP        \ "hb: bad " + name + " tag\n"
4 NAME-CAP + NSFX-N + constant NORET-MSG-CAP     \ "hb: " + name + " returned\n"

create MSG TAG-MSG-CAP NORET-MSG-CAP max allot

: PUT$ ( ptr u8 n n -- n ) {: a:ptr u:n at:n :}
   a MSG at + u BYTE-COPY
   at u + ;

: TAG-MSG ( n -- n ) {: k:n :}
   s" hb: bad " 0 PUT$ {: a:n :}
   k ROW$ a PUT$ {: b:n :}
   S\" \x20tag\n" b PUT$ ;

: NORET-MSG ( n -- n ) {: k:n :}
   s" hb: " 0 PUT$ {: a:n :}
   k ROW$ a PUT$ {: b:n :}
   S\" \x20returned\n" b PUT$ ;

public

\ ---- what the chain asks ------------------------------------------------------
KIND-TAG constant TAG
KIND-NORET constant NO-RET

\ Idempotent: two sites over one family carry one number.
: FAMILY ( ptr u8 n -- n )
   KIND-TAG INTERN ;

\ The name is the CALLEE's, and a separate row from a family of the same spelling.
: NO-RETURN ( ptr u8 n -- n )
   KIND-NORET INTERN ;

: NAME$ ( n -- ptr u8 n )
   ROW-CK ROW$ ;

: KIND@ ( n -- n )
   ROW-CK ROW-KIND ;

: COUNT ( -- n )
   ROWS @ ;

\ A spelling and not an address: select.f resolves it through NDICT:CALL-TARGET
\ like any other callee, which is what makes the target one routine tree-wide.
: ROUTINE$ ( -- ptr u8 n )
   s" NTRAP:TRAP" ;

\ ---- the routine every trap site branches to ----------------------------------
\ Entered with the ordinal and does not come back. A tag matching no arm exits
\ BAD-TAG; a callee that returned exits CODE-CERT - its certificate was false.
: TRAP ( n -- )
   ROW-CK {: k:n :}
   k ROW-KIND {: kind:n :}
   kind KIND-TAG = if
      MSG  k TAG-MSG  ENGINE-ERROR:BAD-TAG die
   then
   kind KIND-NORET = if
      MSG  k NORET-MSG  ENGINE-ERROR:CODE-CERT die
   then
   s" hb: trap row of no kind" ENGINE-ERROR:CODE-CERT die ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
