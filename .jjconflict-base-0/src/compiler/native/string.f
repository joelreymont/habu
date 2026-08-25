\ string.f - where a string literal's bytes live once the chain has compiled one.
\ One concern: turning a literal's body into a permanent address, and turning
\ equal bodies into the SAME permanent address.
\
\ The bytes live in DATA because a DATA address means the same thing after a
\ snapshot restore and an mmap one does not. The store has no reset: every
\ address it has handed out is already compiled into a published routine.

require lib/prelude.f
require lib/errors.f
require lib/string.f

package NSTR
private

\ ---- the store ---------------------------------------------------------------
$80000 constant ARENA-CAP            \ 512 KB of bodies
8192 constant ROWS-MAX               \ distinct bodies
16384 constant SLOTS                 \ a power of two, twice ROWS-MAX

create ARENA ARENA-CAP allot
create R-OFF ROWS-MAX cells allot    \ each row's offset into the arena
create R-LEN ROWS-MAX cells allot    \ and its length
create SLOT SLOTS cells allot        \ hash slot: row index plus one, zero is empty

variable USED
variable ROWS
variable PROBE
variable FOUND
variable HV

\ The checker has no term for "the integer this pointer is", and the elaborator
\ stages the arena address as an ordinary integer literal. Retires with
\ habu-guard-an-executed-8a0f2f77.
TRUSTED: PTR>N ( ptr a -- n ) ;

: BASE ( -- n )
   ARENA PTR>N ;

: ROW-OFF ( n -- n ) {: k:n :}
   k cells R-OFF + @ ;

: ROW-LEN ( n -- n ) {: k:n :}
   k cells R-LEN + @ ;

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   ARENA k ROW-OFF +  k ROW-LEN ;

\ ---- finding a body that is already here --------------------------------------
\ FNV-1a, masked by an `and` against a positive constant so a hash whose top bit
\ ran into the sign still lands inside the table.
: HASH ( ptr u8 n -- n ) {: a:ptr u:n :}
   $811C9DC5 HV !
   u 0 ?do
      a i + c@ HV @ xor  $1000193 *  HV !
   loop
   HV @ SLOTS 1- and ;

: PROBE-NEXT ( -- )
   PROBE @ 1+ SLOTS 1- and PROBE ! ;

: SLOT@ ( -- n )
   PROBE @ cells SLOT + @ ;

\ Bounded by the table rather than by an empty slot, so a full table cannot spin.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u HASH PROBE !
   -1 FOUND !
   SLOTS 0 ?do
      SLOT@ 0= if leave then
      SLOT@ 1- ROW$ a u STR= if SLOT@ 1- FOUND ! leave then
      PROBE-NEXT
   loop
   FOUND @ ;

: FREE-SLOT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u HASH PROBE !
   -1 FOUND !
   SLOTS 0 ?do
      SLOT@ 0= if PROBE @ FOUND ! leave then
      PROBE-NEXT
   loop
   FOUND @ ;

\ ---- putting one in ------------------------------------------------------------
\ Every ceiling is checked before anything moves, so a refusal leaves no trace.
: ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   ROWS @ ROWS-MAX >= if E-NSTR-CAP throw then
   USED @ u + ARENA-CAP > if E-NSTR-CAP throw then
   a u FREE-SLOT {: s:n :}
   s 0 < if E-NSTR-CAP throw then
   a  ARENA USED @ +  u BYTE-COPY
   USED @ ROWS @ cells R-OFF + !
   u ROWS @ cells R-LEN + !
   ROWS @ 1+ s cells SLOT + !
   USED @ u + USED !
   ROWS @ 1+ ROWS !
   ROWS @ 1- ;

public

\ `s" "` is a body: it gets a row and an address like any other.
: INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FIND {: k:n :}
   k 0 >= if BASE k ROW-OFF + exit then
   BASE  a u ADD ROW-OFF  + ;

: COUNT ( -- n )
   ROWS @ ;

: BYTES ( -- n )
   USED @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
