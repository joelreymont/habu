\ trap.f - the one routine a compiled trap branches to, and the table that says
\ which family it is trapping on. One concern: turning a family's name into a
\ number a trap site can carry, and turning that number back into the message the
\ process exits with.
\
\ WHY THE SITE CARRIES A NUMBER AND NOT THE BYTES. The engine's own MATCH puts
\ the whole diagnostic inside every site: src/habu/habu2.f C-DIE-BAD-TAG copies
\ `"hb: bad "`, the family name and `" tag\n"` into the compiled word and then
\ emits the write and the exit after them, which is nine instructions plus the
\ message - 52 bytes for a three-character family, pinned byte for byte in
\ test/match-factor-pin.f. It does that for a reason: the name lives in the
\ type-family string pool, which is grown by doubling and therefore moves, so a
\ pointer into it would dangle (habu2.f:7029). A number does not move. So the
\ site carries the number, this file owns the copy of the bytes that the number
\ stands for, and the message is built once, here, at the moment the process is
\ about to end.
\
\ AND WHY THE COPY LIVES IN DATA. The same reason src/compiler/native/string.f
\ gives for a string literal: DATA is at a fixed virtual address that means the
\ same thing after a snapshot restore, while an mmap span is process-local and an
\ address into it is wrong in the next image without saying so.
\
\ THE TABLE HAS NO RESET. An ordinal is compiled into published routines, so
\ handing the same number out for two different families is the one thing that
\ must never happen - which a word that emptied the table would do. Registering
\ is idempotent instead: a family already here answers the ordinal it already
\ has, so re-elaborating a definition that ran out of registers costs nothing and
\ a refused definition leaves no row nothing points at.
\
\ WHAT THE ROUTINE IS, AND WHY IT IS ORDINARY CHECKED HABU. It is a word of one
\ argument that never returns. Everything it does - read a row, build a message,
\ write it, end the process - the checked language already says, so there is no
\ hand-written machine code here and no new engine primitive: the chain reaches
\ it exactly as it reaches any other word whose address it resolved, and this
\ file's contract can be tested by calling it.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/core/engine-error.f

package NTRAP
private

\ ---- the table ---------------------------------------------------------------
\ Sized from the tree it has to hold rather than from a guess: the repository
\ declares 361 type families whose longest tail is 31 bytes, so the row ceiling
\ is roughly four times that count and the arena holds every one of those names
\ at the ceiling length.
64 constant NAME-CAP                 \ the longest family name a row may hold
1024 constant ROWS-MAX               \ distinct families
$8000 constant ARENA-CAP             \ 32 KB of names

create ARENA ARENA-CAP allot
create R-OFF ROWS-MAX cells allot    \ each row's offset into the arena
create R-LEN ROWS-MAX cells allot    \ and its length

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

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   ARENA k ROW-OFF +  k ROW-LEN ;

\ Which row holds this name, or -1. The table is small and it is walked whole:
\ registering happens once per family per process, and reading happens once per
\ process because the read is the last thing the process does.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1 FOUND !
   ROWS @ 0 ?do
      i ROW$ a u STR= if i FOUND ! leave then
   loop
   FOUND @ ;

\ Every ceiling is checked before anything moves, so a refused name leaves the
\ arena and the row count exactly as it found them.
: ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 <= if E-NTRAP-NAME throw then
   u NAME-CAP > if E-NTRAP-NAME throw then
   ROWS @ ROWS-MAX >= if E-NTRAP-CAP throw then
   USED @ u + ARENA-CAP > if E-NTRAP-CAP throw then
   a  ARENA USED @ +  u BYTE-COPY
   USED @ ROWS @ cells R-OFF + !
   u ROWS @ cells R-LEN + !
   USED @ u + USED !
   ROWS @ 1+ ROWS !
   ROWS @ 1- ;

\ ---- the message -------------------------------------------------------------
\ The exact bytes the engine's own inline trap writes, so a compiled MATCH and an
\ interpreted one end the process saying the same thing: test/gate-engine-lib.f
\ GE-MATCH-BAD-TAG reads them off stderr.
8 constant PFX-N                     \ "hb: bad "
5 constant SFX-N                     \ " tag\n"

create MSG PFX-N NAME-CAP + SFX-N + allot

: PFX! ( -- )
   s" hb: bad " {: a:ptr u:n :}
   a MSG u BYTE-COPY ;

: SFX! ( n -- ) {: at:n :}
   S\" \x20tag\n" {: a:ptr u:n :}
   a MSG at + u BYTE-COPY ;

: NAME! ( n -- n ) {: k:n :}
   k ROW$ {: a:ptr u:n :}
   a MSG PFX-N + u BYTE-COPY
   PFX-N u + ;

public

\ ---- what the chain asks ------------------------------------------------------
\ The number a trap site carries for this family. Registering is idempotent, so
\ two sites over one family carry one number and a second elaboration of the same
\ definition adds no row.
: FAMILY ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FIND {: k:n :}
   k 0 >= if k exit then
   a u ADD ;

\ The name an ordinal stands for. Published because it is how a caller checks
\ that the number it is about to compile into a routine means what it thinks.
: NAME$ ( n -- ptr u8 n )
   ROW-CK ROW$ ;

: COUNT ( -- n )
   ROWS @ ;

\ The name of the routine below, for the pass that has to resolve its address.
\ It is published as a SPELLING and not as an address because resolving one is
\ the dictionary layer's job and this file has no business knowing about it:
\ src/compiler/native/select.f asks NDICT:CALL-TARGET for this name exactly as it
\ asks for any other callee, so a trap site reaches this routine through the same
\ door, under the same refusal when a name is not callable, as every other call
\ the chain compiles. It is also what makes the target ONE routine tree-wide:
\ there is one name here and one place that reads it.
: ROUTINE$ ( -- ptr u8 n )
   s" NTRAP:BAD-TAG" ;

\ ---- the routine every trap site branches to ----------------------------------
\ It is entered with the family ordinal and it does not come back: the process
\ ends here with the diagnostic on standard error and ENGINE-ERROR:BAD-TAG as its
\ status, which is the whole observable contract of a scrutinee whose tag matches
\ no arm.
\
\ AN ORDINAL OUTSIDE THE TABLE IS NOT A FAMILY AND IS NOT REPORTED AS ONE. The
\ number was written into a published routine by this same process, so a number
\ that is not a row means the module or the table has been corrupted, and naming
\ some other family would be worse than saying nothing. ROW-CK throws, which
\ leaves the process with the throw code named rather than with a diagnostic that
\ is not true.
: BAD-TAG ( n -- )
   ROW-CK {: k:n :}
   PFX!
   k NAME! {: at:n :}
   at SFX!
   MSG at SFX-N +  ENGINE-ERROR:BAD-TAG die ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
