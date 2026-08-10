\ trap.f - the one routine a compiled trap branches to, and the table that says
\ what it is trapping on. One concern: turning a name into a number a trap site
\ can carry, and turning that number back into the message the process exits
\ with.
\
\ TWO THINGS A TRAP CAN BE ABOUT, AND A ROW SAYS WHICH. The first is a scrutinee
\ whose tag matches no arm of a `MATCH`, and the name is the family's. The second
\ is a path the compiler was told could not be reached: a call to a word the
\ checker certified as never returning ends the block it is in, and the
\ instruction after that call exists only because a block must end somewhere. If
\ control ever arrives there, the certificate the caller was compiled against was
\ false, and the name is the CALLEE's. The two are one table and one routine
\ because a trap site carries one number and branches to one address; they are
\ two KINDS because "hb: bad option tag" is not what a word that came back from
\ `throw` did, and a diagnostic that would be a lie if it printed is not one this
\ compiler writes.
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
create R-KIND ROWS-MAX cells allot   \ and which of the two things it is about

\ The two kinds. They are stored codes rather than a flag because a row cell
\ holds a number, and the routine below reads it back as an exact case: a row
\ written with anything else decodes as neither and is refused rather than
\ reported as one of them.
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

\ Which row holds this name UNDER THIS KIND, or -1. The kind is half the key
\ because the two vocabularies are not one: a type family and a word may be
\ spelled the same, and answering one caller with the other's row would make a
\ trap report the wrong thing about the wrong subject. The table is small and it
\ is walked whole: registering happens once per subject per process, and reading
\ happens once per process because the read is the last thing the process does.
: FIND ( ptr u8 n n -- n ) {: a:ptr u:n kind:n :}
   -1 FOUND !
   ROWS @ 0 ?do
      i ROW-KIND kind = if
         i ROW$ a u STR= if i FOUND ! leave then
      then
   loop
   FOUND @ ;

\ Every ceiling is checked before anything moves, so a refused name leaves the
\ arena and the row count exactly as it found them.
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
\ compiled MATCH and an interpreted one end the process saying the same thing:
\ test/gate-engine-lib.f GE-MATCH-BAD-TAG reads them off stderr.
\
\ THE NO-RETURN FORM NAMES THE WORD THAT CAME BACK, because that is the whole of
\ what is known and the whole of what is useful: the caller was compiled against
\ a certificate saying this callee never returns, and it did.
5 constant SFX-N                     \ " tag\n"
10 constant NSFX-N                   \ " returned\n"
8 NAME-CAP + SFX-N + constant TAG-MSG-CAP        \ "hb: bad " + name + " tag\n"
4 NAME-CAP + NSFX-N + constant NORET-MSG-CAP     \ "hb: " + name + " returned\n"

create MSG TAG-MSG-CAP NORET-MSG-CAP max allot

\ One run of bytes into the message at an offset, answering the offset after it.
\ Every part of both messages goes through it, so where a part lands is decided
\ by what came before it and never by a second count of the same prefix.
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
\ The two kinds a caller registers under, published because a caller that reads a
\ row back has to be able to say which kind it expected.
KIND-TAG constant TAG
KIND-NORET constant NO-RET

\ The number a trap site carries for this family. Registering is idempotent, so
\ two sites over one family carry one number and a second elaboration of the same
\ definition adds no row.
: FAMILY ( ptr u8 n -- n )
   KIND-TAG INTERN ;

\ And the number a site carries for the instruction after a call that does not
\ come back. The name is the CALLEE's, so a trap that ever printed would say
\ which word broke its certificate. It is a separate row from a family of the
\ same spelling, and idempotent for the same reason.
: NO-RETURN ( ptr u8 n -- n )
   KIND-NORET INTERN ;

\ The name an ordinal stands for. Published because it is how a caller checks
\ that the number it is about to compile into a routine means what it thinks.
: NAME$ ( n -- ptr u8 n )
   ROW-CK ROW$ ;

\ And which kind it stands for, which is the other half of that check.
: KIND@ ( n -- n )
   ROW-CK ROW-KIND ;

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
   s" NTRAP:TRAP" ;

\ ---- the routine every trap site branches to ----------------------------------
\ It is entered with the ordinal and it does not come back: the process ends here
\ with the diagnostic on standard error and the status its row's kind calls for.
\ A scrutinee whose tag matches no arm exits ENGINE-ERROR:BAD-TAG, which is the
\ whole observable contract of a MATCH; a word that returned from a call the
\ certificate said never returns exits ENGINE-ERROR:CODE-CERT, because what was
\ false is the certificate the caller was compiled against.
\
\ AND THAT DISTINCTION EARNED MORE SINCE IT WAS MADE. A caller whose every path
\ ends is now published under a routine contract that declares no frame and no
\ saved return address at all (src/compiler/native/abi.f NORET-FRAMED), and what
\ licenses both is that same certificate - so a callee that comes back does not
\ merely arrive somewhere the compiler thought unreachable, it arrives in a
\ routine that has nowhere to return to. BAD-TAG would say something untrue about
\ a scrutinee; CODE-CERT names the thing that was false.
\
\ AN ORDINAL OUTSIDE THE TABLE IS NOT A ROW AND IS NOT REPORTED AS ONE. The
\ number was written into a published routine by this same process, so a number
\ that is not a row means the module or the table has been corrupted, and naming
\ some other subject would be worse than saying nothing. ROW-CK throws, which
\ leaves the process with the throw code named rather than with a diagnostic that
\ is not true. A row whose kind is neither is the same corruption seen from the
\ other side, and it says so rather than falling into either message.
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
