\ clobber.f - what a routine the native chain published really destroys, kept
\ against the address its code starts at. One concern: the record a call site
\ narrows its caller-save discipline against.
\
\ WHY THERE IS ANYTHING TO RECORD. Nothing in a Habu word's convention is
\ callee-saved (src/compiler/native/abi.f), so a call site has to assume the
\ callee destroys every register the caller could be holding a value in, and put
\ all of them somewhere the callee cannot reach. That assumption is exactly right
\ for a callee the chain did not compile, and much too wide for one it did: an
\ emission the validator accepted says which registers it writes, register by
\ register, and most routines write a handful. This file is where that answer
\ lives between the publication that made it and the call site that uses it.
\
\ THE KEY IS THE ADDRESS AND NOT THE NAME, and that is the whole soundness
\ argument. A call site branches to an ADDRESS - the entry the migration staged
\ and the emitter measured its displacement from - so what a site needs to know
\ is what the code at that address destroys. A name can be redefined; the code at
\ an address cannot be rewritten by this system, because
\ src/compiler/native/publish.f writes every emission at the engine's free code
\ slot and moves the pointer past it, so no two publications ever claim one slot.
\ A row here is therefore written once and never contradicted, and a caller
\ compiled against it stays right for as long as the code it calls is there.
\
\ AND THE ONE RULE THAT KEEPS IT TRUE IS THAT A ROW MAY ONLY EVER NARROW. A
\ caller compiled while this file knew nothing about an address saved everything,
\ which is right whatever the callee turns out to destroy. A caller compiled
\ against a row saved everything outside that row's set, and stays right only
\ while the code at that address destroys no more than the row says. So a second
\ record for one address is accepted when it destroys a SUBSET of what the first
\ one did and refused by name when it destroys anything more - E-NCLOB-WIDEN -
\ rather than silently invalidating callers nothing tracks. The refusal cannot be
\ reached through the publication seam, for the reason above; it is here because
\ "the code pointer only moves forward" is a property of another file, and a rule
\ that callers' correctness rests on should fail closed where it is used rather
\ than be argued about where it is provided.
\
\ WHAT AN UNKNOWN ADDRESS ANSWERS. The worst case its caller states, which is
\ what the caller was doing before this file existed. A word the engine's own
\ emitter compiled has no row here and never will, and a site that calls one
\ saves everything it holds - so the narrowing is something a chain-compiled
\ callee EARNS, and the discipline is unchanged everywhere else.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f

package NCLOB

private

\ How many published routines this file can remember in one process. It is a
\ fixed table for the reason src/compiler/native/publish.f's log is one: this
\ runs while the engine is compiling and has nowhere to allocate from. A row can
\ never be dropped to make space - dropping one would silently widen what every
\ caller compiled against it assumed - so the ceiling is a refusal, E-NCLOB-CAP,
\ and it is the same number of routines the publication log holds.
128 constant ROWS-MAX

create R-ENTRY ROWS-MAX cells allot
create R-GPR ROWS-MAX cells allot
create R-FPR ROWS-MAX cells allot
variable ROWS-N
0 ROWS-N !

\ Which row this address has, or -1. Linear, because the table is small and the
\ answer has to be exact: a hash that collided would hand one routine's
\ destroyed set to another routine's callers.
: ROW-OF ( n -- n )
   {: entry:n :}
   -1
   ROWS-N @ 0 ?do
      i cells R-ENTRY + @ entry = if drop i leave then
   loop ;

\ The bits, so that "is every register of the new set already in the old one" is
\ one question asked of one file at a time.
: NARROWS? ( n n -- bool )
   {: old:n new:n :}
   new old and new = ;

: ROW+ ( n n n -- )
   {: entry:n g:n f:n :}
   ROWS-N @ {: k:n :}
   k ROWS-MAX >= if E-NCLOB-CAP throw then
   entry k cells R-ENTRY + !
   g k cells R-GPR + !
   f k cells R-FPR + !
   k 1+ ROWS-N ! ;

public

\ Does this file know what the routine at this address destroys?
: KNOWN? ( n -- bool )
   ROW-OF 0 >= ;

\ What the routine at this address destroys, or the worst case the caller states
\ for an address with no row. Two readers rather than one, because a shortage in
\ one register file is not answered by the other and neither is a consumer's
\ worst case.
: GPR-CLOB ( n A64EFF:gprs -- A64EFF:gprs )
   {: entry:n worst:A64EFF:gprs :}
   entry ROW-OF {: k:n :}
   k 0 < if worst exit then
   k cells R-GPR + @ A64EFF:GPR-SET ;

: FPR-CLOB ( n A64EFF:fprs -- A64EFF:fprs )
   {: entry:n worst:A64EFF:fprs :}
   entry ROW-OF {: k:n :}
   k 0 < if worst exit then
   k cells R-FPR + @ A64EFF:FPR-SET ;

\ Is there room for a row this file does not have yet? The publication seam asks
\ before it writes a byte, because RECORD below is called once the routine is in
\ the code arena and the dictionary record points at it - and a refusal there
\ would leave a published word this file knows nothing about. Asked and answered
\ in front, the one refusal a caller can really reach is taken while a refusal
\ still costs nothing.
: ROOM-CK ( n -- )
   {: entry:n :}
   entry ROW-OF 0 >= if exit then
   ROWS-N @ ROWS-MAX >= if E-NCLOB-CAP throw then ;

\ Record what the routine published at this address destroys. A first row is
\ taken as it stands; a second one for the same address is taken only when it
\ destroys no register the first did not, because every caller compiled in
\ between saved exactly what the first row let it skip.
: RECORD ( n A64EFF:gprs A64EFF:fprs -- )
   {: entry:n g:A64EFF:gprs f:A64EFF:fprs :}
   g A64EFF:GPRS-N {: gb:n :}
   f A64EFF:FPRS-N {: fb:n :}
   entry ROW-OF {: k:n :}
   k 0 < if entry gb fb ROW+ exit then
   k cells R-GPR + @ gb NARROWS? 0= if E-NCLOB-WIDEN throw then
   k cells R-FPR + @ fb NARROWS? 0= if E-NCLOB-WIDEN throw then
   gb k cells R-GPR + !
   fb k cells R-FPR + ! ;

\ How many routines this file remembers, which is what a test measures a
\ publication against.
: ROWS ( -- n )
   ROWS-N @ ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
