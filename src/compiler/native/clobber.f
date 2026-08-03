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
\ is what the code at that address destroys. A name can be redefined; a row here
\ describes the code at an address, and it is right for exactly as long as that
\ code is the routine it was written for.
\
\ WHICH IS NOT AS LONG AS THE PROCESS RUNS, AND THAT IS WHY ROWS ARE DROPPED.
\ src/compiler/native/publish.f writes every emission at the engine's free code
\ slot and moves the pointer past it, so no two publications claim one slot
\ WHILE THE POINTER ONLY GOES FORWARD. It does not: a FORGET
\ (src/habu/xref.f) and a declaration rollback
\ (src/core/generated-declaration-dictionary.f) both move it back, and the bytes
\ above it are handed to whatever is compiled next. A row left behind then
\ describes a routine nobody can reach any more, and - worse - a later
\ publication or an engine definition can land at exactly that address and
\ inherit it, which is a caller told that the routine it is about to branch to
\ destroys the registers of a routine that no longer exists. That was reproduced
\ end to end: a stale narrow row made a caller skip saving two registers the
\ engine's own emitter writes, and the caller computed 86 where 76 was right.
\
\ SO ROW LIFETIME IS A CONSEQUENCE OF CODE LIFETIME. This file registers with
\ src/habu/xref.f's CODE-RECLAIM, which is the one word every checked
\ reclamation of code space goes through, and drops every row at or above the
\ floor before the bytes are released. Nothing else may drop a row, which is why
\ DROP-FROM below is private and reachable only through that registration: the
\ argument that a row may only narrow is an argument about LIVE code, and it is
\ untouched, because a dropped row's code is gone.
\
\ AND THE TABLE IS THE BETTER FOR IT. A row was previously kept for every
\ address this process ever published at, so a forget-and-re-migrate cycle burnt
\ a row per turn and ROWS-MAX became a limit on how many times a program could
\ recompile a word rather than on how many routines it has. Dropping the rows of
\ reclaimed code gives the table its end back, so a slot whose routine is gone is
\ a slot a later publication can have.
\
\ AND THE ONE RULE THAT KEEPS IT TRUE IS THAT A ROW MAY ONLY EVER NARROW. A
\ caller compiled while this file knew nothing about an address saved everything,
\ which is right whatever the callee turns out to destroy. A caller compiled
\ against a row saved everything outside that row's set, and stays right only
\ while the code at that address destroys no more than the row says. So a second
\ record for one address is accepted when it destroys a SUBSET of what the first
\ one did and refused by name when it destroys anything more - E-NCLOB-WIDEN -
\ rather than silently invalidating callers nothing tracks. The refusal cannot be
\ reached through the publication seam, because a slot is claimed once between
\ reclamations and a reclamation drops the row; it is here because that is a
\ property of two other files, and a rule that callers' correctness rests on
\ should fail closed where it is used rather than be argued about where it is
\ provided.
\
\ AND THE REFUSAL IS ALSO ASKED IN FRONT. RECORD below runs after the routine is
\ in the code arena and the dictionary record points at it, so a refusal there
\ would leave running code with a row that describes something else - which is
\ exactly the shape the widen rule exists to prevent. RECORD-CK is the same
\ question asked while a refusal still costs nothing, and the publication seam
\ asks it before it writes a byte.
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

\ How many LIVE published routines this file can remember at once. It is a fixed
\ table for the reason src/compiler/native/publish.f's log is one: this runs
\ while the engine is compiling and has nowhere to allocate from. A row is never
\ dropped TO MAKE SPACE - dropping one would silently widen what every caller
\ compiled against it assumed - so the ceiling is a refusal, E-NCLOB-CAP, and it
\ is the same number of routines the publication log holds. A row IS dropped
\ when the code it describes is reclaimed, which is a different thing entirely:
\ there is no caller left to widen anything for.
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

\ The first row at or above this address, or the end of the table. What makes one
\ number the whole answer is that the live table is in publication order and a
\ publication's slot is above every slot claimed before it - which
\ src/compiler/native/publish.f holds as a REFUSAL, E-NPUB-SLOT, rather than as
\ an assumption - so the rows a reclamation takes away are a SUFFIX and the cut
\ is where it starts.
: FLOOR-ROW ( n -- n )
   {: floor:n :}
   ROWS-N @
   ROWS-N @ 0 ?do
      i cells R-ENTRY + @ floor >= if drop i leave then
   loop ;

\ ...and that the rest of the table really is above the floor. A row below it
\ after the cut would mean this table is not the sequence the cut rests on, which
\ is a defect in this file rather than anything a program can ask for: there is
\ no correct answer to give and no caller to give it to, so it dies here rather
\ than dropping the wrong rows. A watcher may not throw - the reclamation it is
\ answering is already half done - and this is the shape src/core/decl-event.f
\ uses for the same class of defect.
: ORDER-CK ( n n -- )
   {: floor:n k:n :}
   ROWS-N @ k ?do
      i cells R-ENTRY + @ floor < if
         s" nclob: recorded routines out of publication order" 76 die
      then
   loop ;

\ Drop every row whose routine starts at or above this address. It is private and
\ registered ONCE, below, with the one word every checked reclamation of code
\ space goes through: dropping a row is sound exactly when the code it describes
\ is gone, and this file is not in a position to know that - the file that
\ reclaims the space is. Dropping a suffix rather than sifting the table is what
\ leaves the surviving rows where they were, so nothing that counted them has to
\ be told, and the slots above the cut are a later publication's to take.
: DROP-FROM ( n -- )
   {: floor:n :}
   floor FLOOR-ROW {: k:n :}
   floor k ORDER-CK
   k ROWS-N ! ;

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

\ Would RECORD below take this set at this address? Both of its refusals, asked
\ while a refusal still costs nothing. RECORD runs once the routine is in the
\ code arena and the dictionary record points at it, so a refusal THERE leaves
\ running code with no row or - worse, and this really happened - with a row
\ that describes a different routine: a widening publication was refused after
\ its word was live and retargeted, and the next caller compiled against the row
\ the refusal left behind computed 119 where 110 was right. So the publication
\ seam asks this before it writes a byte, and the refusals inside RECORD are the
\ backstop rather than the path.
: RECORD-CK ( n A64EFF:gprs A64EFF:fprs -- )
   {: entry:n g:A64EFF:gprs f:A64EFF:fprs :}
   entry ROW-OF {: k:n :}
   k 0 < if
      ROWS-N @ ROWS-MAX >= if E-NCLOB-CAP throw then
      exit
   then
   k cells R-GPR + @ g A64EFF:GPRS-N NARROWS? 0= if E-NCLOB-WIDEN throw then
   k cells R-FPR + @ f A64EFF:FPRS-N NARROWS? 0= if E-NCLOB-WIDEN throw then ;

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

\ Hear about every reclamation of code space, for as long as this file is
\ loaded. It is one registration and there is no way to undo it: a row that
\ outlived its code is what this whole file's answer is read against.
: WATCH-INSTALL ( -- )
   [: DROP-FROM ;] CODE-RECLAIM:WATCH ;

WATCH-INSTALL

get-current prot-wid-add

public
get-current prot-wid-add

;package
