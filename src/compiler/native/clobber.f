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
\ a row per turn and the table's end became a limit on how many times a program
\ could recompile a word rather than on how many routines it has. Dropping the
\ rows of reclaimed code gives the table its end back, so a slot whose routine is
\ gone is a slot a later publication can have.
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
require lib/vector.f
require src/compiler/a64-effect.f

package NCLOB

private

\ ---- how many routines this file can remember at once -------------------------
\ ONE ROW PER LIVE PUBLISHED ROUTINE, AND THE PROGRAM SAYS HOW MANY THAT IS. A
\ row is never dropped TO MAKE SPACE - dropping one would silently widen what
\ every caller compiled against it assumed - so a table that could not grow was a
\ limit on how much of a program the chain may compile, and not a limit on
\ anything the record is about. It was 128 rows, which is what the system
\ migrated when the record was written; a whole-tree census hit that number
\ exactly and reported it as the size of the compilable tree.
\
\ SO THE ROWS ARE A GROWABLE VECTOR AND NOT A FIXED ARRAY. lib/vector.f is the
\ tree's growable cell array: mapped storage, doubled and copied when it fills,
\ and the span it grew out of handed back to the OS. Three of them, one per
\ column, because the lookup below reads only the address column and a row's
\ three cells are never read together.
\
\ AND "NOWHERE TO ALLOCATE FROM" WAS NOT TRUE. That was the reason given for the
\ fixed array, and the chain disproves it on every migration it makes: each one
\ runs inside src/compiler/ir/context.f's WITH-CONTEXT, which maps half a
\ megabyte through MEM:WITH-BYTES and gives it back, and the publication this
\ file serves happens inside that. What is true is that the space must be taken
\ BEFORE the commit phase, because a publication's commit may not throw, and
\ ROOM-CK below is where it is taken.
128 constant ROWS-SEED

\ ---- and the one ceiling that is left -----------------------------------------
\ A row exists because a publication claimed a code slot for it, slots are
\ claimed in strictly increasing order (src/compiler/native/publish.f SLOT-CK
\ refuses one below the last routine's end) and every one of them is an
\ instruction-aligned address inside the engine's code region. So the rows this
\ record can be holding at once are at most the instruction slots that region
\ has, and E-NCLOB-CAP is that bound.
\
\ IT IS A BACKSTOP AND NOT A PATH, which is the shape the widen refusal in RECORD
\ already has here. The publication seam runs out of code arena long before it
\ runs out of slots - src/compiler/native/publish.f ROOM-CK refuses with
\ E-NPUB-ROOM at the end reserve - so a program cannot reach this number through
\ the seam at all. It is asked because a caller that records addresses of its own
\ is not the seam, and a record that grew without a bound would be a table with
\ no answer for how large it may become.
4 constant INSN-BYTES
REGION INSN-BYTES / constant ROWS-CEIL

create R-ENTRY VEC-HEADER-CELLS cells allot
create R-GPR VEC-HEADER-CELLS cells allot
create R-FPR VEC-HEADER-CELLS cells allot

: TABLE-INIT ( -- )
   R-ENTRY ROWS-SEED VEC-COUNT VEC-INIT
   R-GPR ROWS-SEED VEC-COUNT VEC-INIT
   R-FPR ROWS-SEED VEC-COUNT VEC-INIT ;

TABLE-INIT

\ How many rows are live. The three columns are written and truncated together,
\ so the address column's length is the table's length.
: ROWS# ( -- n )
   R-ENTRY VEC-LEN@ LEN>N ;

\ The address column, read the way the three scans below read it: the storage
\ base and one cell out of it. They walk every live row, so this is the one
\ reader on a path whose length is the population, and the checked element
\ accessor - six nested calls to prove an index the loop bound already proves -
\ made a lookup twelve times what it costs here. The row columns are read once
\ per operation rather than once per row, so they keep the checked accessor.
: ENTRY-AT ( n -- n ) {: k:n :}
   R-ENTRY VEC-DATA@ k cells + @ ;

: GPR-AT ( n -- n ) {: k:n :}
   R-GPR k VEC-IDX VEC-N@ ;

: FPR-AT ( n -- n ) {: k:n :}
   R-FPR k VEC-IDX VEC-N@ ;

: GPR-AT! ( n n -- ) {: v:n k:n :}
   v R-GPR k VEC-IDX VEC-N! ;

: FPR-AT! ( n n -- ) {: v:n k:n :}
   v R-FPR k VEC-IDX VEC-N! ;

\ Cut the table to its first k rows. The three columns move together or the
\ record would answer one routine's address with another routine's registers.
: TRUNC-TO ( n -- ) {: k:n :}
   k VEC-LEN R-ENTRY VEC-LEN!
   k VEC-LEN R-GPR VEC-LEN!
   k VEC-LEN R-FPR VEC-LEN! ;

\ Which row this address has, or -1. Linear, because the answer has to be exact:
\ a hash that collided would hand one routine's destroyed set to another
\ routine's callers.
: ROW-OF ( n -- n )
   {: entry:n :}
   -1
   ROWS# 0 ?do
      i ENTRY-AT entry = if drop i leave then
   loop ;

\ The bits, so that "is every register of the new set already in the old one" is
\ one question asked of one file at a time.
: NARROWS? ( n n -- bool )
   {: old:n new:n :}
   new old and new = ;

\ Room for one more row, taken in front. Growing is an allocation and an
\ allocation can fail, so it happens where a refusal still costs nothing rather
\ than in the append, which runs in a publication's commit phase and may not
\ throw. Taking room changes no row: a caller refused after this ran finds the
\ record holding exactly what it held before.
: ROOM-CK ( -- )
   ROWS# 1+ {: need:n :}
   need ROWS-CEIL > if E-NCLOB-CAP throw then
   R-ENTRY need VEC-COUNT VEC-ENSURE
   R-GPR need VEC-COUNT VEC-ENSURE
   R-FPR need VEC-COUNT VEC-ENSURE ;

: ROW+ ( n n n -- )
   {: entry:n g:n f:n :}
   ROOM-CK
   entry R-ENTRY VEC-PUSH-N drop
   g R-GPR VEC-PUSH-N drop
   f R-FPR VEC-PUSH-N drop ;

\ The first row at or above this address, or the end of the table. What makes one
\ number the whole answer is that the live table is in publication order and a
\ publication's slot is above every slot claimed before it - which
\ src/compiler/native/publish.f holds as a REFUSAL, E-NPUB-SLOT, rather than as
\ an assumption - so the rows a reclamation takes away are a SUFFIX and the cut
\ is where it starts.
: FLOOR-ROW ( n -- n )
   {: floor:n :}
   ROWS#
   ROWS# 0 ?do
      i ENTRY-AT floor >= if drop i leave then
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
   ROWS# k ?do
      i ENTRY-AT floor < if
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
   k TRUNC-TO ;

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
   k GPR-AT A64EFF:GPR-SET ;

: FPR-CLOB ( n A64EFF:fprs -- A64EFF:fprs )
   {: entry:n worst:A64EFF:fprs :}
   entry ROW-OF {: k:n :}
   k 0 < if worst exit then
   k FPR-AT A64EFF:FPR-SET ;

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
   k 0 < if ROOM-CK exit then
   k GPR-AT g A64EFF:GPRS-N NARROWS? 0= if E-NCLOB-WIDEN throw then
   k FPR-AT f A64EFF:FPRS-N NARROWS? 0= if E-NCLOB-WIDEN throw then ;

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
   k GPR-AT gb NARROWS? 0= if E-NCLOB-WIDEN throw then
   k FPR-AT fb NARROWS? 0= if E-NCLOB-WIDEN throw then
   gb k GPR-AT!
   fb k FPR-AT! ;

\ How many routines this file remembers, which is what a test measures a
\ publication against.
: ROWS ( -- n )
   ROWS# ;

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
