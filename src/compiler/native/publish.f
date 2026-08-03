\ publish.f - making a sealed emission an ordinary word of the running engine.
\ One concern: the engine boundary between machine code the chain emitted and a
\ dictionary record every caller already reaches.
\
\ WHAT THIS IS FOR. Up to here the chain's output was entered through an address
\ on the data stack - `execute` on a number a fixture kept - which is one
\ indirect branch the engine's own words never pay. A word of this engine is a
\ dictionary record whose first cell is the address of its first instruction, so
\ making the chain's routine a real word is not a new mechanism: it is writing
\ that cell. Afterwards the interpreter branches to it the way it branches to
\ any word, and a definition compiled after it either calls it with one direct
\ branch or inlines its body, which is exactly what the engine does for every
\ other word of that size.
\
\ THE ONLY DOOR IS THE SEALED EMISSION. REPUBLISH takes a NAME and nothing else.
\ Every byte it writes is read back out of A64EMIT - the instruction count, each
\ instruction word, and the byte offset the emitter's own source map recorded for
\ it - and every one of those readers refuses before the emitter has sealed a
\ run (E-A64EMIT-STATE), which A64EMIT:EMIT only reaches after A64RAV has
\ accepted the allocation for that very module (E-A64EMIT-ALLOC). So there is no
\ argument through which a caller could present bytes of its own, and no way to
\ publish an emission the validator has not accepted: the second door would have
\ to be a second parameter, and there is none.
\
\ THE OFFSETS COME FROM THE SOURCE MAP. Instruction i is written at the offset
\ the emitter recorded for it, not at four times its position, so a map that lost
\ a row or moved an offset leaves a hole in the published routine rather than a
\ routine nobody compared the map against. An offset outside the emission, or one
\ that is not instruction aligned, is refused by name here.
\
\ AND THE SLOT IS ALSO WHAT THE ROUTINE'S OWN BRANCHES WERE MEASURED FROM. A
\ routine that calls another word carries a branch whose displacement is the
\ callee's address less the address of the branch instruction itself, so it is
\ only correct where the emitter was told the routine would be written. This seam
\ is what decides that: NEXT-SLOT answers the free code slot before the emission
\ is made, the emitter records the answer it was given, and REPUBLISH refuses a
\ pair that no longer agrees. There is exactly one authority on where a routine
\ lands, it is asked twice, and the second answer is held against the first
\ before any byte is written.
\
\ THE CODE SPACE IS THE ENGINE'S, CLAIMED THE ENGINE'S WAY. `cp@` is the free
\ slot of the one bump pointer the engine compiles every definition into, and
\ `cp!` moves it. A routine is written at the free slot and the pointer is moved
\ past it, so the next definition the engine compiles starts after this one -
\ there is no second allocator and no second arena. The room test is the
\ engine's own: src/habu/habu2.f admits a definition only while the pointer is
\ still below the end reserve, and this seam refuses by name at the same line
\ rather than letting `patch32` walk past it.
\
\ WHY THE RECORDED LENGTH IS THE EMISSION MINUS ONE INSTRUCTION. The engine
\ stores a word's code length EXCLUDING its trailing return (src/habu/habu2.f
\ EM-COMPILE-FLUSH-PEND writes `CP - entry - 4`), because that is the span its
\ inliner copies into a caller. Writing the whole size instead would make a
\ caller copy the return as well, so the subtraction is this seam's own
\ responsibility and an emission too short to hold a return is refused.
\
\ WHAT IT DOES NOT DECIDE. Whether the routine was compiled under the convention
\ a Habu word is entered through, and whether the name's checked effect matches
\ what the routine really consumes and leaves. Both are the caller's: this seam
\ sees bytes and a record, and A64EMIT does not publish the routine contract the
\ emission was made under. The migration entry that drives the chain is what
\ states the convention, and src/compiler/native/migrate.f is where that is said.
\
\ WHAT IT KEEPS. The record it replaced. A republication is a definition
\ transaction, so what was there before is evidence: it is what a refusal has to
\ leave untouched, and it is the byte count of the code the old emitter produced
\ for the same name, which nothing else remembers once the record is rewritten.
\
\ AND WHAT IT PUBLISHES BESIDES THE BYTES: WHAT THE ROUTINE DESTROYS. A call site
\ has to put every value it is holding somewhere the callee cannot reach, and
\ "every register the caller could be holding a value in" is the only honest
\ answer for a callee nobody knows anything about. This seam knows something
\ about the one it is publishing: the emission was made under an allocation the
\ validator accepted, and that allocation says which registers the routine writes
\ (A64EMIT:GPR-CLOBBER, derived in src/compiler/native/regalloc-verify.f and held
\ against the emission's own destination fields before it seals). So the answer
\ is recorded here, against the ADDRESS this seam is writing the routine at,
\ which is the address a later call site branches to - src/compiler/native/
\ clobber.f is the record and carries the argument for why an address is the
\ right key and why a row may only ever narrow.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/clobber.f
require src/compiler/native/emit.f

package NPUB

private

\ ---- the two engine facts this seam depends on -------------------------------
\ The end reserve the engine keeps below the top of the code arena. It is the
\ literal src/habu/habu2.f EM-INTERPRET-COLON tests the code pointer against
\ before it admits a definition's first instruction; a routine published past it
\ would leave the engine unable to compile the next definition.
$4000 constant CODE-RESERVE

4 constant INSN-BYTES

\ How many republications this seam can remember in one process. The log is
\ evidence and not a cache - it is the only thing that still knows what the old
\ emitter produced for a name once the record has been rewritten - so a row can
\ never be dropped to make space and the ceiling is a real refusal, E-NPUB-CAP,
\ rather than an eviction.
\
\ THE NUMBER IS WHAT THE SYSTEM ACTUALLY MIGRATES, and it moves when that does.
\ It is a fixed table because this seam runs while the engine is compiling and
\ has nowhere to allocate from; at 64 bytes of name and six cells a row, this
\ ceiling is about fourteen kilobytes, which is small beside the code arena it
\ writes into. Dot habu-grow-the-republication-52ef5df0 carries making it grow
\ with the program, which is what a whole-system migration will need.
128 constant LOG-MAX
64 constant NAME-MAX

\ ---- the one primitive that writes ------------------------------------------
\ `patch32` is the engine's 32-bit poke: it flips the code region writable,
\ stores, flips it back and syncs the instruction cache for the line it wrote,
\ all inside engine text. It is refused from checked code, so it is wrapped once
\ here and this word chooses nothing - both the value and the address are
\ computed by the checked words below. src/habu/xref.f wraps the same primitive
\ the same way to retire a record.
TRUSTED: POKE ( n ptr a -- )
   patch32 ;

\ A dictionary record is cells; `patch32` writes half of one, so a cell store is
\ its two halves. The record address is carried as a number because the cell
\ being written is addressed rather than dereferenced.
: HALF-LO ( n -- n )
   $FFFFFFFF and ;

: HALF-HI ( n -- n )
   $20 rshift $FFFFFFFF and ;

: REC-CELL! ( n n n -- )
   {: v:n rec:n k:n :}
   v HALF-LO   rec k cells + XREF-N>REC POKE
   v HALF-HI   rec k cells + INSN-BYTES + XREF-N>REC POKE ;

\ ---- the replacement log -----------------------------------------------------
\ One row per republished name: what the record held before, and what it holds
\ now. The name is stored so a caller can ask about a word rather than about a
\ row number.
LOG-MAX NAME-MAX * BUFFER: LOG-NAMES
create LOG-LENS LOG-MAX cells allot
create LOG-WIDS LOG-MAX cells allot
create LOG-OLD-START LOG-MAX cells allot
create LOG-OLD-LEN LOG-MAX cells allot
create LOG-NEW-START LOG-MAX cells allot
create LOG-NEW-LEN LOG-MAX cells allot
variable LOG-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

: LOG-NAME-AT ( n -- ptr u8 )
   NAME-MAX * LOG-NAMES + ;

: LOG-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k LOG-NAME-AT  LOG-LENS k SLOT @ ;

: LOG-ROW? ( n ptr u8 n n -- bool ) {: k:n a:ptr u:n wid:n :}
   LOG-WIDS k SLOT @ wid <> if false exit then
   k LOG-NAME$ a u STR= ;

: LOG-FIND ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   -1
   LOG-N @ 0 ?do
      i a u wid LOG-ROW? if drop i leave then
   loop ;

: LOG-OK ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   a u wid LOG-FIND dup 0 < if E-NPUB-LOG throw then ;

: LOG+ ( ptr u8 n n n n n n -- )
   {: a:ptr u:n wid:n os:n ol:n ns:n nl:n :}
   LOG-N @ LOG-MAX >= if E-NPUB-CAP throw then
   u NAME-MAX > if E-NPUB-CAP throw then
   a  LOG-N @ LOG-NAME-AT  u STR-LEN BYTE-COPY-LEN
   u   LOG-LENS LOG-N @ SLOT !
   wid LOG-WIDS LOG-N @ SLOT !
   os LOG-OLD-START LOG-N @ SLOT !
   ol LOG-OLD-LEN LOG-N @ SLOT !
   ns LOG-NEW-START LOG-N @ SLOT !
   nl LOG-NEW-LEN LOG-N @ SLOT !
   LOG-N @ 1+ LOG-N ! ;

\ ---- which names may be republished -----------------------------------------
\ A republishable record is a live word of a real wordlist whose body the running
\ program calls. The four that are refused are refused because rewriting their
\ first cell would mean something other than "this word's code moved": a package
\ record's first cell is a wordlist id and not an address, a retired record is
\ not reachable at all, an engine-internal word is one the interpreter refuses
\ to enter, and an immediate word's caller is the compiler, which would run the
\ routine at compile time rather than call it.
\ A word of this engine is a tail in a wordlist, so that pair is what names it
\ here. A bare tail alone would resolve only in the global wordlist, and a
\ package word's record would either be missed or, worse, found under some other
\ package's identical tail.
: NAME-REC ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   a u wid XREF-FIND-WL-INDEX dup 0 < if E-NPUB-NAME throw then ;

: WORDLIST-CK ( n -- ) {: idx:n :}
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   wid XREF-RETIRED-WL = if E-NPUB-NAME throw then
   wid XREF-NAMESPACE-WL = if E-NPUB-NAME throw then ;

: FLAGS-CK ( n -- ) {: idx:n :}
   idx XREF-REC XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if E-NPUB-NAME throw then
   f DNAME-IMM and 0<> if E-NPUB-NAME throw then ;

: TARGET ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   a u wid NAME-REC {: idx:n :}
   idx WORDLIST-CK
   idx FLAGS-CK
   idx ;

\ ---- the emission, measured --------------------------------------------------
\ Both readers refuse before the emitter has sealed a run, so a size asked for
\ here is a size some accepted allocation stands behind.
: SIZE-CK ( -- n )
   A64EMIT:SIZE {: n:n :}
   A64EMIT:INSNS 0 <= if E-NPUB-SIZE throw then
   n INSN-BYTES < if E-NPUB-SIZE throw then
   n INSN-BYTES mod 0<> if E-NPUB-SIZE throw then
   n ;

: OFFSET-CK ( n n -- n ) {: off:n size:n :}
   off 0 < if E-NPUB-OFFSET throw then
   off INSN-BYTES + size > if E-NPUB-OFFSET throw then
   off INSN-BYTES mod 0<> if E-NPUB-OFFSET throw then
   off ;

\ ---- claiming the code space -------------------------------------------------
\ The top of what the engine will compile into. Above it the engine's own
\ definition path stops admitting instructions, so a routine written there would
\ be the last thing this process could publish.
: CODE-CEILING ( -- n )
   dbase@ REGION + CODE-RESERVE - ;

: ROOM-CK ( n -- n ) {: size:n :}
   cp@ {: fn:n :}
   fn size + CODE-CEILING > if E-NPUB-ROOM throw then
   fn ;

\ ---- the routine's branches and the slot it is written at --------------------
\ A routine whose body calls ANOTHER word carries a branch measured from the
\ address the routine itself was going to occupy - the emitter cannot compute
\ such a displacement any other way, and where a routine occupies is THIS seam's
\ answer and nobody else's. So the emitter is told the slot before it emits, and
\ the emission answers which address it was told; the check is that it is the
\ slot being claimed now.
\
\ WHY THIS IS ONE AUTHORITY AND NOT TWO. The seam decides where a routine lands,
\ full stop: NEXT-SLOT below is the same `cp@` ROOM-CK reads, so a caller cannot
\ present a placement of its own devising and have it believed - it can only
\ present the answer this file gave it, and if anything moved the code pointer in
\ between, the two disagree and the publication is refused with the record
\ untouched. An emission that was made against no placement has no branch that
\ depends on one, so it publishes wherever it fits, exactly as before.
: PLACE-CK ( n -- ) {: fn:n :}
   A64EMIT:PLACED? 0= if exit then
   A64EMIT:PLACEMENT fn <> if E-NPUB-PLACE throw then ;

\ ---- what the routine branches to, read off the instructions ------------------
\ A routine destroys what its own instructions write AND everything the routines
\ it calls destroy. The emitter counts both while it emits (A64EMIT's NOTE-WRITE
\ and NOTE-CALLEE) and hands the union over as one answer; this is the second
\ derivation of the callee half, made the only other way there is - by finding
\ the branches in the finished instruction stream and computing where each one
\ goes. A recorded set that does not cover a callee's own recorded set is refused
\ here, before a byte is written, rather than published as a routine that
\ destroys less than it does. That is worth a decoder because it is the half that
\ is silently wrong when it is wrong: a caller of THIS routine would keep a value
\ in a register the callee's callee writes, and nothing downstream would ever ask
\ again.
\
\ A BRANCH TO THIS ROUTINE'S OWN ENTRY IS NOT A CALLEE. The callee is this same
\ routine, so what it destroys is the set being computed and the union of a set
\ with itself is that set. It is also not recorded yet - this runs before RECORD
\ does - so asking about it would answer the worst case and refuse every routine
\ that calls itself.
$FC000000 constant BL-MASK
$94000000 constant BL-OP
$03FFFFFF constant BL-IMM
26 constant BL-BITS

: BL-AT? ( n -- bool )
   BL-MASK and BL-OP = ;

\ The address a branch-with-link goes to: its own address plus its signed
\ twenty-six-bit word displacement.
: BL-TARGET ( n n -- n ) {: w:n at:n :}
   w BL-IMM and {: imm:n :}
   imm  1 BL-BITS 1- lshift  and 0<> if imm 1 BL-BITS lshift - else imm then
   INSN-BYTES * at + ;

: INSN-ADDR ( n n n -- n ) {: fn:n size:n k:n :}
   k A64EMIT:MAP-OFFSET@ size OFFSET-CK fn + ;

: TARGET-CK ( n n n n -- ) {: t:n fn:n g:n f:n :}
   t fn = if exit then
   t A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N
   g invert and 0<> if E-NPUB-CLOBBER throw then
   t A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N
   f invert and 0<> if E-NPUB-CLOBBER throw then ;

: BRANCH-CK ( n n -- ) {: fn:n size:n :}
   A64EMIT:GPR-CLOBBER A64EFF:GPRS-N {: g:n :}
   A64EMIT:FPR-CLOBBER A64EFF:FPRS-N {: f:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ BL-AT? if
         i A64EMIT:WORD@  fn size i INSN-ADDR  BL-TARGET  fn g f TARGET-CK
      then
   loop ;

\ Write the emission at the claimed slot and move the code pointer past it, so
\ the next definition the engine compiles begins after this routine.
: WRITE ( n n -- ) {: fn:n size:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@
      i A64EMIT:MAP-OFFSET@ size OFFSET-CK fn +  XREF-N>REC
      POKE
   loop
   fn size + cp! ;

\ Point the record at the new routine. The length excludes the trailing return
\ because that is what the engine's own record means by a word's length.
: RETARGET ( n n n -- ) {: idx:n fn:n size:n :}
   fn   idx XREF-REC-ADDR XREF-START-SLOT REC-CELL!
   size INSN-BYTES -  idx XREF-REC-ADDR XREF-LEN-SLOT REC-CELL! ;

public

\ Make the sealed emission the code of the word this tail names in this
\ wordlist. Global words are wordlist zero; a package word's wordlist is the one
\ its own record carries.
\
\ Everything that can refuse refuses before the first byte is written: the
\ emission is read and measured, the name is resolved and admitted, and the code
\ space is claimed, all before WRITE. So a refusal leaves the dictionary record
\ exactly as it found it, and the word keeps running the code it was running.
: REPUBLISH ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   SIZE-CK {: size:n :}
   a u wid TARGET {: idx:n :}
   size ROOM-CK {: fn:n :}
   fn PLACE-CK
   fn NCLOB:ROOM-CK
   fn size BRANCH-CK
   idx XREF-REC XREF-START {: os:n :}
   idx XREF-REC XREF-LEN {: ol:n :}
   fn size WRITE
   idx fn size RETARGET
   fn  A64EMIT:GPR-CLOBBER  A64EMIT:FPR-CLOBBER  NCLOB:RECORD
   a u wid os ol fn  size INSN-BYTES -  LOG+ ;

\ The address the next republication will write its first instruction at. It is
\ the engine's own free code slot, which is what REPUBLISH claims, so a caller
\ that has to know where a routine will land - because its branches are measured
\ from there - asks the one file that decides it. Asking is free and changes
\ nothing: a caller that asks and never publishes has moved no pointer.
: NEXT-SLOT ( -- n )
   cp@ ;

\ How many words this process has republished.
: REPUBLISHED ( -- n )
   LOG-N @ ;

: REPUBLISHED? ( ptr u8 n n -- bool ) {: a:ptr u:n wid:n :}
   a u wid LOG-FIND 0 >= ;

\ The code the record held before the named word was republished: where it
\ started, and how many bytes of it the engine recorded. This is the only
\ surviving measurement of what the old emitter produced for that name.
: OLD-START ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   LOG-OLD-START  a u wid LOG-OK SLOT @ ;

: OLD-LEN ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   LOG-OLD-LEN  a u wid LOG-OK SLOT @ ;

: NEW-START ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   LOG-NEW-START  a u wid LOG-OK SLOT @ ;

: NEW-LEN ( ptr u8 n n -- n ) {: a:ptr u:n wid:n :}
   LOG-NEW-LEN  a u wid LOG-OK SLOT @ ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
