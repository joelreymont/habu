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
\ IT IS ONE WINDOW, AND THAT IS THE SHAPE OF THE WHOLE FILE. A publication used
\ to be a sequence of pokes: `patch32` per instruction, each one flipping the
\ code region writable, storing four bytes, flipping it back and syncing a cache
\ line - two protection syscalls apiece - and four more pokes for the two record
\ cells. A four-instruction routine cost sixteen protection syscalls, and the
\ record's cells were torn across two windows each. It is now ONE call to
\ `code-publish`: one flip to writable, one copy of the whole emission, one flip
\ back, one flush of exactly the range written. Two syscalls, whatever the
\ routine's length.
\
\ THE WINDOW CANNOT BE COMPOSED OUT OF SMALLER PIECES, which is why it is an
\ engine primitive rather than a loop here. Flipping the code region to writable
\ removes execute permission from the page the running interpreter is on, so
\ everything between the two flips has to be engine text; a Habu caller that held
\ the region open across its own loop would have unmapped itself. src/habu/
\ habu1.f BCODEPUBLISH is that window and this file is its only caller.
\
\ AND `patch32` IS NOT USED HERE ANY MORE. It remains the engine's ISOLATED
\ writer - one already-published instruction, rewritten in place, with its own
\ protection pair and its own line flush - which is what a debugger breakpoint
\ is and what src/habu/xref.f retires a record with. Publication is an append of
\ a whole routine and uses the window; a single-word edit of live code is the
\ other operation and uses the poke. Nothing in this file pokes.
\
\ THE TWO PHASES, AND THE LINE BETWEEN THEM IS THE POINT. Everything that can
\ refuse refuses in the first phase, before a single byte of code, a single map
\ bit or a single record cell has moved: the emission is measured, its source map
\ is held against the bytes it describes, the name is resolved and admitted, the
\ code space is claimed, the placement is held against the slot, the clobber row
\ is checked for room and for narrowing, every branch is decoded and held against
\ what the routine claims to destroy, and the replacement log is checked for room
\ and for name length. The second phase writes, and NOTHING in it can throw. A
\ refusal therefore leaves the dictionary record, the code arena, the call map
\ and the clobber table exactly as it found them, and the word keeps running the
\ code it was running. This was not true before: an overlong name reached
\ E-NPUB-CAP from the replacement log AFTER the live start address had already
\ been changed.
\
\ THE OFFSETS COME FROM THE SOURCE MAP, AND THE MAP HAS TO BE THE IDENTITY. A
\ bulk copy writes the emitter's buffer as it stands, so the claim that
\ instruction i belongs at offset 4i is no longer something the writer can honour
\ one instruction at a time - it is something the publication has to CHECK. So it
\ is checked, for every instruction, before the copy: an offset that is not four
\ times its position, or one outside the emission, or one that is not instruction
\ aligned, is refused by name. That is strictly more than the per-instruction
\ writer proved. It wrote each instruction wherever the map said and would have
\ published a permuted routine without complaint; this refuses one.
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
\ AND THE SLOT IT CLAIMS IS HELD AGAINST THE LAST ONE IT CLAIMED. "No two
\ publications ever claim one slot" is the sentence everything keyed to a code
\ address rests on - src/compiler/native/clobber.f and
\ src/compiler/native/inline.f both cite it - and it is not a property of this
\ file alone: the pointer goes BACK when code space is reclaimed
\ (src/habu/xref.f FORGET-DEFS-FROM,
\ src/core/generated-declaration-dictionary.f ROLLBACK). Both reclamations go
\ through CODE-RECLAIM, which tells this seam the floor and tells the two
\ records to drop what they hold above it, so re-using a slot after a
\ reclamation is exactly as sound as using a fresh one. What must not happen is
\ re-using one WITHOUT that notice, and SLOT-CK below is where that fails
\ closed: the seam remembers where its last routine ended, a claim below that
\ line is E-NPUB-SLOT, and a rewind nobody announced becomes a refusal at the
\ next publication instead of a caller compiled against another routine's row.
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
\ AND WHAT IT PUBLISHES BESIDES THE BYTES: THE RELOCATION RECORD OF EVERY CALL
\ IT WROTE. A call from one region address to another keeps its distance wherever
\ the region is mapped; a call from the region into the engine's loaded text does
\ not, because the kernel picks the region base and the loader picks the image
\ base independently. So a snapshot has to rewrite the second kind and must not
\ touch the first, and which kind a branch is has to be recorded where the branch
\ is BUILT - src/habu/layout.f says why nothing may recognise one afterwards by
\ decoding region bytes. The engine's own emitter records it at its call-emit
\ chokepoint. This publisher writes calls too, and for a long time recorded none
\ of them: a migrated routine calling sealed engine `abs` decoded to the right
\ target with its map bit clear, so a snapshot restore preserved the writing
\ run's displacement and branched into whatever now sits there.
\
\ IT IS WRITTEN HERE NOW, AND BOTH DIRECTIONS ARE WRITTEN. The window clears the
\ map over the whole span it publishes, so every word of a fresh routine starts
\ with no claim on it - which is what a reused slot needs, because the bits above
\ a reclaimed routine described calls that no longer exist. Then this file sets
\ the bit for exactly those branches whose target lies outside the region. A
\ branch that stays inside sets nothing, and needs to set nothing, because the
\ clear already ran. There is one writer of a code address in this system and it
\ is this file; there is one durable store of what that address is and it is the
\ XREF record.
\
\ WHICH IS THE SAME SENTENCE AS THE RECLAMATION INVARIANT. No address-keyed fact
\ may outlive the code it describes. The call map is given back by the window
\ that overwrites the span; the clobber row and the recorded body are given back
\ by CODE-RECLAIM (src/habu/xref.f), which is the notice every reclamation goes
\ through; and the claimed-slot line below follows the code pointer down. A fact
\ this file records is reachable only through an address the XREF record still
\ points at, and it stops being reachable when that record does.
\
\ AND THE RECORD IS COMMITTED IN ONE ORDER, WITH RELEASE. A record's first two
\ cells are the address a caller branches to and the number of bytes the engine's
\ inliner may copy from there; they mean nothing apart. `xref-retarget` writes
\ both in one protection window - a cell store built out of two 32-bit pokes tore
\ each of them across two windows - and it writes the LENGTH first and the
\ ADDRESS last, with a store-release. The address is what makes the new routine
\ reachable, so everything that has to be true before a caller may enter is
\ ordered before it: the instruction bytes, the cache flush the window already
\ did, the relocation bits, and the length. A reader that acquires the start
\ address sees the length that belongs to it. Retargeting start-first would leave
\ a window in which callers reach new code carrying the old routine's length,
\ which is the span the inliner copies into them.
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
require src/compiler/native/branch.f
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

\ ---- the three primitives that write -----------------------------------------
\ All three are code injection, so the checker admits them only through a trusted
\ boundary; each is wrapped exactly once here and none of these words chooses
\ anything. Every address and every value they are handed is computed by the
\ checked words below, and each of them runs only after the whole publication has
\ been validated.
\
\ The bulk window: one flip to writable, the whole emission copied, one flip
\ back, the call map cleared over the span, the code pointer advanced past it,
\ and one flush of exactly that range.
TRUSTED: CODE-WINDOW ( ptr u8 n n -- )
   code-publish ;

\ One call site recorded as reaching outside the region. The window above has
\ already cleared every bit of the span, so this only ever sets.
TRUSTED: RELOC-EXTERNAL ( n -- )
   callmap-set ;

\ The record's two cells, written whole and in one order: length, then the start
\ address with a store-release.
TRUSTED: RETARGET-REC ( n n n -- )
   xref-retarget ;

\ The certified minimum input arity, OR'd into the flags of the record the count
\ has just published. The engine's own publish tail pokes the same byte into the
\ same field (src/habu/habu2.f EM-REC-WIDE-PUBLISH); the primitive carries no
\ checker axiom, so it is admitted here on the same terms as the three writers
\ above - wrapped exactly once, choosing nothing, handed values the checked words
\ below computed.
TRUSTED: MIN-IN-REC ( n n -- )
   min-in-mark ;

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

\ The log's two refusals, asked in the validation phase. They used to be asked by
\ the append itself, which runs after the code and the record have already
\ moved - an overlong name really did return E-NPUB-CAP with the word already
\ pointing at its new routine. So the questions are asked here, where a refusal
\ still costs nothing, and the append below cannot answer anything but yes. The
\ throws inside it are the backstop and not the path, which is the shape
\ src/compiler/native/clobber.f keeps for the same reason.
: LOG-CK ( n -- ) {: u:n :}
   LOG-N @ LOG-MAX >= if E-NPUB-CAP throw then
   u NAME-MAX > if E-NPUB-CAP throw then ;

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

\ ---- the slot is above every slot this seam has already claimed ---------------
\ Where the routine published last ends. Everything keyed to a code address is
\ keyed on the strength of this line never being crossed downwards without a
\ reclamation saying so, so the line is kept rather than assumed: CLAIMED is
\ raised by each publication and lowered only by the reclamation notice below.
variable CLAIMED
0 CLAIMED !

: SLOT-CK ( n -- ) {: fn:n :}
   fn CLAIMED @ < if E-NPUB-SLOT throw then ;

: CLAIM ( n n -- ) {: fn:n size:n :}
   fn size + CLAIMED ! ;

\ The floor of a code reclamation. The bytes above it are the engine's again, so
\ the slots this seam claimed up there are claimable again too - and a routine
\ that starts below the floor is untouched, which is why this lowers the line to
\ the floor rather than to nothing.
: RECLAIMED ( n -- ) {: floor:n :}
   floor CLAIMED @ < if floor CLAIMED ! then ;

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
\
\ WHAT A BRANCH SAYS IS NOT DECIDED HERE. Reading the form and computing where
\ one goes is src/compiler/native/branch.f, which is also what the redirection
\ seam moves a call site with and what the workload scan counts call sites with.
\ Three copies of one signed displacement is three chances for one of them to
\ drift while the others stay right.
: INSN-ADDR ( n n n -- n ) {: fn:n size:n k:n :}
   k A64EMIT:MAP-OFFSET@ size OFFSET-CK fn + ;

: TARGET-CK ( n n n n -- ) {: t:n fn:n g:n f:n :}
   t fn = if exit then
   t A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N
   g invert and 0<> if E-NPUB-CLOBBER throw then
   t A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N
   f invert and 0<> if E-NPUB-CLOBBER throw then ;

\ ---- and a branch that LEAVES the routine is a call site too -------------------
\ A routine can end by BRANCHING to another word rather than calling it, so that
\ the callee's own return goes to this routine's caller. What the two forms do to
\ the register file is the same thing: the callee runs, and every register it
\ destroys is destroyed under this routine's name. A reader that looked only for
\ the branch-with-link would record a routine as destroying less than it does,
\ and a caller of it would keep a value in a register the tail callee writes -
\ silently, because nothing downstream ever asks again.
\
\ WHICH BRANCHES THOSE ARE IS DECIDED BY WHERE THEY GO, NOT BY THEIR OPCODE. A
\ routine's own control flow is unconditional branches too - a loop's back edge,
\ a block that is not laid out next - and every one of those lands inside the
\ span about to be written. A branch to an address outside that span is the only
\ kind that leaves, and it is exactly the kind whose callee has to be accounted
\ for. The span is [fn, fn+size), which this seam is the one authority on.
: LEAVES? ( n n n -- bool ) {: fn:n size:n t:n :}
   t fn < if true exit then
   t fn size + >= ;

: BRANCH-TARGET-CK ( n n n n n -- ) {: fn:n size:n at:n g:n f:n :}
   at A64EMIT:WORD@ {: w:n :}
   w NBR:BL? if
      fn size at INSN-ADDR  w  NBR:BL-TARGET  fn g f TARGET-CK exit
   then
   w NBR:B? 0= if exit then
   fn size at INSN-ADDR  w  NBR:B-TARGET {: t:n :}
   fn size t LEAVES? 0= if exit then
   t fn g f TARGET-CK ;

: BRANCH-CK ( n n -- ) {: fn:n size:n :}
   A64EMIT:GPR-CLOBBER A64EFF:GPRS-N {: g:n :}
   A64EMIT:FPR-CLOBBER A64EFF:FPRS-N {: f:n :}
   A64EMIT:INSNS 0 ?do
      fn size i g f BRANCH-TARGET-CK
   loop ;

\ ---- the source map describes the bytes the window will copy ------------------
\ A bulk copy takes the emitter's buffer as it stands, so "instruction i lives at
\ offset 4i" stops being something a per-instruction writer can arrange and
\ becomes something the publication has to prove. Every row is held against its
\ own position here, in the validation phase, so a map that lost a row, gained
\ one, or reordered two is a refusal rather than a permuted routine. The bounds
\ and alignment questions OFFSET-CK asks are asked of the same row on the way
\ past, so a map row is admitted by one word rather than by two that could
\ disagree.
: MAP-CK ( n -- ) {: size:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:MAP-OFFSET@ size OFFSET-CK  i INSN-BYTES * <> if
         E-NPUB-OFFSET throw
      then
   loop ;

\ ---- the relocation record of every call the emission carries -----------------
\ Which branches reach outside the region, decided the way BRANCH-CK decides
\ where a branch goes: through src/compiler/native/branch.f, the one reader of
\ that displacement. A target inside the region keeps its distance wherever the
\ region is mapped and needs no record; one outside does not, and is the only
\ kind a snapshot rewrites.
: EXTERNAL? ( n -- bool ) {: t:n :}
   t dbase@ < if true exit then
   t dbase@ REGION + >= ;

\ A TAIL BRANCH OUT OF THE ROUTINE IS A CALL SITE HERE TOO, and it is the one
\ this seam cannot record. The engine's loader walks the recorded sites and
\ refuses an image in which one of them does not hold a branch-with-link
\ (src/habu/habu2.f EMIT-CALLS, which compares the top six bits against BL-OP-HI
\ and exits CALLMAP-RC otherwise), so a `b` recorded there would turn a restored
\ snapshot into a refusal rather than into a relocated branch. Widening that
\ comparison is a change to the engine's emitted relocation pass and to the model
\ formal/Common/Reloc.v holds it against: dot habu-relocate-a-tail-96d571af.
\
\ SO THE PUBLICATION REFUSES ONE INSTEAD, and refuses it in the validation phase
\ where a refusal still costs nothing. A tail branch that stays inside the region
\ needs no record at all - it keeps its distance wherever the region is mapped,
\ for the same reason an internal call does - and that is every tail branch the
\ chain builds today, because a chain-published callee is in the region. One to
\ the engine's own loaded text is the case that has no record, and it is named
\ here rather than published as a branch a restored image would follow into
\ whatever now sits at that address.
: TAIL-RELOC-CK ( n n -- ) {: fn:n size:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ {: w:n :}
      w NBR:B? if
         fn size i INSN-ADDR  w  NBR:B-TARGET {: t:n :}
         fn size t LEAVES? t EXTERNAL? and if E-NPUB-RELOC throw then
      then
   loop ;

\ Set the bit for every call site of the routine just published. It runs after
\ the window, which cleared the whole span, so a site that needs no record is
\ already right and nothing here has to clear one.
: RELOC-CALLS ( n -- ) {: fn:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ NBR:BL? if
         fn i INSN-BYTES * +  i A64EMIT:WORD@  NBR:BL-TARGET EXTERNAL? if
            fn i INSN-BYTES * + RELOC-EXTERNAL
         then
      then
   loop ;

\ ---- how much of the routine a caller may copy --------------------------------
\ The engine stores a word's code length EXCLUDING its trailing return
\ (src/habu/habu2.f EM-COMPILE-FLUSH-PEND writes `CP - entry - 4`), because that
\ is the span its inliner copies into a caller and copying the return as well
\ would return from the caller. So the subtraction is this seam's own
\ responsibility - and it is a subtraction of the RETURN, not of an instruction.
\
\ A ROUTINE THAT LEAVES BY BRANCHING HAS NO RETURN TO LEAVE OUT, so its recorded
\ length is the whole emission. Subtracting anyway would record a four-byte tail
\ branch as a word of no length at all, and every reader of a routine's extent -
\ the engine's inliner, the workload scan, the byte column of the codegen
\ comparison - would be reading a span that is not the routine.
\
\ AND THE ENGINE'S INLINER STILL DECLINES IT, which is what makes the whole
\ length safe to record. Whichever span C-CALL takes, the tail branch or the call
\ that made the routine reserve a frame is inside it: a routine with no prologue
\ gives up the whole of [entry, entry+len), which ends on the branch; one whose
\ first word is the prologue gives up [entry+8, entry+len-8), and a routine only
\ reserves a frame because it makes a call it comes back from, which stands
\ strictly between the link save and the link restore. C-CALL-SCAN-SAFE refuses a
\ span containing either, so the copy falls back to the branch it always could.
: RECORDED-LEN ( n -- n ) {: size:n :}
   A64EMIT:LEAVES-BY-BRANCH? if size exit then
   size INSN-BYTES - ;

public

\ Make the sealed emission the code of the word this tail names in this
\ wordlist. Global words are wordlist zero; a package word's wordlist is the one
\ its own record carries.
\
\ THE VALIDATION PHASE IS EVERY REFUSAL THIS PUBLICATION CAN MAKE, and it ends
\ before the first byte moves. Nothing below the line can throw: the window, the
\ relocation bits, the record retarget and the three bookkeeping steps are all
\ writes whose preconditions have just been proved. So a refusal leaves the code
\ arena, the call map, the dictionary record, the clobber table and the log
\ exactly as it found them, and the word keeps running the code it was running.
\ Everything the validation asks about the EMISSION and the slot it is going
\ into - which is every question that does not depend on how the record was
\ reached. A republication reaches its record by name; a held publication is
\ handed the record the engine withheld. Both then have exactly this to prove.
: VALIDATE-EMISSION ( n -- n ) {: size:n :}
   size ROOM-CK {: fn:n :}
   fn PLACE-CK
   fn SLOT-CK
   fn  A64EMIT:GPR-CLOBBER  A64EMIT:FPR-CLOBBER  NCLOB:RECORD-CK
   fn size BRANCH-CK
   fn size TAIL-RELOC-CK
   fn ;

: VALIDATE ( ptr u8 n n n -- n n ) {: a:ptr u:n wid:n size:n :}
   size MAP-CK
   a u wid TARGET {: idx:n :}
   size VALIDATE-EMISSION {: fn:n :}
   u LOG-CK
   idx fn ;

\ THE COMMIT PHASE, IN THE ONE ORDER THAT IS CORRECT. The bytes go in first and
\ the window flushes them; then the relocation record of each call it wrote;
\ then, LAST, the record cell that makes the routine reachable at all, written
\ with a release so everything above it is visible to whoever acquires it. The
\ clobber row, the claimed-slot line and the log follow, none of which any caller
\ can reach before the retarget.
: COMMIT ( n n n -- ) {: idx:n fn:n size:n :}
   A64EMIT:BYTES fn size CODE-WINDOW
   fn RELOC-CALLS
   fn  size RECORDED-LEN  idx RETARGET-REC
   fn  A64EMIT:GPR-CLOBBER  A64EMIT:FPR-CLOBBER  NCLOB:RECORD
   fn size CLAIM ;

: REPUBLISH ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   SIZE-CK {: size:n :}
   a u wid size VALIDATE {: idx:n fn:n :}
   idx XREF-REC XREF-START {: os:n :}
   idx XREF-REC XREF-LEN {: ol:n :}
   idx fn size COMMIT
   a u wid os ol fn  size RECORDED-LEN  LOG+ ;

\ ---- committing a publication the engine withheld ----------------------------
\ WHAT A HELD RECORD IS. When the engine certifies a definition whose tape the
\ chain is recording, it publishes nothing: the count does not move, the name
\ never enters the index, and the checker's facts are not poked into the record
\ (src/habu/habu2.f EM-COMPILE-PUBLISH-HOOKED, the `held` leg). What it leaves
\ behind is a COMPLETE dictionary record - name, wordlist, flags and entry
\ address, all written by `:` - sitting at the one slot the count points at.
\ This word turns that record into a word of the running engine.
\
\ THERE IS NO CREATE PATH HERE, AND THAT IS THE DESIGN. It would be easy to read
\ "the chain publishes its own definitions" as "the chain builds its own
\ records", and it is wrong. `:` is the sole constructor of a dictionary record
\ in this system: it allocates the slot, qualifies and stores the name, sets the
\ flags and writes the entry cell (habu2.f EM-INTERPRET-COLON). A second
\ constructor in this file would have to agree with that one about every field,
\ forever, across every future change to what a record holds - and the two would
\ be edited by different people for different reasons. So the chain does not
\ build a record; it COMMITS the one the engine already built. What the engine's
\ publication tail does, and all it does, is move the count, insert the name and
\ apply the checker's facts, and those three are what this word performs.
\
\ WHICH RECORD, ASKED RATHER THAN STATED. The held record is always the slot the
\ count points at - that is what "unpublished" means - so this word takes no
\ index, no name and no wordlist. Everything it needs is read off that record,
\ for the same reason REPUBLISH takes only a name: a caller that could state
\ these could state them wrongly, and there is no second authority to prefer.
\
\ THE COUNT IS RAISED THROUGH THE ENGINE'S OWN SINK. `ndict!` is the one door
\ Habu has to the published count, and its raise leg rebuilds the name index
\ from the raised range (src/habu/habu1.f BNDSET), so the record becomes findable
\ by the same table every other word is found in. The engine's own publish tail
\ inserts one entry instead of rebuilding; that difference is a cost, not a
\ semantic gap, and it is measured rather than assumed - dot
\ habu-insert-one-held-record tracks exposing the single-record insert if the
\ rebuild proves too slow for a whole-tree cut.
\
\ AND THE TWO PHASES ARE THE SAME TWO. Every refusal happens before a byte moves;
\ the writes below cannot throw. A refused commit leaves the code arena, the call
\ map, the record, the clobber table and the log as it found them - and, because
\ nothing was published, leaves the definition exactly as unpublished as it was.
: HELD-IDX ( -- n )
   ndict@ ;

\ The held record has to be the one the engine is holding. Nothing else in the
\ dictionary is a legal target: a lower index is a word that IS published, and a
\ higher one is a slot no `:` has written.
: HELD-CK ( n -- ) {: idx:n :}
   idx HELD-IDX <> if E-NPUB-HELD throw then
   idx WORDLIST-CK
   idx FLAGS-CK ;

\ The checker's own facts about the definition, applied to the record the count
\ has just made newest. These are the two the engine's publish tail applies
\ after ndict++ (habu2.f EM-REC-WIDE-PUBLISH), read from the same two latches,
\ so a chain-published word carries the same guards as an engine-published one.
: HELD-FACTS ( n -- ) {: idx:n :}
   REC-WIDE-PUBLISH
   REC-MIN-IN@ {: mi:n :}
   mi 0<> if idx mi MIN-IN-REC then ;

\ Make the sealed emission the code of the record the engine withheld, and
\ publish that record.
: COMMIT-HELD ( -- )
   SIZE-CK {: size:n :}
   size MAP-CK
   HELD-IDX {: idx:n :}
   idx HELD-CK
   idx XREF-REC XREF-NAME$ {: a:ptr u:n :}
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   size VALIDATE-EMISSION {: fn:n :}
   u LOG-CK
   idx fn size COMMIT
   ndict@ 1+ ndict!
   idx HELD-FACTS
   a u wid 0 0 fn  size RECORDED-LEN  LOG+ ;

\ The address the next republication will write its first instruction at. It is
\ the engine's own free code slot, which is what REPUBLISH claims, so a caller
\ that has to know where a routine will land - because its branches are measured
\ from there - asks the one file that decides it. Asking is free and changes
\ nothing: a caller that asks and never publishes has moved no pointer.
: NEXT-SLOT ( -- n )
   cp@ ;

\ Is this address one a branch from a published routine keeps its distance to,
\ wherever the region is mapped? Everything inside the region moves with it, so
\ a branch that stays inside needs no relocation record and survives a snapshot
\ restore untouched; one that leaves does not, and this seam cannot record a
\ tail branch (TAIL-RELOC-CK above says why), so a caller deciding whether it may
\ leave through a callee at all asks here BEFORE it commits to the shape. The
\ refusal above stays as the fail-closed backstop: this is the question, that is
\ the guarantee.
: IN-REGION? ( n -- bool )
   EXTERNAL? 0= ;

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

\ Hear about every reclamation of code space, for as long as this file is
\ loaded, so that the line a claimed slot is held against follows the pointer
\ down when the space above it is really given back.
: WATCH-INSTALL ( -- )
   [: RECLAIMED ;] CODE-RECLAIM:WATCH ;

WATCH-INSTALL

get-current prot-wid-add

public
get-current prot-wid-add

;package
