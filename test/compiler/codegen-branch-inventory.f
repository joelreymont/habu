\ codegen-branch-inventory.f - counting the branch chains in emitted code. One
\ concern: tools/codegen-branch-inventory.f, and the four ways a tool that reads
\ branches off a routine's bytes can answer confidently and wrongly.
\
\ WHY THE TOOL HAS TO BE RIGHT BEFORE THE TRANSFORM IS WRITTEN. The chain count
\ is what the collapse lane is measured by: it is the structural evidence a byte
\ count cannot give, because bytes falling says a routine got smaller and only
\ chains falling to zero says THIS transform is what made it smaller. A miscount
\ would therefore not merely be wrong, it would be the thing the lane cites as
\ proof. So the four mistakes below are each pinned by a fixture built to make
\ them, and each fixture asserts its own precondition - that it really is the
\ hazardous shape - so that a fixture which quietly stopped being dangerous
\ fails here instead of passing vacuously.
\
\   1. READING A CALL AS A BRANCH. `b` and `bl` differ in ONE BIT of the opcode
\      and share every other field, so a mask that lost that bit would count
\      every call as an unconditional branch and every called routine as the
\      middle of a chain. CODEGEN-CORPUS:FACT-N is the row that pins it, and it
\      is the ONLY one that can: the chain copies every callee in these corpora
\      into its caller, so the call rows emit no call instruction at all and a
\      recursion is the one call that survives. FACT-N holds exactly one call and
\      no branch at all, so a mask that had lost the bit would report one hop
\      where the suite demands none.
\
\   2. READING PAST THE END OF A ROUTINE THAT LEAVES BY A BRANCH. A word's
\      recorded length EXCLUDES its trailing return, so a walk over the routine
\      normally adds one instruction back. A routine that leaves by a tail branch
\      has no trailing return to add, and adding one anyway reads the first word
\      of whatever was published next and reports it as this routine's. This is
\      the mistake this tool actually made: SPAN added one unconditionally.
\      CODEGEN-CORPUS5:TAIL-CHAIN-N is ONE instruction long and that instruction
\      is the leaving branch, so the bad version read exactly one word too many.
\      The fixture asserts both preconditions - it leaves by a branch, and it is
\      one instruction - before asking for the counts.
\
\   3. FOLLOWING A BRANCH OUT OF THE ROUTINE. A branch that leaves is a call site
\      to src/compiler/native/publish.f, not control flow, so following one would
\      count a call as a chain and could read a target in another routine
\      entirely. The same tail row pins it: its branch leaves, so it is not a hop.
\
\   4. CALLING A CONDITIONAL TARGET A CHAIN. A retarget is sound only when the
\      instruction landed on is an UNCONDITIONAL branch, because arriving at a
\      block that decides something is not the same as arriving at either of its
\      successors. CODEGEN-CORPUS4:LADDER-N is eight conditionals whose arms all
\      reach one join; it holds six branches and no chain at all.
\
\ AND ONE GAP THIS TOOL FOUND HAS SINCE CLOSED. CODEGEN-CORPUS:FACT-N used to
\ branch over a block that emitted NOTHING: the fall-through rule asks about
\ POSITIONS, and a zero-length block laid between two blocks leaves them adjacent
\ in bytes but two positions apart, so the branch survived. The collapse removed
\ the cause rather than the symptom - a block that emits nothing before its
\ terminator is exactly a block control passes through, and there is no
\ zero-length position left to jump over. The count is asserted at zero on the
\ row that had it, so reintroducing such a position fails here.
\
\ THE FIXTURES ARE THE CORPUS ROWS, AND THAT IS DELIBERATE. Every count this tool
\ makes is about the layout the native CHAIN chooses, and the same bodies
\ compiled by the engine's emitter do not reproduce it - so the subjects have to
\ be the migrated `-N` rows. Those rows already exist, their bytes are pinned by
\ test/compiler/codegen-chain-baseline*.txt, and each shape this suite needs is
\ among them; a private copy of one would be a second fixture to keep in step
\ with the first for no gain. tools/codegen-loop-inventory.f's suite names its
\ subjects the same way.

require lib/prelude.f
require lib/test.f
require src/compiler/native/branch.f
require tools/codegen-branch-inventory.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated2.f
require tools/codegen-compare-migrated3.f
require tools/codegen-compare-migrated4.f
require tools/codegen-compare-migrated5.f

package NBI-TEST

using NBRINV
using NTAILPROBE

public

: FLOOR-CASES ( -- )
   s" a routine with no branch holds no branch, no chain and nothing idle"
   T-LABEL
   s" CODEGEN-CORPUS:ADD3-N" HOPS 0 T=
   s" CODEGEN-CORPUS:ADD3-N" CHAINS 0 T=
   s" CODEGEN-CORPUS:ADD3-N" IDLE 0 T= ;

\ THE THREE ROWS THE COLLAPSE WAS BUILT FOR, AND WHY THEY NOW READ ZERO. Each
\ held a branch to a branch before src/compiler/native/emit.f began redirecting
\ them: a counted loop left one, an early exit out of a loop left two, and so did
\ a loop storing through a pointer. Their branch counts fell with their chain
\ counts, which is the half a byte column cannot show.
\
\ AND ZERO IS NOW STRUCTURAL RATHER THAN LUCKY, which is worth stating because it
\ is what makes these assertions weak on their own. A block whose FIRST
\ instruction is a branch has nothing before its terminator, so it emits nothing
\ before it, so it is exactly the block the collapse redirects every branch past
\ and drops. A chain therefore cannot survive this emitter at all, and no row of
\ any corpus can hold one - which also means this suite can no longer show the
\ counter answering anything but zero. Dot habu-hand-built-fixture-a6a4efe7 pins the
\ non-zero path with hand-built code instead; until it lands, the counter's
\ ability to count is carried by the branch column below and by the mutations
\ recorded in this file's header.
: CHAIN-CASES ( -- )
   s" a counted loop keeps its back edge and loses its trampoline" T-LABEL
   s" CODEGEN-CORPUS:SUM-TO-N" HOPS 1 T=
   s" CODEGEN-CORPUS:SUM-TO-N" CHAINS 0 T=

   s" an early exit out of a loop loses two of them" T-LABEL
   s" CODEGEN-CORPUS:BYTE-FIND-N" HOPS 1 T=
   s" CODEGEN-CORPUS:BYTE-FIND-N" CHAINS 0 T=

   s" and a loop that stores through a pointer loses every branch it had"
   T-LABEL
   s" CODEGEN-CORPUS4:STORE-LOAD-N" HOPS 0 T=
   s" CODEGEN-CORPUS4:STORE-LOAD-N" CHAINS 0 T= ;

\ Mistake 1. The preconditions are asserted first - this row really does hold a
\ call, and really does hold a branch beside it - so the mask is being asked the
\ question it can get wrong under both answers at once.
: CALL-CASES ( -- )
   s" a call is not a branch, however alike the two encodings are" T-LABEL
   s" CODEGEN-CORPUS:FACT-N" CALLS 1 T=
   s" CODEGEN-CORPUS:FACT-N" HOPS 0 T=
   s" CODEGEN-CORPUS:FACT-N" CHAINS 0 T=

   s" and a row whose callees were all copied holds neither" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-N" CALLS 0 T=
   s" CODEGEN-CORPUS4:CALL-FAN-N" HOPS 0 T= ;

\ Mistakes 2 and 3. Both preconditions are asserted: it leaves by a branch, and
\ it is one instruction long - so a walk that added a trailing return would read
\ a word that is not this routine's at all.
: TAIL-CASES ( -- )
   s" a routine that leaves by a branch has no trailing return to walk into"
   T-LABEL
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" TAIL-BRANCH? TTRUE
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" INSNS 1 T=
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" CODE-BYTES NBR:INSN-BYTES T=

   s" so the tool walks exactly its one instruction and not the next word"
   T-LABEL
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" EXTENT 1 T=

   s" while a routine that does end in a return has that return walked" T-LABEL
   s" CODEGEN-CORPUS:ADD3-N" TAIL-BRANCH? TFALSE
   s" CODEGEN-CORPUS:ADD3-N" EXTENT
      s" CODEGEN-CORPUS:ADD3-N" INSNS 1+ T=

   s" and its branch leaves, so it is neither a hop nor a chain" T-LABEL
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" HOPS 0 T=
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" CHAINS 0 T=
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" IDLE 0 T= ;

\ Mistake 4. The precondition is the point: the ladder holds branches in
\ quantity, so a tool that called any branched target a chain would say so here.
: CONDITIONAL-CASES ( -- )
   s" a branch to something that is not a branch is not a chain" T-LABEL
   s" CODEGEN-CORPUS4:LADDER-N" HOPS 6 T=
   s" CODEGEN-CORPUS4:LADDER-N" CHAINS 0 T= ;

\ THE GAP THAT CLOSED ITSELF. This row used to branch over a block that emitted
\ nothing, because the fall-through rule asks about POSITIONS and a zero-length
\ block laid between two blocks leaves them adjacent in bytes but not in
\ positions. The collapse removes the cause rather than the symptom: a block that
\ emits nothing before its terminator is precisely a block control passes
\ through, so it is now branched past and dropped, and there is no zero-length
\ position left for a branch to jump over. The count is asserted at zero on the
\ row that had it, so that reintroducing such a position fails here.
: IDLE-CASES ( -- )
   s" no branch jumps over a block that occupies no bytes" T-LABEL
   s" CODEGEN-CORPUS:FACT-N" IDLE 0 T=
   s" CODEGEN-CORPUS:FACT-N" CHAINS 0 T= ;

: CASES ( -- )
   FLOOR-CASES
   CHAIN-CASES
   CALL-CASES
   TAIL-CASES
   CONDITIONAL-CASES
   IDLE-CASES ;

;using

;package

T-RESET
NBI-TEST:CASES
T-REPORT
