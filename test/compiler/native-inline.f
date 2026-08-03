\ native-inline.f - the body of a small routine, recorded when it is published
\ and copied into every later caller instead of being called. One concern:
\ src/compiler/native/inline.f and the splice src/compiler/native/elaborate.f
\ makes out of it.
\
\ WHAT THIS SUITE HAS TO SHOW. Eight things, and the last six are the ones a
\ change to the rule would break.
\
\   1. That the record answers about an ADDRESS, keeps the tokens and the arity
\      it was told, refuses a second body for one address, and refuses to be read
\      about an address it has no row for or a token a row does not hold.
\   2. That the size rule is DERIVED and not chosen: what one side of a call to a
\      routine of a given arity costs in instructions, and a routine admitted
\      exactly when its whole emission is within twice that.
\   3. That a real migration records a body, and that a caller compiled
\      afterwards contains NO call instruction at all, no frame and no saved link
\      register - and answers exactly what the same body compiled by the engine
\      answers, on inputs including a negative one and one that overflows.
\   4. That the rule BITES, one instruction either side of it. Two callees of the
\      same shape are migrated whose emissions differ by a single instruction:
\      the smaller is copied and the larger is called. Nothing else about the two
\      callers differs, so a change that widened or narrowed the rule moves
\      exactly one of these two counts.
\   5. That the refusals are refusals about the BODY and not about its size,
\      which is checked by asking the size rule about the same routine and
\      getting a yes: a callee with a control structure, a callee that calls
\      itself, and a callee that calls a word NOTHING RECORDED are all small
\      enough to copy and none of them is recorded.
\   6. That a copied body brings its own memory order and its own trap with it. A
\      caller with no memory word of its own compiles and answers correctly when
\      the body it copies has one, and a caller that copies a division carries
\      the division's zero-divisor guard in its own instructions.
\   7. That a copied body is handed its arguments as the CELLS its own routine
\      took them as, so a caller holding a computed double where the record has
\      a cell store compiles, and the eight bytes that reach memory are the
\      double's own.
\   8. That a routine whose OWN calls were copied is itself recorded, as the
\      tokens that replaced them: the row is flat, the literals inside it survive
\      each further copy to the bit, the chain stops at whichever ceiling it
\      reaches first - the size rule or the row's capacity - and a routine
\      refused at either ceiling is CALLED by its callers and still answers
\      correctly.
\
\ WHY THE COUNTS ARE OF INSTRUCTIONS AND NOT OF BYTES. A byte count moves for any
\ reason at all. What copying a body removes is exactly the branch-with-link, the
\ data-stack traffic of the interface, and the frame a calling routine reserves -
\ so those three are what is counted, by decoding the published word's own
\ instructions. A change that made the code smaller some other way would not move
\ these numbers, and a change that stopped copying would move all three.

require lib/test.f
require src/compiler/native/migrate.f
require src/compiler/native/inline.f

package NINL-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite compiles and runs words that did not exist when the suite was
\ compiled.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

8 constant REGS                      \ scratch registers the migrated routines may use
0 constant GLOBAL-WID
4 constant INSN-BYTES

: FLAG# ( bool -- n )
   if 1 else 0 then ;

\ ---- addresses no code occupies ----------------------------------------------
\ The cases that are about the TABLE use addresses of their own rather than
\ borrowing a published routine's: a case that recorded a second body over a real
\ publication would be changing what a real caller is compiled against. They are
\ instruction aligned because every address this record is ever handed is.
$20000 constant A1
$20004 constant A2
$20008 constant A3

\ ---- the table, on its own -----------------------------------------------------
: TABLE-CASES ( -- )
   s" an address with no row is unknown and cannot be read" T-LABEL
   A3 NINL:KNOWN? FLAG# 0 T=
   [: A3 NINL:TOKENS drop ;] E-NINL-BOUND TTHROWSQ
   [: A3 NINL:IN@ drop ;] E-NINL-BOUND TTHROWSQ

   s" a staged body keeps its tokens, its kinds and its arity" T-LABEL
   1 2 NINL:STAGE-BEGIN
   7 NINL:STAGE-INT
   s" +" NINL:STAGE-NAME
   NINL:STAGED-TOKENS 2 T=
   A1 NINL:COMMIT
   A1 NINL:KNOWN? FLAG# 1 T=
   A1 NINL:IN@ 1 T=
   A1 NINL:OUT@ 2 T=
   A1 NINL:TOKENS 2 T=
   A1 0 NINL:LIT@ 7 T=
   A1 1 NINL:SPELL$ s" +" STR= TTRUE

   s" and a token answers only for the kind it is" T-LABEL
   [: A1 0 NINL:SPELL$ drop drop ;] E-NINL-BOUND TTHROWSQ
   [: A1 1 NINL:LIT@ drop ;] E-NINL-BOUND TTHROWSQ
   [: A1 2 NINL:KIND@ drop ;] E-NINL-BOUND TTHROWSQ

   s" a commit consumes the staging, so a second one has nothing to key" T-LABEL
   NINL:STAGED? FLAG# 0 T=
   [: A2 NINL:COMMIT ;] E-NINL-STATE TTHROWSQ

   s" a second body for one address is refused, and leaves the first" T-LABEL
   1 1 NINL:STAGE-BEGIN
   9 NINL:STAGE-INT
   [: A1 NINL:COMMIT ;] E-NINL-DUP TTHROWSQ
   NINL:STAGE-CLEAR
   A1 NINL:TOKENS 2 T=
   A1 0 NINL:LIT@ 7 T=

   s" a second staging over a live one is refused" T-LABEL
   1 1 NINL:STAGE-BEGIN
   [: 1 1 NINL:STAGE-BEGIN ;] E-NINL-STATE TTHROWSQ
   NINL:STAGE-CLEAR

   s" and a token staged with nothing open is refused" T-LABEL
   [: 3 NINL:STAGE-INT ;] E-NINL-STATE TTHROWSQ ;

\ ---- the size rule, which is derived and not chosen ---------------------------
\ One side of a call to a routine of arity (in -> out) is its stores, its three
\ instructions of branch and pointer adjustment, and its loads; the routine pays
\ the mirror of that. So the interface is `in + out + 3` on each side, and a
\ routine is small exactly when its whole emission is within both halves - which
\ is when its BODY is no longer than the call site's own half.
: RULE-CASES ( -- )
   s" one side of a call is the arity plus the branch and the two moves" T-LABEL
   1 1 NINL:INTERFACE-INSNS 5 T=
   2 1 NINL:INTERFACE-INSNS 6 T=
   0 0 NINL:INTERFACE-INSNS 3 T=

   s" and a routine is small exactly when its emission is within twice that"
   T-LABEL
   1 1 9 NINL:SMALL? TTRUE
   1 1 10 NINL:SMALL? TTRUE
   1 1 11 NINL:SMALL? TFALSE
   3 2 16 NINL:SMALL? TTRUE
   3 2 17 NINL:SMALL? TFALSE

   s" a row holds what the rule admits, and says so before anything is staged"
   T-LABEL
   1 NINL:FITS? TTRUE
   16 NINL:FITS? TTRUE
   17 NINL:FITS? TFALSE
   1 NINL:SPELL-FITS? TTRUE
   0 NINL:SPELL-FITS? TFALSE
   64 NINL:SPELL-FITS? TFALSE ;

\ ---- reading a live word's own machine code ----------------------------------
\ The code start and the code length come off the word's own dictionary record,
\ which is where the publication seam wrote them.
variable CODE-AT

: CODE-PTR ( -- ptr u8 )
   CODE-AT 0 ptr-field @ ;

: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

: WORD-REC ( ptr u8 n -- ptr a ) {: a:ptr u:n :}
   a u GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if drop E-NPUB-NAME throw then ;

: ENTRY-OF ( ptr u8 n -- n )
   WORD-REC XREF-START ;

\ How many instructions the published word holds. The record's length excludes
\ the trailing return, which is what the engine means by a word's length, so the
\ emission is one instruction more than the record measures.
: WORD-INSNS ( ptr u8 n -- n )
   WORD-REC XREF-LEN INSN-BYTES / 1+ ;

\ How many instructions of the word's own code match one form. The mask and the
\ value are the caller's, so each case below says which instruction it is
\ counting and nothing here has an opinion about it.
: FORM-COUNT ( ptr u8 n n n -- n ) {: a:ptr u:n mask:n want:n :}
   a u WORD-REC
   dup XREF-START CODE-AT !
   XREF-LEN {: len:n :}
   0
   len INSN-BYTES / 0 ?do
      CODE-PTR i INSN-BYTES * + U32@ mask and want = if 1+ then
   loop ;

\ Branch with link: the top six bits are 100101. It is the same encoding
\ src/habu/habu2.f names `$94000000 constant C-CALL-BL-IMM`.
: BL-COUNT ( ptr u8 n -- n )
   $FC000000 $94000000 FORM-COUNT ;

\ `sub sp, sp, #16`, which is the whole of the frame a calling routine reserves:
\ one slot for the caller's return address. A routine with no call in it reserves
\ none, so this count is how the contract shows up in the code.
: FRAME-COUNT ( ptr u8 n -- n )
   $FFFFFFFF $D10043FF FORM-COUNT ;

\ `brk #imm`, which is the trap the division form branches over when the divisor
\ is not zero - the same three instructions the engine's own `/` is.
: BRK-COUNT ( ptr u8 n -- n )
   $FFE0001F $D4200000 FORM-COUNT ;

\ ---- a body really copied, end to end ------------------------------------------
\ How many bodies the record held before the four callees below were migrated, so
\ that "it recorded these" is a count that moved rather than rows that were there.
variable ROWS-BEFORE

\ The callee is a doubling written five times. Its body is five additions and
\ nothing else, so its emission is the interface plus five - exactly twice the
\ interface, which is the largest a copied body may be.
: MIGRATE-SMALL ( -- )
   s" : NINL-EDGE-IN ( n -- n ) dup + dup + dup + dup + dup + ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ The same shape with ONE addition more, which is one instruction past the rule.
: MIGRATE-LARGE ( -- )
   s" : NINL-EDGE-OUT ( n -- n ) dup + dup + dup + dup + dup + dup + ;"
   1 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-COPIES ( -- )
   s" NINL-EDGE-IN" s" NINL-EDGE-IN" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-COPIES ( n -- n ) NINL-EDGE-IN ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-CALLS ( -- )
   s" NINL-EDGE-OUT" s" NINL-EDGE-OUT" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-CALLS ( n -- n ) NINL-EDGE-OUT ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ A third callee, whose body carries LITERALS. The two above are additions of a
\ value to itself, so a copy that lost or altered a literal would answer the same
\ as one that did not; this one answers differently for every literal in it.
: MIGRATE-LIT ( -- )
   s" : NINL-LIT ( n -- n ) 3 * 7 + ;" 1 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-USE-LIT ( -- )
   s" NINL-LIT" s" NINL-LIT" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-USE-LIT ( n -- n ) NINL-LIT ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ The same three bodies as the engine compiles them, so that what the copied
\ caller answers is held against a second compiler and not only against
\ arithmetic written down here.
: DEFINE-ENGINE-TWINS ( -- )
   s" : NINL-ENGINE-COPIES ( n -- n ) dup + dup + dup + dup + dup + ;" EV
   s" : NINL-ENGINE-CALLS ( n -- n ) dup + dup + dup + dup + dup + dup + ;" EV
   s" : NINL-ENGINE-LIT ( n -- n ) 3 * 7 + ;" EV ;

: EDGE-CASES ( -- )
   s" six routines were migrated and the record grew by the four that qualify"
   T-LABEL
   NINL:ROWS ROWS-BEFORE @ 4 + T=

   s" a small routine's body is recorded when it is published" T-LABEL
   s" NINL-EDGE-IN" ENTRY-OF NINL:KNOWN? FLAG# 1 T=
   s" NINL-EDGE-IN" ENTRY-OF NINL:IN@ 1 T=
   s" NINL-EDGE-IN" ENTRY-OF NINL:OUT@ 1 T=
   s" NINL-EDGE-IN" ENTRY-OF NINL:TOKENS 10 T=

   s" and the one instruction larger routine's body is not" T-LABEL
   s" NINL-EDGE-OUT" ENTRY-OF NINL:KNOWN? FLAG# 0 T=

   s" which is exactly where the rule puts them" T-LABEL
   s" NINL-EDGE-IN" WORD-INSNS 10 T=
   s" NINL-EDGE-OUT" WORD-INSNS 11 T=
   1 1 s" NINL-EDGE-IN" WORD-INSNS NINL:SMALL? TTRUE
   1 1 s" NINL-EDGE-OUT" WORD-INSNS NINL:SMALL? TFALSE

   s" the caller of the recorded one contains no call at all" T-LABEL
   s" NINL-COPIES" BL-COUNT 0 T=

   s" and reserves no frame and saves no return address" T-LABEL
   s" NINL-COPIES" FRAME-COUNT 0 T=

   s" while the caller of the other calls it, with a frame to call from" T-LABEL
   s" NINL-CALLS" BL-COUNT 1 T=
   s" NINL-CALLS" FRAME-COUNT 1 T=

   s" and both answer what the engine's code for the same body answers" T-LABEL
   s" 3 NINL-COPIES" EV-N  s" 3 NINL-ENGINE-COPIES" EV-N T=
   s" 0 NINL-COPIES" EV-N  s" 0 NINL-ENGINE-COPIES" EV-N T=
   s" -7 NINL-COPIES" EV-N  s" -7 NINL-ENGINE-COPIES" EV-N T=
   s" 3 NINL-CALLS" EV-N  s" 3 NINL-ENGINE-CALLS" EV-N T=
   s" -7 NINL-CALLS" EV-N  s" -7 NINL-ENGINE-CALLS" EV-N T=

   s" including the input whose doublings run off the top of a cell" T-LABEL
   s" 1152921504606846977 NINL-COPIES" EV-N
   s" 1152921504606846977 NINL-ENGINE-COPIES" EV-N T=

   s" and the copied answer is the arithmetic itself, not a call that vanished"
   T-LABEL
   s" 3 NINL-COPIES" EV-N 96 T=
   s" 3 NINL-CALLS" EV-N 192 T=

   s" a copied body carries its literals, not just its shape" T-LABEL
   s" NINL-USE-LIT" BL-COUNT 0 T=
   s" 5 NINL-USE-LIT" EV-N  s" 5 NINL-ENGINE-LIT" EV-N T=
   s" 0 NINL-USE-LIT" EV-N  s" 0 NINL-ENGINE-LIT" EV-N T=
   s" -4 NINL-USE-LIT" EV-N  s" -4 NINL-ENGINE-LIT" EV-N T=
   s" 5 NINL-USE-LIT" EV-N 22 T=

   s" the callee is still a word of its own and still runs" T-LABEL
   s" 3 NINL-EDGE-IN" EV-N 96 T=
   s" NINL-EDGE-IN" GLOBAL-WID NPUB:REPUBLISHED? TTRUE ;

\ ---- what is refused about a BODY rather than about its size -------------------
\ The first of these is small enough to copy and is still not recorded, and its
\ case says so twice: the record does not know the address, and the size rule
\ asked about that same routine's own emission answers yes. So what refused it is
\ the shape of its body, which is the claim.
\
\   a control structure   a copied body is spliced into the block the call site
\                         stands in, and this elaborator counts its blocks in a
\                         walk that never saw the callee
\   a call to ITSELF      `RECURSE` is a control word, so it is refused by the
\                         same rule, and a body that could reach itself is what
\                         that rule is there to make impossible
\
\ THE THIRD REFUSAL IS THE ONE THAT KEEPS EVERY ROW FLAT, and it is stated by the
\ machine code rather than by the size rule, because a routine that really calls
\ can never be small: the call costs it one whole interface, which is half of
\ what the rule allows. What its case says instead is the thing that matters -
\ the routine contains a branch-with-link, and no row holds one, so a caller
\ copying it would be copying a branch. A call the elaboration COULD copy is not
\ this case at all: it leaves no branch behind, and the chain cases below are
\ about that.
: MIGRATE-CTRL ( -- )
   s" : NINL-CTRL ( n -- n ) dup 0 < if drop 0 then ;"
   1 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-VIA-CTRL ( -- )
   s" NINL-CTRL" s" NINL-CTRL" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-VIA-CTRL ( n -- n ) NINL-CTRL ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-CALLER-CALLEE ( -- )
   s" NINL-EDGE-IN" s" NINL-EDGE-IN" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-VIA ( n -- n ) NINL-EDGE-IN ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-SELF ( -- )
   s" : NINL-SELF ( n -- n ) dup 0 > if 1- RECURSE then ;"
   1 1 REGS NMIGRATE:DEFINE-CALL ;

: BODY-REFUSAL-CASES ( -- )
   s" a callee with a control structure is not recorded" T-LABEL
   s" NINL-CTRL" ENTRY-OF NINL:KNOWN? FLAG# 0 T=
   1 1 s" NINL-CTRL" WORD-INSNS NINL:SMALL? TTRUE

   s" and neither is one that calls itself" T-LABEL
   s" NINL-SELF" ENTRY-OF NINL:KNOWN? FLAG# 0 T=

   s" a routine that really CALLS is not recorded, and both of these do" T-LABEL
   s" NINL-VIA-CTRL" ENTRY-OF NINL:KNOWN? FLAG# 0 T=
   s" NINL-VIA-CTRL" BL-COUNT 1 T=
   s" NINL-CALLS" ENTRY-OF NINL:KNOWN? FLAG# 0 T=
   s" NINL-CALLS" BL-COUNT 1 T=

   s" while the caller whose call WAS copied is a body to copy in its turn"
   T-LABEL
   s" NINL-COPIES" ENTRY-OF NINL:KNOWN? FLAG# 1 T=
   s" NINL-VIA" ENTRY-OF NINL:KNOWN? FLAG# 1 T=

   s" and its row is the callee's tokens, with no call left anywhere in it"
   T-LABEL
   s" NINL-VIA" ENTRY-OF NINL:TOKENS 10 T=
   s" NINL-VIA" ENTRY-OF 0 NINL:SPELL$ s" dup" STR= TTRUE
   s" NINL-VIA" ENTRY-OF 1 NINL:SPELL$ s" +" STR= TTRUE
   s" NINL-VIA" ENTRY-OF 9 NINL:SPELL$ s" +" STR= TTRUE

   s" and a caller of one of them CALLS it, which is the answer to refusing it"
   T-LABEL
   s" NINL-VIA-CTRL" BL-COUNT 1 T=
   s" NINL-VIA-CTRL" FRAME-COUNT 1 T=
   s" -6 NINL-VIA-CTRL" EV-N 0 T=
   s" 6 NINL-VIA-CTRL" EV-N 6 T=

   s" and every one of them still runs" T-LABEL
   s" 4 NINL-VIA" EV-N 128 T=
   s" 3 NINL-SELF" EV-N 0 T=
   s" 4 NINL-CTRL" EV-N 4 T=
   s" -4 NINL-CTRL" EV-N 0 T= ;

\ ---- what a copied body brings with it -----------------------------------------
\ A copied body's loads and stores thread the CALLER's memory order, because
\ there is one order per definition and the copy is part of this one. The caller
\ below has no memory word written in it at all, so if the pre-scan that decides
\ whether a definition needs an order answered about the CALL instead of about
\ the body that replaces it, this migration would be refused by name.
: MIGRATE-LOAD ( -- )
   s" : NINL-LOAD ( ptr n -- n ) @ ;" 1 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-USE-LOAD ( -- )
   s" NINL-LOAD" s" NINL-LOAD" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-USE-LOAD ( ptr n -- n ) NINL-LOAD 1 + ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

\ A division survives a copy as the same operation it was, and that operation is
\ the guard and the divide together - the machine form branches over a `brk` when
\ the divisor is not zero. So a caller that copied a division has the trap in its
\ own instructions, which is what makes a copied `/` fault where a called one
\ faults.
: MIGRATE-DIV ( -- )
   s" : NINL-DIV ( n n -- n ) / ;" 2 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-USE-DIV ( -- )
   s" NINL-DIV" s" NINL-DIV" ENTRY-OF 2 1 NMIGRATE:CALLEE
   s" : NINL-USE-DIV ( n n -- n ) NINL-DIV 1 + ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

\ The same division past the size rule, and a caller that therefore CALLS it.
\ It is the control the trap count needs: the guard is in the callee either way,
\ and what the two callers differ in is whether it came with the body.
: MIGRATE-DIV-BIG ( -- )
   s" : NINL-DIV-BIG ( n n -- n ) / 1 + 1 + 1 + 1 + 1 + 1 + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: MIGRATE-CALL-DIV ( -- )
   s" NINL-DIV-BIG" s" NINL-DIV-BIG" ENTRY-OF 2 1 NMIGRATE:CALLEE
   s" : NINL-CALL-DIV ( n n -- n ) NINL-DIV-BIG ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

: DEFINE-CELL ( -- )
   s" create NINL-CELL 16 allot" EV
   s" 42 NINL-CELL !" EV ;

: CARRIED-CASES ( -- )
   s" a caller with no memory word of its own copies one in and reads it" T-LABEL
   s" NINL-USE-LOAD" BL-COUNT 0 T=
   s" NINL-CELL NINL-USE-LOAD" EV-N 43 T=

   s" and the copy is the load, so a changed cell changes the answer" T-LABEL
   s" -9 NINL-CELL !" EV
   s" NINL-CELL NINL-USE-LOAD" EV-N -8 T=

   s" a copied division is still a division, and answers like one" T-LABEL
   s" NINL-USE-DIV" BL-COUNT 0 T=
   s" 20 4 NINL-USE-DIV" EV-N 6 T=
   s" -20 4 NINL-USE-DIV" EV-N -4 T=

   s" and it carries its own zero-divisor trap into its caller" T-LABEL
   s" NINL-USE-DIV" BRK-COUNT 1 T=
   s" NINL-DIV" BRK-COUNT 1 T=

   s" while a caller that really calls a division carries no trap of its own"
   T-LABEL
   s" NINL-CALL-DIV" BL-COUNT 1 T=
   s" NINL-CALL-DIV" BRK-COUNT 0 T=
   s" NINL-DIV-BIG" BRK-COUNT 1 T=
   s" 20 4 NINL-CALL-DIV" EV-N 11 T= ;

\ ---- a chain of records, and the two ceilings that end one -------------------
\ A routine whose own calls were copied is recorded as the tokens that replaced
\ them, so a record can be built out of a record. Nothing bounds that by depth;
\ what bounds it is that each step has to pass the SAME two ceilings a first-level
\ record passes, measured on what the step really produced. These two families
\ walk a chain into each ceiling in turn and show which one stopped it.
\
\ THE LITERAL CHAIN WALKS INTO THE SIZE RULE. Each link multiplies or adds a
\ constant, so each splice adds two instructions to the next routine's emission
\ and the arithmetic is different at every link - a copy that dropped a literal,
\ reordered two, or truncated one answers a different number. F-L1 is recorded;
\ F-L2 copies it and is recorded with F-L1's literals inside it; F-L3 copies F-L2
\ and is recorded with literals that have now survived two splices; F-L4 copies
\ F-L3 and is ONE instruction past the rule for its arity, so it is not recorded;
\ and F-L5, whose callee has no record, makes a real call.
\
\ THE RENAME CHAIN WALKS INTO THE ROW'S CAPACITY INSTEAD, and the pair of them is
\ why the two ceilings have to be separate. A rename is a token and no
\ instruction, so every link of this chain emits exactly the same seven
\ instructions and passes the size rule forever - the row fills up first. F-R2
\ holds sixteen tokens, which is the whole of a row; F-R3 would need twenty-four
\ and is refused, and the case asserts the size rule says YES about that same
\ routine, so what refused it was the capacity and not the rule.
\
\ BOTH REFUSALS ARE SOFT, AND THAT IS THE POINT OF ASSERTING WHAT STILL RUNS. A
\ body this file will not hold is a body its callers call, exactly as they call a
\ word the engine compiled: F-L4 and F-R3 are published, answer correctly, and
\ their own callers carry one branch each.
: MIGRATE-LIT-CHAIN ( -- )
   s" : NINL-L1 ( n -- n ) 3 * ;" 1 1 REGS NMIGRATE:DEFINE
   s" NINL-L1" s" NINL-L1" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-L2 ( n -- n ) NINL-L1 7 + ;" 1 1 REGS NMIGRATE:DEFINE-CALLING
   s" NINL-L2" s" NINL-L2" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-L3 ( n -- n ) NINL-L2 ;" 1 1 REGS NMIGRATE:DEFINE-CALLING
   s" NINL-L3" s" NINL-L3" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-L4 ( n -- n ) NINL-L3 5 - ;" 1 1 REGS NMIGRATE:DEFINE-CALLING
   s" NINL-L4" s" NINL-L4" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NINL-L5 ( n -- n ) NINL-L4 ;" 1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-RENAME-CHAIN ( -- )
   s" : NINL-R1 ( n n -- n n ) swap swap swap swap swap swap swap swap ;"
   2 2 REGS NMIGRATE:DEFINE
   s" NINL-R1" s" NINL-R1" ENTRY-OF 2 2 NMIGRATE:CALLEE
   s" : NINL-R2 ( n n -- n n ) NINL-R1 NINL-R1 ;" 2 2 REGS NMIGRATE:DEFINE-CALLING
   s" NINL-R1" s" NINL-R1" ENTRY-OF 2 2 NMIGRATE:CALLEE
   s" NINL-R2" s" NINL-R2" ENTRY-OF 2 2 NMIGRATE:CALLEE
   s" : NINL-R3 ( n n -- n n ) NINL-R2 NINL-R1 ;" 2 2 REGS NMIGRATE:DEFINE-CALLING
   s" NINL-R3" s" NINL-R3" ENTRY-OF 2 2 NMIGRATE:CALLEE
   s" : NINL-R4 ( n n -- n n ) NINL-R3 ;" 2 2 REGS NMIGRATE:DEFINE-CALLING ;

\ The same arithmetic as the whole literal chain, compiled by the ENGINE, so what
\ the copied chain answers is held against a second compiler.
: DEFINE-CHAIN-TWIN ( -- )
   s" : NINL-ENGINE-CHAIN ( n -- n ) 3 * 7 + 5 - ;" EV ;

: CHAIN-CASES ( -- )
   s" a routine whose own call was copied is recorded, and so is one of those"
   T-LABEL
   s" NINL-L1" ENTRY-OF NINL:KNOWN? FLAG# 1 T=
   s" NINL-L2" ENTRY-OF NINL:KNOWN? FLAG# 1 T=
   s" NINL-L3" ENTRY-OF NINL:KNOWN? FLAG# 1 T=

   s" and each row is the operations, never the call that was written" T-LABEL
   s" NINL-L1" ENTRY-OF NINL:TOKENS 2 T=
   s" NINL-L2" ENTRY-OF NINL:TOKENS 4 T=
   s" NINL-L3" ENTRY-OF NINL:TOKENS 4 T=

   s" the literals survive two splices, in the right order and to the bit"
   T-LABEL
   s" NINL-L3" ENTRY-OF 0 NINL:LIT@ 3 T=
   s" NINL-L3" ENTRY-OF 1 NINL:SPELL$ s" *" STR= TTRUE
   s" NINL-L3" ENTRY-OF 2 NINL:LIT@ 7 T=
   s" NINL-L3" ENTRY-OF 3 NINL:SPELL$ s" +" STR= TTRUE

   s" one link further is one instruction past the rule, so it is not recorded"
   T-LABEL
   s" NINL-L4" ENTRY-OF NINL:KNOWN? FLAG# 0 T=
   s" NINL-L3" WORD-INSNS 9 T=
   s" NINL-L4" WORD-INSNS 11 T=
   1 1 s" NINL-L3" WORD-INSNS NINL:SMALL? TTRUE
   1 1 s" NINL-L4" WORD-INSNS NINL:SMALL? TFALSE

   s" the refused link still copied ITS callee, so it carries no call itself"
   T-LABEL
   s" NINL-L4" BL-COUNT 0 T=
   s" NINL-L4" FRAME-COUNT 0 T=

   s" but a caller of it calls it, because nothing recorded it" T-LABEL
   s" NINL-L5" BL-COUNT 1 T=
   s" NINL-L5" FRAME-COUNT 1 T=
   s" NINL-L5" ENTRY-OF NINL:KNOWN? FLAG# 0 T=

   s" and the whole chain answers what the engine's code for it answers" T-LABEL
   s" 5 NINL-L4" EV-N  s" 5 NINL-ENGINE-CHAIN" EV-N T=
   s" 0 NINL-L4" EV-N  s" 0 NINL-ENGINE-CHAIN" EV-N T=
   s" -7 NINL-L4" EV-N  s" -7 NINL-ENGINE-CHAIN" EV-N T=
   s" 5 NINL-L4" EV-N 17 T=
   s" 5 NINL-L5" EV-N 17 T=

   s" a chain of renames fills the ROW before it reaches the size rule" T-LABEL
   s" NINL-R1" ENTRY-OF NINL:TOKENS 8 T=
   s" NINL-R2" ENTRY-OF NINL:TOKENS 16 T=
   s" NINL-R3" ENTRY-OF NINL:KNOWN? FLAG# 0 T=

   s" and it is the capacity that refused it, because the rule says yes" T-LABEL
   s" NINL-R3" WORD-INSNS 7 T=
   2 2 s" NINL-R3" WORD-INSNS NINL:SMALL? TTRUE
   16 NINL:FITS? TTRUE
   17 NINL:FITS? TFALSE

   s" the refused row's routine copied its own callees and still runs" T-LABEL
   s" NINL-R3" BL-COUNT 0 T=
   s" 3 4 NINL-R3 drop" EV-N 3 T=
   s" 3 4 NINL-R3 nip" EV-N 4 T=

   s" while its caller calls it, and runs too" T-LABEL
   s" NINL-R4" BL-COUNT 1 T=
   s" 3 4 NINL-R4 drop" EV-N 3 T=
   s" 3 4 NINL-R4 nip" EV-N 4 T= ;

\ ---- the arguments a copied body is handed --------------------------------
\ A routine reads its arguments out of data-stack slots, so its entry block takes
\ CELLS and the recorded tokens were elaborated with cells in those positions.
\ The caller below has just COMPUTED a double where the callee's own compilation
\ had a cell, which is the one place the difference shows: `!` stores a cell, and
\ a double reaching it is refused by name. So the splice crosses the arguments
\ first, exactly as the call it replaces crossed everything live, and the case is
\ the bits that reach memory - a crossing that computed anything, or one that was
\ not made, changes them.
: MIGRATE-STORE ( -- )
   s" : NINL-STORE ( r ptr a -- ) ! ;" 2 0 REGS NMIGRATE:DEFINE ;

: MIGRATE-PUT ( -- )
   s" NINL-STORE" s" NINL-STORE" ENTRY-OF 2 0 NMIGRATE:CALLEE
   s" : NINL-PUT ( r ptr a -- ) {: v:r b:ptr :} v v f+ b NINL-STORE ;"
   2 0 REGS NMIGRATE:DEFINE-CALLING ;

: ARG-CASES ( -- )
   s" a body that stores its argument is recorded as the store it is" T-LABEL
   s" NINL-STORE" ENTRY-OF NINL:KNOWN? FLAG# 1 T=
   s" NINL-STORE" ENTRY-OF NINL:TOKENS 1 T=
   s" NINL-STORE" ENTRY-OF 0 NINL:SPELL$ s" !" STR= TTRUE

   s" and a caller that copies it with a COMPUTED double makes no call" T-LABEL
   s" NINL-PUT" BL-COUNT 0 T=
   s" NINL-PUT" FRAME-COUNT 0 T=

   s" and the eight bytes that reach memory are the double, to the bit" T-LABEL
   s" 2.5 NINL-CELL NINL-PUT NINL-CELL @" EV-N
   s" 5.0 NINL-CELL ! NINL-CELL @" EV-N T=
   s" -0.75 NINL-CELL NINL-PUT NINL-CELL @" EV-N
   s" -1.5 NINL-CELL ! NINL-CELL @" EV-N T=
   s" 0.0 NINL-CELL NINL-PUT NINL-CELL @" EV-N 0 T= ;

public

: RUN ( -- )
   T-RESET
   TABLE-CASES
   RULE-CASES
   DEFINE-ENGINE-TWINS
   NINL:ROWS ROWS-BEFORE !
   MIGRATE-SMALL
   MIGRATE-LARGE
   MIGRATE-COPIES
   MIGRATE-CALLS
   MIGRATE-LIT
   MIGRATE-USE-LIT
   EDGE-CASES
   MIGRATE-CTRL
   MIGRATE-VIA-CTRL
   MIGRATE-CALLER-CALLEE
   MIGRATE-SELF
   BODY-REFUSAL-CASES
   DEFINE-CELL
   MIGRATE-LOAD
   MIGRATE-USE-LOAD
   MIGRATE-DIV
   MIGRATE-USE-DIV
   MIGRATE-DIV-BIG
   MIGRATE-CALL-DIV
   CARRIED-CASES
   DEFINE-CHAIN-TWIN
   MIGRATE-LIT-CHAIN
   MIGRATE-RENAME-CHAIN
   CHAIN-CASES
   MIGRATE-STORE
   MIGRATE-PUT
   ARG-CASES
   T-REPORT ;

;package

NINL-TEST:RUN
