\ native-defer.f - `[: … ;] is FOO` through the whole chain. One concern: what
\ the native chain compiles the token `is` into, and whether the deferred word
\ really dispatches to the body afterwards.
\
\ WHAT THIS SUITE HAS TO SHOW, AND WHY NOTHING SHORTER WOULD.
\
\   1. That the migrated installer BINDS. A migration that returned says nothing:
\      an installer that stored nowhere returns exactly the same way. So the
\      deferred word is called afterwards, through the interpreter and through a
\      compiled caller, and the answer is the body's.
\   2. That the store went through the engine's own store-and-declare primitive
\      and not through a store of this chain's own. A cell that holds a
\      JIT-region address has to be moved when a snapshot image is restored, and
\      the engine finds those cells from a table filled where the cell's KIND is
\      decided - so a chain that emitted a bare store would leave a restored
\      image jumping into the writing run's memory on the defer's first call
\      (dot habu-relocate-persisted-defer-7aa681c4). The emission is DECODED:
\      exactly one branch-with-link, and it goes to `xt!`. That derivation is
\      independent of anything this chain records about itself.
\   3. That the cell really is declared afterwards, read off the engine's own
\      relocation table. The decode says which primitive was called; this says
\      what calling it did. src/core/checker.f's own suite
\      test/snapshot-xt-cell-decl.f owns the RULE - which stores declare a cell
\      and which do not - and this owns the one new writer of it.
\   4. That a target which is not a deferred word is refused BY NAME, and that
\      the refusal is structural rather than a guess about a number. The hostile
\      fixture is a `create`d word whose first data cell holds the defer magic
\      exactly: a reader that looked for the magic anywhere near the record, or
\      that took the cell after any record for a dispatch cell, would bind to it.
\   5. That a name which denotes nothing, and a target token which is not a name
\      at all, are refused by the same name rather than compiled against
\      whatever the arithmetic landed on.

require lib/test.f
require src/compiler/native/migrate.f
require src/compiler/native/codewalk.f

package NDEFER-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite compiles a caller for a word that did not exist when the
\ suite was compiled. Every execution below goes through it rather than through
\ a compiled call site, for the reason LESSONS.md records: a call site can be
\ copied by the inliner, and a test written as one then proves nothing about the
\ record it meant to test.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

\ The dispatch cell's contents, and the engine's relocation table. Both are
\ reads of memory the dictionary named, which is the one thing checked Habu has
\ no type for; the deciding above them is ordinary checked Habu.
TRUSTED: CELL@ ( n -- n ) @ ;
TRUSTED: PCELL@ ( ptr a -- n ) @ ;
TRUSTED: DBASE-N ( -- n ) data-base ;

4 constant REGS
0 constant GLOBAL-WID

: REC ( ptr u8 n -- ptr a )
   GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if E-NPUB-NAME throw then ;

: REC-START ( ptr u8 n -- n )   REC XREF-START ;
: REC-LEN ( ptr u8 n -- n )     REC XREF-LEN ;

\ ---- the engine's own table of declared address cells ------------------------
\ Read through src/habu/layout.f's published offsets, which is where the engine
\ writes them. test/snapshot-xt-cell-decl.f reads the same two numbers the same
\ way; both are readers of one authority rather than two copies of a rule.
: XT-COUNT ( -- n )
   data-base SNAP-RELOC:XTCELL-N-CELL + PCELL@ ;

: XT-ROW ( n -- n ) {: row:n :}
   data-base SNAP-RELOC:XTCELL-ROWS-OFF + row cells + PCELL@ ;

variable HIT

: XT-LISTED? ( n -- bool ) {: off:n :}
   0 HIT !
   XT-COUNT 0 ?do
      i XT-ROW off = if 1 HIT ! then
   loop
   HIT @ 0<> ;

\ ---- reading the emitted instructions ----------------------------------------
\ A branch-with-link is the top six bits `100101` and a signed twenty-six-bit
\ word displacement; the address it reaches is its own address plus four times
\ that. It is decoded from the encoding rather than compared against an expected
\ word, so a case states the ADDRESS it means.
$FC000000 constant BL-MASK
$94000000 constant BL-FORM
1 25 lshift constant BL-SIGN
4 constant INSN-BYTES

: BL? ( n -- bool )
   BL-MASK and BL-FORM = ;

: BL-DELTA ( n -- n )
   {: w:n :}
   w $3FFFFFF and {: d:n :}
   d BL-SIGN and 0<> if d BL-SIGN 2 * - exit then
   d ;

: INSN-AT ( n n -- n )
   {: start:n k:n :}
   start k INSN-BYTES * + NWALK:INSN@ ;

: INSNS ( ptr u8 n -- n )
   REC-LEN INSN-BYTES / ;

: BLS ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u REC-START {: start:n :}
   0
   a u INSNS 0 ?do
      start i INSN-AT BL? if 1+ then
   loop ;

: BL-AT ( ptr u8 n -- n )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u REC-START {: start:n :}
   -1
   a u INSNS 0 ?do
      start i INSN-AT BL? if drop i leave then
   loop ;

\ Where the word's j-th branch-with-link stands. The running pair is in cells
\ because a counted loop's body cannot rebind a local, and a walk that stopped
\ at the first hit could not answer for the second.
variable BLN-AT   variable BLN-SEEN

: BL-NTH ( ptr u8 n n -- n )
   {: a u:n j:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   -1 BLN-AT !  0 BLN-SEEN !
   a u INSNS 0 ?do
      a u REC-START i INSN-AT BL? if
         BLN-SEEN @ j = if i BLN-AT ! then
         BLN-SEEN @ 1+ BLN-SEEN !
      then
   loop
   BLN-AT @ ;

: BL-TARGET ( ptr u8 n n -- n )
   {: a u:n j:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u j BL-NTH {: k:n :}
   a u REC-START  k INSN-BYTES * +  {: site:n :}
   site  a u REC-START k INSN-AT BL-DELTA INSN-BYTES *  + ;

\ ---- the words the fixtures bind ---------------------------------------------
\ The deferred word, the body it is bound to, and a compiled caller of it. The
\ caller exists because a defer is an engine trampoline and a body that calls one
\ compiles to a plain branch: it is the shape a program really writes, and it
\ reads the cell at run time rather than at compile time.
: SETUP ( -- )
   s" defer ND-HOOK ( n -- n )" EV
   s" : ND-IMPL ( n -- n ) 1 + ;" EV
   s" : ND-ACTION ( n -- n ) ND-HOOK ;" EV ;

: DEF-INSTALL ( -- )
   s" : ND-INSTALL ( -- ) [: ND-IMPL ;] is ND-HOOK ;" 0 0 REGS NMIGRATE:DEFINE ;

\ The cell ND-HOOK dispatches through, asked of the same resolver the chain asks.
: HOOK-CELL ( -- n )
   s" ND-HOOK" NDICT:SPELL-DEFER-CELL ;

: BIND-CASE ( -- )
   SETUP
   DEF-INSTALL
   s" the installer is the chain's code" T-LABEL
   s" ND-INSTALL" REC-START  s" ND-INSTALL" GLOBAL-WID NPUB:NEW-START T=
   s" and running it binds the deferred word to the body" T-LABEL
   s" ND-INSTALL" EV
   s" 41 ND-HOOK" EV-N 42 T=
   s" which a compiled caller of the defer then reaches" T-LABEL
   s" 41 ND-ACTION" EV-N 42 T= ;

\ WHAT THE DECODE RULES OUT. A bare store into the cell would be one or two
\ instructions and no branch at all, so a branch-with-link where the store would
\ be is the claim that separates the two emissions - and naming its target as
\ `xt!`'s own entry is what says WHICH primitive, rather than that some call was
\ made. The address comes from the dictionary at the moment of the assertion, so
\ a re-published `xt!` moves both sides together.
\
\ THERE ARE TWO BRANCHES AND BOTH ARE NAMED, because the emission holds two
\ functions: the installer, whose one call is the store, and the quotation body,
\ whose one call is to the word it runs. Asserting only the count would pass for
\ an emission that called the body's word twice and stored nothing, so each
\ branch is held against the address it must reach and the pair against the
\ count - which together say there is no third instruction branching anywhere.
: DECODE-CASE ( -- )
   s" the emission holds exactly two branches" T-LABEL
   s" ND-INSTALL" BLS 2 T=
   s" the installer's own branch goes to the store-and-declare primitive" T-LABEL
   s" ND-INSTALL" 0 BL-TARGET  s" xt!" NDICT:CALL-TARGET  T=
   s" and the body's goes to the word the quotation runs" T-LABEL
   s" ND-INSTALL" 1 BL-TARGET  s" ND-IMPL" REC-START  T= ;

\ WHAT THE TABLE ASSERTION ADDS. The decode says the primitive was called; this
\ says what the call did. The cell is declared by DATA OFFSET, which is how the
\ writer's canonicalise and the loader's relocate both index it.
: DECLARED-CASE ( -- )
   s" the cell now holds an address inside the installer's own emission" T-LABEL
   HOOK-CELL 0<> TTRUE
   HOOK-CELL CELL@ {: xt:n :}
   xt  s" ND-INSTALL" REC-START  >= TTRUE
   xt  s" ND-INSTALL" REC-START  s" ND-INSTALL" REC-LEN +  < TTRUE
   s" and the cell is in the engine's relocation table" T-LABEL
   HOOK-CELL DBASE-N - XT-LISTED? TTRUE ;

\ ---- the targets that are not deferred words ---------------------------------
\ THE HOSTILE ONE IS THE `create`d WORD. Its first data cell holds the defer
\ magic exactly, which is the value the trailer of a real defer starts with - so
\ a reader that searched for the magic near the record, or that simply took the
\ cell after a record's code as a dispatch cell, would bind a quotation into an
\ ordinary data word and the program would branch into whatever the next cell
\ held. What refuses it is that the trailer is read at the record's own
\ START+LEN, which for a created word is code space and not the data the word
\ names: measured, the two are in different regions entirely.
\
\ THE PLAIN COLON WORD IS THE SECOND HALF OF THE SAME CLAIM. Its trailer read
\ lands on the next definition's instructions - a perfectly ordinary integer -
\ and it answers absent for the same reason.
\
\ AND THE RESOLVER IS WHAT IS ASKED HERE, NOT THE CHAIN, because a program that
\ writes `is` on a non-defer never reaches the chain at all: the engine's own
\ `is` handler refuses the target before the definition is certified, so the
\ source dies with the engine's code and the elaborator is never entered. That
\ is measured below, and it is why the elaborator's own refusal is exercised on
\ a hand-built tape in test/compiler/native-elaborate.f - a shape the engine
\ never produces, and the only way to reach a backstop that exists because this
\ pass reads a tape rather than source.
: SETUP-BAD ( -- )
   s" create ND-DATA  $4842444546455201 ,  0 ," EV
   s" : ND-PLAIN ( n -- n ) 2 * ;" EV ;

: BAD-DATA ( -- )
   s" : ND-BAD1 ( -- ) [: ND-IMPL ;] is ND-DATA ;" EV ;

: BAD-COLON ( -- )
   s" : ND-BAD2 ( -- ) [: ND-IMPL ;] is ND-PLAIN ;" EV ;

: BAD-ABSENT ( -- )
   s" : ND-BAD3 ( -- ) [: ND-IMPL ;] is ND-NOWHERE ;" EV ;

76 constant DIE-RC                   \ the engine's `is` refusing its target
70 constant NAME-RC                  \ the engine refusing a name that denotes nothing

: REFUSE-CASE ( -- )
   SETUP-BAD
   s" the created word's first data cell really does hold the defer magic" T-LABEL
   s" ND-DATA" NDICT:FIXED-VALUE CELL@  DEFER-MAGIC  T=
   s" and the resolver still answers it no dispatch cell" T-LABEL
   s" ND-DATA" NDICT:SPELL-DEFER-CELL 0 T=
   s" an ordinary colon word answers none either" T-LABEL
   s" ND-PLAIN" NDICT:SPELL-DEFER-CELL 0 T=
   s" a name that denotes nothing answers none" T-LABEL
   s" ND-NOWHERE" NDICT:SPELL-DEFER-CELL 0 T=
   s" while the real deferred word answers one" T-LABEL
   s" ND-HOOK" NDICT:SPELL-DEFER-CELL 0<> TTRUE
   s" and such a program never reaches the chain: the engine refuses it first" T-LABEL
   [: BAD-DATA ;] DIE-RC TTHROWSQ
   [: BAD-COLON ;] DIE-RC TTHROWSQ
   [: BAD-ABSENT ;] NAME-RC TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   BIND-CASE
   DECODE-CASE
   DECLARED-CASE
   REFUSE-CASE
   T-REPORT ;

;package

NDEFER-TEST:RUN
