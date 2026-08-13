\ hir-word.f - the straight-line HIR dialect's source-word model: what a Habu
\ word means to the elaborator, and which words it refuses to compile at all.
\
\ docs/compiler-ir-design.md section 7.2 with section 7.3 line 758. This is the
\ second half of the dialect that src/compiler/native/hir.f opens:
\ src/compiler/native/hir.f says which operations exist, and this file says
\ which Habu source words the dialect can compile and what each one means. The
\ two are separate packages because each seals its own wordlists, and the
\ dependency runs one way: HIR-WORD names HIR's opcodes and HIR knows nothing
\ about source words.
\
\ THE MEANINGS AND A REFUSAL. A word of the straight-line subset means exactly
\ one of these:
\   op        it elaborates to one operation of this dialect;
\   const-op  it is one integer literal followed by one operation - `1-` is `1`
\             then `-` - and the row carries both;
\   control   it decides which blocks the definition has, and stages nothing;
\   rename    it only rearranges the compile-time value vector and produces no
\             operation at all;
\   fixed     it pushes one value and nothing else, which is what a `create`d
\             data word and a `constant` both do, and the row carries that
\             value;
\   callable  it is another word this definition calls, and the row carries
\             where that word's code starts and how many values it takes and
\             leaves. It is not `control` the way `RECURSE` is: `RECURSE` means
\             the definition being compiled and needs no payload at all, while a
\             callable word is a different routine per row;
\   open-locals   it starts a `{: … :}` group, so the names after it are the
\   close-locals  program's own locals and the closer binds one value to each,
\                 right to left. Neither stages an operation and neither carries
\                 a payload: the work is the elaborator's, over the rows between
\                 them, and a bound name is just a value of the compile-time
\                 vector.
\   unmodeled a named boundary: checked source may not compile it yet, and the
\             row says which capability has to land first.
\ Three further meanings - `literal`, `real-literal` and `string-literal` -
\ belong to a source-tape token rather than to a word, so no row ever stores
\ one: an integer literal is not a call, a string literal is not a name, and the
\ tape's own token kind is what says which it is. A word this table never
\ declared is refused exactly as a declared unmodeled boundary is: to checked
\ source they are the same event, this dialect cannot compile that word.
\
\ WHY A RENAME IS DATA AND NOT SIX SPECIAL CASES. Section 7.3 says `DUP`,
\ `DROP`, `SWAP`, `OVER`, `NIP` and `ROT` "produce no SIR operation and therefore
\ no runtime instruction": they change the compile-time value vector and nothing
\ else. A row therefore records that change as data - how many values the word
\ consumes off the top, and which of them it puts back, in order - so the
\ stack-to-SSA converter applies a rename by reading it rather than by carrying
\ its own copy of what `OVER` means. An input is named by its depth in the
\ consumed window, zero being the top:
\   dup  ( a -- a a )        consumes 1, puts back 0 0
\   drop ( a -- )            consumes 1, puts back nothing
\   swap ( a b -- b a )      consumes 2, puts back 0 1
\   over ( a b -- a b a )    consumes 2, puts back 1 0 1
\   nip  ( a b -- b )        consumes 2, puts back 0
\   rot  ( a b c -- b c a )  consumes 3, puts back 1 0 2
\   2drop ( a b -- )         consumes 2, puts back nothing
\ Picks are listed bottom first, which is the order they are pushed. A rename
\ may repeat an input, as `DUP` and `OVER` do, and may drop one, as `DROP`,
\ `NIP` and `2DROP` do; the one rule is that it can only put back an input it
\ consumed.
\
\ HOW `ROT`'S PICK LIST IS DERIVED. Reading a pick list off a stack comment is
\ mechanical, and `rot` is the one where getting it wrong is easy, so here is the
\ derivation in full. `rot` consumes three values, a b c, with c on top; in the
\ consumed window depth zero is the top, so c is depth 0, b is depth 1 and a is
\ depth 2. Standard Forth `rot` brings the third value to the top and leaves
\ b c a, read bottom to top. Picks are listed bottom first, so the list is the
\ depth of b, then the depth of c, then the depth of a: 1 0 2. The neighbouring
\ orders come out different, which is what makes the order provable rather than
\ asserted: `-rot` ( a b c -- c a b ) would be 0 2 1, and leaving the three
\ values where they are would be 2 1 0. The elaborator suite pins the difference
\ with a subtraction, whose operands are not interchangeable, so a skewed pick
\ index reds rather than computing the same answer by another route.
\
\ THE OPCODE A WORD MEANS IS A TYPE, NOT A LOOKUP. A row stores the stable code
\ of a `HIR:opcode`, so binding a word to an operation this dialect does not
\ have is not a runtime check that can be forgotten - it is unwritable, and a
\ stored code outside the family is refused by the decoder at first touch.
\
\ WHAT A DECLARATION CHECKS. Every symbol a row holds is checked to belong to
\ this table's module and to have really been interned by that module, no word
\ is declared twice, and both ceilings are committed at creation. Belonging and
\ existing are two different facts: an identity is arithmetic away from any
\ other identity of the same module, so a row could otherwise name an ordinal
\ the interner never minted and sit in the table for ever, matching no source
\ token. There are two ways to reach a module's interner and each declarer takes
\ one of them - the module's symbol rows, the way src/compiler/native/immediate.f
\ does, or the builder that holds them privately while the module is still being
\ built - and both end in the same IR-SYM refusal.
\
\ SPELLINGS ARE BYTES AND A ROW IS KEYED BY THEIR FOLD. REGISTER-WORDS interns
\ the subset's words exactly as `docs/forth.md` spells them, built-ins in lower
\ case and `RECURSE` in capitals, and every row is then keyed by the FOLD of that
\ spelling - which is the same fold the engine applies when it decides what a
\ token of a checked body means. A body may therefore write `IF`, `if` or `If`
\ and reach the one row, exactly as it may write any of them to the engine. The
\ fold and the argument for it are at KEY-SYM below. Which spelling the real
\ lexer records is still the tape producer's fact, tracked by dot
\ habu-feed-the-src-f7ed8733; nothing here guesses at it, and nothing here folds
\ the tape's own record of it.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/build.f
require src/compiler/native/tape.f
require src/compiler/native/hir.f
require src/compiler/native/dict.f

package HIR-WORD
public

\ A symbol this module's interner has answered for. Owning the right module is
\ not the same as existing: an identity is arithmetic away from any other
\ identity of the same module, so a row could otherwise name an ordinal the
\ interner never minted, and no source token could ever spell it. ROW-ADD takes
\ one of these, and the only two words that make one are the two ways to ask a
\ module's interner, so no declarer in this file can write a row for a symbol
\ nobody asked about. It carries the symbol rather than retyping it, because
\ minting an IR-ID identity is IR-ID's alone and this type claims no such power;
\ and it is public only because a generated constructor has to be. Making one
\ outside this package proves nothing and buys nothing: every word that consumes
\ one is private.
STRUCTURE interned 0
   FIELD sym IR-ID:ir-symbol-id
;STRUCTURE

private

CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n ) ;

\ ---- layout ------------------------------------------------------------------
$48575031 constant WPOOL-MAGIC       \ "HWP1": the pick-pool header format tag
$48575231 constant WROW-MAGIC        \ "HWR1": the word-table header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-SYM                   \ the source word's symbol ordinal
1 constant OFF-MEAN                  \ the stored meaning code
2 constant OFF-A                     \ op and const-op: the opcode code; rename: the pick-list start; control: the control code; rstack: the transfer code; unmodeled: the reason ordinal plus one; callable: the callee's entry address
3 constant OFF-IN                    \ rename: the number of values consumed; rstack: the number of cells moved; const-op: the constant; fixed: the value the word pushes; callable: the values the callee takes; otherwise zero
4 constant OFF-N                     \ rename: the number of values put back; callable: the values the callee leaves; otherwise zero
5 constant OFF-GLUE                  \ callable: which of the callee's result cells belong to a multi-cell value; otherwise zero
6 constant OFF-DEAD                  \ callable: whether control comes back from the callee; otherwise zero
7 constant ROW-CELLS
0 constant UNUSED                    \ a payload cell this meaning does not use
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant ROW-CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CAP-MAX

\ The deepest classical Forth stack rename is `2over` ( a b c d -- a b c d a b ):
\ it consumes four values and puts back six. These ceilings hold that shape with
\ headroom and keep the staging buffer a fixed array; a rename that wants more
\ is a capability to add here, not a value to widen silently.
4 constant INPUT-MAX
8 constant PICK-MAX

\ ---- stored codes ------------------------------------------------------------
\ The stored codes are this table's stable vocabulary. Both decoders are exact
\ cases, so a row written past this package's declarers cannot decode as some
\ other meaning or some other operation.

\ Whether control comes back from a callee. It is a stored CODE and not a raw
\ flag for the same reason the meanings are: a row cell holds a number, and a
\ number this file did not name is a number some other reader could read as
\ something else. COMES-BACK is zero so that it is also what an unset payload
\ cell says, which makes "a declarer that had no answer" and "a callee control
\ comes back from" the same row - the safe direction, since the only cost of it
\ is a refusal where a finer answer would have compiled.
0 constant COMES-BACK
1 constant NO-RETURN

: NORET-CODE ( bool -- n )
   if NO-RETURN else COMES-BACK then ;
: MEAN-CODE ( HIR:meaning -- n )
   MATCH HIR:meaning
      literal   OF 0 ENDOF
      real-literal OF 10 ENDOF
      string-literal OF 11 ENDOF
      op        OF 1 ENDOF
      rename    OF 2 ENDOF
      rstack    OF 12 ENDOF
      unmodeled OF 3 ENDOF
      const-op  OF 4 ENDOF
      control   OF 5 ENDOF
      fixed     OF 6 ENDOF
      callable  OF 9 ENDOF
      open-locals  OF 7 ENDOF
      close-locals OF 8 ENDOF
   ;MATCH ;

\ Codes zero, ten and eleven - `literal`, `real-literal` and `string-literal` -
\ are deliberately absent: all three are a TOKEN's meaning, so a row that claims
\ one is corrupt rather than unusual.
: N>MEAN ( n -- HIR:meaning )
   case
      1 of HIR-MEANING:OP endof
      2 of HIR-MEANING:RENAME endof
      3 of HIR-MEANING:UNMODELED endof
      4 of HIR-MEANING:CONST-OP endof
      5 of HIR-MEANING:CONTROL endof
      6 of HIR-MEANING:FIXED endof
      7 of HIR-MEANING:OPEN-LOCALS endof
      8 of HIR-MEANING:CLOSE-LOCALS endof
      9 of HIR-MEANING:CALLABLE endof
      12 of HIR-MEANING:RSTACK endof
      E-HIR-CLASS throw
   endcase ;

: OPCODE-CODE ( HIR:opcode -- n )
   MATCH HIR:opcode
      const  OF 0 ENDOF
      add    OF 1 ENDOF
      sub    OF 2 ENDOF
      mul    OF 3 ENDOF
      div    OF 12 ENDOF
      return OF 4 ENDOF
      lt     OF 5 ENDOF
      le     OF 6 ENDOF
      gt     OF 18 ENDOF
      ge     OF 19 ENDOF
      ne     OF 20 ENDOF
      and    OF 21 ENDOF
      or     OF 22 ENDOF
      xor    OF 23 ENDOF
      lshift OF 24 ENDOF
      rshift OF 25 ENDOF
      invert OF 26 ENDOF
      br     OF 7 ENDOF
      brz    OF 8 ENDOF
      mem    OF 9 ENDOF
      load   OF 10 ENDOF
      store  OF 11 ENDOF
      bload  OF 13 ENDOF
      bstore OF 14 ENDOF
      equal  OF 15 ENDOF
      call   OF 16 ENDOF
      wordcall OF 17 ENDOF
      fconst   OF 27 ENDOF
      fadd     OF 28 ENDOF
      fsub     OF 29 ENDOF
      fmul     OF 30 ENDOF
      fdiv     OF 31 ENDOF
      fneg     OF 32 ENDOF
      fabs     OF 33 ENDOF
      fsqrt    OF 34 ENDOF
      intreal  OF 35 ENDOF
      realint  OF 36 ENDOF
      bitsreal OF 37 ENDOF
      realbits OF 38 ENDOF
      flt      OF 39 ENDOF
      fgt      OF 40 ENDOF
      feq      OF 41 ENDOF
      fltz     OF 42 ENDOF
      feqz     OF 43 ENDOF
      trap     OF 44 ENDOF
      quot     OF 45 ENDOF
   ;MATCH ;

: N>OPCODE ( n -- HIR:opcode )
   case
      0 of HIR-OPCODE:CONST endof
      1 of HIR-OPCODE:ADD endof
      2 of HIR-OPCODE:SUB endof
      3 of HIR-OPCODE:MUL endof
      4 of HIR-OPCODE:RETURN endof
      5 of HIR-OPCODE:LT endof
      6 of HIR-OPCODE:LE endof
      7 of HIR-OPCODE:BR endof
      8 of HIR-OPCODE:BRZ endof
      9 of HIR-OPCODE:MEM endof
      10 of HIR-OPCODE:LOAD endof
      11 of HIR-OPCODE:STORE endof
      12 of HIR-OPCODE:DIV endof
      13 of HIR-OPCODE:BLOAD endof
      14 of HIR-OPCODE:BSTORE endof
      15 of HIR-OPCODE:EQUAL endof
      16 of HIR-OPCODE:CALL endof
      17 of HIR-OPCODE:WORDCALL endof
      18 of HIR-OPCODE:GT endof
      19 of HIR-OPCODE:GE endof
      20 of HIR-OPCODE:NE endof
      21 of HIR-OPCODE:AND endof
      22 of HIR-OPCODE:OR endof
      23 of HIR-OPCODE:XOR endof
      24 of HIR-OPCODE:LSHIFT endof
      25 of HIR-OPCODE:RSHIFT endof
      26 of HIR-OPCODE:INVERT endof
      27 of HIR-OPCODE:FCONST endof
      28 of HIR-OPCODE:FADD endof
      29 of HIR-OPCODE:FSUB endof
      30 of HIR-OPCODE:FMUL endof
      31 of HIR-OPCODE:FDIV endof
      32 of HIR-OPCODE:FNEG endof
      33 of HIR-OPCODE:FABS endof
      34 of HIR-OPCODE:FSQRT endof
      35 of HIR-OPCODE:INTREAL endof
      36 of HIR-OPCODE:REALINT endof
      37 of HIR-OPCODE:BITSREAL endof
      38 of HIR-OPCODE:REALBITS endof
      39 of HIR-OPCODE:FLT endof
      40 of HIR-OPCODE:FGT endof
      41 of HIR-OPCODE:FEQ endof
      42 of HIR-OPCODE:FLTZ endof
      43 of HIR-OPCODE:FEQZ endof
      44 of HIR-OPCODE:TRAP endof
      45 of HIR-OPCODE:QUOT endof
      E-HIR-OPCODE throw
   endcase ;

\ The control actions, under the same discipline: a stable stored code per
\ member and an exact decoder, so a row written past this package's declarers
\ cannot decode as some other control word.
: CTRL-CODE ( HIR:ctrl -- n )
   MATCH ctrl
      open-if      OF 0 ENDOF
      close-if     OF 1 ENDOF
      open-begin   OF 2 ENDOF
      close-until  OF 3 ENDOF
      open-do      OF 4 ENDOF
      close-loop   OF 5 ENDOF
      index        OF 6 ENDOF
      drop-loop    OF 7 ENDOF
      early-exit   OF 8 ENDOF
      self-call    OF 9 ENDOF
      mid-while    OF 10 ENDOF
      close-repeat OF 11 ENDOF
      mid-else     OF 12 ENDOF
      open-match   OF 13 ENDOF
      match-arm    OF 14 ENDOF
      close-arm    OF 15 ENDOF
      close-match  OF 16 ENDOF
      open-case    OF 17 ENDOF
      close-case   OF 18 ENDOF
      make-bundle  OF 19 ENDOF
      open-quot    OF 20 ENDOF
      close-quot   OF 21 ENDOF
      bind-defer   OF 22 ENDOF
      exec         OF 23 ENDOF
      open-do-skip OF 24 ENDOF
      close-again  OF 25 ENDOF
      early-leave  OF 26 ENDOF
      catch        OF 27 ENDOF
   ;MATCH ;

: N>CTRL ( n -- HIR:ctrl )
   case
      0 of HIR-CTRL:OPEN-IF endof
      1 of HIR-CTRL:CLOSE-IF endof
      2 of HIR-CTRL:OPEN-BEGIN endof
      3 of HIR-CTRL:CLOSE-UNTIL endof
      4 of HIR-CTRL:OPEN-DO endof
      5 of HIR-CTRL:CLOSE-LOOP endof
      6 of HIR-CTRL:INDEX endof
      7 of HIR-CTRL:DROP-LOOP endof
      8 of HIR-CTRL:EARLY-EXIT endof
      9 of HIR-CTRL:SELF-CALL endof
      10 of HIR-CTRL:MID-WHILE endof
      11 of HIR-CTRL:CLOSE-REPEAT endof
      12 of HIR-CTRL:MID-ELSE endof
      13 of HIR-CTRL:OPEN-MATCH endof
      14 of HIR-CTRL:MATCH-ARM endof
      15 of HIR-CTRL:CLOSE-ARM endof
      16 of HIR-CTRL:CLOSE-MATCH endof
      17 of HIR-CTRL:OPEN-CASE endof
      18 of HIR-CTRL:CLOSE-CASE endof
      19 of HIR-CTRL:MAKE-BUNDLE endof
      20 of HIR-CTRL:OPEN-QUOT endof
      21 of HIR-CTRL:CLOSE-QUOT endof
      22 of HIR-CTRL:BIND-DEFER endof
      23 of HIR-CTRL:EXEC endof
      24 of HIR-CTRL:OPEN-DO-SKIP endof
      25 of HIR-CTRL:CLOSE-AGAIN endof
      26 of HIR-CTRL:EARLY-LEAVE endof
      27 of HIR-CTRL:CATCH endof
      E-HIR-CONTROL throw
   endcase ;

\ The return-stack transfers, under the discipline the two above are under. The
\ codes start at zero because they are read out of a payload cell this meaning
\ owns outright, and a row of any other meaning is never decoded through here.
: RSTACK-CODE ( HIR:rmove -- n )
   MATCH rmove
      to-r    OF 0 ENDOF
      from-r  OF 1 ENDOF
      fetch-r OF 2 ENDOF
   ;MATCH ;

: N>RSTACK ( n -- HIR:rmove )
   case
      0 of HIR-RMOVE:TO-R endof
      1 of HIR-RMOVE:FROM-R endof
      2 of HIR-RMOVE:FETCH-R endof
      E-HIR-CLASS throw
   endcase ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

\ ---- headers and shape -------------------------------------------------------
: RSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-HIR-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-HIR-STATE throw then ;

: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-HIR-STATE throw then ;

\ The two arenas of this table and the module's other arenas all have the same
\ checked type, so each one rechecks its own header tag: a pair swapped at a
\ call site dies on the tag instead of reading a foreign row.
: RHDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED RSHAPE-CK
   a HC-MAGIC LCELL@ WROW-MAGIC <> if E-HIR-STATE throw then ;

: PHDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED PSHAPE-CK
   a HC-MAGIC LCELL@ WPOOL-MAGIC <> if E-HIR-STATE throw then ;

: CNT ( IR-ARENA:arena -- n )
   IR-ARENA:USED HDR-CELLS - ROW-CELLS / ;

: PCELLS ( IR-ARENA:arena -- n )
   IR-ARENA:USED HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-HIR-OWNER throw then ;

: KEY-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   r RHDR-CK
   r HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

\ Both arenas must be halves of one table, which is one more thing a swapped
\ pair cannot survive.
: PAIR-CK ( IR-ARENA:arena IR-ARENA:arena -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   p PHDR-CK
   r RHDR-CK
   p HC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK ;

\ A declared symbol carries its owning module, so no presented key is needed to
\ bind a row to this table's module.
: SYM-OWNER-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r RHDR-CK
   r HC-SERIAL LCELL@ id IR-ID:SYMBOL-OWNER MID-SERIAL SERIAL-CK ;

\ ---- the key a spelling has in this table ------------------------------------
\ WHAT A ROW IS KEYED BY, AND WHY IT IS NOT THE BYTES THE SOURCE WROTE. A Habu
\ name is case-insensitive to the ENGINE, and in both of the places that decide
\ what a token of a checked body means: src/habu/habu2.f LKWCMP, which is how
\ `if`, `begin`, `?do` and `{:` are recognised, and src/habu/habu1.f
\ C-HIDX-HASH with the FIND compare beside it, which is how every dictionary word
\ is found. Both apply one rule to each byte - a byte in `A`..`Z` gets $20 set,
\ every other byte stands - so `IF` and `if` are ONE name to the engine, and a
\ table that kept them apart refused a body for its spelling alone.
\
\ SO THIS TABLE'S KEY IS THAT FOLD, ON BOTH SIDES OF THE COMPARISON. Every row is
\ written under the fold of its word's spelling - BKEY-CK below, which every
\ builder-side declarer goes through - and every question is asked under the fold
\ of the token's spelling, which is KEY-SYM. One function, one canonical form,
\ one ordinal comparison: there is no second spelling for a row to be written
\ under, and no second rule for a lookup to miss by.
\
\ AND IT IS THIS TABLE'S KEY RATHER THAN A POLICY ABOUT NAMES. The tape's own
\ symbol still holds the bytes the source wrote - that is what a refusal names,
\ and what a string literal's body IS - and a `{: … :}` local is not folded at
\ all, because the engine's own local lookup (src/habu/habu2.f EMIT-LOC-FIND)
\ compares those bytes raw where its keyword and dictionary compares fold. The
\ fold is applied where the engine folds and nowhere else.
$41 constant KEY-A                   \ the first byte the fold moves
$5A constant KEY-Z                   \ and the last
$20 constant KEY-BIT                 \ the bit it sets

\ The longest spelling this table can key. It is the ceiling the declarers that
\ read a spelling back already keep - FIX-NAME-CAP below and
\ src/compiler/native/migrate.f's staging - so no row of any table this file
\ builds can have a longer spelling than this, and a longer one is answered
\ unfolded rather than truncated into a name that denotes some other word.
64 constant KEY-CAP

create KEY-BUF KEY-CAP allot

: FOLD-C ( n -- n )
   {: b:n :}
   b KEY-A < if b exit then
   b KEY-Z > if b exit then
   b KEY-BIT or ;

\ Whether these bytes are already their own fold. The interner answers ONE
\ identity per byte string, so interning bytes it already holds under an identity
\ can only answer that identity again - which makes this a shortcut rather than a
\ second rule. It is worth taking because the scan costs a compare per byte where
\ the intern costs a digest of the whole spelling and a walk of the module's
\ symbols, and the elaborator asks this of every name token of every body.
: FOLDED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   true
   u 0 ?do
      a i + c@ dup FOLD-C = 0= if drop false leave then
   loop ;

: FOLD-INTO ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do
      a i + c@ FOLD-C  KEY-BUF i + c!
   loop ;

public

\ The key these bytes have in a table of this module. It is what a caller holding
\ a spelling rather than an identity asks - the splice, which reads a recorded
\ body's names back as bytes - so that a copied token reaches the same row the
\ token it was copied from reached.
: KEY-SPELL ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:ptr u:n :}
   u KEY-CAP > if c b a u IR-BUILD:INTERN-SYMBOL exit then
   a u FOLDED? if c b a u IR-BUILD:INTERN-SYMBOL exit then
   a u FOLD-INTO
   c b KEY-BUF u IR-BUILD:INTERN-SYMBOL ;

\ The same key for a spelling this module has already interned, which is the form
\ the elaborator asks: it holds a tape row's symbol and wants the row that
\ symbol's WORD has. A spelling too long to be any row's answers itself, because
\ no key of any case can find a row for it and the refusal should name the word
\ the body wrote.
: KEY-SYM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-LEN KEY-CAP > if id exit then
   c b id KEY-BUF KEY-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   KEY-BUF u FOLDED? if id exit then
   KEY-BUF u FOLD-INTO
   c b KEY-BUF u IR-BUILD:INTERN-SYMBOL ;

private

\ ---- symbols the module's interner has answered for --------------------------
\ The module's symbol rows, held directly. IR-SYM refuses an identity of another
\ module and an ordinal past the interned count, and the refusal is the
\ interner's own, exactly as src/compiler/native/immediate.f asks it.
\
\ THIS DOOR STATES ITS KEY RATHER THAN COMPUTING IT, which is the one difference
\ from the builder-side door below. Reading a spelling back out of a frozen
\ module needs its byte pool and this is handed only its rows, so the fold cannot
\ be applied here; a caller that declares a row through this door under a
\ spelling that is not already its own fold writes a row no lookup can reach.
\ That fails closed - the word is refused as unmodeled, by name - and
\ test/compiler/native-hir.f pins it, so the unreachable row cannot be mistaken
\ for a modelled word.
: SYM-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR-WORD:interned )
   {: sy:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   sy id IR-SYM:LEN@ drop
   id HIR--WORD-INTERNED:MAKE ;

\ The same question about a module that is still being built, whose interner
\ src/compiler/ir/build.f holds privately. It answers by asking IR-SYM, so a
\ symbol refused here is refused for the same reason and under the same name.
: BSYM-CK ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- HIR-WORD:interned )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-CK
   id HIR--WORD-INTERNED:MAKE ;

\ The symbol a declaration through that door writes its row under: the key of the
\ word's spelling, answered for by the same interner. Every builder-side declarer
\ below goes through this and none of them touches BSYM-CK directly, so a row
\ under any other key cannot be written - the declarers do not have to remember
\ the rule, they cannot express its opposite.
: BKEY-CK ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- HIR-WORD:interned )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b  c b id KEY-SYM  BSYM-CK ;

\ ---- row addressing ----------------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

: PC@ ( IR-ARENA:arena n -- n )
   HDR-CELLS + LCELL@ ;

\ The row that models this symbol, or a negative answer. One scan serves the
\ lookup, the duplicate check, and the inventory walk.
: FIND ( IR-ARENA:arena n -- n )
   {: r:IR-ARENA:arena so:n :}
   -1
   r CNT 0 ?do
      r i OFF-SYM RC@ so = if drop i leave then
   loop ;

: ROW-CAP-OK ( n -- )
   dup 1 < over ROW-CAP-MAX > or if E-HIR-CAP throw then
   drop ;

: POOL-CAP-OK ( n -- )
   dup 0 < over POOL-CAP-MAX > or if E-HIR-CAP throw then
   drop ;

: ROW-ROOM-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r CNT r HC-CAP LCELL@ >= if E-HIR-CAP throw then ;

: POOL-ROOM-CK ( IR-ARENA:arena n -- )
   {: p:IR-ARENA:arena want:n :}
   p PCELLS want + p HC-CAP LCELL@ > if E-HIR-CAP throw then ;

public

\ ---- creation ----------------------------------------------------------------
\ Create a module's word model: the pick pool committed to exactly pcap cells
\ and the row table to exactly rcap words, both headers bound to key's module
\ serial. The two handles plus the key are the table, and it dies with the
\ owning context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key rcap:n pcap:n :}
   rcap ROW-CAP-OK
   pcap POOL-CAP-OK
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p WPOOL-MAGIC IR-ARENA:PUSH drop
   c p key KEY-SERIAL IR-ARENA:PUSH drop
   c p pcap IR-ARENA:PUSH drop
   c rcap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r WROW-MAGIC IR-ARENA:PUSH drop
   c r key KEY-SERIAL IR-ARENA:PUSH drop
   c r rcap IR-ARENA:PUSH drop
   p r ;

private

\ Append one validated row. Every declarer ends here, so the ownership, the
\ duplicate rule and the ceiling are proved in one place, and the symbol it
\ takes has already been answered for by the module's interner.
: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned n n n n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena w:HIR-WORD:interned mean:n a:n
      in:n n:n glue:n dead:n :}
   w HIR--WORD-INTERNED:UNMAKE {: id:IR-ID:ir-symbol-id :}
   r id SYM-OWNER-CK
   id IR-ID:SYMBOL-LOCAL {: so:n :}
   r so FIND 0 < 0= if E-HIR-DUP throw then
   r ROW-ROOM-CK
   c r so IR-ARENA:PUSH drop
   c r mean IR-ARENA:PUSH drop
   c r a IR-ARENA:PUSH drop
   c r in IR-ARENA:PUSH drop
   c r n IR-ARENA:PUSH drop
   c r glue IR-ARENA:PUSH drop
   c r dead IR-ARENA:PUSH drop ;

\ The row an operation word writes, once its symbol has been answered for. The
\ two declarers below differ only in which interner answered.
: OP-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:opcode -- )
   {: o:HIR:opcode :}
   HIR-MEANING:OP MEAN-CODE
   o OPCODE-CODE
   UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The row a constant-and-operation word writes. Some Habu words are one integer
\ literal followed by one binary operation and nothing else - `1-` is `1` then
\ `-` - and the honest model of them is that pair rather than a second opcode
\ that means the same thing. The row therefore carries both: which operation,
\ and which constant it is applied with.
: CONST-OP-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:opcode n -- )
   {: o:HIR:opcode v:n :}
   HIR-MEANING:CONST-OP MEAN-CODE
   o OPCODE-CODE
   v UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The row a word that pushes one fixed value writes. A `create`d data word is
\ decided once, when the word is created, and every mention of it is that one
\ number; the row therefore carries the number and no opcode, because what the
\ word means is the value and not an operation. The value sits in the same cell
\ a constant-and-operation row keeps its constant in, so the two readers below
\ read one concept out of one place.
\
\ AND IT CARRIES WHAT THE NUMBER IS, in the cell an operation row keeps its
\ opcode in. A `constant`'s number is a number; a `create`d or `variable` word's
\ is the address of storage in the engine's DATA region, which a snapshot moves
\ with that region - so the two are not interchangeable one line further down,
\ where the literal is staged and the address kind decides whether the site is
\ recorded for relocation. The distinction is the DEFINER's, read off the record
\ (src/compiler/native/dict.f SPELL-FIXED), and it travels with the value rather
\ than being worked out again from the value's size or its range.
: FIXED-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned n n -- )
   {: v:n kind:n :}
   HIR-MEANING:FIXED MEAN-CODE
   kind
   v UNUSED UNUSED UNUSED
   ROW-ADD ;

\ Where the spelling of a fixed word is read back out of the module's interner so
\ the dictionary can be asked about it. The ceiling is the longest spelling a
\ program may write for such a word, and a qualified `NAME:tail` is the longer of
\ the two forms it can take; a spelling past it is refused by the interner's own
\ copy rather than truncated into a name that denotes another word.
64 constant FIX-NAME-CAP

create FIX-NAME FIX-NAME-CAP allot

\ What the number a definer decided IS, as the literal staging says it: an
\ address of the DATA region for the two definers that hand out storage, an
\ ordinary number for the one that hands out a number. This is the whole of the
\ translation between the dictionary's vocabulary for definers and this chain's
\ vocabulary for literals, and it lives here because this file is the one that
\ already speaks both. A kind neither definer stamped never reaches it: the
\ value's own reader refuses that spelling before there is anything to classify.
: LIT-KIND ( n -- n )
   {: k:n :}
   k NDICT:FIXED-ADDR = if HIR:ADDR-DATA exit then
   HIR:ADDR-NONE ;

\ The row a word that is CALLED writes: where the callee's code starts, and how
\ many values it takes and leaves. Those three are the whole of what a call site
\ needs to know about a callee - the entry is where the branch goes, and the
\ arity is how many values the site publishes for it and takes back afterwards.
\
\ WHAT THIS ROW CHECKS AND WHAT IT LEAVES TO THE MACHINE. It checks the two facts
\ it owns: no code lives at the null address, so an entry of zero or below names
\ nothing; and a call site cannot publish minus one value, so neither count may be
\ negative. It does NOT check that the address is the address of a whole
\ instruction, or that a branch can reach that far - those are facts about the
\ machine, and src/compiler/native/a64ir.f's own field statement and
\ src/compiler/native/emit.f's reach check are where they belong. This is the
\ SOURCE dialect's table and a second copy of a machine bound here could only
\ drift from the one that decides.
\
\ WHAT THIS ROW DOES NOT PROVE. That the address really is the named word's, and
\ that the arity really is that word's declared effect. Both are the caller's
\ statement today, exactly as a `create`d data word's address is (FIXED-ROW
\ above); reading them off the dictionary record and the checker's own accepted
\ effect is dot habu-resolve-a-callee-0340dfde, and nothing else here changes
\ when it lands.
\ THE GLUE IS THE CALLEE'S RESULT SHAPE AND NOT ITS SIZE. A callee that leaves a
\ value occupying several cells leaves cells the caller may not reorder
\ separately, and the arity says only how many there are. Which of them are one
\ value is a fact of the callee's declared effect, so it travels with the address
\ and the arity rather than being worked out at the call site
\ (dot habu-rename-over-rows-982167af). Zero means nothing the callee leaves is
\ bundled, which is the answer for every one-cell row and the safe reading of a
\ row a declarer had no glue for.
\ AND THE DEADNESS IS THE CALLEE'S CONTROL EFFECT, TRAVELLING THE SAME ROAD FOR
\ THE SAME REASON. Whether control comes back from a call is a fact of the
\ callee, not of the site: `throw`, `die` and every definition whose own paths
\ all end in one have no normal continuation, and a caller that compiled such a
\ call as an ordinary one would go on to make the path it is on meet another
\ one. Zero is "control comes back", which is what every ordinary word answers
\ and the safe reading of a row a declarer had no answer for: it can only cost a
\ refusal where a finer answer would have compiled, never the reverse.
: CALLABLE-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned n n n n n -- )
   {: entry:n in:n out:n glue:n dead:n :}
   entry 0 <= if E-HIR-CALLEE throw then
   in 0 < out 0 < or if E-HIR-CALLEE throw then
   HIR-MEANING:CALLABLE MEAN-CODE
   entry
   in out glue dead
   ROW-ADD ;

\ The row a structured control word writes. It stages no operation of its own -
\ what a control word does is decide which blocks a definition has and which
\ values cross between them - so the only thing a row holds is which control
\ action it is.
: CONTROL-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:ctrl -- )
   {: k:HIR:ctrl :}
   HIR-MEANING:CONTROL MEAN-CODE
   k CTRL-CODE
   UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The row a return-stack transfer writes: which way it moves cells and how many.
\ Like a control row it stages no operation, so those two numbers are the whole
\ payload.
\
\ THE COUNT IS HELD AGAINST WHAT A ROW CAN MEAN, not merely against a ceiling. A
\ transfer of no cells is a word that does nothing and would elaborate to a
\ silent no-op rather than a refusal; a transfer wider than the pair forms the
\ dialect spells is a row no declarer here writes, and letting one exist would
\ let a later reader move cells this file never sanctioned.
2 constant RSTACK-CELLS-MAX          \ `2>r` and its two siblings are the widest forms

: RSTACK-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:rmove n -- )
   {: k:HIR:rmove cells:n :}
   cells 1 < cells RSTACK-CELLS-MAX > or if E-HIR-CLASS throw then
   HIR-MEANING:RSTACK MEAN-CODE
   k RSTACK-CODE
   cells
   UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The row a word with no payload at all writes. The two halves of a typed
\ locals group are the only words of this dialect like that: `{:` starts reading
\ the names that follow and `:}` binds them, and both of those are the
\ elaborator's work over the rows between them, so there is nothing for a row to
\ carry but the meaning. The meaning is held against the two that qualify, so
\ this declarer cannot be used to write an `op` row with no opcode or a `rename`
\ row with no picks.
: PLAIN-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:meaning -- )
   {: m:HIR:meaning :}
   m HIR-MEANING:OPEN-LOCALS HIR-MEANING:EQ
   m HIR-MEANING:CLOSE-LOCALS HIR-MEANING:EQ or
   0= if E-HIR-CLASS throw then
   m MEAN-CODE
   UNUSED UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The same declaration for a module still being built: the builder answers for
\ its own interner. This is how REGISTER-WORDS declares the subset's vocabulary
\ into a module whose symbol rows no caller can hold.
: BDECLARE-PLAIN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:meaning -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id m:HIR:meaning :}
   c r  c b id BKEY-CK  m PLAIN-ROW ;

: BDECLARE-OP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id o:HIR:opcode :}
   c r  c b id BKEY-CK  o OP-ROW ;

: BDECLARE-CONST-OP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id o:HIR:opcode v:n :}
   c r  c b id BKEY-CK  o v CONST-OP-ROW ;

: BDECLARE-CONTROL ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:ctrl -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id k:HIR:ctrl :}
   c r  c b id BKEY-CK  k CONTROL-ROW ;

: BDECLARE-RSTACK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:rmove n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id k:HIR:rmove cells:n :}
   c r  c b id BKEY-CK  k cells RSTACK-ROW ;

public

\ Declare that a source word pushes one fixed value. This is how a `create`d
\ data word or a `constant` enters a definition the chain compiles. It is the
\ builder form and there is no frozen one, because which data words a program
\ mentions is known while its module is being built and never afterwards.
\
\ THE VALUE IS NOT A PARAMETER, AND THAT IS THE WHOLE POINT OF THE WORD. It used
\ to be one, and every caller obtained it by running the word and handing the
\ number over. Two authorities for one fact is one authority too many: the
\ caller's copy goes stale the moment the word is retired and redefined, and a
\ stale address is an ordinary integer that nothing downstream can tell from a
\ live one. So the spelling the module interned is the whole of the declaration
\ and src/compiler/native/dict.f answers it - the same spelling the definition's
\ body writes, resolved in the same order the engine resolves that body, entered
\ the way any word is entered. There is no longer a parameter for anyone to
\ answer wrongly.
: DECLARE-FIXED ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   c b id FIX-NAME FIX-NAME-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   c r  c b id BKEY-CK
   FIX-NAME u NDICT:FIXED-VALUE  FIX-NAME u NDICT:SPELL-FIXED LIT-KIND  FIXED-ROW ;

\ Declare that a source word is another word this definition CALLS: where its
\ code starts and what its declared effect is. This is how a call to a word that
\ is not the one being compiled enters the chain, and it is the builder form for
\ the same reason DECLARE-FIXED is - which words a program calls is a fact about
\ that program and not about the dialect, so it is known while the program's
\ module is being built and never afterwards.
\ A caller that states a callee by hand states no glue and no deadness, and gets
\ neither: its rows read as entirely unbundled and as coming back, which is what
\ every one-cell row is anyway and what this declarer's callers have always
\ compiled against. RESOLVE-CALLABLE below asks the checker instead and states
\ the real answer to both.
: DECLARE-CALLABLE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id entry:n in:n out:n :}
   c r  c b id BKEY-CK  entry in out NDICT:GLUE-NONE COMES-BACK CALLABLE-ROW ;

\ Make a FIXED row for a spelling nobody staged, by asking the engine which
\ definer made it.
\
\ WHY THIS IS ASKED BEFORE THE CALLABLE QUESTION AND NOT INSTEAD OF IT. A name a
\ body writes for a `constant` or a `create`d word denotes a value that was
\ decided when that word was defined, and the only reason it used to compile into
\ a call is that nothing here could tell such a record from an ordinary one. Now
\ the record says, so the question is asked first: a stamped record is never a
\ call, and an unstamped one is never anything but. The two answers cannot both
\ be true of one record, so the order is not a preference between them - it is
\ the cheaper question first.
\
\ NO IS AN ORDINARY ANSWER, exactly as it is below. A spelling too long to ask
\ about, one that denotes nothing here, one whose record no definer stamped, one
\ retired since - all answer false and leave the token to the callable question,
\ which leaves it to be refused by name if it cannot answer either. Nothing here
\ decides that a name is not foldable; it reports that the engine did not say it
\ was.
: RESOLVE-FIXED ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-LEN FIX-NAME-CAP > if false exit then
   c b id FIX-NAME FIX-NAME-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   FIX-NAME u NDICT:SPELL-FIXED {: k:n :}
   k NDICT:FIXED-NONE = if false exit then
   c r  c b id BKEY-CK
   FIX-NAME u NDICT:FIXED-VALUE  k LIT-KIND  FIXED-ROW
   true ;

\ Make that row for a spelling nobody staged, by asking the engine about it.
\
\ WHY THIS IS THE SAME WORD AS THE ONE ABOVE AND NOT A SECOND ROAD. A body that
\ names a word the dialect does not model used to be refused unless its caller
\ had staged the name, the callee's entry address and the callee's arity by hand.
\ All three of those facts belong to the running engine, and the caller obtained
\ them from it a moment earlier: two authorities for one fact, with the caller's
\ copy going stale the instant the callee is retired and redefined, and a stated
\ arity that disagrees with the certified one compiling a routine that moves the
\ wrong number of cells with nothing to refuse it. So the spelling is the whole
\ of the question here too, and the row is built from the engine's own answers -
\ src/compiler/native/dict.f resolves the entry in the order the engine resolves
\ the body that wrote the name, and the arity is the effect the CHECKER accepted
\ for it. There is no parameter left for anyone to answer wrongly.
\
\ NO IS AN ORDINARY ANSWER HERE, and that is the difference from the declarers
\ above. They serve a caller that has already decided a word belongs in the
\ table; this serves the elaborator meeting a token it has no opinion about yet,
\ so every way the engine can fail to answer - a spelling too long to be asked
\ about, one that denotes no word in this scope, one the checker certified no
\ effect for, one whose certified effect has a term whose width cannot be stated,
\ and one whose certified effect MOVES THE CALLER'S RETURN STACK - answers false
\ and leaves the token to be refused as unmodeled, by name, with the capability it
\ is waiting for recorded. It never answers a row it guessed.
\
\ THE RETURN-STACK CLAUSE IS THE ONE THAT IS NOT ABOUT MISSING INFORMATION. The
\ other four are the engine declining to say; this one is the engine saying
\ something the elaborator cannot honour. Its return stack is a compile-time
\ vector and a call has nowhere to put a callee's motion of it, so the row is
\ refused rather than built - src/compiler/native/dict.f SPELL-RET-NEUTRAL? gives
\ the whole argument, and the checker publishes the answer because it is the only
\ authority on what a signature's return rows say.
: RESOLVE-CALLABLE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-LEN FIX-NAME-CAP > if false exit then
   c b id FIX-NAME FIX-NAME-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   FIX-NAME u NDICT:CALL-TARGET {: entry:n :}
   entry 0= if false exit then
   FIX-NAME u NDICT:SPELL-ARITY {: in:n out:n :}
   in NDICT:ARITY-NONE = if false exit then
   FIX-NAME u NDICT:SPELL-RET-NEUTRAL? 0= if false exit then
   FIX-NAME u NDICT:SPELL-GLUE nip {: glue:n :}   \ the callee's RESULT cells are the caller's concern
   FIX-NAME u NDICT:SPELL-DEAD? {: dead:bool :}
   c r  c b id BKEY-CK  entry in out glue  dead NORET-CODE  CALLABLE-ROW
   true ;

\ Declare that a source word elaborates to one operation of this dialect. The
\ arena pair is this table's rows and the module's symbol rows: the second is
\ the interner that has to have minted the word's spelling.
: DECLARE-OP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id o:HIR:opcode :}
   c r  sy id SYM-CK  o OP-ROW ;

\ Declare a named boundary this dialect cannot compile. The reason symbol names
\ the capability whose absence is why, so a refusal can say what has to land
\ before the boundary can be retired. Both symbols are the module's, so both are
\ asked of its interner.
: DECLARE-UNMODELED ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id why:IR-ID:ir-symbol-id :}
   sy why SYM-CK drop
   r why SYM-OWNER-CK
   c r  sy id SYM-CK
   HIR-MEANING:UNMODELED MEAN-CODE
   why IR-ID:SYMBOL-LOCAL 1+
   UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

private

\ ---- the staged rename -------------------------------------------------------
\ One package-owned stage under the single-task compilation discipline, the same
\ protocol IR-TYPE, IR-ATTR and IR-SCHEMA use: a begin opens it, the picks fill
\ it, and the end validates and appends. Any end consumes the stage whatever its
\ outcome, so no half-staged rename survives into the next declaration.
0 constant MODE-NONE
1 constant MODE-OPEN

here CELL 1- and CELL swap - CELL 1- and allot
variable STG-MODE
MODE-NONE STG-MODE !
variable STG-IN
variable STG-N
create STG-PICK PICK-MAX cells allot

: STG-OPEN-CK ( -- )
   STG-MODE @ MODE-OPEN <> if E-HIR-STAGE throw then ;

: STG-TAKE ( -- )
   STG-MODE @ {: have:n :}
   MODE-NONE STG-MODE !
   have MODE-OPEN <> if E-HIR-STAGE throw then ;

: SP@ ( n -- n )
   cells STG-PICK + @ ;

: SP! ( n n -- )
   cells STG-PICK + ! ;

\ The row a rename writes, once its stage is closed and its symbol has been
\ answered for. The picks land in the pool before the row that points at them,
\ so a refused declaration leaves the table without a row that names cells
\ outside it.
: RENAME-ROW ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena HIR-WORD:interned -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena r:IR-ARENA:arena w:HIR-WORD:interned :}
   w HIR--WORD-INTERNED:UNMAKE {: id:IR-ID:ir-symbol-id :}
   p r PAIR-CK
   r id SYM-OWNER-CK
   id IR-ID:SYMBOL-LOCAL {: so:n :}
   r so FIND 0 < 0= if E-HIR-DUP throw then
   r ROW-ROOM-CK
   p STG-N @ POOL-ROOM-CK
   p PCELLS {: st:n :}
   STG-N @ 0 ?do
      c p i SP@ IR-ARENA:PUSH drop
   loop
   c r w
   HIR-MEANING:RENAME MEAN-CODE
   st STG-IN @ STG-N @ UNUSED UNUSED
   ROW-ADD ;

\ The same declaration for a module still being built. The stage is consumed
\ first either way, so a symbol the interner refuses leaves no rename open.
: BDECLARE-RENAME ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   STG-TAKE
   c p r  c b id BKEY-CK  RENAME-ROW ;

public

\ Open a rename that consumes `in` values off the top of the compile-time value
\ vector.
: BEGIN-RENAME ( n -- )
   {: in:n :}
   STG-MODE @ MODE-NONE <> if E-HIR-STAGE throw then
   in 0 < in INPUT-MAX > or if E-HIR-PICK throw then
   MODE-OPEN STG-MODE !
   in STG-IN !
   0 STG-N ! ;

\ Put one of the consumed values back, named by its depth in the consumed
\ window with zero being the top. Picks are listed bottom first.
: ADD-PICK ( n -- )
   {: d:n :}
   STG-OPEN-CK
   d 0 < d STG-IN @ >= or if E-HIR-PICK throw then
   STG-N @ {: n:n :}
   n PICK-MAX >= if E-HIR-PICK throw then
   d n SP!
   n 1+ STG-N ! ;

\ Abandon an open rename without declaring it.
: ABANDON-RENAME ( -- )
   STG-TAKE ;

\ Close the staged rename and bind it to a source word. The arenas are this
\ table's pick pool, its rows, and the module's symbol rows; the stage is
\ consumed before anything else, so a refusal of any kind leaves no rename open.
: DECLARE-RENAME ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena r:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   STG-TAKE
   c p r  sy id SYM-CK  RENAME-ROW ;

\ ---- reading -----------------------------------------------------------------
: MODELED ( IR-ARENA:arena -- n )
   dup RHDR-CK CNT ;

private

: ROW-OF ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id SYM-OWNER-CK
   r id IR-ID:SYMBOL-LOCAL FIND
   dup 0 < if E-HIR-UNMODELED throw then ;

\ The row of a word this table models with the meaning the caller is about to
\ read. Asking a rename for its opcode, or an op for its picks, is a category
\ error rather than a missing value.
: ROW-AS ( IR-ARENA:arena IR-ID:ir-symbol-id HIR:meaning -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id want:HIR:meaning :}
   r id ROW-OF {: l:n :}
   r l OFF-MEAN RC@ N>MEAN want HIR-MEANING:EQ
   0= if E-HIR-CLASS throw then
   l ;

public

\ What this table says a word means. A word it never declared has no meaning at
\ all, which is the same refusal a declared boundary gets.
: MEANING@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:meaning )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id ROW-OF {: l:n :}
   r l OFF-MEAN RC@ N>MEAN ;

\ Whether this table models the word at all, asked without being refused. Every
\ other reader here treats an undeclared word as an error, which is right when
\ the answer is about to be used; this one exists because the elaborator has to
\ ask a question no other caller asks - whether a name the PROGRAM chose for a
\ `{: … :}` local collides with a word of the dialect - and the answer "no" is
\ the ordinary case rather than a failure.
: MODELS? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id SYM-OWNER-CK
   r id IR-ID:SYMBOL-LOCAL FIND 0 >= ;

\ The bare name inside one typed local's declaration spelling. A declaration
\ reads `name:type`, and the tape carries the whole of it as one token - proved
\ by test/compiler/native-feed.f, which records `{: a:n b:n t:n :}` off the
\ engine's own reader - while the body reads the name alone. So the annotation
\ has to be cut off somewhere, and it is cut off here: this file is the one that
\ knows how a source word of this dialect is spelled, and the elaborator holds
\ no spelling of its own. An unannotated local is a real shape too (`{: a b :}`
\ is ordinary Habu), and its whole spelling is its name.
$3A constant ANN-C                   \ the `:` that separates a local from its type

: LOCAL-NAME-LEN ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u
   u 0 ?do
      a i + c@ ANN-C = if drop i leave then
   loop ;

\ The gate. Answers what checked source may compile this word into, and refuses
\ everything else by name.
: ADMIT ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:meaning )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:UNMODELED HIR-MEANING:EQ if E-HIR-UNMODELED throw then
   m ;

: OPCODE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:opcode )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:OP ROW-AS {: l:n :}
   r l OFF-A RC@ N>OPCODE ;

\ The operation a constant-and-operation word applies, and the constant it
\ applies it with. Asking one of them about a word of any other meaning is a
\ category error rather than a missing value, which is ROW-AS's rule.
: CONST-OPCODE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:opcode )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CONST-OP ROW-AS {: l:n :}
   r l OFF-A RC@ N>OPCODE ;

: CONST-VALUE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CONST-OP ROW-AS {: l:n :}
   r l OFF-IN RC@ ;

\ The value a word that pushes one fixed value pushes. Asking it about a word of
\ any other meaning is a category error rather than a missing value, which is
\ ROW-AS's rule.
: FIXED-VALUE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:FIXED ROW-AS {: l:n :}
   r l OFF-IN RC@ ;

\ And what that value IS, which the site staging it needs before the number is
\ just a number: an address of the DATA region, or an ordinary integer.
: FIXED-KIND@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:FIXED ROW-AS {: l:n :}
   r l OFF-A RC@ ;

\ Where a callable word's code starts, and its declared effect. Each is asked of
\ a row that carries that meaning, which is ROW-AS's rule: asking a rename for an
\ entry address is a category error rather than a missing value.
: ENTRY@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: l:n :}
   r l OFF-A RC@ ;

: CALLEE-IN@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: l:n :}
   r l OFF-IN RC@ ;

: CALLEE-OUT@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: l:n :}
   r l OFF-N RC@ ;

\ Whether control comes back from a call to this callee. The one reader of the
\ fact RESOLVE-CALLABLE put in the row, so every pass that has to know - the
\ block count, the walk, the tail decision - asks one question and gets one
\ answer, instead of each asking the engine again and risking three.
: CALLEE-DEAD? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: l:n :}
   r l OFF-DEAD RC@ NO-RETURN = ;

\ Which control action a structured control word is.
: CTRL@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:ctrl )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CONTROL ROW-AS {: l:n :}
   r l OFF-A RC@ N>CTRL ;

\ Which way a return-stack word moves cells, and how many. Both go through
\ ROW-AS, so asking either of a row of another meaning is a category error rather
\ than a number read out of a cell that means something else.
: RSTACK@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:rmove )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RSTACK ROW-AS {: l:n :}
   r l OFF-A RC@ N>RSTACK ;

: RSTACK-CELLS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RSTACK ROW-AS {: l:n :}
   r l OFF-IN RC@ ;

\ Which of a callee's result cells belong to a multi-cell value, as the bitmask
\ src/compiler/native/dict.f builds: bit i for the i-th cell from the bottom of
\ the row, which is the order they reach the caller's value vector.
: OUT-GLUE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: l:n :}
   r l OFF-GLUE RC@ ;

\ How many values a rename consumes off the top of the value vector.
: INPUTS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RENAME ROW-AS {: l:n :}
   r l OFF-IN RC@ ;

\ How many values it puts back.
: PICKS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RENAME ROW-AS {: l:n :}
   r l OFF-N RC@ ;

\ The i-th value it puts back, named by its depth in the consumed window.
\ Bottom first. Both the window into the pool and the depth are rechecked, so a
\ row written past this package's declarers cannot read a cell outside the live
\ pool or name a value the rename never consumed.
: PICK@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: p:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id i:n :}
   p r PAIR-CK
   r id HIR-MEANING:RENAME ROW-AS {: l:n :}
   r l OFF-N RC@ {: n:n :}
   i 0 < i n >= or if E-HIR-BOUND throw then
   r l OFF-A RC@ {: st:n :}
   st 0 < st n + p PCELLS > or if E-HIR-STATE throw then
   p st i + PC@ {: d:n :}
   d 0 < d r l OFF-IN RC@ >= or if E-HIR-PICK throw then
   d ;

\ The capability a boundary is waiting for. Only an unmodeled entry names one.
: REASON@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-symbol-id :}
   r key KEY-CK
   r id HIR-MEANING:UNMODELED ROW-AS {: l:n :}
   r l OFF-A RC@
   dup UNUSED = if E-HIR-STATE throw then
   1- key swap IR-ID:PACK-SYMBOL ;

\ The i-th declared word, in declaration order. This is what an inventory of the
\ remaining boundaries walks.
: AT ( IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key i:n :}
   r key KEY-CK
   i 0 < if E-HIR-BOUND throw then
   i r CNT >= if E-HIR-BOUND throw then
   key r i OFF-SYM RC@ IR-ID:PACK-SYMBOL ;

\ ---- the tape join -----------------------------------------------------------
\ The elaborator walks a sealed source tape and asks, token by token, what this
\ dialect makes of it. A literal is a literal whatever any table says, because
\ its kind is what makes it one, and the tape has two literal kinds this dialect
\ models - an integer and a double - which are two meanings because they stage
\ two different operations. A name is looked up by its key. A character or
\ string literal is a kind the straight-line subset does not model at all, and is
\ refused as such rather than resolved as a name.
\
\ THE KEY IS PRESENTED RATHER THAN DERIVED, because deriving it needs the
\ module's interner and this join is also asked of a module that has been frozen
\ - the caller holds whichever of the two the module still has. It is the token's
\ key, which is KEY-SYM of the symbol the tape recorded for row `i`; the two
\ named-symbol forms in src/compiler/native/elaborate.f take a symbol beside a
\ token for the same reason, so this is the shape its neighbours already have.
: ADMIT-TOKEN ( IR-ARENA:view IR-ARENA:arena n IR-ID:ir-symbol-id -- HIR:meaning )
   {: v:IR-ARENA:view r:IR-ARENA:arena i:n sy:IR-ID:ir-symbol-id :}
   v i NTAPE:KIND@ {: k:NTAPE:kind :}
   k NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if HIR-MEANING:LITERAL exit then
   k NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if HIR-MEANING:REAL-LITERAL exit then
   k NTAPE-KIND:STRING-LITERAL NTAPE-KIND:EQ if HIR-MEANING:STRING-LITERAL exit then
   k NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-HIR-KIND throw then
   r sy ADMIT ;

\ ---- the subset's vocabulary -------------------------------------------------
\ The words the straight-line subset models, and the exact ceilings they need,
\ so a caller commits a table to what this registration writes and not to a
\ guess. The pick cells are the picks the eight renames put back, added up:
\ four for `2dup`, two for `dup`, none for `drop`, two for `swap`, three for
\ `over`, one for `nip`, three for `rot` and none for `2drop`. A `{: … :}` group
\ adds two words and no picks: its halves stage nothing and the names between
\ them are the program's, so they never become rows of this table. The three
\ tag-dispatch forms add seven more words and no picks, for the same reason: what
\ a family or variant token means is the registry's answer and never a row here.
\ The two halves of a quotation add two more words and no picks, and for a third
\ statement of the same rule: what stands between them is another FUNCTION's
\ tokens, so none of them is a row of this table either. `is` and `execute` add
\ two more and no picks: each stages one call, and the one thing a pick cell
\ could record - how the compile-time vector is permuted - is not something
\ either of them does. The counted loop has two openers and adds one more word
\ and no picks: `do` and `?do` open the same structure and differ only in the
\ code the engine emits for them, and neither moves anything on the vector that
\ the other does not. `again` and `leave` add one word each and no picks: `again`
\ closes a `begin` loop with a back edge and `leave` branches out of the
\ innermost counted loop, and neither takes anything off the compile-time vector
\ or puts anything back on it. `catch` adds one more word and no picks, for the
\ same reason `execute` does: it stages one call, and what it moves on the
\ compile-time vector is the window the checker certified at that site rather
\ than a permutation this table could record.
\ The return-stack transfers add SIX and no picks: three actions at two widths
\ each, and a pick cell records how the compile-time DATA vector is permuted,
\ which is not what any of them does - what they move, they move between two
\ vectors, and the row says which way and how many cells rather than which
\ position went where.
82 constant WORDS
15 constant PICK-CELLS

private

\ The four arithmetic words this dialect has operations for.
: DEF-ARITH ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" +" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:ADD BDECLARE-OP
   c b r c b s" -" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:SUB BDECLARE-OP
   c b r c b s" *" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:MUL BDECLARE-OP
   c b r c b s" /" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:DIV BDECLARE-OP ;

\ The six comparisons, each bound to the opcode that names its own relation.
\ `>` is not `<` with the operands turned round and `<>` is not `=` inverted:
\ a row says which opcode a word means and nothing else, so a relation the
\ dialect has no opcode for could not be written down here at all.
: DEF-COMPARE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" <" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:LT BDECLARE-OP
   c b r c b s" <=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:LE BDECLARE-OP
   c b r c b s" >" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:GT BDECLARE-OP
   c b r c b s" >=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:GE BDECLARE-OP
   c b r c b s" =" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:EQUAL BDECLARE-OP
   c b r c b s" <>" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:NE BDECLARE-OP ;

\ The bitwise words. `and`, `or` and `xor` combine two values bit for bit;
\ `lshift` and `rshift` move one value by a count the program computed, which is
\ why they are two-operand words here and not a value and a field; `invert` is
\ the one unary operation of the subset.
: DEF-BITWISE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" and" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:AND BDECLARE-OP
   c b r c b s" or" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:OR BDECLARE-OP
   c b r c b s" xor" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:XOR BDECLARE-OP
   c b r c b s" lshift" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:LSHIFT BDECLARE-OP
   c b r c b s" rshift" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:RSHIFT BDECLARE-OP
   c b r c b s" invert" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:INVERT BDECLARE-OP ;

\ `1-` ( n -- n ) and `1+` ( n -- n ): subtract or add one. Each is one token of
\ source and two operations of this dialect, and the row says exactly that
\ rather than claiming an increment or decrement opcode the dialect does not
\ have.
\
\ `0=` ( n -- bool ) and `cells` ( n -- n ) are the same shape with other
\ numbers. `0=` is `0` then `=`: the engine's own `0=` compares its argument
\ against zero and answers a Habu flag, so it answers false for EVERY nonzero
\ value and not only for a flag - which is exactly what an equality against the
\ literal zero computes, and is why this is a constant-and-operation row rather
\ than a complement. `cells` is `8` then `*`, one cell being eight bytes; the
\ engine shifts left by three, and multiplying by eight is the same function of
\ the same argument on every bit pattern.
: DEF-STEP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" 1-" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:SUB 1 BDECLARE-CONST-OP
   c b r c b s" 1+" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:ADD 1 BDECLARE-CONST-OP
   c b r c b s" 0=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:EQUAL 0 BDECLARE-CONST-OP
   c b r c b s" cells" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:MUL 8 BDECLARE-CONST-OP ;

\ The four memory words, two per width. `@` ( ptr -- n ) reads the cell an
\ address names and `!` ( n ptr -- ) writes one; `c@` ( ptr -- n ) reads the
\ BYTE an address names and `c!` ( n ptr -- ) writes one. The order they happen
\ in is the memory order the dialect's own token carries, and
\ src/compiler/native/elaborate.f threads it, so nothing about it is stored in
\ these rows. The width is not stored either: it is which opcode the row names,
\ because src/compiler/native/hir.f makes the width a form.
: DEF-MEMORY ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" @" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:LOAD BDECLARE-OP
   c b r c b s" !" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:STORE BDECLARE-OP
   c b r c b s" c@" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:BLOAD BDECLARE-OP
   c b r c b s" c!" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:BSTORE BDECLARE-OP ;

\ The nine float words of the engine's vocabulary that compute rather than
\ compare. src/habu/habu1.f EMIT-FP-PRIMS publishes fifteen; f. is a decimal
\ printer and no part of the arithmetic, and the five comparisons are declared
\ beside these in DEF-FCOMPARE below. These nine are one operation each and one
\ row each.
\
\ THE TWO CONVERSIONS ARE TWO ROWS BECAUSE THEY ARE TWO ROUNDINGS. `s>f` rounds
\ to nearest with ties to even and is exact up to 2^53; `f>s` truncates toward
\ zero, saturates at the ends rather than wrapping, and answers zero for a NaN.
\ The survey at the head of tools/codegen-compare-corpus3.f measures both on this
\ engine, and the machine forms the dialect lowers them to are the instructions
\ that behave that way, so the rounding is the hardware's and not a rule stated
\ here.
: DEF-FLOAT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" f+" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FADD BDECLARE-OP
   c b r c b s" f-" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FSUB BDECLARE-OP
   c b r c b s" f*" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FMUL BDECLARE-OP
   c b r c b s" f/" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FDIV BDECLARE-OP
   c b r c b s" fnegate" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FNEG BDECLARE-OP
   c b r c b s" fabs" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FABS BDECLARE-OP
   c b r c b s" fsqrt" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FSQRT BDECLARE-OP
   c b r c b s" s>f" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:INTREAL BDECLARE-OP
   c b r c b s" f>s" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:REALINT BDECLARE-OP ;

\ The five float comparisons, which are the whole of what the engine has: three
\ that take two doubles and two that take one and compare it against zero. There
\ is no `f<=`, no `f>=` and no float inequality in the engine's vocabulary, so
\ there is no row for one - a row here is a source word a program can write, and
\ a row for a word that does not exist would be a promise.
\
\ THE TWO AGAINST ZERO ARE TWO ROWS AND NOT `f<` WITH A LITERAL, because they are
\ two OPERATIONS: FCMP against the immediate zero is one instruction and takes no
\ second register, which is what the engine's own `f0<` and `f0=` emit. A row
\ that pointed `f0<` at `hir.flt` would need a materialised zero the instruction
\ does not use.
: DEF-FCOMPARE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" f<" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FLT BDECLARE-OP
   c b r c b s" f>" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FGT BDECLARE-OP
   c b r c b s" f=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FEQ BDECLARE-OP
   c b r c b s" f0<" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FLTZ BDECLARE-OP
   c b r c b s" f0=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FEQZ BDECLARE-OP ;

\ The structured control words. Three structures, the two words that stand in
\ the middle of one, the loop index, the word that drops a loop frame, the two
\ words that leave from the middle - one the innermost counted loop and one the
\ definition - and
\ `RECURSE`; nothing else of Habu's control vocabulary is declared, because
\ nothing else has a block construction in src/compiler/native/elaborate.f yet,
\ and a word declared here without one would be a promise rather than a model.
\
\ `begin` HAS THREE CLOSERS, WHICH IS THE SOURCE LANGUAGE'S SHAPE AND NOT A
\ CHOICE MADE HERE. `begin … until` goes round while its test is false,
\ `begin … while … repeat` goes round while its test is true and leaves through
\ the `while`, and `begin … again` goes round unconditionally and never leaves at
\ all; all three open with the same word, so the row for `begin` says only that a
\ loop opens and the elaborator's control stack learns which closer it met.
\ `else` is the same kind of fact for `if`.
\
\ THE COUNTED LOOP IS THE MIRROR OF THAT: TWO OPENERS AND ONE CLOSER. `do` and
\ `?do` take the same pair and close with the same `loop`, and the row is what
\ tells them apart, because the engine emits different code for them - J-?DO is
\ J-DO with a comparison and a branch out in front (src/habu/habu2.f). So the
\ two rows differ and the FRAME both openers push does not: the elaborator's
\ control stack records the structure, which is one counted loop either way, and
\ `loop` closes it without having to know which word opened it.
\
\ `RECURSE` IS SPELLED IN UPPER CASE, WHICH IS NOT AN EXCEPTION TO THE RULE ABOVE.
\ The rule is that this table interns each word exactly as `docs/forth.md` spells
\ it, and § "RECURSE uses the declared effect" spells this one in capitals - so
\ the row and the source agree by following one authority, not by two guesses
\ landing on the same bytes. Which case is written here decides nothing about
\ which case a body may write: the row is keyed by the spelling's fold, so
\ `RECURSE` and `recurse` reach it alike, exactly as they reach the same word in
\ the engine.
: DEF-CONTROL ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" if" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-IF BDECLARE-CONTROL
   c b r c b s" else" IR-BUILD:INTERN-SYMBOL HIR-CTRL:MID-ELSE BDECLARE-CONTROL
   c b r c b s" then" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-IF BDECLARE-CONTROL
   c b r c b s" begin" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-BEGIN BDECLARE-CONTROL
   c b r c b s" while" IR-BUILD:INTERN-SYMBOL HIR-CTRL:MID-WHILE BDECLARE-CONTROL
   c b r c b s" until" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-UNTIL BDECLARE-CONTROL
   c b r c b s" repeat" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-REPEAT BDECLARE-CONTROL
   c b r c b s" again" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-AGAIN BDECLARE-CONTROL
   c b r c b s" do" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-DO BDECLARE-CONTROL
   c b r c b s" ?do" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-DO-SKIP BDECLARE-CONTROL
   c b r c b s" loop" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-LOOP BDECLARE-CONTROL
   c b r c b s" i" IR-BUILD:INTERN-SYMBOL HIR-CTRL:INDEX BDECLARE-CONTROL
   c b r c b s" unloop" IR-BUILD:INTERN-SYMBOL HIR-CTRL:DROP-LOOP BDECLARE-CONTROL
   c b r c b s" leave" IR-BUILD:INTERN-SYMBOL HIR-CTRL:EARLY-LEAVE BDECLARE-CONTROL
   c b r c b s" exit" IR-BUILD:INTERN-SYMBOL HIR-CTRL:EARLY-EXIT BDECLARE-CONTROL
   c b r c b s" RECURSE" IR-BUILD:INTERN-SYMBOL HIR-CTRL:SELF-CALL BDECLARE-CONTROL ;

\ The three tag-dispatch forms, seven words. `of` and `endof` are ONE row each
\ and serve both `MATCH` and `case`, exactly as they do in the engine and in the
\ checker; which form an arm belongs to is decided by the structure the
\ elaborator has open, never by the token.
\
\ EVERY ONE OF THEM IS SPELLED IN LOWER CASE HERE AND MATCHES IN ANY CASE, and
\ that is the same rule the rest of this table follows for the same reason. A row
\ is keyed by the fold of its spelling (BKEY-CK) and a token is looked up under
\ the fold of its own (KEY-SYM), and the fold is the one the ENGINE applies when
\ it recognises a keyword (src/habu/habu2.f LKWCMP: a byte in `A`..`Z` gets $20
\ set, every other byte stands) and the one the CHECKER applies before it
\ compares a token against `match`, `of`, `endof` or `;match` (src/core/checker.f
\ DO-TOK1 folds into TKF, and MATCH-VARIANT-TOK compares the folded token). So
\ `MATCH` and `match` reach one row here because they reach one keyword there,
\ and there is no second rule for a body to fall between.
: DEF-ADT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" match" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-MATCH BDECLARE-CONTROL
   c b r c b s" of" IR-BUILD:INTERN-SYMBOL HIR-CTRL:MATCH-ARM BDECLARE-CONTROL
   c b r c b s" endof" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-ARM BDECLARE-CONTROL
   c b r c b s" ;match" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-MATCH BDECLARE-CONTROL
   c b r c b s" case" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-CASE BDECLARE-CONTROL
   c b r c b s" endcase" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-CASE BDECLARE-CONTROL
   c b r c b s" construct" IR-BUILD:INTERN-SYMBOL HIR-CTRL:MAKE-BUNDLE BDECLARE-CONTROL ;

\ The two tokens a quotation is written with. They are control actions and not
\ operations for the reason HIR's own ctrl family gives: what stands between them
\ is a second FUNCTION of the module, so the opener's whole job is to stage one
\ value and take the tokens up to its closer out of the enclosing body's hands.
\ Neither row carries a payload - which function the body becomes is decided by
\ the elaborator, and what the body TAKES is decided by whoever consumes the
\ value, neither of which is a fact about the dialect.
: DEF-QUOT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" [:" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-QUOT BDECLARE-CONTROL
   c b r c b s" ;]" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-QUOT BDECLARE-CONTROL ;

\ The three words a program uses a quotation with. None carries a payload, and
\ that is the whole reason they are control rows rather than callable ones: a
\ callable row states its callee's entry AND its declared arity, and none of
\ these has an arity to state - `is` moves what the DEFERRED WORD declares, and
\ the token after it names which; `execute` moves one cell more than whatever
\ quotation reaches it, which is a fact about the site; and `catch` moves the
\ window that quotation takes, which the checker certified at the site and
\ nowhere else. Where each branches to is a question for
\ src/compiler/native/dict.f at the site, exactly as every other callee's entry
\ is, so no address is recorded here either.
: DEF-QUOT-USE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" is" IR-BUILD:INTERN-SYMBOL HIR-CTRL:BIND-DEFER BDECLARE-CONTROL
   c b r c b s" execute" IR-BUILD:INTERN-SYMBOL HIR-CTRL:EXEC BDECLARE-CONTROL
   c b r c b s" catch" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CATCH BDECLARE-CONTROL ;

\ The two halves of a typed locals group. Neither stages an operation and
\ neither carries a payload: what the opener does is start reading the names
\ that follow it and what the closer does is bind them, and both of those are
\ the elaborator's work over the tape rows between them. The row therefore holds
\ the meaning and nothing else.
: DEF-LOCALS ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" {:" IR-BUILD:INTERN-SYMBOL HIR-MEANING:OPEN-LOCALS BDECLARE-PLAIN
   c b r c b s" :}" IR-BUILD:INTERN-SYMBOL HIR-MEANING:CLOSE-LOCALS BDECLARE-PLAIN ;

\ dup ( a -- a a ): consume the top value and put it back twice.
: DEF-DUP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   1 BEGIN-RENAME
   0 ADD-PICK
   0 ADD-PICK
   c b p r c b s" dup" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ drop ( a -- ): consume the top value and put nothing back.
: DEF-DROP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   1 BEGIN-RENAME
   c b p r c b s" drop" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ swap ( a b -- b a ): consume two and put them back the other way round.
: DEF-SWAP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   0 ADD-PICK
   1 ADD-PICK
   c b p r c b s" swap" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ over ( a b -- a b a ): consume two and put back three, the lower one twice.
: DEF-OVER ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   1 ADD-PICK
   c b p r c b s" over" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ nip ( a b -- b ): consume two and put back only the one that was on top.
: DEF-NIP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   0 ADD-PICK
   c b p r c b s" nip" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ 2dup ( a b -- a b a b ): consume two and put both back twice, in order. It is
\ `over over` written once, and the corpus's two-way branch reads its two
\ arguments with it.
: DEF-2DUP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   1 ADD-PICK
   0 ADD-PICK
   c b p r c b s" 2dup" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ 2drop ( a b -- ): consume two and put neither back. It is `drop drop` written
\ once, and it is a rename for the same reason `drop` is - the two values simply
\ leave the compile-time vector and no instruction is needed to make them go.
: DEF-2DROP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   c b p r c b s" 2drop" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ rot ( a b c -- b c a ): consume three and put all three back rotated, so the
\ deepest of them ends on top. Bottom first that is b, then c, then a, whose
\ depths in the consumed window are 1, 0 and 2 - the derivation at the head of
\ this file.
: DEF-ROT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   3 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   2 ADD-PICK
   c b p r c b s" rot" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ ---- the return-stack words --------------------------------------------------
\ >r r> r@ and their two-cell forms. Six rows over three actions and two widths,
\ and the widths are declared rather than derived because `2>r` is its own source
\ word: a body that spells it moves the pair in one step, and a table that only
\ knew `>r` would have to decide that two of them are the same thing, which is a
\ rule about the SOURCE and not about the transfer.
\
\ THE PAIR FORMS PRESERVE ORDER, which is the one thing about them that is not
\ obvious and is the elaborator's business rather than this table's. `2>r` moves
\ the top two cells so that the LOWER one stays lower on the return stack, so
\ `2r>` puts them back the way they came; the row says only "two cells, this
\ direction", and src/compiler/native/elaborate.f is where that order is kept.
: DEF-RSTACK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" >r"  IR-BUILD:INTERN-SYMBOL HIR-RMOVE:TO-R    1 BDECLARE-RSTACK
   c b r c b s" r>"  IR-BUILD:INTERN-SYMBOL HIR-RMOVE:FROM-R  1 BDECLARE-RSTACK
   c b r c b s" r@"  IR-BUILD:INTERN-SYMBOL HIR-RMOVE:FETCH-R 1 BDECLARE-RSTACK
   c b r c b s" 2>r" IR-BUILD:INTERN-SYMBOL HIR-RMOVE:TO-R    2 BDECLARE-RSTACK
   c b r c b s" 2r>" IR-BUILD:INTERN-SYMBOL HIR-RMOVE:FROM-R  2 BDECLARE-RSTACK
   c b r c b s" 2r@" IR-BUILD:INTERN-SYMBOL HIR-RMOVE:FETCH-R 2 BDECLARE-RSTACK ;

public

\ Declare the whole straight-line source vocabulary into one word model: the
\ arithmetic, comparison and bitwise words this dialect has operations for, the
\ four step words that are a literal and an operation, the four memory words,
\ the structured control words, the two halves of a typed locals group, the
\ stack words that only rename values, and the return-stack transfers.
\ The builder is the module's symbol
\ interner, so the spellings become identities of the same module the table is
\ bound to. A `create`d data word is NOT here, and neither is a word this
\ definition calls: which data words exist and which words a program calls are
\ facts about the program being compiled and not about the dialect, so a caller
\ declares them with DECLARE-FIXED and DECLARE-CALLABLE and commits the table to
\ the extra rows.
: REGISTER-WORDS ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b r DEF-ARITH
   c b r DEF-COMPARE
   c b r DEF-BITWISE
   c b r DEF-STEP
   c b r DEF-MEMORY
   c b r DEF-FLOAT
   c b r DEF-FCOMPARE
   c b r DEF-CONTROL
   c b r DEF-ADT
   c b r DEF-LOCALS
   c b r DEF-QUOT
   c b r DEF-QUOT-USE
   c b p r DEF-2DUP
   c b p r DEF-DUP
   c b p r DEF-DROP
   c b p r DEF-SWAP
   c b p r DEF-OVER
   c b p r DEF-NIP
   c b p r DEF-ROT
   c b p r DEF-2DROP
   c b r DEF-RSTACK ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
