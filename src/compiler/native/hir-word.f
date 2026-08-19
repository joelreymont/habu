\ hir-word.f - the straight-line HIR dialect's source-word model: what a Habu
\ word means to the elaborator, and which words it refuses to compile at all.
\
\ A rename is DATA: a row records how many values the word consumes off the top
\ and which of them it puts back. An input is named by its DEPTH in the consumed
\ window, zero being the top, and picks are listed BOTTOM first - so `over`
\ ( a b -- a b a ) consumes 2 and puts back 1 0 1, and `rot` ( a b c -- b c a )
\ consumes 3 and puts back 1 0 2. It may only put back an input it consumed.
\
\ Every row is keyed by the FOLD of its word's spelling, which is the fold the
\ engine applies when it decides what a token of a checked body means, so `IF`,
\ `if` and `If` reach one row. A `{: … :}` local is NOT folded, because the
\ engine's own local lookup compares those bytes raw.
\
\ `literal`, `real-literal` and `string-literal` belong to a TAPE TOKEN and
\ never to a word, so no row ever stores one.
\
\ A row stores the stable code of a HIR:opcode, so binding a word to an
\ operation this dialect does not have is unwritable rather than a check.

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

\ Owning the right module is not the same as existing: an identity is arithmetic
\ away from any other, so a row could name an ordinal the interner never minted.
\ A symbol this module's interner has answered for. Owning the right module is
STRUCTURE interned 0
   FIELD sym IR-ID:ir-symbol-id
;STRUCTURE

private

CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

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

\ The deepest classical rename is `2over`, four in and six back; a rename that
\ wants more is a capability to add here, not a value to widen silently.
\ The deepest classical Forth stack rename is `2over` ( a b c d -- a b c d a b ):
4 constant INPUT-MAX
8 constant PICK-MAX

\ Stable stored codes with exact decoders, so a row written past this package's
\ declarers cannot decode as some other meaning. COMES-BACK is zero so an unset
\ payload cell reads as "control comes back", which is the safe direction.
\ ---- stored codes ------------------------------------------------------------
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

\ The three token meanings are deliberately absent: a row claiming one is corrupt.
\ Codes zero, ten and eleven - `literal`, `real-literal` and `string-literal` -
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

\ A stable code per member and an exact decoder.
\ The control actions, under the same discipline: a stable stored code per
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
      outer-index  OF 28 ENDOF
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
      28 of HIR-CTRL:OUTER-INDEX endof
      E-HIR-CONTROL throw
   endcase ;

\ The codes start at zero because they are read out of a payload cell this
\ meaning owns outright.
\ The return-stack transfers, under the discipline the two above are under. The
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

\ Each arena rechecks its own header tag, so a pair swapped at a call site dies
\ on the tag instead of reading a foreign row.
\ The two arenas of this table and the module's other arenas all have the same
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

\ A Habu name is case-insensitive to the ENGINE, so both sides of the comparison
\ use one fold: a row is written under BKEY-CK's and a token asked under KEY-SYM's.
\ The tape's own symbol still holds the bytes the source wrote.
\ ---- the key a spelling has in this table ------------------------------------
$41 constant KEY-A                   \ the first byte the fold moves
$5A constant KEY-Z                   \ and the last
$20 constant KEY-BIT                 \ the bit it sets

\ A longer spelling is answered unfolded rather than truncated into a name that
\ denotes some other word.
\ The longest spelling this table can key. It is the ceiling the declarers that
64 constant KEY-CAP

create KEY-BUF KEY-CAP allot

: FOLD-C ( n -- n )
   {: b:n :}
   b KEY-A < if b exit then
   b KEY-Z > if b exit then
   b KEY-BIT or ;

\ The interner answers ONE identity per byte string, so this is a shortcut and
\ not a second rule - and the elaborator asks it of every name token.
\ Whether these bytes are already their own fold. The interner answers ONE
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

\ What a caller holding a spelling rather than an identity asks, so a copied
\ token reaches the same row the token it was copied from reached.
\ The key these bytes have in a table of this module. It is what a caller holding
: KEY-SPELL ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:ptr u:n :}
   u KEY-CAP > if c b a u IR-BUILD:INTERN-SYMBOL exit then
   a u FOLDED? if c b a u IR-BUILD:INTERN-SYMBOL exit then
   a u FOLD-INTO
   c b KEY-BUF u IR-BUILD:INTERN-SYMBOL ;

\ A spelling too long to be any row's answers itself, so the refusal names the
\ word the body wrote.
\ The same key for a spelling this module has already interned, which is the form
: KEY-SYM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-LEN KEY-CAP > if id exit then
   c b id KEY-BUF KEY-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   KEY-BUF u FOLDED? if id exit then
   KEY-BUF u FOLD-INTO
   c b KEY-BUF u IR-BUILD:INTERN-SYMBOL ;

private

\ This door states its key rather than computing it: reading a spelling back
\ needs the byte pool and this is handed only the rows. A row written here under
\ an unfolded spelling is unreachable, and fails closed as unmodeled.
\ ---- symbols the module's interner has answered for --------------------------
: SYM-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR-WORD:interned )
   {: sy:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   sy id IR-SYM:LEN@ drop
   id HIR--WORD-INTERNED:MAKE ;

\ It answers by asking IR-SYM, so a symbol refused here is refused by name.
\ The same question about a module that is still being built, whose interner
: BSYM-CK ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- HIR-WORD:interned )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-CK
   id HIR--WORD-INTERNED:MAKE ;

\ Every builder-side declarer goes through this, so a row under any other key
\ cannot be written - the declarers cannot express its opposite.
\ The symbol a declaration through that door writes its row under: the key of the
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

\ One scan serves the lookup, the duplicate check and the inventory walk.
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

\ The two handles plus the key are the table, and it dies with its context.
\ ---- creation ----------------------------------------------------------------
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

\ Every declarer ends here, so ownership, the duplicate rule and the ceiling are
\ proved in one place.
\ Append one validated row. Every declarer ends here, so the ownership, the
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

\ Some words are one integer literal followed by one operation - `1-` is `1`
\ then `-` - so the row carries both rather than claiming a second opcode.
\ The row a constant-and-operation word writes. Some Habu words are one integer
: CONST-OP-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:opcode n -- )
   {: o:HIR:opcode v:n :}
   HIR-MEANING:CONST-OP MEAN-CODE
   o OPCODE-CODE
   v UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The value sits in the cell a const-op row keeps its constant in, and the kind
\ in the cell an op row keeps its opcode in: a `constant`'s number is a number,
\ a `create`d word's is a DATA address a snapshot moves with that region.
\ The row a word that pushes one fixed value writes. A `create`d data word is
: FIXED-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned n n -- )
   {: v:n kind:n :}
   HIR-MEANING:FIXED MEAN-CODE
   kind
   v UNUSED UNUSED UNUSED
   ROW-ADD ;

\ A qualified `NAME:tail` is the longer form a spelling can take; one past this
\ is refused by the interner rather than truncated.
\ Where the spelling of a fixed word is read back out of the module's interner so
64 constant FIX-NAME-CAP

create FIX-NAME FIX-NAME-CAP allot

\ The whole translation between the dictionary's vocabulary for definers and
\ this chain's for literals; a kind neither definer stamped never reaches it.
\ What the number a definer decided IS, as the literal staging says it: an
: LIT-KIND ( n -- n )
   {: k:n :}
   k NDICT:FIXED-ADDR = if HIR:ADDR-DATA exit then
   HIR:ADDR-NONE ;

\ Entry and arity are the caller's statement (habu-resolve-a-callee-0340dfde).
\ The GLUE says which result cells are one value, because the arity says only
\ how many there are; zero means every cell is a value of its own, which is the
\ safe reading. Deadness is zero for "control comes back", equally safe.
\ The row a word that is CALLED writes: where the callee's code starts, and how
: CALLABLE-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned n n n n n -- )
   {: entry:n in:n out:n glue:n dead:n :}
   entry 0 <= if E-HIR-CALLEE throw then
   in 0 < out 0 < or if E-HIR-CALLEE throw then
   HIR-MEANING:CALLABLE MEAN-CODE
   entry
   in out glue dead
   ROW-ADD ;

\ A control word stages no operation, so the row holds only which action it is.
\ The row a structured control word writes. It stages no operation of its own -
: CONTROL-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:ctrl -- )
   {: k:HIR:ctrl :}
   HIR-MEANING:CONTROL MEAN-CODE
   k CTRL-CODE
   UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The count is held against what a row can MEAN: a transfer of no cells would
\ elaborate to a silent no-op, and one wider than the pair forms is unsanctioned.
\ The row a return-stack transfer writes: which way it moves cells and how many.
2 constant RSTACK-CELLS-MAX          \ `2>r` and its two siblings are the widest forms

: RSTACK-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:rmove n -- )
   {: k:HIR:rmove cells:n :}
   cells 1 < cells RSTACK-CELLS-MAX > or if E-HIR-CLASS throw then
   HIR-MEANING:RSTACK MEAN-CODE
   k RSTACK-CODE
   cells
   UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The meaning is held against the two that qualify, so this cannot write an `op`
\ row with no opcode or a `rename` row with no picks.
\ The row a word with no payload at all writes. The two halves of a typed
: PLAIN-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:meaning -- )
   {: m:HIR:meaning :}
   m HIR-MEANING:OPEN-LOCALS HIR-MEANING:EQ
   m HIR-MEANING:CLOSE-LOCALS HIR-MEANING:EQ or
   0= if E-HIR-CLASS throw then
   m MEAN-CODE
   UNUSED UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The same declaration for a module still being built: the builder answers for
\ The builder answers for its own interner.
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
\ The value is NOT a parameter: a caller's copy goes stale the moment the word
\ is retired, and dict.f resolves the spelling the body itself writes.
: DECLARE-FIXED ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   c b id FIX-NAME FIX-NAME-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   c r  c b id BKEY-CK
   FIX-NAME u NDICT:FIXED-VALUE  FIX-NAME u NDICT:SPELL-FIXED LIT-KIND  FIXED-ROW ;

\ Declare that a source word is another word this definition CALLS: where its
\ A caller that states a callee by hand states no glue and no deadness, and gets
\ the safe reading of both.
: DECLARE-CALLABLE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id entry:n in:n out:n :}
   c r  c b id BKEY-CK  entry in out NDICT:GLUE-NONE COMES-BACK CALLABLE-ROW ;

\ Make a FIXED row for a spelling nobody staged, by asking the engine which
\ Asked BEFORE the callable question because a stamped record is never a call
\ and an unstamped one is never anything but - the cheaper question first.
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
\ Every way the engine can fail to answer leaves the token to be refused as
\ unmodeled, by name. The return-stack clause is the one that is not missing
\ information: the elaborator's return stack is a compile-time vector, and a
\ call has nowhere to put a callee's motion of it.
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
   glue NDICT:GLUE-UNKNOWN = if false exit then   \ a result shape nothing can state is not a call this builds
   FIX-NAME u NDICT:SPELL-DEAD? {: dead:bool :}
   c r  c b id BKEY-CK  entry in out glue  dead NORET-CODE  CALLABLE-ROW
   true ;

\ Declare that a source word elaborates to one operation of this dialect. The
\ The second arena is the interner that has to have minted the word's spelling.
: DECLARE-OP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id o:HIR:opcode :}
   c r  sy id SYM-CK  o OP-ROW ;

\ Declare a named boundary this dialect cannot compile. The reason symbol names
\ The reason symbol names the capability whose absence is why, so a refusal can
\ say what has to land first.
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
\ One package-owned stage under the single-task discipline: any end consumes it
\ whatever the outcome, so no half-staged rename survives.
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
\ The picks land in the pool before the row that points at them, so a refusal
\ leaves no row naming cells outside it.
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
\ The stage is consumed before anything else, so a refusal leaves no rename open.
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
\ Asking a rename for its opcode is a category error, not a missing value.
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
\ The elaborator asks whether a name the PROGRAM chose for a local collides with
\ a word of the dialect, and "no" is the ordinary case rather than a failure.
: MODELS? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id SYM-OWNER-CK
   r id IR-ID:SYMBOL-LOCAL FIND 0 >= ;

\ The bare name inside one typed local's declaration spelling. A declaration
\ The tape carries `name:type` as one token while the body reads the name alone,
\ so the annotation is cut off here. `{: a b :}` is a real unannotated shape.
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
\ Through ROW-AS, so asking a word of another meaning is a category error.
: CONST-OPCODE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:opcode )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CONST-OP ROW-AS {: l:n :}
   r l OFF-A RC@ N>OPCODE ;

: CONST-VALUE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CONST-OP ROW-AS {: l:n :}
   r l OFF-IN RC@ ;

\ The value a word that pushes one fixed value pushes. Asking it about a word of
\ Through ROW-AS, for the same reason.
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
\ Through ROW-AS, for the same reason.
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
\ The one reader of the fact RESOLVE-CALLABLE put in the row, so every pass that
\ needs it asks one question instead of asking the engine again.
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
\ Both go through ROW-AS.
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
\ Both the window into the pool and the depth are rechecked, so a row written
\ past this package's declarers cannot read outside the live pool.
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
\ The key is PRESENTED rather than derived, because deriving it needs the
\ module's interner and this join is also asked of a frozen module. A character
\ or string literal is a kind this subset does not model and is refused as such.
: ADMIT-TOKEN ( IR-ARENA:view IR-ARENA:arena n IR-ID:ir-symbol-id -- HIR:meaning )
   {: v:IR-ARENA:view r:IR-ARENA:arena i:n sy:IR-ID:ir-symbol-id :}
   v i NTAPE:KIND@ {: k:NTAPE:kind :}
   k NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if HIR-MEANING:LITERAL exit then
   k NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if HIR-MEANING:REAL-LITERAL exit then
   k NTAPE-KIND:STRING-LITERAL NTAPE-KIND:EQ if HIR-MEANING:STRING-LITERAL exit then
   k NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-HIR-KIND throw then
   r sy ADMIT ;

\ ---- the subset's vocabulary -------------------------------------------------
\ The exact ceilings this registration writes, so a caller commits a table to
\ them rather than to a guess. Only the eight renames contribute pick cells.
83 constant WORDS
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
\ A row says which opcode a word means and nothing else, so a relation the
\ dialect has no opcode for could not be written down here at all.
: DEF-COMPARE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" <" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:LT BDECLARE-OP
   c b r c b s" <=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:LE BDECLARE-OP
   c b r c b s" >" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:GT BDECLARE-OP
   c b r c b s" >=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:GE BDECLARE-OP
   c b r c b s" =" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:EQUAL BDECLARE-OP
   c b r c b s" <>" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:NE BDECLARE-OP ;

\ `lshift` and `rshift` take a count the program computed, so they are two
\ operand words here and not a value and a field.
\ The bitwise words. `and`, `or` and `xor` combine two values bit for bit;
: DEF-BITWISE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" and" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:AND BDECLARE-OP
   c b r c b s" or" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:OR BDECLARE-OP
   c b r c b s" xor" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:XOR BDECLARE-OP
   c b r c b s" lshift" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:LSHIFT BDECLARE-OP
   c b r c b s" rshift" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:RSHIFT BDECLARE-OP
   c b r c b s" invert" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:INVERT BDECLARE-OP ;

\ `0=` is `0` then `=`, which answers false for EVERY nonzero value; `cells` is
\ `8` then `*`, the same function on every bit pattern as the engine's shift.
\ `1-` ( n -- n ) and `1+` ( n -- n ): subtract or add one. Each is one token of
: DEF-STEP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" 1-" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:SUB 1 BDECLARE-CONST-OP
   c b r c b s" 1+" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:ADD 1 BDECLARE-CONST-OP
   c b r c b s" 0=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:EQUAL 0 BDECLARE-CONST-OP
   c b r c b s" cells" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:MUL 8 BDECLARE-CONST-OP ;

\ The width is not stored: it is which opcode the row names, because hir.f makes
\ the width a form. The memory order is the dialect's own token.
\ The four memory words, two per width. `@` ( ptr -- n ) reads the cell an
: DEF-MEMORY ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" @" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:LOAD BDECLARE-OP
   c b r c b s" !" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:STORE BDECLARE-OP
   c b r c b s" c@" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:BLOAD BDECLARE-OP
   c b r c b s" c!" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:BSTORE BDECLARE-OP ;

\ `s>f` rounds to nearest with ties to even; `f>s` truncates toward zero,
\ saturates at the ends and answers zero for a NaN - two roundings, two rows.
\ The nine float words of the engine's vocabulary that compute rather than
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

\ Five, which is the whole of the engine's vocabulary. The two against zero are
\ two OPERATIONS: FCMP against the immediate zero takes no second register.
\ The five float comparisons, which are the whole of what the engine has: three
: DEF-FCOMPARE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" f<" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FLT BDECLARE-OP
   c b r c b s" f>" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FGT BDECLARE-OP
   c b r c b s" f=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FEQ BDECLARE-OP
   c b r c b s" f0<" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FLTZ BDECLARE-OP
   c b r c b s" f0=" IR-BUILD:INTERN-SYMBOL HIR-OPCODE:FEQZ BDECLARE-OP ;

\ `begin` has three closers and the counted loop two openers with one closer, so
\ a row says which word it is and the elaborator's control stack learns the rest.
\ `RECURSE` is interned in capitals because docs/forth.md spells it so; the row
\ is keyed by the fold, so any case reaches it.
\ The structured control words. Three structures, the two words that stand in
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
   c b r c b s" j" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OUTER-INDEX BDECLARE-CONTROL
   c b r c b s" unloop" IR-BUILD:INTERN-SYMBOL HIR-CTRL:DROP-LOOP BDECLARE-CONTROL
   c b r c b s" leave" IR-BUILD:INTERN-SYMBOL HIR-CTRL:EARLY-LEAVE BDECLARE-CONTROL
   c b r c b s" exit" IR-BUILD:INTERN-SYMBOL HIR-CTRL:EARLY-EXIT BDECLARE-CONTROL
   c b r c b s" RECURSE" IR-BUILD:INTERN-SYMBOL HIR-CTRL:SELF-CALL BDECLARE-CONTROL ;

\ `of` and `endof` are ONE row each and serve both `MATCH` and `case`; which
\ form an arm belongs to is decided by the structure the elaborator has open.
\ The three tag-dispatch forms, seven words. `of` and `endof` are ONE row each
: DEF-ADT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" match" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-MATCH BDECLARE-CONTROL
   c b r c b s" of" IR-BUILD:INTERN-SYMBOL HIR-CTRL:MATCH-ARM BDECLARE-CONTROL
   c b r c b s" endof" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-ARM BDECLARE-CONTROL
   c b r c b s" ;match" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-MATCH BDECLARE-CONTROL
   c b r c b s" case" IR-BUILD:INTERN-SYMBOL HIR-CTRL:OPEN-CASE BDECLARE-CONTROL
   c b r c b s" endcase" IR-BUILD:INTERN-SYMBOL HIR-CTRL:CLOSE-CASE BDECLARE-CONTROL
   c b r c b s" construct" IR-BUILD:INTERN-SYMBOL HIR-CTRL:MAKE-BUNDLE BDECLARE-CONTROL ;

\ Control actions, because what stands between them is a second FUNCTION of the
\ module. Neither row carries a payload.
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

\ None carries a payload: `is` moves what the DEFERRED word declares, `execute`
\ moves one cell more than the quotation reaching it, and `catch` moves the
\ window the checker certified at that site - all facts about the site.
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

\ Neither stages an operation and neither carries a payload: the work is the
\ elaborator's over the tape rows between them.
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

\ `over over` written once.
\ 2dup ( a b -- a b a b ): consume two and put both back twice, in order. It is
: DEF-2DUP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   1 ADD-PICK
   0 ADD-PICK
   c b p r c b s" 2dup" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ `drop drop` written once: the two values leave the compile-time vector and no
\ instruction is needed to make them go.
\ 2drop ( a b -- ): consume two and put neither back. It is `drop drop` written
: DEF-2DROP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   c b p r c b s" 2drop" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ Bottom first that is b, then c, then a - depths 1, 0 and 2.
\ rot ( a b c -- b c a ): consume three and put all three back rotated, so the
: DEF-ROT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   3 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   2 ADD-PICK
   c b p r c b s" rot" IR-BUILD:INTERN-SYMBOL BDECLARE-RENAME ;

\ Six rows over three actions and two widths; the widths are declared because
\ `2>r` is its own source word. `2>r` keeps the LOWER cell lower on the return
\ stack, and elaborate.f is where that order is kept.
\ ---- the return-stack words --------------------------------------------------
: DEF-RSTACK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" >r"  IR-BUILD:INTERN-SYMBOL HIR-RMOVE:TO-R    1 BDECLARE-RSTACK
   c b r c b s" r>"  IR-BUILD:INTERN-SYMBOL HIR-RMOVE:FROM-R  1 BDECLARE-RSTACK
   c b r c b s" r@"  IR-BUILD:INTERN-SYMBOL HIR-RMOVE:FETCH-R 1 BDECLARE-RSTACK
   c b r c b s" 2>r" IR-BUILD:INTERN-SYMBOL HIR-RMOVE:TO-R    2 BDECLARE-RSTACK
   c b r c b s" 2r>" IR-BUILD:INTERN-SYMBOL HIR-RMOVE:FROM-R  2 BDECLARE-RSTACK
   c b r c b s" 2r@" IR-BUILD:INTERN-SYMBOL HIR-RMOVE:FETCH-R 2 BDECLARE-RSTACK ;

public

\ A `create`d data word and a called word are NOT here: which of those a program
\ has is a fact about the program, declared by its own caller.
\ Declare the whole straight-line source vocabulary into one word model: the
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
