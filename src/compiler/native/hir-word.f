\ hir-word.f - the straight-line HIR dialect's source-word model: one row per
\ Habu word saying what it means to the elaborator (operation, rename, control,
\ callable, fixed value, unmodeled), and which words it refuses to compile.
\ The session registers the vocabulary once per load (SESSION-MODEL); each
\ definition's overlay (NEW-LINKED) holds its own callable and fixed rows.
\ Expansion rows name sequences that live in src/compiler/native/elaborate.f.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/build.f
require src/compiler/ir/symbol.f
require src/compiler/native/tape.f
require src/compiler/native/hir.f
require src/compiler/native/dict.f
require src/compiler/native/checker-owner.f
require src/habu/layout.f
require src/habu/terminal-call.f
require src/compiler/native/emission.f

package HIR-WORD
public

\ A symbol this module's interner has answered for. Owning the right module is
\ not enough: an identity is arithmetic away from any other, so a row could
\ name an ordinal the interner never minted.
STRUCTURE interned
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
3 constant HC-LINK                   \ how this table stands to the session
4 constant HDR-CELLS
0 constant LINK-NONE                 \ a table of its own module and nothing else
1 constant LINK-CLONE                \ an overlay whose module is a session clone
2 constant LINK-SESSION              \ the session's own vocabulary table
0 constant OFF-SYM                   \ the source word's symbol ordinal
1 constant OFF-MEAN                  \ the stored meaning code
2 constant OFF-A                     \ op and const-op: the opcode code; rename: the pick-list start, a cast's output glue; control: the control code; rstack: the transfer code; unmodeled: the reason ordinal plus one; callable: the callee's entry address
3 constant OFF-IN                    \ rename: the number of values consumed, a cast's cells; rstack: the number of cells moved; const-op: the constant; fixed: the value the word pushes; callable: the values the callee takes; otherwise zero
4 constant OFF-N                     \ rename: the number of values put back, none for a cast; callable: the values the callee leaves; otherwise zero
5 constant OFF-GLUE                  \ callable result glue; rename host semantic class
6 constant OFF-DEAD                  \ callable: return control; fixed: DATA owner ordinal
7 constant ROW-CELLS
0 constant UNUSED                    \ a payload cell this meaning does not use
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant ROW-CAP-MAX
$FFFFFFFF HDR-CELLS - constant POOL-CAP-MAX

\ The deepest classical rename is `2over`, four in and six back; a rename that
\ wants more is a capability to add here, not a value to widen silently.
4 constant INPUT-MAX
8 constant PICK-MAX

\ ---- stored codes ------------------------------------------------------------
\ Each code has an exact decoder, so a row written past this package's declarers
\ cannot decode as another meaning. COMES-BACK is zero so an unset payload cell
\ reads as "control comes back", the safe direction.
0 constant COMES-BACK
1 constant NO-RETURN
2 constant TERMINAL

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
      expansion OF 13 ENDOF
   ;MATCH ;

\ Codes zero, ten and eleven (`literal`, `real-literal`, `string-literal`) belong
\ to a tape token and never to a word: a row claiming one is corrupt.
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
      13 of HIR-MEANING:EXPANSION endof
      E-HIR-CLASS throw
   endcase ;

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
      tick         OF 29 ENDOF
      eval         OF 30 ENDOF
      finally      OF 31 ENDOF
      close-loop-step OF 32 ENDOF
      c2-invoke OF 33 ENDOF
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
      29 of HIR-CTRL:TICK endof
      30 of HIR-CTRL:EVAL endof
      31 of HIR-CTRL:FINALLY endof
      32 of HIR-CTRL:CLOSE-LOOP-STEP endof
      33 of HIR-CTRL:C2-INVOKE endof
      E-HIR-CONTROL throw
   endcase ;

\ The codes start at zero because they are read out of a payload cell this
\ meaning owns outright.
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

: EXPAND-CODE ( HIR:expand -- n )
   MATCH expand
      cell-index OF 0 ENDOF
      modulo     OF 1 ENDOF
      maximum    OF 2 ENDOF
      minimum    OF 3 ENDOF
   ;MATCH ;

: N>EXPAND ( n -- HIR:expand )
   case
      0 of HIR-EXPAND:CELL-INDEX endof
      1 of HIR-EXPAND:MODULO endof
      2 of HIR-EXPAND:MAXIMUM endof
      3 of HIR-EXPAND:MINIMUM endof
      E-HIR-CLASS throw
   endcase ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   IR-ARENA:READ ;

\ ---- headers and shape -------------------------------------------------------
: RSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-HIR-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-HIR-STATE throw then ;

: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-HIR-STATE throw then ;

\ Each arena rechecks its own header tag, so a pair swapped at a call site dies
\ on the tag instead of reading a foreign row.
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
\ A row is keyed by the lowercase fold of its spelling, the fold the engine
\ applies to a checked body's tokens, so `IF`, `if` and `If` reach one row. Rows
\ are written under BKEY-CK's fold and asked under KEY-SYM's; the tape's symbol
\ keeps the source bytes. A local matches its declared spelling byte for byte,
\ not this fold (elaborate.f WRAW), and resolves before this model.
$41 constant KEY-A
$5A constant KEY-Z
$20 constant KEY-BIT

\ The longest spelling this table folds. A longer one is its own key, unfolded,
\ rather than truncated into a name that denotes some other word; its row is
\ written and asked under that same symbol.
64 constant KEY-CAP

KEY-CAP CWORK-SPAN KEY-BUF u8

: FOLD-C ( n -- n )
   {: b:n :}
   b KEY-A < if b exit then
   b KEY-Z > if b exit then
   b KEY-BIT or ;

\ The interner answers one identity per byte string, so a spelling already its
\ own fold is its own key: a shortcut, not a second rule. The elaborator asks it
\ of every name token.
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

\ The key these bytes have in a table of this module: a copied token reaches the
\ same row as the token it was copied from.
: KEY-SPELL ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:ptr u:n :}
   u KEY-CAP > if c b a u IR-BUILD:INTERN-SYMBOL exit then
   a u FOLDED? if c b a u IR-BUILD:INTERN-SYMBOL exit then
   a u FOLD-INTO
   c b KEY-BUF u IR-BUILD:INTERN-SYMBOL ;

\ The same key for a spelling this module has already interned. A spelling past
\ KEY-CAP answers itself, so its row, or the refusal of it, names the word as
\ the body wrote it.
: KEY-SYM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-LEN KEY-CAP > if id exit then
   c b id KEY-BUF KEY-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
   KEY-BUF u FOLDED? if id exit then
   KEY-BUF u FOLD-INTO
   c b KEY-BUF u IR-BUILD:INTERN-SYMBOL ;

private

\ ---- the session model -------------------------------------------------------
\ One vocabulary table per load, read by every definition. A definition's module
\ is cloned from the session prototype, so a spelling the prototype holds sits
\ at the same ordinal in every clone and a body token's symbol indexes the
\ vocabulary with no lookup. A reader holds rows and a symbol but no interner,
\ so spelling questions are answered at registration: ORD-ROW maps a prototype
\ ordinal to its session row, ROW-ORD maps it back for the binding gate to read
\ bytes. The prototype interns about 230 symbols and the vocabulary is WORDS
\ rows; both ceilings refuse by name rather than grow.
512 constant ORD-CAP                 \ prototype ordinals the map can cover
128 constant SROW-CAP                \ session rows the gate can remember

ORD-CAP CWORK-SPAN ORD-ROW n     \ prototype ordinal -> session row plus one
SROW-CAP CWORK-SPAN ROW-ORD n    \ session row -> its prototype ordinal
SROW-CAP CWORK-SPAN CHK-GEN n    \ session row -> the definition that asked
SROW-CAP CWORK-SPAN CHK-BND n    \ session row -> what that definition got

1 CWORK-BUFFER SESS-POOL IR-ARENA:arena   \ the prototype interner's byte pool
1 CWORK-BUFFER SESS-ROWS IR-ARENA:arena   \ and its rows
1 CWORK-BUFFER SESS-KEY IR-ID:ir-module-key
1 CWORK-BUFFER SESS-P IR-ARENA:arena      \ the session table's pick pool
1 CWORK-BUFFER SESS-R IR-ARENA:arena      \ and its rows
0 CWORK-VARIABLE SESS-ON n
0 CWORK-VARIABLE ORD-N n            \ how many prototype ordinals the map covers

: ORD-ROW@ ( n -- n ) cells ORD-ROW + @ ;
: ORD-ROW! ( n n -- ) cells ORD-ROW + ! ;
: ROW-ORD@ ( n -- n ) cells ROW-ORD + @ ;
: ROW-ORD! ( n n -- ) cells ROW-ORD + ! ;

: SESS-FORGET ( -- )
   0 SESS-ON !
   0 ORD-N ! ;
SESS-FORGET

\ The session row a prototype ordinal names, or a negative answer. A token the
\ prototype never held has an ordinal past the map, which is the ordinary case
\ for every word a body defines or calls.
: SESS-ROW ( n -- n )
   {: ord:n :}
   SESS-ON @ 0= if -1 exit then
   ord 0 < ord ORD-N @ >= or if -1 exit then
   ord ORD-ROW@ 1- ;

\ The registration declares the whole vocabulary, so this is the plain intern.
: MODEL-SYM ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-symbol-id )
   IR-BUILD:INTERN-SYMBOL ;

\ ---- symbols the module's interner has answered for --------------------------
\ SYM-CK states its key rather than computing it: reading a spelling back needs
\ the byte pool and it is handed only the rows. A row written under an unfolded
\ spelling is unreachable and fails closed as unmodeled.
: SYM-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR-WORD:interned )
   {: sy:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   sy id IR-SYM:LEN@ drop
   id HIR--WORD-INTERNED:MAKE ;

\ A module still being built: this asks IR-SYM, so a refused symbol is refused
\ by name.
: BSYM-CK ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- HIR-WORD:interned )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-CK
   id HIR--WORD-INTERNED:MAKE ;

\ The key a declaration writes its row under. Every builder-side declarer goes
\ through this, so a row under any other key cannot be written.
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
\ A table is its two arenas plus the key and dies with its context. `link` is a
\ header cell, not package state: LINK-CLONE says the module is a clone of the
\ session prototype, which lets a body token's ordinal name a session row.
: NEW-TABLE ( IR-CTX:ctx IR-ID:ir-module-key n n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key rcap:n pcap:n link:n :}
   rcap ROW-CAP-OK
   pcap POOL-CAP-OK
   c pcap HDR-CELLS + IR-ARENA:NEW {: p:IR-ARENA:arena :}
   c p HDR-CELLS IR-ARENA:RESERVE
   c p WPOOL-MAGIC IR-ARENA:PUSH drop
   c p key KEY-SERIAL IR-ARENA:PUSH drop
   c p pcap IR-ARENA:PUSH drop
   c p link IR-ARENA:PUSH drop
   c rcap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r HDR-CELLS IR-ARENA:RESERVE
   c r WROW-MAGIC IR-ARENA:PUSH drop
   c r key KEY-SERIAL IR-ARENA:PUSH drop
   c r rcap IR-ARENA:PUSH drop
   c r link IR-ARENA:PUSH drop
   p r ;

\ A table of its own module, as a caller outside a session and every test
\ builds: it consults no session rows.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   LINK-NONE NEW-TABLE ;

private

\ Every declarer ends here, so ownership, the duplicate rule and the ceiling are
\ proved in one place.
: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned n n n n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena w:HIR-WORD:interned mean:n a:n
      in:n n:n glue:n dead:n :}
   w HIR--WORD-INTERNED:UNMAKE {: id:IR-ID:ir-symbol-id :}
   r id SYM-OWNER-CK
   id IR-ID:SYMBOL-LOCAL {: so:n :}
   r so FIND 0 < 0= if E-HIR-DUP throw then
   r ROW-ROOM-CK
   c r ROW-CELLS IR-ARENA:RESERVE
   c r so IR-ARENA:PUSH drop
   c r mean IR-ARENA:PUSH drop
   c r a IR-ARENA:PUSH drop
   c r in IR-ARENA:PUSH drop
   c r n IR-ARENA:PUSH drop
   c r glue IR-ARENA:PUSH drop
   c r dead IR-ARENA:PUSH drop ;

: OP-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:opcode -- )
   {: o:HIR:opcode :}
   HIR-MEANING:OP MEAN-CODE
   o HIR:ORD
   UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

\ Some words are one integer literal followed by one operation - `1-` is `1`
\ then `-` - so the row carries both rather than claiming a second opcode.
: CONST-OP-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:opcode n -- )
   {: o:HIR:opcode v:n :}
   HIR-MEANING:CONST-OP MEAN-CODE
   o HIR:ORD
   v UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The value sits in the cell a const-op row keeps its constant in, and the kind
\ in the cell an op row keeps its opcode in: a `constant`'s number is a number,
\ a `create`d word's is a DATA address a snapshot moves with that region.
: FIXED-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned n n n -- )
   {: v:n kind:n owner:n :}
   HIR-MEANING:FIXED MEAN-CODE
   kind
   v UNUSED UNUSED owner
   ROW-ADD ;

\ The spelling of a word the body names, read back out of the module's interner
\ into a byte row that grows to it, so the dictionary is asked about the whole
\ name, as long as the body capture let it be. The span lasts until the next
\ spelling is taken.
DYNAMIC-BUFFER FIX-NAME u8

public

: FIX-SPELL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- ptr u8 n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder id:IR-ID:ir-symbol-id :}
   c b id IR-BUILD:SYMBOL-LEN {: u:n :}
   u 1 max FIX-NAME-RESERVE
   0 FIX-NAME  c b id 0 FIX-NAME u IR-BUILD:SYMBOL-COPY ;

private

\ The translation between the dictionary's kinds for definers and the literal
\ staging's; a kind neither definer stamped never reaches it.
public

: LIT-KIND ( n -- n )
   {: k:n :}
   k NDICT:FIXED-ADDR = if NEMIT:ADDR-DATA exit then
   HIR:ADDR-NONE ;

private

\ Entry and arity are the caller's statement. The glue says which result cells
\ are one value, since the arity says only how many; zero means every cell is
\ its own value, the safe reading. Dead zero is "control comes back", as safe.
: CALLABLE-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned n n n n n -- )
   {: entry:n in:n out:n glue:n dead:n :}
   entry 0 <= if E-HIR-CALLEE throw then
   in 0 < out 0 < or if E-HIR-CALLEE throw then
   HIR-MEANING:CALLABLE MEAN-CODE
   entry
   in out glue dead
   ROW-ADD ;

: CONTROL-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:ctrl -- )
   {: k:HIR:ctrl :}
   HIR-MEANING:CONTROL MEAN-CODE
   k CTRL-CODE
   UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The sequence is the whole payload: its operations and constants belong to the
\ sequence, not the word.
: EXPANSION-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:expand -- )
   {: k:HIR:expand :}
   HIR-MEANING:EXPANSION MEAN-CODE
   k EXPAND-CODE
   UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

\ The count is held against what a row can MEAN: a transfer of no cells would
\ elaborate to a silent no-op, and one wider than the pair forms is unsanctioned.
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
: PLAIN-ROW ( IR-CTX:ctx IR-ARENA:arena HIR-WORD:interned HIR:meaning -- )
   {: m:HIR:meaning :}
   m HIR-MEANING:OPEN-LOCALS HIR-MEANING:EQ
   m HIR-MEANING:CLOSE-LOCALS HIR-MEANING:EQ or
   0= if E-HIR-CLASS throw then
   m MEAN-CODE
   UNUSED UNUSED UNUSED UNUSED UNUSED
   ROW-ADD ;

: BDECLARE-PLAIN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:meaning -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id m:HIR:meaning :}
   c r  c b id BKEY-CK  m PLAIN-ROW ;

\ A row stands for the seeded primitive of its spelling, so it claims a token
\ only when the engine's lookup binds that primitive's own record (NDICT
\ SPELL-PRIM?, habu2.f C-OP-ROW-GATE). A package word of that spelling, or a
\ global one defined after `undefine`, is another record with its own checked
\ effect: after `undefine dup : dup ( n -- n ) 100 + ;` scope and the global
\ wordlist agree on the new `dup`, so asking the global wordlist is wrong.
\ It takes bytes, not a symbol: the reader's binding gate holds no interner.
: INTRINSIC-BOUND? ( ptr u8 n -- bool )
   NDICT:SPELL-PRIM? ;

\ ---- where the gate is asked -------------------------------------------------
\ A spelling the engine no longer owns models nothing, so the token falls
\ through to the callable path. The session's table is read by every definition
\ and the answer moves between them (test/compiler/native-word-binding.f needs
\ `xor` present, absent while a package owns it, present again), so it is asked
\ at the reading token (ROW-BOUND?). Any other table is read inside one
\ definition by a reader with no interner, so it is asked here, while the
\ builder is in hand, and a refused word gets no row.
: BVOCAB? ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r HC-LINK LCELL@ LINK-SESSION = if true exit then
   c b id FIX-SPELL INTRINSIC-BOUND? ;

: BDECLARE-OP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id o:HIR:opcode :}
   c b r id BVOCAB? 0= if exit then
   c r  c b id BKEY-CK  o OP-ROW ;

: BDECLARE-CONST-OP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id o:HIR:opcode v:n :}
   c b r id BVOCAB? 0= if exit then
   c r  c b id BKEY-CK  o v CONST-OP-ROW ;

: BDECLARE-CONTROL ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:ctrl -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id k:HIR:ctrl :}
   c b r id BVOCAB? 0= if exit then
   c r  c b id BKEY-CK  k CONTROL-ROW ;

: BDECLARE-EXPAND ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:expand -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id k:HIR:expand :}
   c b r id BVOCAB? 0= if exit then
   c r  c b id BKEY-CK  k EXPANSION-ROW ;

: BDECLARE-RSTACK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id HIR:rmove n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id k:HIR:rmove cells:n :}
   c b r id BVOCAB? 0= if exit then
   c r  c b id BKEY-CK  k cells RSTACK-ROW ;

public

\ The value is NOT a parameter: a caller's copy goes stale the moment the word
\ is retired, and dict.f resolves the spelling the body itself writes.
: DECLARE-FIXED ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   c b id FIX-SPELL {: a:ptr u:n :}
   c r  c b id BKEY-CK
   a u NDICT:FIXED-VALUE  a u NDICT:SPELL-FIXED LIT-KIND
      a u NDICT:FIXED-OWNER FIXED-ROW ;

\ Direct tapes have no source checker window. Resolve their dictionary words
\ through the ordinary model path; the recorded-source entry below never calls
\ either of these readers when an exact site fact is missing.
: RESOLVE-FIXED ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   c b id FIX-SPELL {: a:ptr u:n :}
   a u NDICT:SPELL-FIXED {: k:n :}
   k NDICT:FIXED-NONE = if false exit then
   c r  c b id BKEY-CK
   a u NDICT:FIXED-VALUE  k LIT-KIND
      a u NDICT:FIXED-OWNER FIXED-ROW
   true ;

\ A caller that states a callee by hand states no glue and no deadness, and gets
\ the safe reading of both.
: DECLARE-CALLABLE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id entry:n in:n out:n :}
   c r  c b id BKEY-CK  entry in out NDICT:GLUE-NONE COMES-BACK CALLABLE-ROW ;

\ Declare the exact callable facts already bound to a dictionary entry.  This
\ door is for a caller that captured those facts together, before a later
\ definition of the same name could change the checker's name lookup.
: DECLARE-BOUND-CALLABLE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id n n n n bool -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id entry:n in:n out:n glue:n dead:bool :}
   c r  c b id BKEY-CK  entry in out glue  dead NORET-CODE  CALLABLE-ROW ;

\ The second arena is the interner that has to have minted the word's spelling.
: DECLARE-OP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id o:HIR:opcode :}
   c r  sy id SYM-CK  o OP-ROW ;

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

align
0 CWORK-VARIABLE STG-MODE n
0 CWORK-VARIABLE STG-IN n
0 CWORK-VARIABLE STG-N n
0 CWORK-VARIABLE STG-HOST n
public
0 constant HOST-SCALAR
1 constant HOST-VIEW
2 constant HOST-CAST
private
PICK-MAX CWORK-SPAN STG-PICK n

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
   \ The row is reserved HERE, not left to ROW-ADD: its pool window goes in
   \ first, and a window with no row is a table whose rows no longer name their
   \ own cells. ROW-ADD's own reservation then finds the room already there.
   c p STG-N @ IR-ARENA:RESERVE
   c r ROW-CELLS IR-ARENA:RESERVE
   p PCELLS {: st:n :}
   STG-N @ 0 ?do
      c p i SP@ IR-ARENA:PUSH drop
   loop
   c r w
   HIR-MEANING:RENAME MEAN-CODE
   st STG-IN @ STG-N @ STG-HOST @ UNUSED
   ROW-ADD ;

\ The stage is consumed first either way, so a symbol the interner refuses
\ leaves no rename open.
: BDECLARE-RENAME ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   STG-TAKE
   c b r id BVOCAB? 0= if exit then
   c p r  c b id BKEY-CK  RENAME-ROW ;

public

\ A rename is data: how many values it consumes off the top and which it puts
\ back, each named by depth in the consumed window (zero the top), bottom first.
\ `over` ( a b -- a b a ) consumes 2 and puts back 1 0 1; `rot` ( a b c -- b c a )
\ consumes 3 and puts back 1 0 2. It may put back only an input it consumed.
: BEGIN-RENAME ( n -- )
   {: in:n :}
   STG-MODE @ MODE-NONE <> if E-HIR-STAGE throw then
   in 0 < in INPUT-MAX > or if E-HIR-PICK throw then
   MODE-OPEN STG-MODE !
   in STG-IN !
   0 STG-N !
   HOST-SCALAR STG-HOST ! ;

: ADD-PICK ( n -- )
   {: d:n :}
   STG-OPEN-CK
   d 0 < d STG-IN @ >= or if E-HIR-PICK throw then
   STG-N @ {: n:n :}
   n PICK-MAX >= if E-HIR-PICK throw then
   d n SP!
   n 1+ STG-N ! ;

: ABANDON-RENAME ( -- )
   STG-TAKE ;

\ The stage is consumed before anything else, so a refusal leaves no rename open.
: DECLARE-RENAME ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx p:IR-ARENA:arena r:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id :}
   STG-TAKE
   c p r  sy id SYM-CK  RENAME-ROW ;

\ A cast keeps the cells it consumes in place, the same SSA values, and regroups
\ them by the output glue: a view unpack makes one two-cell value two, a pack
\ makes two values one. The checker admits only casts whose sides hold the same
\ cells; `in` and `out` are those counts and `glue` the output glue. The row has
\ no pick list (HOST-RENAME@ answers HOST-CAST). Its authority is the captured
\ dictionary definer, not the vocabulary.
: DECLARE-BOUND-CAST ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena
      id:IR-ID:ir-symbol-id in:n out:n glue:n :}
   in 1 < in out <> or if E-HIR-PICK throw then
   c r  c b id BKEY-CK
   HIR-MEANING:RENAME MEAN-CODE glue in UNUSED HOST-CAST UNUSED
   ROW-ADD ;

private

: RESOLVED-NORET ( ptr u8 n n bool -- n )
   {: a:ptr u:n entry:n dead:bool :}
   dead 0= if COMES-BACK exit then
   a u entry TERMINAL-CALL:BOUND? if TERMINAL else NO-RETURN then ;

: BOUND@ ( ptr u8 n -- n ) cells + CELL-VIEW @ ;

: OVERLAY-MODELS? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id SYM-OWNER-CK
   id IR-ID:SYMBOL-LOCAL r swap FIND 0 >= ;

: SESSION-MODELS? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r HC-LINK LCELL@ LINK-CLONE <> if false exit then
   id IR-ID:SYMBOL-LOCAL SESS-ROW 0 >= ;

: SITE-PRIM! ( IR-ARENA:arena IR-ID:ir-symbol-id bool -- )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id seeded:bool :}
   r HC-LINK LCELL@ LINK-CLONE <> if exit then
   id IR-ID:SYMBOL-LOCAL SESS-ROW {: sl:n :}
   sl 0 < if exit then
   seeded if 1 else 0 then sl cells CHK-BND + !
   r HC-SERIAL LCELL@ sl cells CHK-GEN + ! ;

public

: OVERLAY? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   OVERLAY-MODELS? ;

: RESOLVE-CALLABLE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   c b id FIX-SPELL {: a:ptr u:n :}
   a u NDICT:CALL-BINDING {: entry:n kind:n :}
   entry 0= if false exit then
   a u NDICT:SPELL-CALL {: in:n out:n glue:n :}
   in NDICT:ARITY-NONE = if false exit then
   glue NDICT:GLUE-UNKNOWN = if false exit then
   kind DKIND:CAST = if c b r id in out glue DECLARE-BOUND-CAST true exit then
   c r  c b id BKEY-CK  entry in out glue
   a u entry a u NDICT:SPELL-DEAD? RESOLVED-NORET CALLABLE-ROW
   true ;

\ SITE-PRIM! caches the seeded decision before any model is read. A call to an
\ internal record binds as NDICT:CALL-BINDING binds one (NDICT:INT-CALL?).
: RESOLVE-SITE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id n -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena id:IR-ID:ir-symbol-id ix:n :}
   ix CHECKER-OWNER:SOURCE-BINDING {: row:ptr size:n :}
   size 0= if CHECKER-OWNER-ABI:BINDING-RC throw then
   size CHECKER-OWNER-ABI:BOUND-CELLS cells <> if E-NCOMP-OWNER throw then
   row CHECKER-OWNER-ABI:BOUND-ORD BOUND@ ix <> if CHECKER-OWNER-ABI:BINDING-RC throw then
   row CHECKER-OWNER-ABI:BOUND-KIND BOUND@ {: source-kind:n :}
   source-kind CHECKER-OWNER-ABI:BOUND-DICT =
   source-kind CHECKER-OWNER-ABI:BOUND-INTRINSIC = or
   source-kind CHECKER-OWNER-ABI:BOUND-PENDING = or
   source-kind CHECKER-OWNER-ABI:BOUND-UNRESOLVED = or 0= if
      CHECKER-OWNER-ABI:BINDING-RC throw then
   source-kind CHECKER-OWNER-ABI:BOUND-UNRESOLVED = if E-HIR-UNMODELED throw then
   row CHECKER-OWNER-ABI:BOUND-CTL BOUND@
      CHECKER-OWNER-ABI:BOUND-SEEDED and 0<>
   source-kind CHECKER-OWNER-ABI:BOUND-INTRINSIC = or {: seeded:bool :}
   r id seeded SITE-PRIM!
   r id OVERLAY-MODELS? if true exit then
   seeded if r id SESSION-MODELS? if true exit then then
   source-kind CHECKER-OWNER-ABI:BOUND-DICT <> if true exit then
   row CHECKER-OWNER-ABI:BOUND-SYM BOUND@ 0= if CHECKER-OWNER-ABI:BINDING-RC throw then
   row CHECKER-OWNER-ABI:BOUND-ENTRY BOUND@ {: entry:n :}
   entry 0= if CHECKER-OWNER-ABI:BINDING-RC throw then
   row CHECKER-OWNER-ABI:BOUND-FLAGS BOUND@ {: flags:n :}
   flags DKIND:MASK and {: kind:n :}
   kind DKIND:VAL = kind DKIND:ADDR = or if
      kind DKIND:VAL = if NDICT:FIXED-VAL else NDICT:FIXED-ADDR then {: fixed:n :}
      c r c b id BKEY-CK entry fixed NDICT:BOUND-VALUE fixed LIT-KIND
         kind DKIND:ADDR = if
            row CHECKER-OWNER-ABI:BOUND-RECORD BOUND@
         else -1 then FIXED-ROW
      true exit
   then
   row CHECKER-OWNER-ABI:BOUND-EFFECT BOUND@ 0= if CHECKER-OWNER-ABI:BINDING-RC throw then
   flags DNAME-IMM and 0<> if true exit then
   flags DNAME-INT and 0<> if
      flags NDICT:INT-CALL? 0= if false exit then
   then
   row CHECKER-OWNER-ABI:BOUND-IN BOUND@ {: in:n :}
   row CHECKER-OWNER-ABI:BOUND-OUT BOUND@ {: out:n :}
   row CHECKER-OWNER-ABI:BOUND-GLUE BOUND@ {: glue:n :}
   in 0< out 0< or glue NDICT:GLUE-UNKNOWN = or if CHECKER-OWNER-ABI:BINDING-RC throw then
   kind DKIND:CAST = if c b r id in out glue DECLARE-BOUND-CAST true exit then
   c b id FIX-SPELL {: a:ptr u:n :}
   c r  c b id BKEY-CK  entry in out glue
   a u entry row CHECKER-OWNER-ABI:BOUND-DEAD BOUND@ 0= 0=
      RESOLVED-NORET CALLABLE-ROW
   true ;

\ ---- reading -----------------------------------------------------------------
: MODELED ( IR-ARENA:arena -- n )
   dup RHDR-CK CNT ;

private

\ ---- the binding gate --------------------------------------------------------
\ Vocabulary membership is a fact about the dialect, registered once; whether
\ the engine still owns a spelling is a fact about the dictionary and moves
\ mid-load when a package binds `+` or `dup`, so it is asked at the first token
\ that reads the row and remembered for that definition, named by the overlay's
\ header serial: a new definition is a new module, so nothing is cleared.
64 constant GATE-CAP                 \ the longest vocabulary spelling the gate reads

GATE-CAP CWORK-SPAN GATE-BUF u8

: CHK-GEN@ ( n -- n ) cells CHK-GEN + @ ;
: CHK-GEN! ( n n -- ) cells CHK-GEN + ! ;
: CHK-BND@ ( n -- n ) cells CHK-BND + @ ;
: CHK-BND! ( n n -- ) cells CHK-BND + ! ;

: ROW-SPELL ( n -- ptr u8 n )
   {: sl:n :}
   0 SESS-KEY @ sl ROW-ORD@ IR-ID:PACK-SYMBOL {: id:IR-ID:ir-symbol-id :}
   0 SESS-POOL @ 0 SESS-ROWS @ id GATE-BUF GATE-CAP IR-SYM:COPY
   GATE-BUF swap ;

\ A plain row is a marker no package can own - `{:` and `:}` are syntax and not
\ words - so it answers without asking the dictionary at all.
: GATED? ( n -- bool )
   {: sl:n :}
   0 SESS-R @ sl OFF-MEAN RC@ N>MEAN {: m:HIR:meaning :}
   m HIR-MEANING:OPEN-LOCALS HIR-MEANING:EQ if false exit then
   m HIR-MEANING:CLOSE-LOCALS HIR-MEANING:EQ if false exit then
   true ;

: ROW-BOUND? ( IR-ARENA:arena n -- bool )
   {: r:IR-ARENA:arena sl:n :}
   sl GATED? 0= if true exit then
   r HC-SERIAL LCELL@ {: gen:n :}
   sl CHK-GEN@ gen = if sl CHK-BND@ 0<> exit then
   \ Direct table readers have no source site. The compiler fills this cache
   \ from each source decision before it reads an ordinary call's model.
   sl ROW-SPELL INTRINSIC-BOUND? {: bound:bool :}
   bound if 1 else 0 then sl CHK-BND!
   gen sl CHK-GEN!
   bound ;

\ ---- which table answers ------------------------------------------------------
\ The overlay is asked first, by symbol; only a table whose header says
\ LINK-CLONE goes on to the vocabulary, by ordinal. A row the gate refuses
\ answers as no row, which hands a shadowed spelling to the callable path.
: LOOKUP ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ARENA:arena n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id SYM-OWNER-CK
   id IR-ID:SYMBOL-LOCAL {: so:n :}
   r so FIND {: l:n :}
   l 0 >= if r l exit then
   r HC-LINK LCELL@ LINK-CLONE <> if r -1 exit then
   so SESS-ROW {: sl:n :}
   sl 0 < if r -1 exit then
   r sl ROW-BOUND? 0= if r -1 exit then
   0 SESS-R @ sl ;

: ROW-OF ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ARENA:arena n )
   LOOKUP
   dup 0 < if E-HIR-UNMODELED throw then ;

\ Asking a rename for its opcode is a category error, not a missing value.
: ROW-AS ( IR-ARENA:arena IR-ID:ir-symbol-id HIR:meaning -- IR-ARENA:arena n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id want:HIR:meaning :}
   r id ROW-OF {: ra:IR-ARENA:arena l:n :}
   ra l OFF-MEAN RC@ N>MEAN want HIR-MEANING:EQ
   0= if E-HIR-CLASS throw then
   ra l ;

public

\ What this table says a word means. A word it never declared has no meaning at
\ all, which is the same refusal a declared boundary gets.
: MEANING@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:meaning )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id ROW-OF {: ra:IR-ARENA:arena l:n :}
   ra l OFF-MEAN RC@ N>MEAN ;

\ The elaborator asks whether a name the PROGRAM chose for a local collides with
\ a word of the dialect, and "no" is the ordinary case rather than a failure.
: MODELS? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id LOOKUP nip 0 >= ;

\ The tape carries `name:type` as one token while the body reads the name alone,
\ so the annotation is cut off here. `{: a b :}` is a real unannotated shape.
$3A constant ANN-C                   \ the `:` that separates a local from its type

: LOCAL-NAME-LEN ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u
   u 0 ?do
      a i + c@ ANN-C = if drop i leave then
   loop ;

: ADMIT ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:meaning )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id MEANING@ {: m:HIR:meaning :}
   m HIR-MEANING:UNMODELED HIR-MEANING:EQ if E-HIR-UNMODELED throw then
   m ;

: OPCODE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:opcode )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:OP ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-A RC@ HIR:NTH ;

: CONST-OPCODE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:opcode )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CONST-OP ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-A RC@ HIR:NTH ;

: CONST-VALUE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CONST-OP ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-IN RC@ ;

: FIXED-VALUE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:FIXED ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-IN RC@ ;

\ And what that value IS, which the site staging it needs before the number is
\ just a number: an address of the DATA region, or an ordinary integer.
: FIXED-KIND@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:FIXED ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-A RC@ ;

: FIXED-OWNER@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:FIXED ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-DEAD RC@ ;

: ENTRY@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-A RC@ ;

: CALLEE-IN@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-IN RC@ ;

: CALLEE-OUT@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-N RC@ ;

\ The one reader of the fact RESOLVE-CALLABLE put in the row, so every pass that
\ needs it asks one question instead of asking the engine again.
: CALLEE-DEAD? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-DEAD RC@ COMES-BACK <> ;

\ Only live dictionary resolution can prove this stronger fact. A caller's
\ stated no-return contract still gets the diagnostic if its callee returns.
: TERMINAL? ( IR-ARENA:arena IR-ID:ir-symbol-id -- bool )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-DEAD RC@ TERMINAL = ;

: CTRL@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:ctrl )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CONTROL ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-A RC@ N>CTRL ;

: EXPAND@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:expand )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:EXPANSION ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-A RC@ N>EXPAND ;

: RSTACK@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- HIR:rmove )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RSTACK ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-A RC@ N>RSTACK ;

: RSTACK-CELLS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RSTACK ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-IN RC@ ;

\ Which of a callee's result cells belong to a multi-cell value, as the bitmask
\ src/compiler/native/dict.f builds: bit i for the i-th cell from the bottom of
\ the row, which is the order they reach the caller's value vector.
: OUT-GLUE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:CALLABLE ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-GLUE RC@ ;

private

\ Only a cast row answers for its cells and glue: another rename keeps its pick
\ list in those cells.
: CAST-ROW ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ARENA:arena n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RENAME ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-GLUE RC@ HOST-CAST <> if E-HIR-CLASS throw then
   ra l ;

public

: CAST-CELLS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   CAST-ROW OFF-IN RC@ ;

: CAST-GLUE@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   CAST-ROW OFF-A RC@ ;

\ A cast row keeps its regrouped cell count in this slot: read CAST-CELLS@.
: INPUTS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RENAME ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-IN RC@ ;

: HOST-RENAME@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RENAME ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-GLUE RC@ ;

\ A cast row puts none back by pick (CAST-GLUE@ gives its output grouping).
: PICKS ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   r id HIR-MEANING:RENAME ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-N RC@ ;

\ Both the pool window and the depth are rechecked, so a row written past this
\ package's declarers cannot read outside the live pool. The picks belong to
\ the table the row came from; the caller's pair is checked either way, then the
\ answering table's pool against its rows.
: PICK@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n -- n )
   {: p:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-symbol-id i:n :}
   p r PAIR-CK
   r id HIR-MEANING:RENAME ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra HC-LINK LCELL@ LINK-SESSION =
   if 0 SESS-P @ else p then
   {: pa:IR-ARENA:arena :}
   pa ra PAIR-CK
   ra l OFF-N RC@ {: n:n :}
   i 0 < i n >= or if E-HIR-BOUND throw then
   ra l OFF-A RC@ {: st:n :}
   st 0 < st n + pa PCELLS > or if E-HIR-STATE throw then
   pa st i + PC@ {: d:n :}
   d 0 < d ra l OFF-IN RC@ >= or if E-HIR-PICK throw then
   d ;

: REASON@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-symbol-id :}
   r key KEY-CK
   r id HIR-MEANING:UNMODELED ROW-AS {: ra:IR-ARENA:arena l:n :}
   ra l OFF-A RC@
   dup UNUSED = if E-HIR-STATE throw then
   1- key swap IR-ID:PACK-SYMBOL ;

: AT ( IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key i:n :}
   r key KEY-CK
   i 0 < if E-HIR-BOUND throw then
   i r CNT >= if E-HIR-BOUND throw then
   key r i OFF-SYM RC@ IR-ID:PACK-SYMBOL ;

\ ---- the tape join -----------------------------------------------------------
\ The key is PRESENTED rather than derived, because deriving it needs the
\ module's interner and this join is also asked of a frozen module. Literal
\ token kinds carry their own meaning; only names need word-model lookup.
: ADMIT-TOKEN ( IR-ARENA:view IR-ARENA:arena n IR-ID:ir-symbol-id -- HIR:meaning )
   {: v:IR-ARENA:view r:IR-ARENA:arena i:n sy:IR-ID:ir-symbol-id :}
   v i NTAPE:KIND@ {: k:NTAPE:kind :}
   k NTAPE-KIND:INT-LITERAL NTAPE-KIND:EQ if HIR-MEANING:LITERAL exit then
   k NTAPE-KIND:CHAR-LITERAL NTAPE-KIND:EQ if HIR-MEANING:LITERAL exit then
   k NTAPE-KIND:REAL-LITERAL NTAPE-KIND:EQ if HIR-MEANING:REAL-LITERAL exit then
   k NTAPE-KIND:STRING-LITERAL NTAPE-KIND:EQ if HIR-MEANING:STRING-LITERAL exit then
   k NTAPE-KIND:COUNTED-STRING-LITERAL NTAPE-KIND:EQ if HIR-MEANING:STRING-LITERAL exit then
   k NTAPE-KIND:PRINTED-STRING-LITERAL NTAPE-KIND:EQ if HIR-MEANING:STRING-LITERAL exit then
   k NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-HIR-KIND throw then
   r sy ADMIT ;

\ ---- the subset's vocabulary -------------------------------------------------
\ The exact ceilings this registration writes, so a caller commits a table to
\ them rather than to a guess. Only the fourteen renames contribute pick cells.
99 constant WORDS
33 constant PICK-CELLS

private

: DEF-ARITH ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" +" MODEL-SYM HIR-OPCODE:ADD BDECLARE-OP
   c b r c b s" -" MODEL-SYM HIR-OPCODE:SUB BDECLARE-OP
   c b r c b s" *" MODEL-SYM HIR-OPCODE:MUL BDECLARE-OP
   c b r c b s" /" MODEL-SYM HIR-OPCODE:DIV BDECLARE-OP ;

: DEF-COMPARE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" <" MODEL-SYM HIR-OPCODE:LT BDECLARE-OP
   c b r c b s" <=" MODEL-SYM HIR-OPCODE:LE BDECLARE-OP
   c b r c b s" >" MODEL-SYM HIR-OPCODE:GT BDECLARE-OP
   c b r c b s" >=" MODEL-SYM HIR-OPCODE:GE BDECLARE-OP
   c b r c b s" =" MODEL-SYM HIR-OPCODE:EQUAL BDECLARE-OP
   c b r c b s" <>" MODEL-SYM HIR-OPCODE:NE BDECLARE-OP ;

\ `lshift` and `rshift` take a count the program computed, so they are two
\ operand words here and not a value and a field.
: DEF-BITWISE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" and" MODEL-SYM HIR-OPCODE:AND BDECLARE-OP
   c b r c b s" or" MODEL-SYM HIR-OPCODE:OR BDECLARE-OP
   c b r c b s" xor" MODEL-SYM HIR-OPCODE:XOR BDECLARE-OP
   c b r c b s" lshift" MODEL-SYM HIR-OPCODE:LSHIFT BDECLARE-OP
   c b r c b s" rshift" MODEL-SYM HIR-OPCODE:RSHIFT BDECLARE-OP
   c b r c b s" invert" MODEL-SYM HIR-OPCODE:INVERT BDECLARE-OP ;

\ `0=` is `0` then `=`, which answers false for EVERY nonzero value; `cells` is
\ the language cell byte width then `*`; pointer width is a separate target fact.
: DEF-STEP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" 1-" MODEL-SYM HIR-OPCODE:SUB 1 BDECLARE-CONST-OP
   c b r c b s" 1+" MODEL-SYM HIR-OPCODE:ADD 1 BDECLARE-CONST-OP
   c b r c b s" 0=" MODEL-SYM HIR-OPCODE:EQUAL 0 BDECLARE-CONST-OP
   c b r c b s" cells" MODEL-SYM HIR-OPCODE:MUL HIR:CELL-BYTES BDECLARE-CONST-OP
   c b r c b s" cell+" MODEL-SYM HIR-OPCODE:ADD HIR:CELL-BYTES BDECLARE-CONST-OP ;

\ The width is not stored: it is which opcode the row names, because hir.f makes
\ the width a form. The memory order is the dialect's own token.
: DEF-MEMORY ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" @" MODEL-SYM HIR-OPCODE:LOAD BDECLARE-OP
   c b r c b s" !" MODEL-SYM HIR-OPCODE:STORE BDECLARE-OP
   c b r c b s" c@" MODEL-SYM HIR-OPCODE:BLOAD BDECLARE-OP
   c b r c b s" c!" MODEL-SYM HIR-OPCODE:BSTORE BDECLARE-OP ;

\ Words that are a short sequence of this dialect's operations (`ptr-field`,
\ `mod`, `max`, `min`); the sequences live in src/compiler/native/elaborate.f
\ and a row says only which one.
: DEF-EXPAND ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" ptr-field" MODEL-SYM HIR-EXPAND:CELL-INDEX BDECLARE-EXPAND
   c b r c b s" mod" MODEL-SYM HIR-EXPAND:MODULO BDECLARE-EXPAND
   c b r c b s" max" MODEL-SYM HIR-EXPAND:MAXIMUM BDECLARE-EXPAND
   c b r c b s" min" MODEL-SYM HIR-EXPAND:MINIMUM BDECLARE-EXPAND ;

\ `s>f` rounds to nearest with ties to even; `f>s` truncates toward zero,
\ saturates at the ends and answers zero for a NaN - two roundings, two rows.
\ A NaN made from numbers - infinity minus infinity, zero times infinity, zero
\ over zero, infinity over infinity, the root of a negative - is
\ $7FF8000000000000 on every target, and a quiet NaN operand passes through
\ unchanged, the left one of two (docs/xcomp.md "The NaN an operation makes").
: DEF-FLOAT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" f+" MODEL-SYM HIR-OPCODE:FADD BDECLARE-OP
   c b r c b s" f-" MODEL-SYM HIR-OPCODE:FSUB BDECLARE-OP
   c b r c b s" f*" MODEL-SYM HIR-OPCODE:FMUL BDECLARE-OP
   c b r c b s" f/" MODEL-SYM HIR-OPCODE:FDIV BDECLARE-OP
   c b r c b s" fnegate" MODEL-SYM HIR-OPCODE:FNEG BDECLARE-OP
   c b r c b s" fabs" MODEL-SYM HIR-OPCODE:FABS BDECLARE-OP
   c b r c b s" fsqrt" MODEL-SYM HIR-OPCODE:FSQRT BDECLARE-OP
   c b r c b s" s>f" MODEL-SYM HIR-OPCODE:INTREAL BDECLARE-OP
   c b r c b s" f>s" MODEL-SYM HIR-OPCODE:REALINT BDECLARE-OP ;

\ The two comparisons against zero are their own operations: FCMP against the
\ immediate zero takes no second register.
: DEF-FCOMPARE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" f<" MODEL-SYM HIR-OPCODE:FLT BDECLARE-OP
   c b r c b s" f>" MODEL-SYM HIR-OPCODE:FGT BDECLARE-OP
   c b r c b s" f=" MODEL-SYM HIR-OPCODE:FEQ BDECLARE-OP
   c b r c b s" f0<" MODEL-SYM HIR-OPCODE:FLTZ BDECLARE-OP
   c b r c b s" f0=" MODEL-SYM HIR-OPCODE:FEQZ BDECLARE-OP ;

\ `begin` has three closers and the counted loop two openers with one closer, so
\ a row says which word it is and the elaborator's control stack learns the rest.
\ `RECURSE` is interned in capitals because docs/forth.md spells it so; the row
\ is keyed by the fold, so any case reaches it.
: DEF-CONTROL ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" if" MODEL-SYM HIR-CTRL:OPEN-IF BDECLARE-CONTROL
   c b r c b s" else" MODEL-SYM HIR-CTRL:MID-ELSE BDECLARE-CONTROL
   c b r c b s" then" MODEL-SYM HIR-CTRL:CLOSE-IF BDECLARE-CONTROL
   c b r c b s" begin" MODEL-SYM HIR-CTRL:OPEN-BEGIN BDECLARE-CONTROL
   c b r c b s" while" MODEL-SYM HIR-CTRL:MID-WHILE BDECLARE-CONTROL
   c b r c b s" until" MODEL-SYM HIR-CTRL:CLOSE-UNTIL BDECLARE-CONTROL
   c b r c b s" repeat" MODEL-SYM HIR-CTRL:CLOSE-REPEAT BDECLARE-CONTROL
   c b r c b s" again" MODEL-SYM HIR-CTRL:CLOSE-AGAIN BDECLARE-CONTROL
   c b r c b s" do" MODEL-SYM HIR-CTRL:OPEN-DO BDECLARE-CONTROL
   c b r c b s" ?do" MODEL-SYM HIR-CTRL:OPEN-DO-SKIP BDECLARE-CONTROL
   c b r c b s" loop" MODEL-SYM HIR-CTRL:CLOSE-LOOP BDECLARE-CONTROL
   c b r c b s" +loop" MODEL-SYM HIR-CTRL:CLOSE-LOOP-STEP BDECLARE-CONTROL
   c b r c b s" i" MODEL-SYM HIR-CTRL:INDEX BDECLARE-CONTROL
   c b r c b s" j" MODEL-SYM HIR-CTRL:OUTER-INDEX BDECLARE-CONTROL
   c b r c b s" unloop" MODEL-SYM HIR-CTRL:DROP-LOOP BDECLARE-CONTROL
   c b r c b s" leave" MODEL-SYM HIR-CTRL:EARLY-LEAVE BDECLARE-CONTROL
   c b r c b s" exit" MODEL-SYM HIR-CTRL:EARLY-EXIT BDECLARE-CONTROL
   c b r c b s" RECURSE" MODEL-SYM HIR-CTRL:SELF-CALL BDECLARE-CONTROL ;

\ `of` and `endof` are ONE row each and serve both `MATCH` and `case`; which
\ form an arm belongs to is decided by the structure the elaborator has open.
: DEF-ADT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" match" MODEL-SYM HIR-CTRL:OPEN-MATCH BDECLARE-CONTROL
   c b r c b s" of" MODEL-SYM HIR-CTRL:MATCH-ARM BDECLARE-CONTROL
   c b r c b s" endof" MODEL-SYM HIR-CTRL:CLOSE-ARM BDECLARE-CONTROL
   c b r c b s" ;match" MODEL-SYM HIR-CTRL:CLOSE-MATCH BDECLARE-CONTROL
   c b r c b s" case" MODEL-SYM HIR-CTRL:OPEN-CASE BDECLARE-CONTROL
   c b r c b s" endcase" MODEL-SYM HIR-CTRL:CLOSE-CASE BDECLARE-CONTROL
   c b r c b s" construct" MODEL-SYM HIR-CTRL:MAKE-BUNDLE BDECLARE-CONTROL ;

\ Control actions, because what stands between them is a second FUNCTION of the
\ module. Neither row carries a payload.
: DEF-QUOT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" [:" MODEL-SYM HIR-CTRL:OPEN-QUOT BDECLARE-CONTROL
   c b r c b s" ;]" MODEL-SYM HIR-CTRL:CLOSE-QUOT BDECLARE-CONTROL
   c b r c b s" [']" MODEL-SYM HIR-CTRL:TICK BDECLARE-CONTROL ;

\ None carries a payload: `is` moves what the DEFERRED word declares, `execute`
\ moves one cell more than the quotation reaching it, and `catch` moves the
\ window the checker certified at that site - all facts about the site.
\ Branch targets come from src/compiler/native/dict.f at the site.
: DEF-QUOT-USE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" is" MODEL-SYM HIR-CTRL:BIND-DEFER BDECLARE-CONTROL
   c b r c b s" execute" MODEL-SYM HIR-CTRL:EXEC BDECLARE-CONTROL
   c b r c b s" catch" MODEL-SYM HIR-CTRL:CATCH BDECLARE-CONTROL
   c b r c b s" evaluate" MODEL-SYM HIR-CTRL:EVAL BDECLARE-CONTROL
   c b r c b s" finally" MODEL-SYM HIR-CTRL:FINALLY BDECLARE-CONTROL
   c b r c b s" c2-invoke" MODEL-SYM HIR-CTRL:C2-INVOKE BDECLARE-CONTROL ;

\ Neither stages an operation and neither carries a payload: the work is the
\ elaborator's over the tape rows between them.
: DEF-LOCALS ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" {:" MODEL-SYM HIR-MEANING:OPEN-LOCALS BDECLARE-PLAIN
   c b r c b s" :}" MODEL-SYM HIR-MEANING:CLOSE-LOCALS BDECLARE-PLAIN ;

: DEF-DUP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   1 BEGIN-RENAME
   0 ADD-PICK
   0 ADD-PICK
   c b p r c b s" dup" MODEL-SYM BDECLARE-RENAME ;

: DEF-DROP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   1 BEGIN-RENAME
   c b p r c b s" drop" MODEL-SYM BDECLARE-RENAME ;

: DEF-SWAP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   0 ADD-PICK
   1 ADD-PICK
   c b p r c b s" swap" MODEL-SYM BDECLARE-RENAME ;

: DEF-OVER ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   1 ADD-PICK
   c b p r c b s" over" MODEL-SYM BDECLARE-RENAME ;

: DEF-NIP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   0 ADD-PICK
   c b p r c b s" nip" MODEL-SYM BDECLARE-RENAME ;

: DEF-2DUP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   1 ADD-PICK
   0 ADD-PICK
   c b p r c b s" 2dup" MODEL-SYM BDECLARE-RENAME ;

\ 2drop emits no instruction: the two values leave the compile-time vector.
: DEF-2DROP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   c b p r c b s" 2drop" MODEL-SYM BDECLARE-RENAME ;

\ The two pairs are four logical values, each of which may occupy several
\ cells. Keep their bundle boundaries when the native tier renames them.
: DEF-2SWAP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   4 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   3 ADD-PICK
   2 ADD-PICK
   c b p r c b s" 2swap" MODEL-SYM BDECLARE-RENAME ;

: DEF-2OVER ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   4 BEGIN-RENAME
   3 ADD-PICK
   2 ADD-PICK
   1 ADD-PICK
   0 ADD-PICK
   3 ADD-PICK
   2 ADD-PICK
   c b p r c b s" 2over" MODEL-SYM BDECLARE-RENAME ;

: DEF-ROT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   3 BEGIN-RENAME
   1 ADD-PICK
   0 ADD-PICK
   2 ADD-PICK
   c b p r c b s" rot" MODEL-SYM BDECLARE-RENAME ;

\ -rot ( a b c -- c a b ): preserve each input value's complete cell bundle.
: DEF-MROT ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   3 BEGIN-RENAME
   0 ADD-PICK
   2 ADD-PICK
   1 ADD-PICK
   c b p r c b s" -rot" MODEL-SYM BDECLARE-RENAME ;

: DEF-TUCK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   2 BEGIN-RENAME
   0 ADD-PICK
   1 ADD-PICK
   0 ADD-PICK
   c b p r c b s" tuck" MODEL-SYM BDECLARE-RENAME ;

\ The memory views ( ptr a -- ptr b ) put the same value back: a view changes
\ only the pointee the checker sees, and stages no operation.
: DEF-CELL-VIEW ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   1 BEGIN-RENAME
   0 ADD-PICK
   HOST-VIEW STG-HOST !
   c b p r c b s" cell-view" MODEL-SYM BDECLARE-RENAME ;

: DEF-BYTE-VIEW ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   1 BEGIN-RENAME
   0 ADD-PICK
   HOST-VIEW STG-HOST !
   c b p r c b s" byte-view" MODEL-SYM BDECLARE-RENAME ;

\ ---- the return-stack words --------------------------------------------------
\ `2>r` keeps the lower cell lower on the return stack; elaborate.f keeps that
\ order.
: DEF-RSTACK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:IR-ARENA:arena :}
   c b r c b s" >r"  MODEL-SYM HIR-RMOVE:TO-R    1 BDECLARE-RSTACK
   c b r c b s" r>"  MODEL-SYM HIR-RMOVE:FROM-R  1 BDECLARE-RSTACK
   c b r c b s" r@"  MODEL-SYM HIR-RMOVE:FETCH-R 1 BDECLARE-RSTACK
   c b r c b s" 2>r" MODEL-SYM HIR-RMOVE:TO-R    2 BDECLARE-RSTACK
   c b r c b s" 2r>" MODEL-SYM HIR-RMOVE:FROM-R  2 BDECLARE-RSTACK
   c b r c b s" 2r@" MODEL-SYM HIR-RMOVE:FETCH-R 2 BDECLARE-RSTACK ;

private

\ A `create`d data word and a called word are NOT here: which of those a program
\ has is a fact about the program, declared by its own caller.
: REGISTER-ALL ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b r DEF-ARITH
   c b r DEF-COMPARE
   c b r DEF-BITWISE
   c b r DEF-STEP
   c b r DEF-MEMORY
   c b r DEF-EXPAND
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
   c b p r DEF-MROT
   c b p r DEF-TUCK
   c b p r DEF-CELL-VIEW
   c b p r DEF-BYTE-VIEW
   c b p r DEF-2DROP
   c b p r DEF-2SWAP
   c b p r DEF-2OVER
   c b r DEF-RSTACK ;

public

: REGISTER-WORDS ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- )
   REGISTER-ALL ;

private

\ ---- the session's one table -------------------------------------------------
\ Whether this module's ordinals are the prototype's: IR-BUILD started its
\ interner as a copy of the prototype, so it is asked there. Sampling spellings
\ answers yes for any module holding two of them at two ordinals and silently
\ maps a definition's symbols onto session rows.
: CLONE? ( IR-BUILD:builder -- bool )
   {: b:IR-BUILD:builder :}
   SESS-ON @ 0= if false exit then
   0 SESS-R @ CNT 0= if false exit then
   b 0 SESS-ROWS @ IR-BUILD:CLONED-FROM? ;

public

\ The overlay a definition writes its own callable and fixed rows into, keyed by
\ the definition's module; it defers to the session vocabulary for a spelling it
\ does not hold.
: NEW-LINKED ( IR-CTX:ctx IR-BUILD:builder n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rcap:n pcap:n :}
   c b IR-BUILD:MODULE-KEY rcap pcap
   b CLONE? if LINK-CLONE else LINK-NONE then NEW-TABLE ;

\ Register the vocabulary once per load, then intern its spellings in the
\ prototype so every clone holds them at the ordinals the map records; the map
\ is keyed by the prototype's ordinals because that is what a token presents.
: SESSION-MODEL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-BUILD:builder -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena sr:IR-ARENA:arena
      k:IR-ID:ir-module-key b:IR-BUILD:builder :}
   SESS-FORGET
   c b IR-BUILD:MODULE-KEY WORDS PICK-CELLS LINK-SESSION NEW-TABLE
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r REGISTER-WORDS
   r CNT SROW-CAP > if E-HIR-CAP throw then
   ORD-CAP 0 ?do 0 i ORD-ROW! loop
   SROW-CAP 0 ?do 0 i CHK-GEN! loop
   r CNT 0 ?do
      r  b IR-BUILD:MODULE-KEY  i AT {: id:IR-ID:ir-symbol-id :}
      c b id GATE-BUF GATE-CAP IR-BUILD:SYMBOL-COPY {: u:n :}
      c a sr k GATE-BUF u IR-SYM:INTERN IR-ID:SYMBOL-LOCAL {: ord:n :}
      ord ORD-CAP >= if E-HIR-CAP throw then
      ord ORD-ROW@ 0<> if E-HIR-DUP throw then
      i 1+ ord ORD-ROW!
      ord i ROW-ORD!
   loop
   a 0 SESS-POOL !
   sr 0 SESS-ROWS !
   k 0 SESS-KEY !
   p 0 SESS-P !
   r 0 SESS-R !
   sr IR-SYM:SYMBOLS {: ns:n :}
   ns ORD-CAP > if ORD-CAP else ns then ORD-N !
   1 SESS-ON ! ;

\ Give the session's table up. The arenas die with the session context; what is
\ forgotten here is the claim that they are readable.
: SESSION-MODEL-CLEAR ( -- )
   SESS-FORGET ;

\ How many rows the session's vocabulary holds, which is what a caller checks
\ when it wants to know the load registered once and not once per definition.
: SESSION-ROWS ( -- n )
   SESS-ON @ 0= if 0 exit then
   0 SESS-R @ CNT ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
