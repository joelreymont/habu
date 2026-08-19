\ immediate.f - the native immediate-word contract table: which compile-time
\ words checked source may compile, and under what contract.
\
\ A `compile-time` row is permission and not a seal: nothing yet confines such a
\ word to the HIR builder (habu-seal-the-compile-5f56e5e9).
\
\ One table per module. Rows live on the context's arena, keyed by that module's
\ own interned symbols, so a foreign module's symbol cannot enter it.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f
require src/compiler/native/tape.f

package NIMM
public

\ intrinsic: a registered front-end compiler word. compile-time: a checked
\ immediate that may run during elaboration. unmodeled: a named boundary.
ENUM class DERIVE eq
   intrinsic
   compile-time
   unmodeled
;ENUM

private

CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n )
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n )

\ ---- layout ------------------------------------------------------------------
$4E494D31 constant IMM-MAGIC         \ "NIM1": the contract-table header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-SYM                   \ the classified word's symbol ordinal
1 constant OFF-CLASS
2 constant OFF-REASON                \ the naming symbol's ordinal plus one
3 constant ROW-CELLS
0 constant NO-REASON                 \ reason cell of a class that names nothing
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX

\ ---- stored codes ------------------------------------------------------------
: CLASS-CODE ( NIMM:class -- n )
   MATCH class
      intrinsic    OF 0 ENDOF
      compile-time OF 1 ENDOF
      unmodeled    OF 2 ENDOF
   ;MATCH ;

: N>CLASS ( n -- NIMM:class )
   case
      0 of NIMM-CLASS:INTRINSIC endof
      1 of NIMM-CLASS:COMPILE-TIME endof
      2 of NIMM-CLASS:UNMODELED endof
      E-NIMM-CLASS throw
   endcase ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

\ ---- header and shape --------------------------------------------------------
: SHAPE-CK ( n -- )
   dup HDR-CELLS < if E-NIMM-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-NIMM-STATE throw then ;

: HDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED SHAPE-CK
   a HC-MAGIC LCELL@ IMM-MAGIC <> if E-NIMM-STATE throw then ;

: CNT ( IR-ARENA:arena -- n )
   IR-ARENA:USED HDR-CELLS - ROW-CELLS / ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-NIMM-OWNER throw then ;

: KEY-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: a:IR-ARENA:arena key:IR-ID:ir-module-key :}
   a HDR-CK
   a HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

\ A declared symbol carries its owning module, so no presented key is needed to
\ bind a row to this table's module.
: SYM-OWNER-CK ( IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: a:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   a HDR-CK
   a HC-SERIAL LCELL@ id IR-ID:SYMBOL-OWNER MID-SERIAL SERIAL-CK ;

\ ---- row addressing ----------------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

\ One scan serves the lookup, the duplicate check and the inventory walk.
: FIND ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena so:n :}
   -1
   a CNT 0 ?do
      a i OFF-SYM RC@ so = if drop i leave then
   loop ;

: CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-NIMM-CAP throw then
   drop ;

: ROOM-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a CNT a HC-CAP LCELL@ >= if E-NIMM-CAP throw then ;

public

\ Create a module's immediate-word contract table, its cell ceiling committed
\ to exactly cap declarations and its header bound to key's module serial.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n -- IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key cap:n :}
   cap CAP-OK
   c cap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a IMM-MAGIC IR-ARENA:PUSH drop
   c a key KEY-SERIAL IR-ARENA:PUSH drop
   c a cap IR-ARENA:PUSH drop
   a ;

private

\ Both checks have already passed: the symbol is this module's and interned.
: WRITE ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-symbol-id n n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena id:IR-ID:ir-symbol-id cls:n rsn:n :}
   id IR-ID:SYMBOL-LOCAL {: so:n :}
   a so FIND 0 < 0= if E-NIMM-DUP throw then
   a ROOM-CK
   c a so IR-ARENA:PUSH drop
   c a cls IR-ARENA:PUSH drop
   c a rsn IR-ARENA:PUSH drop ;

\ The symbol store is asked for the symbol's length, which is its own ownership
\ and bound check, so a spelling this module never interned cannot be classified.
: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id cls:n rsn:n :}
   a id SYM-OWNER-CK
   sy id IR-SYM:LEN@ drop
   c a id cls rsn WRITE ;

\ A module still being built holds its interner privately in ir/build.f, so the
\ builder answers - by asking IR-SYM, the same refusal under the same name.
: BROW-ADD ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ARENA:arena
      id:IR-ID:ir-symbol-id cls:n rsn:n :}
   a id SYM-OWNER-CK
   c b id IR-BUILD:SYMBOL-CK
   c a id cls rsn WRITE ;

public

\ `unmodeled` is refused here: a boundary has to name what it is waiting for.
: DECLARE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id NIMM:class -- )
   {: cl:NIMM:class :}
   cl NIMM-CLASS:UNMODELED NIMM-CLASS:EQ if E-NIMM-CLASS throw then
   cl CLASS-CODE NO-REASON ROW-ADD ;

\ ir/build.f is the single mutation route to a module still being built.
: DECLARE-INTO ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id NIMM:class -- )
   {: cl:NIMM:class :}
   cl NIMM-CLASS:UNMODELED NIMM-CLASS:EQ if E-NIMM-CLASS throw then
   cl CLASS-CODE NO-REASON BROW-ADD ;

\ The reason symbol names the capability whose absence is why checked source may
\ not compile this word, so a refusal can say what has to land.
: DECLARE-UNMODELED ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- )
   {: a:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id why:IR-ID:ir-symbol-id :}
   a why SYM-OWNER-CK
   sy why IR-SYM:LEN@ drop
   a sy id
   NIMM-CLASS:UNMODELED CLASS-CODE
   why IR-ID:SYMBOL-LOCAL 1+
   ROW-ADD ;

\ Both symbols are that module's, so both go to its builder.
: DECLARE-UNMODELED-INTO ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ARENA:arena
      id:IR-ID:ir-symbol-id why:IR-ID:ir-symbol-id :}
   a why SYM-OWNER-CK
   c b why IR-BUILD:SYMBOL-CK
   c b a id
   NIMM-CLASS:UNMODELED CLASS-CODE
   why IR-ID:SYMBOL-LOCAL 1+
   BROW-ADD ;

\ ---- reading -----------------------------------------------------------------
: DECLARED ( IR-ARENA:arena -- n )
   dup HDR-CK CNT ;

private

: ROW-OF ( IR-ARENA:arena IR-ID:ir-symbol-id -- n )
   {: a:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   a id SYM-OWNER-CK
   a id IR-ID:SYMBOL-LOCAL FIND
   dup 0 < if E-NIMM-UNMODELED throw then ;

public

\ A word never classified gets the same refusal a declared unmodeled one does.
: CLASS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- NIMM:class )
   {: a:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   a id ROW-OF {: l:n :}
   a l OFF-CLASS RC@ N>CLASS ;

\ The gate: the contract under which checked source may compile this word.
: ADMIT ( IR-ARENA:arena IR-ID:ir-symbol-id -- NIMM:class )
   {: a:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   a id CLASS@ {: cl:NIMM:class :}
   cl NIMM-CLASS:UNMODELED NIMM-CLASS:EQ if E-NIMM-UNMODELED throw then
   cl ;

\ Only an unmodeled entry names one; asking a modeled word is a category error.
: REASON@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: a:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-symbol-id :}
   a key KEY-CK
   a id ROW-OF {: l:n :}
   a l OFF-CLASS RC@ N>CLASS NIMM-CLASS:UNMODELED NIMM-CLASS:EQ
   0= if E-NIMM-CLASS throw then
   a l OFF-REASON RC@
   dup NO-REASON = if E-NIMM-STATE throw then
   1- key swap IR-ID:PACK-SYMBOL ;

\ Declaration order - what an inventory of the remaining boundaries walks.
: AT ( IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: a:IR-ARENA:arena key:IR-ID:ir-module-key i:n :}
   a key KEY-CK
   i 0 < if E-NIMM-BOUND throw then
   i a CNT >= if E-NIMM-BOUND throw then
   key a i OFF-SYM RC@ IR-ID:PACK-SYMBOL ;

\ ---- the tape join -----------------------------------------------------------
\ Only a name token can name a word; a literal is a caller error, not a boundary.
: ADMIT-TOKEN ( IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena n -- NIMM:class )
   {: v:IR-ARENA:view key:IR-ID:ir-module-key a:IR-ARENA:arena i:n :}
   v i NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ
   0= if E-NTAPE-KIND throw then
   a v key i NTAPE:SPELL@ ADMIT ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
