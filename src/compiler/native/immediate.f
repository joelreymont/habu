\ immediate.f - the native immediate-word contract table: which compile-time
\ words checked source may compile, and under what contract.
\
\ docs/compiler-ir-design.md section 7.1, the three classes of compile-time
\ immediate word. A front-end intrinsic is a registered compiler word that
\ calls HIR builder operations. Sealed compile-time computation is a checked
\ immediate that may run during elaboration but may reach the generated program
\ only through the HIR builder capability. Everything else is an unmodeled
\ boundary: checked source must not compile it, and while such a boundary
\ exists it has to be named, tested, inventoried, and scheduled for retirement.
\
\ WHY A TABLE AND NOT A LIST OF SPECIAL CASES. The elaborator needs one answer
\ per word, and it needs the refusal to name the word rather than fail
\ anonymously somewhere downstream. A declared unmodeled entry carries the
\ symbol naming the capability that must land before it can be retired, so a
\ refusal can say what is missing; an undeclared word refuses too, but has
\ nothing to say beyond its own name. Both refusals are E-NIMM-UNMODELED,
\ because to checked source they are the same event: this word has no
\ elaboration contract.
\
\ WHAT THIS FILE DOES NOT DO. It does not seal the compile-time capability. A
\ `compile-time` entry records that a word is allowed to run during elaboration;
\ the guarantee that it can only reach the program through the builder is the
\ HIR builder's to enforce, and there is no builder yet
\ (habu-elaborate-straight-line-72b55798). Until then this class is a declared
\ intent, and dot habu-seal-the-compile-5f56e5e9 tracks the capability that
\ turns it into a proof. It also does not decide which words are immediate:
\ that is the frozen checker environment's fact (habu-bind-checker-env-ed4f9f87).
\ This table answers "may this immediate word be compiled, and how", not "is
\ this word immediate".
\
\ ONE TABLE SERVES ONE MODULE. Rows live on an IR-ARENA arena owned by the
\ compilation context and are keyed by the module's own interned symbols, so
\ the table dies with its context and a foreign module's symbol cannot enter
\ it. Lookup is a linear scan of the declared rows: the table holds the
\ compiler's immediate vocabulary, tens of entries, and a scan keeps the row
\ shape flat and the ordering observable for the inventory walk that section
\ 7.1 requires of every boundary.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/symbol.f
require src/compiler/native/tape.f

package NIMM
public

\ `intrinsic` is a registered front-end compiler word. `compile-time` is a
\ checked immediate that may run during elaboration. `unmodeled` is a named
\ boundary checked source may not compile.
ENUM class DERIVE eq
   intrinsic
   compile-time
   unmodeled
;ENUM

private

CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n ) ;

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

\ The row that classifies this symbol, or a negative answer. One scan serves
\ the lookup, the duplicate check, and the inventory walk.
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

\ Append one validated row. The symbol store is asked for the symbol's length,
\ which is its own ownership and bound check, so a spelling this module never
\ interned cannot be classified.
: ROW-ADD ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id n n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id cls:n rsn:n :}
   a id SYM-OWNER-CK
   sy id IR-SYM:LEN@ drop
   id IR-ID:SYMBOL-LOCAL {: so:n :}
   a so FIND 0 < 0= if E-NIMM-DUP throw then
   a ROOM-CK
   c a so IR-ARENA:PUSH drop
   c a cls IR-ARENA:PUSH drop
   c a rsn IR-ARENA:PUSH drop ;

public

\ Declare a word this compiler models: a front-end intrinsic, or a checked
\ immediate allowed to run during elaboration. Handing this word `unmodeled`
\ is refused - an unmodeled boundary has to name what it is waiting for, so it
\ has its own declarer.
: DECLARE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id NIMM:class -- )
   {: cl:NIMM:class :}
   cl NIMM-CLASS:UNMODELED NIMM-CLASS:EQ if E-NIMM-CLASS throw then
   cl CLASS-CODE NO-REASON ROW-ADD ;

\ Declare a named unmodeled boundary. The reason symbol names the capability
\ whose absence is why checked source may not compile this word, so a refusal
\ can say what has to land before the boundary can be retired.
: DECLARE-UNMODELED ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- )
   {: a:IR-ARENA:arena sy:IR-ARENA:arena
      id:IR-ID:ir-symbol-id why:IR-ID:ir-symbol-id :}
   a why SYM-OWNER-CK
   sy why IR-SYM:LEN@ drop
   a sy id
   NIMM-CLASS:UNMODELED CLASS-CODE
   why IR-ID:SYMBOL-LOCAL 1+
   ROW-ADD ;

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

\ The declared class of a word. A word this table never classified has no
\ contract at all, which is the same refusal a declared unmodeled boundary
\ gets when it is asked to compile.
: CLASS@ ( IR-ARENA:arena IR-ID:ir-symbol-id -- NIMM:class )
   {: a:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   a id ROW-OF {: l:n :}
   a l OFF-CLASS RC@ N>CLASS ;

\ The gate. Answers the contract under which checked source may compile this
\ immediate word, and refuses everything else by name.
: ADMIT ( IR-ARENA:arena IR-ID:ir-symbol-id -- NIMM:class )
   {: a:IR-ARENA:arena id:IR-ID:ir-symbol-id :}
   a id CLASS@ {: cl:NIMM:class :}
   cl NIMM-CLASS:UNMODELED NIMM-CLASS:EQ if E-NIMM-UNMODELED throw then
   cl ;

\ The capability an unmodeled boundary is waiting for. Only an unmodeled entry
\ names one; asking a modeled word is a category error, not a missing value.
: REASON@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   {: a:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-symbol-id :}
   a key KEY-CK
   a id ROW-OF {: l:n :}
   a l OFF-CLASS RC@ N>CLASS NIMM-CLASS:UNMODELED NIMM-CLASS:EQ
   0= if E-NIMM-CLASS throw then
   a l OFF-REASON RC@
   dup NO-REASON = if E-NIMM-STATE throw then
   1- key swap IR-ID:PACK-SYMBOL ;

\ The i-th declared word, in declaration order. This is what an inventory of
\ the remaining boundaries walks.
: AT ( IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: a:IR-ARENA:arena key:IR-ID:ir-module-key i:n :}
   a key KEY-CK
   i 0 < if E-NIMM-BOUND throw then
   i a CNT >= if E-NIMM-BOUND throw then
   key a i OFF-SYM RC@ IR-ID:PACK-SYMBOL ;

\ ---- the tape join -----------------------------------------------------------
\ The elaborator walks a sealed tape and, for each name token the frozen
\ checker environment says is immediate, asks this table whether it may be
\ compiled. Only a name token can name a word: a literal is not a call, so
\ asking about one is a caller error rather than an unmodeled boundary.
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
