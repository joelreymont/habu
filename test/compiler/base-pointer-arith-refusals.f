\ base-pointer-arith-refusals.f - the rejected programs for "a pointer derived
\ from a base address holds a plain value" (dot habu-bound-ptr-arithmetic-8bf6b54a,
\ first half).
\
\ WHAT WAS WRONG. `data-base` and `NULL-PTR` are the two pointers in the language
\ that are not the address OF a declared element, and both published `-- ptr a`
\ with an ordinary polymorphic pointee. Pointer arithmetic is
\ pointee-polymorphic (`ptr a n + -- ptr a`), so the CALLER chose what the cell
\ under the derived address contained: `NEWTYPE thing 0  : F ( n -- thing )
\ NULL-PTR + @ ;` certified and forged a nominal identity out of an integer, the
\ same program with `( n -- ptr n )` forged an address, and `data-base + @` did
\ both from the engine's own DATA. `data-base @` and `NULL-PTR @` did it at
\ offset zero with no arithmetic at all. The raw-storage rule
\ (habu-refuse-a-ptr-5ad2734e) did not reach any of it: neither base is a raw
\ storage cell, and `data-base` must stay ordinary because DATA-CELL@ and the
\ AOT linker read scalars through it.
\
\ WHAT IS PINNED HERE. The pointee of those two rows is TVK-BASE, which is
\ fenced in VALUE position and permissive inside a POINTEE. So a value READ
\ through a base address is never a nominal type and never a pointer -- refused
\ by name, E-RAW-CELL-PTR with its own reason -- while the pointer ITSELF still
\ compares, subtracts, tests and stores like any pointer, which is every honest
\ use the tree makes of `NULL-PTR`. A wrapper that publishes the derived pointer
\ under a type variable (`( -- ptr a ) data-base OFF +`) is refused at its own
\ declaration instead of at each of its callers.
\
\ The controls beside them matter as much as the refusals: the scalar cell reads
\ the engine and the AOT linker make through `data-base`, the `ptr-field` door
\ that reaches a DATA cell holding a real address, and the byte view the linker
\ takes before it does any arithmetic at all, each keep certifying. A rule that
\ also refuses those is not this rule.
\
\ These run against the live engine's own checker through CHECK-CANDIDATE!, the
\ public probing verdict word, so no `evaluate` boundary and no trust row stands
\ between a fixture and the checker that answers it.
require lib/test.f
require lib/string.f

\ The nominal identity the forgeries mint, and a declared pointer cell for the
\ null store. Both are file-global because the candidates below are checked in
\ the scope a real program writes them in.
NEWTYPE bpathing 0
PTR-VARIABLE BPA-SLOT

package BASE-PTR-ARITH-TEST

private

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   DIAG-BUF DIAG-CAP DIAG-BUFFER!  true DIAG-JSON! ;

: DISARM ( -- )
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

: CODE$ ( -- ptr u8 n )
   S\" \"code\":\"E-RAW-CELL-PTR\"" ;

: REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"base address: a cell reached from data-base or NULL-PTR holds a plain value, never a nominal type or a pointer\"" ;

: REPAIR$ ( -- ptr u8 n )
   S\" \"repair_class\":\"declare_pointer_cell\"" ;

\ The raw-storage rule's own reason. No fixture here may answer it: these
\ programs name no `variable`, `create`, `constant` or `here`, and a rule that
\ borrowed the other one's prose would say the wrong thing about them.
: RAW-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell\"" ;

: NP-CODE$ ( -- ptr u8 n )
   S\" \"code\":\"E-NONPARAMETRIC-EFFECT\"" ;

: NP-SUGGEST$ ( -- ptr u8 n )
   S\" Declare the pointee this base address really reaches" ;

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: LACKS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TFALSE ;

\ A candidate the checker must REFUSE, with the diagnostic it must answer.
\ Asserting the reason and not only the code is what keeps this rule's prose
\ from silently becoming the raw-cell rule's prose.
: REFUSED ( ptr u8 n -- )
   ARM
   CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  REPAIR$ HAS?  REASON$ HAS?  RAW-REASON$ LACKS?
   DISARM ;

: CERTIFIES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- the forgery: a nominal identity out of a base address -------------------

: CASE-NULL-NOMINAL ( -- )
   s" a null plus an offset does not answer a nominal identity" T-LABEL
   s" BPA-F ( n -- bpathing ) NULL-PTR + @" REFUSED ;

: CASE-DATA-NOMINAL ( -- )
   s" and neither does an offset into the engine's own DATA" T-LABEL
   s" BPA-D ( n -- bpathing ) data-base + @" REFUSED ;

\ Offset zero is the same forgery with the arithmetic left out, which is why the
\ fence rides the pointee rather than the `+` token.
: CASE-NULL-ZERO ( -- )
   s" the same refusal with no arithmetic at all: NULL-PTR @" T-LABEL
   s" BPA-NZ ( -- bpathing ) NULL-PTR @" REFUSED ;

: CASE-DATA-ZERO ( -- )
   s" and data-base @, the first cell of the region" T-LABEL
   s" BPA-DZ ( -- bpathing ) data-base @" REFUSED ;

\ ---- the other half: an ADDRESS out of a base address ------------------------

: CASE-NULL-POINTER ( -- )
   s" a pointer fetched through a null-derived address is refused too" T-LABEL
   s" BPA-G ( n -- ptr n ) NULL-PTR + @" REFUSED ;

: CASE-DATA-POINTER ( -- )
   s" a DATA cell answers a number, not the address it was never declared to hold" T-LABEL
   s" BPA-DG ( n -- ptr n ) data-base + @" REFUSED ;

\ The other direction. Refusing the fetch alone would leave the cell holding an
\ address for some other mention to read back.
: CASE-POINTER-IN ( -- )
   s" and a real pointer cannot be put into an undeclared DATA cell either" T-LABEL
   s" BPA-PS ( ptr n n -- ) data-base + !" REFUSED ;

\ ---- the ways around it ------------------------------------------------------
\ The fence rides the POINTEE VARIABLE, not the `+` token, so every route that
\ keeps that variable is refused at whatever token finally reads through it.

: CASE-OTHER-STEPS ( -- )
   s" cell+ steps the same pointee, and answers the same refusal" T-LABEL
   s" BPA-CP ( -- bpathing ) data-base cell+ @" REFUSED ;

: CASE-REVERSED-ADD ( -- )
   s" and so does the offset-first spelling of +" T-LABEL
   s" BPA-RA ( -- bpathing ) 8 data-base + @" REFUSED ;

\ A quotation's inferred effect carries the pointee like any other, so the
\ refusal lands on `execute` rather than on the fetch inside the brackets.
: CASE-THROUGH-QUOTATION ( -- )
   s" a quotation does not launder it either" T-LABEL
   ARM
   s" BPA-Q ( -- bpathing ) [: data-base 8 + @ ;] execute" CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  REASON$ HAS?
   DISARM ;

\ Stored into a DECLARED pointer cell and read back. PTR-VARIABLE seals its
\ pointee TVK-RAW, so the meet is RAW -- the stricter kind, by the ANY < BASE <
\ RAW lattice -- and the RAW rule answers first. The refusal is what is pinned
\ here; the prose belongs to habu-refuse-a-ptr-5ad2734e, not to this rule.
: CASE-THROUGH-DECLARED-CELL ( -- )
   s" a round trip through a declared pointer cell is refused by the raw rule" T-LABEL
   s" BPA-RT ( -- bpathing ) data-base 8 + BPA-SLOT ! BPA-SLOT @ @" CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- the wrapper, refused at its own declaration ------------------------------
\ `( -- ptr a )` over a DATA offset is the shape that carried the forgery into
\ the shipped library: `SB-LEN @` read as a nominal certified, because SB-LEN
\ handed its caller the choice of element type. It is refused where it is
\ written, not at each of its uses, and the suggestion says what to write.

: CASE-PUBLISH-VAR ( -- )
   s" a wrapper may not publish a base-derived pointer under a type variable" T-LABEL
   ARM
   s" BPA-W ( -- ptr a ) data-base 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ INPUT-ONLY is the exemption, and this is why it has to be input-ONLY: a
\ quantifier named on BOTH sides is still published, so excusing it let the
\ caller pick the pointee again through `ptr bpathing BPA-LEAK @`.
: CASE-PUBLISH-INOUT ( -- )
   s" a wrapper that names the variable on both sides is refused too" T-LABEL
   ARM
   s" BPA-LEAK ( ptr a -- ptr a ) drop data-base 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ ---- the controls: every honest use of a base address -------------------------

\ A quantifier restricted through an INPUT ONLY is not a restriction the
\ signature has to spell: src/core/dynamic-storage.f RELEASE is this word, and
\ the null it stores is the one the language's own reset code stores.
: CASE-INPUT-ONLY ( -- )
   s" a null stored through a declared parameter's own pointee certifies" T-LABEL
   s" BPA-IN ( ptr ptr a -- ) NULL-PTR swap !" CERTIFIES ;

\ The pointer ITSELF. Each of these binds the pointee INSIDE a `ptr`, where the
\ kind is permissive, and each is a shape the tree uses: 89 sites store a null
\ into a declared pointer cell and 11 compare one.
: CASE-NULL-STORE ( -- )
   s" a null still goes into a declared pointer cell" T-LABEL
   s" BPA-NS ( -- ) NULL-PTR BPA-SLOT !" CERTIFIES ;

: CASE-NULL-COMPARE ( -- )
   s" and is still comparable against a pointer to a nominal" T-LABEL
   s" BPA-EQ ( ptr bpathing -- bool ) NULL-PTR =" CERTIFIES ;

: CASE-NULL-DIFF ( -- )
   s" and still subtracts from one" T-LABEL
   s" BPA-SUB ( ptr bpathing -- n ) NULL-PTR -" CERTIFIES ;

: CASE-NULL-ZEROTEST ( -- )
   s" and still answers 0=" T-LABEL
   s" BPA-Z ( -- bool ) NULL-PTR 0=" CERTIFIES ;

\ The engine's own reads. These are the shapes the rule had to keep: a scalar
\ cell of the running task's DATA, read and written at an offset.
: CASE-DATA-SCALAR ( -- )
   s" a DATA cell still reads as the number it holds" T-LABEL
   s" BPA-RD ( n -- n ) data-base + @" CERTIFIES ;

: CASE-DATA-STORE ( -- )
   s" and still takes one" T-LABEL
   s" BPA-WR ( n n -- ) data-base + !" CERTIFIES ;

: CASE-DATA-DISTANCE ( -- )
   s" the distance between two addresses is still a number" T-LABEL
   s" BPA-DIST ( ptr n -- n ) data-base -" CERTIFIES ;

\ The declared door. A DATA cell that really holds an address is reached with
\ `ptr-field`, exactly as before this rule: the checker's own SOURCE-CELL and
\ TARGET-CELL, and the AOT span table, are written this way.
: CASE-DATA-FIELD ( -- )
   s" a DATA cell that holds an address is still reached through ptr-field" T-LABEL
   s" BPA-FLD ( -- ptr ptr u8 ) data-base 8 + 0 ptr-field @" CERTIFIES ;

\ The linker's shape: `data-base BYTE-VIEW` first, then arithmetic on a byte
\ pointer whose pointee is concrete. src/habu/aot-closure.f DATA-PTR is this
\ word, and DATA-CELL@ is DATA-PTR CELL-VIEW @.
: CASE-DATA-BYTE-VIEW ( -- )
   s" and the linker's byte view of the region still certifies" T-LABEL
   s" BPA-BV ( n -- n ) data-base BYTE-VIEW swap + CELL-VIEW @" CERTIFIES ;

\ A concrete pointee is the repair the diagnostic names, and it is what the
\ converted library words now declare.
: CASE-DECLARED-POINTEE ( -- )
   s" a wrapper that names its pointee is the repair, and it certifies" T-LABEL
   s" BPA-OK ( -- ptr n ) data-base 8 +" CERTIFIES ;

public

\ NULL-PTR is registered by a PRIM: row in src/core/cell-effects.f rather than a
\ TRUST declaration. The seal-time internal-word marking pass keeps a
\ checker-known name callable, so this executed definition -- ordinary test
\ source, loaded long after the seal -- is the proof that it stayed one, and not
\ only that the checker can quote its effect.
: NULL-STILL-CALLABLE ( -- )
   s" NULL-PTR is still callable from ordinary source after the seal" T-LABEL
   NULL-PTR BPA-SLOT !
   BPA-SLOT @ NULL-PTR = TTRUE
   NULL-PTR NULL-PTR - 0 T=
   NULL-PTR 0= TTRUE ;

: RUN ( -- )
   T-RESET
   CASE-NULL-NOMINAL
   CASE-DATA-NOMINAL
   CASE-NULL-ZERO
   CASE-DATA-ZERO
   CASE-NULL-POINTER
   CASE-DATA-POINTER
   CASE-POINTER-IN
   CASE-OTHER-STEPS
   CASE-REVERSED-ADD
   CASE-THROUGH-QUOTATION
   CASE-THROUGH-DECLARED-CELL
   CASE-PUBLISH-VAR
   CASE-PUBLISH-INOUT
   CASE-INPUT-ONLY
   CASE-NULL-STORE
   CASE-NULL-COMPARE
   CASE-NULL-DIFF
   CASE-NULL-ZEROTEST
   CASE-DATA-SCALAR
   CASE-DATA-STORE
   CASE-DATA-DISTANCE
   CASE-DATA-FIELD
   CASE-DATA-BYTE-VIEW
   CASE-DECLARED-POINTEE
   NULL-STILL-CALLABLE
   T-REPORT ;

;package

BASE-PTR-ARITH-TEST:RUN
