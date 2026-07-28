\ type.f - the compiler type table: canonical, structurally interned type
\ records shared by every dialect.
\
\ docs/compiler-ir-design.md section 6.3 (the type table), sections 5.4/5.5
\ (target and numeric-policy authority), section 7.5 (the scalar vocabulary
\ the native pipeline consumes), and plan item IR-0.4. One table serves one
\ module as two coupled stores, both IR-ARENA arenas owned by the compilation
\ context: a list pool holding function-type parameter/result lists and a row
\ table holding one fixed-shape record per distinct type. Type identities are
\ the existing IR-ID ir-type-id family packed under the module key; this file
\ mints no parallel identity family and no raw converter.
\
\ CLOSED WORLD. The constructor vocabulary is exhaustive ENUM families - kind,
\ width, sign, fmt, space, domain - so an unknown constructor tag is
\ structurally impossible in checked code: there is no raw-integer path into a
\ row. Rows persist the closed families as stable wire codes; every decoder is
\ an exact case whose unmatched code throws E-IR-TYPE-STATE, so a
\ bypass-forged row rejects at first touch instead of decoding as some other
\ type. The kinds are the section 6.3 entries this substrate stage owns:
\ integer(width, sign), float(fmt), pointer(space, pointee),
\ quotation(params, results), code-ref(params, results),
\ memory-token(domain), mask, opaque. The remaining section 6.3 kinds
\ (tuple/layout, register-class, tensor/memref) need the attribute/layout
\ substrate and land with their owning stages.
\
\ STORE SHAPES. Each arena carries a three-cell header (format tag, owning
\ module serial, committed capacity). The row table stores one four-cell row
\ per type: kind code and three kind-specific field cells. The list pool's
\ data cells each hold one module-local type ordinal; a function type's row
\ points at its (params ++ results) window in that pool. Every access
\ rechecks shape, span, and stored references fail-closed (E-IR-TYPE-STATE),
\ so a holder who bypasses this package and appends raw cells cannot make a
\ reader touch cells outside the live ranges or follow a reference upward.
\
\ INTERNING IS STRUCTURAL FIELD EQUALITY. Every constructor scans for an
\ existing row whose complete canonical field set equals the candidate and
\ answers that identity; only a miss appends. Pointer identity plays no part.
\ Because references are stored as module-local ordinals and every referenced
\ type was itself interned, id equality IS structural equality by induction
\ over ordinals: two rows of one table are structurally equal exactly when
\ they are the same row. That is what lets the scan compare stored ordinals
\ cell by cell instead of re-walking the referenced structure.
\
\ REFERENCES ARE ACYCLIC BY CONSTRUCTION. A pointee or list element must be
\ an already constructed type of this same table (owner check, then ordinal
\ strictly below the live count), so a referenced ordinal is strictly below
\ its referer's. A forward reference and any cycle would need a member at or
\ past the count and dies with E-IR-TYPE-BOUND before anything is written.
\ Readers and the renderer re-verify the strict decrease on every stored
\ reference (E-IR-TYPE-STATE on a corrupted row), so recursive walks
\ terminate on any table state.
\
\ TARGET LEGALITY. Construction consumes the owning context's validated
\ binding (IR-CTX:BINDING@ -> CBIND:TARGET@) - never a rederived legality
\ table. A float format outside the contract's feature set and an address
\ space outside the contract's architecture reject with E-IR-TYPE-TARGET, so
\ a table never holds a type its target cannot represent. Integer widths are
\ target-neutral (both targets compute with 1..64-bit integers regardless of
\ pointer width); the one structural scalar reject is a signed 1-bit integer,
\ which no dialect defines (E-IR-TYPE-SCALAR).
\
\ FUNCTION-TYPE LISTS. Habu words cannot pass variable-length id lists on the
\ stack, so function types are built through an explicit staged protocol:
\ FN-BEGIN opens the stage, FN-PARAM/FN-RESULT append type-ids in order, and
\ QUOT or CODE-REF validates the staged lists against the closing table and
\ interns. The stage is one package-owned area under the single-task
\ compilation discipline (like the IR-CTX staging window); begin-while-open
\ and end-without-begin reject (E-IR-TYPE-STAGE), a list past ARITY-MAX
\ rejects (E-IR-TYPE-ARITY), and a rejected end consumes the stage, so no
\ half-staged list leaks into the next build.
\
\ RENDER IS DIAGNOSTIC TEXT. RENDER writes a deterministic spelling into a
\ caller span - "i32"/"u8", "f16".."f64", "ptr<space,pointee>",
\ "[ p... -- r... ]" for quotations, "( p... -- r... )" for code references,
\ "token<domain>", "mask", "opaque". It depends only on the structural
\ content, never on interning history or ordinals, and is never parsed by the
\ compiler (design section 6.6).
\
\ CANONICAL ORDER MUST RENUMBER STORED REFERENCES. Insertion-ordered ordinals
\ are stable but not canonical, so the section 6.6 encoder is free to emit the
\ rows in a structural order of its own choosing. That is strictly more work
\ here than it is for symbols. A symbol row's identity-bearing content is its
\ bytes, and bytes do not move when the row order moves, so for the symbol
\ table a permutation of the rows is the whole job. A type row can store
\ another row's module-local ordinal instead: POINTER stores its pointee's
\ ordinal, and a function type's row names a window of ordinals in the list
\ pool. Permuting this table therefore changes stored row content, and a
\ canonical type encoder must sort structurally AND renumber every embedded
\ reference under the permutation it chose. Sorting the rows and emitting them
\ unchanged is not canonicalization. What two orders agree on is the
\ denotation of a row - the row unfolded into the structure it references -
\ never the row content itself, and the denotation is what an encoder has to
\ preserve. This is machine-checked in formal/Common/Interning.v:
\ Types.ty_both_orders_admissible builds i8, i16 and pointer-to-i8 in the two
\ admissible orders and gets the row lists [i8; i16; ptr->0] and
\ [i16; i8; ptr->1]; Types.structural_rows_not_permutation proves those two
\ lists are not a permutation of each other; and
\ Types.ty_denotation_order_independent shows their denotations agree.
\
\ One premise of that statement is easy to miss: not every build order exists.
\ Because POINTER rejects a pointee ordinal that is not already below the live
\ count (the acyclicity paragraph above), a type can only be interned after
\ everything it references. The admissible build orders are exactly the
\ topological orders of the reference graph, so "any two build orders" means
\ "any two topological orders" for this table.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f

package IR-TYPE
public

\ The exhaustive constructor vocabulary. Closed ENUM families make an unknown
\ tag unrepresentable in checked code; the wire codes below persist them.
ENUM kind DERIVE eq
   int
   float
   pointer
   quotation
   code-ref
   memory-token
   mask
   opaque
;ENUM

ENUM width DERIVE eq
   w1
   w8
   w16
   w32
   w64
;ENUM

ENUM sign DERIVE eq
   unsigned
   signed
;ENUM

ENUM fmt DERIVE eq
   half
   bfloat
   single
   double
;ENUM

ENUM space DERIVE eq
   generic
   global
   shared
   local
   param
   const
;ENUM

ENUM domain DERIVE eq
   data-mem
   dict
   code-pub
   io
   process
   ffi
;ENUM

private

\ The one raw crossing this package needs: one-way projections of the sealed
\ IR-ID identities onto their serials, for header binding and owner
\ comparison. Nothing in this package re-mints a raw cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n ) ;

\ ---- layout ------------------------------------------------------------------
$54594C31 constant TYL-MAGIC         \ "TYL1": the list-pool header format tag
$54595231 constant TYR-MAGIC         \ "TYR1": the row-table header format tag
0 constant HC-MAGIC
1 constant HC-SERIAL
2 constant HC-CAP
3 constant HDR-CELLS
0 constant OFF-KIND
1 constant OFF-A
2 constant OFF-B
3 constant OFF-C
4 constant ROW-CELLS
$FFFFFFFF HDR-CELLS - ROW-CELLS / constant CAP-MAX
$FFFFFFFF HDR-CELLS - constant LIST-MAX
32 constant ARITY-MAX                \ committed per-list stage ceiling

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per family, mirroring CTARGET: a code may be added but
\ never renumbered without a schema bump in the canonical encoder.
0 constant K-INT
1 constant K-FLT
2 constant K-PTR
3 constant K-QUOT
4 constant K-CREF
5 constant K-TOK
6 constant K-MASK
7 constant K-OPQ

: KIND-CODE ( IR-TYPE:kind -- n )
   MATCH kind
      int          OF K-INT ENDOF
      float        OF K-FLT ENDOF
      pointer      OF K-PTR ENDOF
      quotation    OF K-QUOT ENDOF
      code-ref     OF K-CREF ENDOF
      memory-token OF K-TOK ENDOF
      mask         OF K-MASK ENDOF
      opaque       OF K-OPQ ENDOF
   ;MATCH ;

: WIDTH-CODE ( IR-TYPE:width -- n )
   MATCH width
      w1  OF 0 ENDOF
      w8  OF 1 ENDOF
      w16 OF 2 ENDOF
      w32 OF 3 ENDOF
      w64 OF 4 ENDOF
   ;MATCH ;

: SIGN-CODE ( IR-TYPE:sign -- n )
   MATCH sign
      unsigned OF 0 ENDOF
      signed   OF 1 ENDOF
   ;MATCH ;

: FMT-CODE ( IR-TYPE:fmt -- n )
   MATCH fmt
      half   OF 0 ENDOF
      bfloat OF 1 ENDOF
      single OF 2 ENDOF
      double OF 3 ENDOF
   ;MATCH ;

: SPACE-CODE ( IR-TYPE:space -- n )
   MATCH space
      generic OF 0 ENDOF
      global  OF 1 ENDOF
      shared  OF 2 ENDOF
      local   OF 3 ENDOF
      param   OF 4 ENDOF
      const   OF 5 ENDOF
   ;MATCH ;

: DOMAIN-CODE ( IR-TYPE:domain -- n )
   MATCH domain
      data-mem OF 0 ENDOF
      dict     OF 1 ENDOF
      code-pub OF 2 ENDOF
      io       OF 3 ENDOF
      process  OF 4 ENDOF
      ffi      OF 5 ENDOF
   ;MATCH ;

\ ---- wire-code decoders ------------------------------------------------------
\ A stored code outside the vocabulary is a corrupted or forged row; every
\ decoder rejects it named, so malformed records cannot read as types.
: N>KIND ( n -- IR-TYPE:kind )
   case
      K-INT  of IR--TYPE-KIND:INT endof
      K-FLT  of IR--TYPE-KIND:FLOAT endof
      K-PTR  of IR--TYPE-KIND:POINTER endof
      K-QUOT of IR--TYPE-KIND:QUOTATION endof
      K-CREF of IR--TYPE-KIND:CODE-REF endof
      K-TOK  of IR--TYPE-KIND:MEMORY-TOKEN endof
      K-MASK of IR--TYPE-KIND:MASK endof
      K-OPQ  of IR--TYPE-KIND:OPAQUE endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>WIDTH ( n -- IR-TYPE:width )
   case
      0 of IR--TYPE-WIDTH:W1 endof
      1 of IR--TYPE-WIDTH:W8 endof
      2 of IR--TYPE-WIDTH:W16 endof
      3 of IR--TYPE-WIDTH:W32 endof
      4 of IR--TYPE-WIDTH:W64 endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>SIGN ( n -- IR-TYPE:sign )
   case
      0 of IR--TYPE-SIGN:UNSIGNED endof
      1 of IR--TYPE-SIGN:SIGNED endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>FMT ( n -- IR-TYPE:fmt )
   case
      0 of IR--TYPE-FMT:HALF endof
      1 of IR--TYPE-FMT:BFLOAT endof
      2 of IR--TYPE-FMT:SINGLE endof
      3 of IR--TYPE-FMT:DOUBLE endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>SPACE ( n -- IR-TYPE:space )
   case
      0 of IR--TYPE-SPACE:GENERIC endof
      1 of IR--TYPE-SPACE:GLOBAL endof
      2 of IR--TYPE-SPACE:SHARED endof
      3 of IR--TYPE-SPACE:LOCAL endof
      4 of IR--TYPE-SPACE:PARAM endof
      5 of IR--TYPE-SPACE:CONST endof
      E-IR-TYPE-STATE throw
   endcase ;

: N>DOMAIN ( n -- IR-TYPE:domain )
   case
      0 of IR--TYPE-DOMAIN:DATA-MEM endof
      1 of IR--TYPE-DOMAIN:DICT endof
      2 of IR--TYPE-DOMAIN:CODE-PUB endof
      3 of IR--TYPE-DOMAIN:IO endof
      4 of IR--TYPE-DOMAIN:PROCESS endof
      5 of IR--TYPE-DOMAIN:FFI endof
      E-IR-TYPE-STATE throw
   endcase ;

\ ---- target legality ---------------------------------------------------------
\ The legality authority is the context's validated binding; nothing here
\ re-derives a target table.
: CTX-TARGET ( IR-CTX:ctx -- CTARGET:contract )
   IR-CTX:BINDING@ CBIND:TARGET@ ;

\ The feature a float format needs from the target contract.
: FMT-FEATURE ( IR-TYPE:fmt -- CTARGET:features )
   MATCH fmt
      half   OF CTARGET:F-FP16 ENDOF
      bfloat OF CTARGET:F-BF16 ENDOF
      single OF CTARGET:F-FP ENDOF
      double OF CTARGET:F-FP ENDOF
   ;MATCH ;

: FMT-CK ( IR-CTX:ctx IR-TYPE:fmt -- )
   {: c:IR-CTX:ctx f:fmt :}
   c CTX-TARGET CTARGET:FEATURES@ f FMT-FEATURE CTARGET:HAS? 0= if
      E-IR-TYPE-TARGET throw
   then ;

\ Which address spaces an architecture's memory model defines: AArch64 has
\ one flat generic space; PTX defines the full six-space model.
: SPACE-LEGAL? ( CTARGET:arch IR-TYPE:space -- bool )
   {: k:CTARGET:arch s:space :}
   k MATCH CTARGET:arch
      aarch64 OF s IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ ENDOF
      ptx     OF true ENDOF
   ;MATCH ;

: SPACE-CK ( IR-CTX:ctx IR-TYPE:space -- )
   {: c:IR-CTX:ctx s:space :}
   c CTX-TARGET CTARGET:ARCH@ s SPACE-LEGAL? 0= if
      E-IR-TYPE-TARGET throw
   then ;

\ ---- cell access -------------------------------------------------------------
: LCELL@ ( IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena k:n :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

: FCELL@ ( IR-ARENA:view n -- n )
   {: v:IR-ARENA:view k:n :}
   v v k IR-ARENA:FROZEN-NTH IR-ARENA:AT ;

\ ---- headers and shape -------------------------------------------------------
: PSHAPE-CK ( n -- )
   HDR-CELLS < if E-IR-TYPE-STATE throw then ;

: RSHAPE-CK ( n -- )
   dup HDR-CELLS < if E-IR-TYPE-STATE throw then
   HDR-CELLS - ROW-CELLS mod 0 <> if E-IR-TYPE-STATE throw then ;

: PMAGIC-CK ( n -- )
   TYL-MAGIC <> if E-IR-TYPE-STATE throw then ;

: RMAGIC-CK ( n -- )
   TYR-MAGIC <> if E-IR-TYPE-STATE throw then ;

: PHDR-CK ( IR-ARENA:arena -- )
   {: a:IR-ARENA:arena :}
   a IR-ARENA:USED PSHAPE-CK
   a HC-MAGIC LCELL@ PMAGIC-CK ;

: RHDR-CK ( IR-ARENA:arena -- )
   {: r:IR-ARENA:arena :}
   r IR-ARENA:USED RSHAPE-CK
   r HC-MAGIC LCELL@ RMAGIC-CK ;

: FPHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE PSHAPE-CK
   v HC-MAGIC FCELL@ PMAGIC-CK ;

: FRHDR-CK ( IR-ARENA:view -- )
   {: v:IR-ARENA:view :}
   v IR-ARENA:SIZE RSHAPE-CK
   v HC-MAGIC FCELL@ RMAGIC-CK ;

: USED>CNT ( n -- n )
   HDR-CELLS - ROW-CELLS / ;

: CNT ( IR-ARENA:arena -- n )
   IR-ARENA:USED USED>CNT ;

: FCNT ( IR-ARENA:view -- n )
   IR-ARENA:SIZE USED>CNT ;

: PCELLS ( IR-ARENA:arena -- n )
   IR-ARENA:USED HDR-CELLS - ;

: FPCELLS ( IR-ARENA:view -- n )
   IR-ARENA:SIZE HDR-CELLS - ;

\ ---- ownership ---------------------------------------------------------------
: SERIAL-CK ( n n -- )
   <> if E-IR-TYPE-OWNER throw then ;

\ The pair coupling: both stores are what their tags claim and both carry the
\ same owning module serial, so a cross-module pairing rejects before any row
\ window is trusted against the wrong pool.
: PAIR-CK ( IR-ARENA:arena IR-ARENA:arena -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   a PHDR-CK
   r RHDR-CK
   a HC-SERIAL LCELL@ r HC-SERIAL LCELL@ SERIAL-CK ;

: FPAIR-CK ( IR-ARENA:view IR-ARENA:view -- )
   {: pv:IR-ARENA:view rv:IR-ARENA:view :}
   pv FPHDR-CK
   rv FRHDR-CK
   pv HC-SERIAL FCELL@ rv HC-SERIAL FCELL@ SERIAL-CK ;

: KEY-CK ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   a r PAIR-CK
   r HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

: RKEY-CK ( IR-ARENA:arena IR-ID:ir-module-key -- )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   r RHDR-CK
   r HC-SERIAL LCELL@ key KEY-SERIAL SERIAL-CK ;

: FRKEY-CK ( IR-ARENA:view IR-ID:ir-module-key -- )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key :}
   rv FRHDR-CK
   rv HC-SERIAL FCELL@ key KEY-SERIAL SERIAL-CK ;

: ID-OWNER-SERIAL ( IR-ID:ir-type-id -- n )
   IR-ID:TYPE-OWNER MID-SERIAL ;

\ Validate a presented type-id against a resolved (header serial, count):
\ minted under this table's module, ordinal below the constructed count.
: ID-CK-N ( n n IR-ID:ir-type-id -- n )
   {: hs:n cnt:n id:IR-ID:ir-type-id :}
   hs id ID-OWNER-SERIAL SERIAL-CK
   id IR-ID:TYPE-LOCAL
   dup cnt >= if E-IR-TYPE-BOUND throw then ;

: ID-CK ( IR-ARENA:arena IR-ID:ir-type-id -- n )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r RHDR-CK
   r HC-SERIAL LCELL@ r CNT id ID-CK-N ;

: FID-CK ( IR-ARENA:view IR-ID:ir-type-id -- n )
   {: v:IR-ARENA:view id:IR-ID:ir-type-id :}
   v FRHDR-CK
   v HC-SERIAL FCELL@ v FCNT id ID-CK-N ;

\ ---- row and pool addressing -------------------------------------------------
: ROW-CELL ( n n -- n )
   swap ROW-CELLS * HDR-CELLS + + ;

: RC@ ( IR-ARENA:arena n n -- n )
   ROW-CELL LCELL@ ;

: FRC@ ( IR-ARENA:view n n -- n )
   ROW-CELL FCELL@ ;

: PC@ ( IR-ARENA:arena n -- n )
   HDR-CELLS + LCELL@ ;

: FPC@ ( IR-ARENA:view n -- n )
   HDR-CELLS + FCELL@ ;

\ A stored reference re-verified at the point of use: inside the table and
\ strictly below its referer, so recursive walks terminate on any state.
: REF-OK ( n n -- n )
   {: l:n raw:n :}
   raw 0 < raw l >= or if E-IR-TYPE-STATE throw then
   raw ;

\ A function row's pool window revalidated against the pool's live cells on
\ every access, so a forged or bypass-appended row rejects fail-closed.
: SPAN-CK-N ( n n n n -- )
   {: pc:n st:n pn:n rn:n :}
   st 0 < pn 0 < or rn 0 < or if E-IR-TYPE-STATE throw then
   st pn + rn + pc > if E-IR-TYPE-STATE throw then ;

: FN-SPAN ( IR-ARENA:arena IR-ARENA:arena n -- n n n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n :}
   r l OFF-A RC@ r l OFF-B RC@ r l OFF-C RC@
   {: st:n pn:n rn:n :}
   a PCELLS st pn rn SPAN-CK-N
   st pn rn ;

: FFN-SPAN ( IR-ARENA:view IR-ARENA:view n -- n n n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view l:n :}
   rv l OFF-A FRC@ rv l OFF-B FRC@ rv l OFF-C FRC@
   {: st:n pn:n rn:n :}
   pv FPCELLS st pn rn SPAN-CK-N
   st pn rn ;

: FNKIND? ( n -- bool )
   dup K-QUOT = swap K-CREF = or ;

: FNKIND-CK ( n -- )
   FNKIND? 0= if E-IR-TYPE-KIND throw then ;

\ ---- interning scan ----------------------------------------------------------
: ROW4-MATCH? ( IR-ARENA:arena n n n n n -- bool )
   {: r:IR-ARENA:arena l:n k:n x:n y:n z:n :}
   r l OFF-KIND RC@ k <> if false exit then
   r l OFF-A RC@ x <> if false exit then
   r l OFF-B RC@ y <> if false exit then
   r l OFF-C RC@ z = ;

: SCAN4 ( IR-ARENA:arena n n n n -- n )
   {: r:IR-ARENA:arena k:n x:n y:n z:n :}
   -1
   r CNT 0 ?do
      r i k x y z ROW4-MATCH? if drop i leave then
   loop ;

\ The one fixed-field intern path: answer the structurally equal row's
\ identity or append the four validated cells whole. Capacity is checked
\ before the first write, so a full table stays usable and duplicate
\ construction still answers.
: INTERN4 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n n n n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key k:n x:n y:n z:n :}
   a r key KEY-CK
   r k x y z SCAN4 {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-TYPE exit then
   r CNT r HC-CAP LCELL@ >= if E-IR-TYPE-CAP throw then
   r CNT {: l:n :}
   c r k IR-ARENA:PUSH drop
   c r x IR-ARENA:PUSH drop
   c r y IR-ARENA:PUSH drop
   c r z IR-ARENA:PUSH drop
   key l IR-ID:PACK-TYPE ;

\ ---- function-type stage -----------------------------------------------------
here CELL 1- and CELL swap - CELL 1- and allot
variable STG-OPEN
0 STG-OPEN !
variable STG-PN
0 STG-PN !
variable STG-RN
0 STG-RN !
create STG-PO ARITY-MAX cells allot
create STG-PL ARITY-MAX cells allot
create STG-RO ARITY-MAX cells allot
create STG-RL ARITY-MAX cells allot

: PO@ ( n -- n )
   cells STG-PO + @ ;

: PO! ( n n -- )
   cells STG-PO + ! ;

: PL@ ( n -- n )
   cells STG-PL + @ ;

: PL! ( n n -- )
   cells STG-PL + ! ;

: RO@ ( n -- n )
   cells STG-RO + @ ;

: RO! ( n n -- )
   cells STG-RO + ! ;

: RL@ ( n -- n )
   cells STG-RL + @ ;

: RL! ( n n -- )
   cells STG-RL + ! ;

: STG-OPEN-CK ( -- )
   STG-OPEN @ 0= if E-IR-TYPE-STAGE throw then ;

\ Validate one staged (owner serial, local ordinal) pair against the closing
\ table: owned here, strictly below the live count - the same no-forward-
\ reference rule as the pointer constructor.
: STG-REF-CK ( n n n n -- )
   {: hs:n cnt:n os:n loc:n :}
   os hs <> if E-IR-TYPE-OWNER throw then
   loc 0 < loc cnt >= or if E-IR-TYPE-BOUND throw then ;

: STG-VALIDATE ( n n -- )
   {: hs:n cnt:n :}
   STG-PN @ 0 ?do
      hs cnt i PO@ i PL@ STG-REF-CK
   loop
   STG-RN @ 0 ?do
      hs cnt i RO@ i RL@ STG-REF-CK
   loop ;

\ The staged combined list: params first, then results.
: STG-AT ( n -- n )
   {: i:n :}
   i STG-PN @ < if i PL@ else i STG-PN @ - RL@ then ;

: ROWFN-MATCH? ( IR-ARENA:arena IR-ARENA:arena n n -- bool )
   {: a:IR-ARENA:arena r:IR-ARENA:arena l:n kc:n :}
   r l OFF-KIND RC@ kc <> if false exit then
   r l OFF-B RC@ STG-PN @ <> if false exit then
   r l OFF-C RC@ STG-RN @ <> if false exit then
   a r l FN-SPAN {: st:n pn:n rn:n :}
   pn rn + 0 ?do
      a st i + PC@ i STG-AT <> if false unloop exit then
   loop
   true ;

: SCANFN ( IR-ARENA:arena IR-ARENA:arena n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena kc:n :}
   -1
   r CNT 0 ?do
      a r i kc ROWFN-MATCH? if drop i leave then
   loop ;

\ Close the stage and intern the staged function type. The stage is consumed
\ first, so a rejected end never leaks a half-staged list; all validation and
\ both room checks run before the first cell is written, so an intern appends
\ its pool window and its row whole or mutates nothing.
: FN-END ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key kc:n :}
   STG-OPEN-CK
   0 STG-OPEN !
   a r key KEY-CK
   key KEY-SERIAL r CNT STG-VALIDATE
   a r kc SCANFN {: hit:n :}
   hit 0 < 0= if key hit IR-ID:PACK-TYPE exit then
   r CNT r HC-CAP LCELL@ >= if E-IR-TYPE-CAP throw then
   a PCELLS STG-PN @ + STG-RN @ + a HC-CAP LCELL@ > if E-IR-TYPE-LIST throw then
   a PCELLS {: st:n :}
   STG-PN @ STG-RN @ + 0 ?do
      c a i STG-AT IR-ARENA:PUSH drop
   loop
   r CNT {: l:n :}
   c r kc IR-ARENA:PUSH drop
   c r st IR-ARENA:PUSH drop
   c r STG-PN @ IR-ARENA:PUSH drop
   c r STG-RN @ IR-ARENA:PUSH drop
   key l IR-ID:PACK-TYPE ;

\ ---- creation checks ---------------------------------------------------------
: ROW-CAP-OK ( n -- )
   dup 1 < over CAP-MAX > or if E-IR-TYPE-CAP throw then
   drop ;

: LIST-CAP-OK ( n -- )
   dup 1 < over LIST-MAX > or if E-IR-TYPE-LIST throw then
   drop ;

public

\ ---- creation ----------------------------------------------------------------
\ Create a module's type table: the list pool committed to exactly lcap
\ list cells and the row table committed to exactly tcap types, both headers
\ bound to key's module serial. The two handles plus the key are the table;
\ all three stay with the module owner, and the table dies with the owning
\ context.
: NEW ( IR-CTX:ctx IR-ID:ir-module-key n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key tcap:n lcap:n :}
   tcap ROW-CAP-OK
   lcap LIST-CAP-OK
   c lcap HDR-CELLS + IR-ARENA:NEW {: a:IR-ARENA:arena :}
   c a TYL-MAGIC IR-ARENA:PUSH drop
   c a key KEY-SERIAL IR-ARENA:PUSH drop
   c a lcap IR-ARENA:PUSH drop
   c tcap ROW-CELLS * HDR-CELLS + IR-ARENA:NEW {: r:IR-ARENA:arena :}
   c r TYR-MAGIC IR-ARENA:PUSH drop
   c r key KEY-SERIAL IR-ARENA:PUSH drop
   c r tcap IR-ARENA:PUSH drop
   a r ;

\ ---- constructors ------------------------------------------------------------
\ Every constructor interns: the same fields twice answer one identity. The
\ ctx is the allocation and target-legality authority; a hit allocates
\ nothing.
: INT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:width IR-TYPE:sign -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key w:width s:sign :}
   w WIDTH-CODE {: wc:n :}
   s SIGN-CODE {: sc:n :}
   wc 0= sc 1 = and if E-IR-TYPE-SCALAR throw then
   c a r key K-INT wc sc 0 INTERN4 ;

: FLT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:fmt -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key f:fmt :}
   c f FMT-CK
   c a r key K-FLT f FMT-CODE 0 0 INTERN4 ;

\ The pointee must already be a constructed type of this same table, so its
\ ordinal is strictly below the pointer's: no forward references, no cycles.
: POINTER ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:space IR-ID:ir-type-id -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key s:space ptee:IR-ID:ir-type-id :}
   c s SPACE-CK
   r ptee ID-CK {: pl:n :}
   c a r key K-PTR s SPACE-CODE pl 0 INTERN4 ;

: TOKEN ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-TYPE:domain -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key d:domain :}
   c a r key K-TOK d DOMAIN-CODE 0 0 INTERN4 ;

: MASK ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key K-MASK 0 0 0 INTERN4 ;

: OPAQUE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c a r key K-OPQ 0 0 0 INTERN4 ;

\ ---- function-type stage protocol --------------------------------------------
: FN-BEGIN ( -- )
   STG-OPEN @ 0 <> if E-IR-TYPE-STAGE throw then
   1 STG-OPEN !
   0 STG-PN !
   0 STG-RN ! ;

: FN-PARAM ( IR-ID:ir-type-id -- )
   {: id:IR-ID:ir-type-id :}
   STG-OPEN-CK
   STG-PN @ ARITY-MAX >= if E-IR-TYPE-ARITY throw then
   id ID-OWNER-SERIAL STG-PN @ PO!
   id IR-ID:TYPE-LOCAL STG-PN @ PL!
   STG-PN @ 1+ STG-PN ! ;

: FN-RESULT ( IR-ID:ir-type-id -- )
   {: id:IR-ID:ir-type-id :}
   STG-OPEN-CK
   STG-RN @ ARITY-MAX >= if E-IR-TYPE-ARITY throw then
   id ID-OWNER-SERIAL STG-RN @ RO!
   id IR-ID:TYPE-LOCAL STG-RN @ RL!
   STG-RN @ 1+ STG-RN ! ;

: QUOT ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   K-QUOT FN-END ;

: CODE-REF ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   K-CREF FN-END ;

\ ---- live readers ------------------------------------------------------------
: TYPES ( IR-ARENA:arena -- n )
   dup RHDR-CK CNT ;

: KIND@ ( IR-ARENA:arena IR-ID:ir-type-id -- IR-TYPE:kind )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ N>KIND ;

private

: KIND-CK ( n n -- )
   <> if E-IR-TYPE-KIND throw then ;

public

: INT@ ( IR-ARENA:arena IR-ID:ir-type-id -- IR-TYPE:width IR-TYPE:sign )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-INT KIND-CK
   r l OFF-A RC@ N>WIDTH
   r l OFF-B RC@ N>SIGN ;

: FLT@ ( IR-ARENA:arena IR-ID:ir-type-id -- IR-TYPE:fmt )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-FLT KIND-CK
   r l OFF-A RC@ N>FMT ;

: TOKEN@ ( IR-ARENA:arena IR-ID:ir-type-id -- IR-TYPE:domain )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-TOK KIND-CK
   r l OFF-A RC@ N>DOMAIN ;

: POINTER@ ( IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-type-id -- IR-TYPE:space IR-ID:ir-type-id )
   {: r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-type-id :}
   r key RKEY-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ K-PTR KIND-CK
   r l OFF-A RC@ N>SPACE
   key l r l OFF-B RC@ REF-OK IR-ID:PACK-TYPE ;

: ARITY@ ( IR-ARENA:arena IR-ID:ir-type-id -- n n )
   {: r:IR-ARENA:arena id:IR-ID:ir-type-id :}
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ FNKIND-CK
   r l OFF-B RC@ r l OFF-C RC@ ;

: PARAM@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-type-id n -- IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-type-id i:n :}
   a r key KEY-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ FNKIND-CK
   a r l FN-SPAN {: st:n pn:n rn:n :}
   i 0 < i pn >= or if E-IR-TYPE-BOUND throw then
   key l a st i + PC@ REF-OK IR-ID:PACK-TYPE ;

: RESULT@ ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ID:ir-type-id n -- IR-ID:ir-type-id )
   {: a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key id:IR-ID:ir-type-id i:n :}
   a r key KEY-CK
   r id ID-CK {: l:n :}
   r l OFF-KIND RC@ FNKIND-CK
   a r l FN-SPAN {: st:n pn:n rn:n :}
   i 0 < i rn >= or if E-IR-TYPE-BOUND throw then
   key l a st pn + i + PC@ REF-OK IR-ID:PACK-TYPE ;

\ ---- render ------------------------------------------------------------------
private

\ Byte emitters over the caller span: cur rides on top so emission chains
\ read left to right without juggling.
: EMIT-B ( n ptr u8 n n -- n )
   {: cur:n q cap:n b:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   cur cap >= if E-IR-TYPE-RANGE throw then
   b q cur + c!
   cur 1+ ;

: EMIT-S ( n ptr u8 n ptr u8 n -- n )
   {: cur:n q cap:n p u:n :} \ typed-local-lint: allow-bare-local - q and p keep the ptr u8 byte-span roles
   cur u + cap > if E-IR-TYPE-RANGE throw then
   u 0 ?do
      p i + c@ q cur + i + c!
   loop
   cur u + ;

\ Diagnostic spellings for the closed vocabularies. A stored code outside
\ them is a corrupted row, exactly as in the decoders.
: SIGN-STR ( n -- ptr u8 n )
   case
      0 of s" u" endof
      1 of s" i" endof
      E-IR-TYPE-STATE throw
   endcase ;

: WIDTH-STR ( n -- ptr u8 n )
   case
      0 of s" 1" endof
      1 of s" 8" endof
      2 of s" 16" endof
      3 of s" 32" endof
      4 of s" 64" endof
      E-IR-TYPE-STATE throw
   endcase ;

: FMT-STR ( n -- ptr u8 n )
   case
      0 of s" f16" endof
      1 of s" bf16" endof
      2 of s" f32" endof
      3 of s" f64" endof
      E-IR-TYPE-STATE throw
   endcase ;

: SPACE-STR ( n -- ptr u8 n )
   case
      0 of s" generic" endof
      1 of s" global" endof
      2 of s" shared" endof
      3 of s" local" endof
      4 of s" param" endof
      5 of s" const" endof
      E-IR-TYPE-STATE throw
   endcase ;

: DOMAIN-STR ( n -- ptr u8 n )
   case
      0 of s" data-mem" endof
      1 of s" dict" endof
      2 of s" code-pub" endof
      3 of s" io" endof
      4 of s" process" endof
      5 of s" ffi" endof
      E-IR-TYPE-STATE throw
   endcase ;

: EMIT-INT ( n ptr u8 n n n -- n )
   {: cur:n q cap:n wc:n sc:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   cur q cap sc SIGN-STR EMIT-S
   q cap wc WIDTH-STR EMIT-S ;

: EMIT-TOKEN ( n ptr u8 n n -- n )
   {: cur:n q cap:n dc:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   cur q cap s" token<" EMIT-S
   q cap dc DOMAIN-STR EMIT-S
   q cap $3E EMIT-B ;

: OPEN-B ( n -- n )
   K-QUOT = if $5B else $28 then ;

: CLOSE-B ( n -- n )
   K-QUOT = if $5D else $29 then ;

\ Render one row by local ordinal. The pointer and function arms recurse on
\ stored references; RECURSE binds the containing word, so those arms stay
\ inline here while every non-recursive step is a named helper. Each stored
\ reference passes REF-OK's strict decrease, so recursion depth is bounded
\ by the ordinal itself on any table state.
: R-EMIT ( IR-ARENA:arena IR-ARENA:arena ptr u8 n n n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena q cap:n cur:n l:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   r l OFF-KIND RC@ {: k:n :}
   k K-INT = if cur q cap r l OFF-A RC@ r l OFF-B RC@ EMIT-INT exit then
   k K-FLT = if cur q cap r l OFF-A RC@ FMT-STR EMIT-S exit then
   k K-TOK = if cur q cap r l OFF-A RC@ EMIT-TOKEN exit then
   k K-MASK = if cur q cap s" mask" EMIT-S exit then
   k K-OPQ = if cur q cap s" opaque" EMIT-S exit then
   k K-PTR = if
      cur q cap s" ptr<" EMIT-S
      q cap r l OFF-A RC@ SPACE-STR EMIT-S
      q cap $2C EMIT-B
      {: cm:n :}
      a r q cap cm  l r l OFF-B RC@ REF-OK  recurse
      q cap $3E EMIT-B
      exit
   then
   k FNKIND? 0= if E-IR-TYPE-STATE throw then
   a r l FN-SPAN {: st:n pn:n rn:n :}
   cur q cap k OPEN-B EMIT-B
   pn 0 ?do
      q cap $20 EMIT-B
      {: ci:n :}
      a r q cap ci  l a st i + PC@ REF-OK  recurse
   loop
   q cap $20 EMIT-B
   q cap s" --" EMIT-S
   rn 0 ?do
      q cap $20 EMIT-B
      {: ci:n :}
      a r q cap ci  l a st pn + i + PC@ REF-OK  recurse
   loop
   q cap $20 EMIT-B
   q cap k CLOSE-B EMIT-B ;

\ The frozen twin of R-EMIT over the arena views.
: FR-EMIT ( IR-ARENA:view IR-ARENA:view ptr u8 n n n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view q cap:n cur:n l:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   rv l OFF-KIND FRC@ {: k:n :}
   k K-INT = if cur q cap rv l OFF-A FRC@ rv l OFF-B FRC@ EMIT-INT exit then
   k K-FLT = if cur q cap rv l OFF-A FRC@ FMT-STR EMIT-S exit then
   k K-TOK = if cur q cap rv l OFF-A FRC@ EMIT-TOKEN exit then
   k K-MASK = if cur q cap s" mask" EMIT-S exit then
   k K-OPQ = if cur q cap s" opaque" EMIT-S exit then
   k K-PTR = if
      cur q cap s" ptr<" EMIT-S
      q cap rv l OFF-A FRC@ SPACE-STR EMIT-S
      q cap $2C EMIT-B
      {: cm:n :}
      pv rv q cap cm  l rv l OFF-B FRC@ REF-OK  recurse
      q cap $3E EMIT-B
      exit
   then
   k FNKIND? 0= if E-IR-TYPE-STATE throw then
   pv rv l FFN-SPAN {: st:n pn:n rn:n :}
   cur q cap k OPEN-B EMIT-B
   pn 0 ?do
      q cap $20 EMIT-B
      {: ci:n :}
      pv rv q cap ci  l pv st i + FPC@ REF-OK  recurse
   loop
   q cap $20 EMIT-B
   q cap s" --" EMIT-S
   rn 0 ?do
      q cap $20 EMIT-B
      {: ci:n :}
      pv rv q cap ci  l pv st pn + i + FPC@ REF-OK  recurse
   loop
   q cap $20 EMIT-B
   q cap k CLOSE-B EMIT-B ;

public

\ Render a type's deterministic diagnostic spelling into the caller's span
\ and answer the byte length; a span too small rejects named before the
\ overflowing write. The text depends only on structural content, never on
\ interning history.
: RENDER ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-type-id ptr u8 n -- n )
   {: a:IR-ARENA:arena r:IR-ARENA:arena id:IR-ID:ir-type-id q cap:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   a r PAIR-CK
   r id ID-CK {: l:n :}
   a r q cap 0 l R-EMIT ;

\ ---- frozen readers ----------------------------------------------------------
\ A frozen module reads its types through the two arena views; the retired
\ builder handles reject every touch with E-IR-ARENA-FROZEN.
: FTYPES ( IR-ARENA:view -- n )
   dup FRHDR-CK FCNT ;

: FKIND@ ( IR-ARENA:view IR-ID:ir-type-id -- IR-TYPE:kind )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ N>KIND ;

: FINT@ ( IR-ARENA:view IR-ID:ir-type-id -- IR-TYPE:width IR-TYPE:sign )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-INT KIND-CK
   rv l OFF-A FRC@ N>WIDTH
   rv l OFF-B FRC@ N>SIGN ;

: FFLT@ ( IR-ARENA:view IR-ID:ir-type-id -- IR-TYPE:fmt )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-FLT KIND-CK
   rv l OFF-A FRC@ N>FMT ;

: FTOKEN@ ( IR-ARENA:view IR-ID:ir-type-id -- IR-TYPE:domain )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-TOK KIND-CK
   rv l OFF-A FRC@ N>DOMAIN ;

: FPOINTER@ ( IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-type-id -- IR-TYPE:space IR-ID:ir-type-id )
   {: rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-type-id :}
   rv key FRKEY-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ K-PTR KIND-CK
   rv l OFF-A FRC@ N>SPACE
   key l rv l OFF-B FRC@ REF-OK IR-ID:PACK-TYPE ;

: FARITY@ ( IR-ARENA:view IR-ID:ir-type-id -- n n )
   {: rv:IR-ARENA:view id:IR-ID:ir-type-id :}
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ FNKIND-CK
   rv l OFF-B FRC@ rv l OFF-C FRC@ ;

: FPARAM@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-type-id n -- IR-ID:ir-type-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-type-id i:n :}
   pv rv FPAIR-CK
   rv key FRKEY-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ FNKIND-CK
   pv rv l FFN-SPAN {: st:n pn:n rn:n :}
   i 0 < i pn >= or if E-IR-TYPE-BOUND throw then
   key l pv st i + FPC@ REF-OK IR-ID:PACK-TYPE ;

: FRESULT@ ( IR-ARENA:view IR-ARENA:view IR-ID:ir-module-key IR-ID:ir-type-id n -- IR-ID:ir-type-id )
   {: pv:IR-ARENA:view rv:IR-ARENA:view key:IR-ID:ir-module-key id:IR-ID:ir-type-id i:n :}
   pv rv FPAIR-CK
   rv key FRKEY-CK
   rv id FID-CK {: l:n :}
   rv l OFF-KIND FRC@ FNKIND-CK
   pv rv l FFN-SPAN {: st:n pn:n rn:n :}
   i 0 < i rn >= or if E-IR-TYPE-BOUND throw then
   key l pv st pn + i + FPC@ REF-OK IR-ID:PACK-TYPE ;

: FRENDER ( IR-ARENA:view IR-ARENA:view IR-ID:ir-type-id ptr u8 n -- n )
   {: pv:IR-ARENA:view rv:IR-ARENA:view id:IR-ID:ir-type-id q cap:n :} \ typed-local-lint: allow-bare-local - q keeps the ptr u8 byte-span role
   pv rv FPAIR-CK
   rv id FID-CK {: l:n :}
   pv rv q cap 0 l FR-EMIT ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
