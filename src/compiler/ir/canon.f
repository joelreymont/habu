\ canon.f - canonical table order: the one authority that says which order a
\ frozen module's interned tables belong in, and what every stored reference
\ becomes in that order.
\
\ docs/compiler-ir-design.md section 5.7 ("Given identical source bytes,
\ environment manifests, target contract, numeric policy, and pass
\ configuration, every stage must produce the same canonical module and
\ digest. Do not rely on hash-table iteration order, allocation addresses,
\ process IDs, temporary paths, or target timing while constructing semantic
\ artifacts.") and section 6.6, which fixes the order a frozen module
\ serializes in:
\
\     header
\     target and numeric-policy digests
\     dialect/schema versions
\     string and symbol tables
\     type table
\     attribute table
\     source table
\     function table
\     block table
\     operation table
\     value table
\     operand/result/successor pools
\     optional derived indices
\
\ and adds "The encoding includes explicit counts and lengths; it does not
\ serialize host addresses or arena capacities."
\
\ WHAT THIS FILE PRODUCES, AND WHY THAT IS THE ONE AUTHORITY. Section 6.6 names
\ five words - IR:ENCODE, IR:DECODE, IR:DIGEST, IR:RENDER, IR:DIFF - and no
\ canonicalization word, so the split between this stage and the encoder is a
\ decision, not a quotation. It is made this way: this file produces a
\ CANONICAL TABLE - one owned store holding the canonical ordinal of every
\ interned row, followed by a cell stream that is the section 6.6 table order
\ with every stored reference already renumbered - and the encoder turns that
\ stream into bytes, adding the header, the digests, the versions, the integer
\ widths, the framing, and the SHA-256. So order and renumbering have exactly
\ one owner (this file) and framing and digest have exactly one owner (the
\ encoder), and neither can disagree with the other about a module's canonical
\ content.
\
\ The alternative reading - canonicalization re-materialises a second module in
\ canonical order - was rejected for a structural reason, not a convenience
\ one: IR-BUILD:NEW-BUILDER interns the dialect's own name into the new
\ module's symbol table before any caller can intern anything, so a
\ re-materialised module's symbol table always starts with the dialect name and
\ can therefore never be in the sorted order this stage has to produce. A
\ re-materialisation would also mint a second module identity, re-run the
\ freeze verifier over content already verified, and cost seventeen more arena
\ registry slots per module.
\
\ WHY A BARE PERMUTATION IS WRONG. Sorting the rows and emitting them unchanged
\ is not canonicalization, and that is machine-checked rather than argued.
\ formal/Common/Interning.v Types.ty_both_orders_admissible builds i8, i16 and
\ pointer-to-i8 in the two admissible orders and gets the row lists
\ [i8; i16; ptr->0] and [i16; i8; ptr->1]; Types.structural_rows_not_permutation
\ proves those two lists are not a permutation of each other; and
\ Types.ty_denotation_order_independent shows their denotations do agree. A
\ pointer row stores its pointee's module-local ordinal and a function-type row
\ names a window of ordinals, so permuting the type table changes stored row
\ content, and what two orders agree on is the DENOTATION of a row - the row
\ unfolded into the structure it references - never the row content itself.
\ Attributes are worse: an attribute row can hold a symbol ordinal, a type
\ ordinal, or a record pair of (key symbol ordinal, value attribute ordinal), so
\ the attribute stream is renumbered under three permutations at once - the
\ attribute table's own, the symbol table's, and the type table's.
\
\ WHICH BUILD ORDERS EXIST AT ALL. Not every insertion order is admissible.
\ IR-TYPE:POINTER refuses a pointee ordinal that is not already below the live
\ count, IR-ATTR:RECORD refuses a value that is not already constructed, and
\ IR-SOURCE:REGISTER-FROM refuses an origin that is not already registered, so
\ a row can only exist after everything it references. The admissible build
\ orders are exactly the topological orders of the reference graph, and "the
\ same canonical stream for any two build orders" means "for any two
\ topological orders".
\
\ THE ORDER THIS FILE CHOOSES, AND THE ONE RULE BEHIND IT. Every canonicalized
\ table is ordered by the same rule: walk the rows repeatedly, and each time
\ take the row whose canonical key is smallest among the rows all of whose
\ referents already have canonical ordinals. A row's canonical key is its own
\ content with every reference replaced by the referent's canonical ordinal, so
\ by induction over the selection the whole assignment depends only on the
\ denotations of the rows and not on the order they were interned in. The
\ symbol table has no references, so its key is its bytes and the rule reduces
\ to "sorted by bytes" - which is exactly what src/compiler/ir/symbol.f already
\ says canonicalization needs from it. The tables are ordered symbols first,
\ then types, then attributes, then sources, because that is the order in which
\ one table's keys can mention another's ordinals.
\
\ EQUAL KEYS SHARE ONE CANONICAL ORDINAL. The same selection rule carries the
\ one place two distinct rows can have the same canonical key. Symbols, types
\ and attributes are interned, so distinct rows have distinct content and the
\ canonical map is a bijection. The source registry deliberately does not
\ deduplicate ("Identity never deduplicates: every registration mints the next
\ module-local ordinal", source.f), so a module can hold two sources with the
\ same length, the same content digest and the same origin. Those two rows are
\ indistinguishable through every public reader, and a span into either is a
\ span into the same bytes - but a module whose first operation points at the
\ first copy and a module whose first operation points at the second copy are
\ the same module, so a canonical form that numbered them apart would not be
\ canonical. Rows with equal keys are therefore adjacent in the selection and
\ share one canonical ordinal, which makes the canonical source table
\ content-addressed and can make it shorter than the registry. For the interned
\ tables the same rule never fires.
\
\ WHAT IS NOT REORDERED. Functions, blocks, operations, values, operands,
\ results, successors and a function's attribute list keep their order exactly.
\ That order is the module's meaning - block layout, instruction order, and
\ which operand is which - and reordering it would change the program rather
\ than canonicalize its numbering. Their rows are still rewritten, because they
\ name symbols, types, attributes and sources, and those ordinals move.
\
\ WHAT THE STREAM STATES, AND WHAT IT LEAVES OUT. Each section states its row
\ count and then its rows, in section 6.6's order, and each row states its
\ identity-bearing content exactly once. Three classes of cell are deliberately
\ absent. Storage-only fields - a pool window's start offset, the symbol
\ interner's scan filter, an arena's capacity - are not content; section 6.6
\ says the encoding "does not serialize host addresses or arena capacities",
\ and a window is recovered from the explicit counts because the pools are
\ emitted in row order. Fields the freeze verifier proves equal to another
\ field are stated once on the side that owns them: a block's terminator is its
\ last operation (IR-VERIFY:TERM-CK), so the block states its operation count
\ and not its terminator, and a function's first block is where the previous
\ function's window ended. The dialect schema table is not a section at all:
\ section 6.6 serializes "dialect/schema versions", not the schema, and
\ IR-SCHEMA already publishes FTABLE-DIGEST for the encoder to bind it by. The
\ derived block-edge table is section 6.6's "optional derived indices" and is
\ re-derivable from the successor pool, so it is left out too.
\
\ WIRE CODES. The stream spells every closed vocabulary as a small integer, and
\ those integers are this package's own wire vocabulary: one exhaustive MATCH
\ per family, mirroring the owning table's storage code value for value. That
\ is the same arrangement attr.f already keeps for the CNUM and CTARGET
\ families ("The member codes are the components' stable canonical wire codes
\ ... matched value for value"), and it keeps each table's storage codes
\ private to the table while the stream's meaning is fixed here and versioned
\ by the encoder's format version. Because every mapping is an exhaustive
\ MATCH, a new member of any family fails to compile here rather than
\ serializing as some other member.
\
\ THE COMMITTED WORKING SET. The canonical maps, the two name buffers and the
\ two record-pair buffers are package-owned arrays with named ceilings, in the
\ shape IR-VERIFY's dominator working set already uses. The row ceilings match
\ the ceilings IR-BUILD's production plan commits to, so a module built to that
\ plan always fits; a module planned larger, a name longer than NAME-MAX, or a
\ list wider than PAIR-MAX is refused by name with E-IR-CANON-CAP rather than
\ canonicalized partially. The selection is quadratic in the rows of one table
\ and the symbol comparison re-reads both names, which is what those ceilings
\ bound.
\
\ THE RESULT IS OWNED, NOT PUBLISHED INTO THE MODULE. A frozen module's tables
\ cannot be added to, so the canonical table lives in an arena this package
\ creates from the presented context and never lets out: the handle stays in
\ this package's registry, exactly as IR-BUILD keeps a module's seventeen
\ tables, so there is no public word through which a caller could write to a
\ canonical table. RELEASE retires that arena, which frees its ARENA registry
\ slot, and marks this package's slot released while keeping its generation, so
\ a handle used afterwards is named rather than merely unknown. A caller that
\ never releases holds one of the eight slots below and one arena slot until the
\ owning context tears down, which is a committed ceiling with a named refusal
\ and not a leak.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/target.f
require src/compiler/numeric-policy.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/attr.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f

package IR-CANON
public

NEWTYPE table 0

private

CAST: MINT-T ( n -- IR-CANON:table ) ;
CAST: T>N ( IR-CANON:table -- n ) ;

\ The one raw crossing this package needs: one-way projections of the sealed
\ IR-ID identities onto their serials, for the result header's binding and for
\ owner comparison. Nothing here re-mints a raw cell into a nominal.
CAST: KEY-SERIAL ( IR-ID:ir-module-key -- n ) ;
CAST: MID-SERIAL ( IR-ID:ir-module-id -- n ) ;

\ ---- the canonical store's layout --------------------------------------------
$434E5431 constant CAN-MAGIC         \ "CNT1": the canonical-table header tag

0 constant HC-MAGIC
1 constant HC-SERIAL                 \ the owning module's serial
2 constant HC-SYMS                   \ rows in the module's symbol table
3 constant HC-CSYMS                  \ canonical symbols
4 constant HC-TYPES
5 constant HC-CTYPES
6 constant HC-ATTRS
7 constant HC-CATTRS
8 constant HC-SRCS
9 constant HC-CSRCS
10 constant HDR-CELLS

8 constant CELL-BYTES
-1 constant UNSET

\ ---- the committed working set -----------------------------------------------
\ The row ceilings are IR-BUILD's committed production ceilings; a module
\ planned larger is refused rather than canonicalized partially.
256 constant SYM-MAX
128 constant TY-MAX
128 constant AT-MAX
64 constant SRC-MAX
256 constant NAME-MAX                \ bytes of one symbol or one string value
64 constant PAIR-MAX                 \ keyed entries in one record or operation

\ ---- the four canonicalized tables -------------------------------------------
\ One selection rule serves all four, so the maps live in one array with a base
\ per table rather than four arrays with four accessors each.
0 constant TB-SYM
1 constant TB-TY
2 constant TB-AT
3 constant TB-SRC
4 constant TB#

: TB-BASE ( n -- n )
   case
      TB-SYM of 0 endof
      TB-TY  of SYM-MAX endof
      TB-AT  of SYM-MAX TY-MAX + endof
      TB-SRC of SYM-MAX TY-MAX + AT-MAX + endof
      E-IR-CANON-STATE throw
   endcase ;

SYM-MAX TY-MAX + AT-MAX + SRC-MAX + constant MAP-CELLS

\ ---- stable wire codes -------------------------------------------------------
\ Type kinds, mirroring IR-TYPE's storage codes value for value.
0 constant TK-INT
1 constant TK-FLT
2 constant TK-PTR
3 constant TK-QUOT
4 constant TK-CREF
5 constant TK-TOK
6 constant TK-MASK
7 constant TK-OPQ

\ Attribute kinds, mirroring IR-ATTR's storage codes value for value. Code 9 is
\ reserved there for the value-list kind that lands with its owning stage, so it
\ is unused here too.
0 constant AK-INT
1 constant AK-BOOL
2 constant AK-TXT
3 constant AK-SYM
4 constant AK-TYPE
5 constant AK-ILIST
6 constant AK-ENUM
7 constant AK-REC
8 constant AK-DIG

\ Attribute enum families, mirroring IR-ATTR.
0 constant EF-OVF
1 constant EF-FLO
2 constant EF-CON
3 constant EF-FAS
4 constant EF-CMP
5 constant EF-ARCH
6 constant EF-ABI
7 constant EF-END
8 constant EF-PTRW

\ A source's origin cell, mirroring IR-SOURCE: zero for a root, otherwise the
\ canonical parent ordinal plus one.
0 constant ORG-NONE

: TK-CODE ( IR-TYPE:kind -- n )
   MATCH IR-TYPE:kind
      int          OF TK-INT ENDOF
      float        OF TK-FLT ENDOF
      pointer      OF TK-PTR ENDOF
      quotation    OF TK-QUOT ENDOF
      code-ref     OF TK-CREF ENDOF
      memory-token OF TK-TOK ENDOF
      mask         OF TK-MASK ENDOF
      opaque       OF TK-OPQ ENDOF
   ;MATCH ;

: W-CODE ( IR-TYPE:width -- n )
   MATCH IR-TYPE:width
      w1  OF 0 ENDOF
      w8  OF 1 ENDOF
      w16 OF 2 ENDOF
      w32 OF 3 ENDOF
      w64 OF 4 ENDOF
   ;MATCH ;

: S-CODE ( IR-TYPE:sign -- n )
   MATCH IR-TYPE:sign
      unsigned OF 0 ENDOF
      signed   OF 1 ENDOF
   ;MATCH ;

: F-CODE ( IR-TYPE:fmt -- n )
   MATCH IR-TYPE:fmt
      half   OF 0 ENDOF
      bfloat OF 1 ENDOF
      single OF 2 ENDOF
      double OF 3 ENDOF
   ;MATCH ;

: SP-CODE ( IR-TYPE:space -- n )
   MATCH IR-TYPE:space
      generic OF 0 ENDOF
      global  OF 1 ENDOF
      shared  OF 2 ENDOF
      local   OF 3 ENDOF
      param   OF 4 ENDOF
      const   OF 5 ENDOF
   ;MATCH ;

: DM-CODE ( IR-TYPE:domain -- n )
   MATCH IR-TYPE:domain
      data-mem OF 0 ENDOF
      dict     OF 1 ENDOF
      code-pub OF 2 ENDOF
      io       OF 3 ENDOF
      process  OF 4 ENDOF
      ffi      OF 5 ENDOF
   ;MATCH ;

: AK-CODE ( IR-ATTR:kind -- n )
   MATCH IR-ATTR:kind
      int      OF AK-INT ENDOF
      boolean  OF AK-BOOL ENDOF
      text     OF AK-TXT ENDOF
      sym      OF AK-SYM ENDOF
      type-ref OF AK-TYPE ENDOF
      int-list OF AK-ILIST ENDOF
      enum-val OF AK-ENUM ENDOF
      record   OF AK-REC ENDOF
      digest   OF AK-DIG ENDOF
   ;MATCH ;

: EF-CODE ( IR-ATTR:efam -- n )
   MATCH IR-ATTR:efam
      overflow    OF EF-OVF ENDOF
      float-model OF EF-FLO ENDOF
      contraction OF EF-CON ENDOF
      fast-math   OF EF-FAS ENDOF
      compare     OF EF-CMP ENDOF
      arch        OF EF-ARCH ENDOF
      abi         OF EF-ABI ENDOF
      endian      OF EF-END ENDOF
      ptr-width   OF EF-PTRW ENDOF
   ;MATCH ;

: OVF-CODE ( CNUM:overflow -- n )
   MATCH CNUM:overflow
      wrap OF 0 ENDOF
      trap OF 1 ENDOF
   ;MATCH ;

: FLO-CODE ( CNUM:float-model -- n )
   MATCH CNUM:float-model
      ieee754        OF 0 ENDOF
      flush-denormal OF 1 ENDOF
   ;MATCH ;

: CON-CODE ( CNUM:contraction -- n )
   MATCH CNUM:contraction
      forbidden OF 0 ENDOF
      allowed   OF 1 ENDOF
   ;MATCH ;

: FAS-CODE ( CNUM:fast-math -- n )
   MATCH CNUM:fast-math
      bit-exact   OF 0 ENDOF
      reassociate OF 1 ENDOF
      approximate OF 2 ENDOF
   ;MATCH ;

: CMP-CODE ( CNUM:compare -- n )
   MATCH CNUM:compare
      ieee754-unordered OF 0 ENDOF
      total-order       OF 1 ENDOF
      assume-ordered    OF 2 ENDOF
   ;MATCH ;

: ARCH-CODE ( CTARGET:arch -- n )
   MATCH CTARGET:arch
      aarch64 OF 0 ENDOF
      ptx     OF 1 ENDOF
   ;MATCH ;

: ABI-CODE ( CTARGET:abi -- n )
   MATCH CTARGET:abi
      aapcs64-darwin OF 0 ENDOF
      aapcs64-linux  OF 1 ENDOF
      ptx-kernel     OF 2 ENDOF
   ;MATCH ;

: END-CODE ( CTARGET:endian -- n )
   MATCH CTARGET:endian
      little OF 0 ENDOF
      big    OF 1 ENDOF
   ;MATCH ;

: PTRW-CODE ( CTARGET:ptr-width -- n )
   MATCH CTARGET:ptr-width
      bits32 OF 0 ENDOF
      bits64 OF 1 ENDOF
   ;MATCH ;

: DK-CODE ( IR-OP:def-kind -- n )
   MATCH IR-OP:def-kind
      op-result OF 0 ENDOF
      blk-arg   OF 1 ENDOF
   ;MATCH ;

: LNK-CODE ( IR-FUN:linkage -- n )
   MATCH IR-FUN:linkage
      defined     OF 0 ENDOF
      replaceable OF 1 ENDOF
      imported    OF 2 ENDOF
   ;MATCH ;

: VIS-CODE ( IR-FUN:visibility -- n )
   MATCH IR-FUN:visibility
      hidden   OF 0 ENDOF
      exported OF 1 ENDOF
   ;MATCH ;

: CC-CODE ( IR-FUN:convention -- n )
   MATCH IR-FUN:convention
      habu   OF 0 ENDOF
      c-abi  OF 1 ENDOF
      kernel OF 2 ENDOF
   ;MATCH ;

\ ---- the registry ------------------------------------------------------------
\ One slot per canonical table, keyed by a nonzero, monotonic, never-reused
\ generation serial, exactly as IR-CTX, IR-ARENA and IR-BUILD do it.
8 constant SLOT-MAX
$7FFFFFFF constant CGEN-MAX
1 constant ST-LIVE
2 constant ST-RELEASED

here CELL 1- and CELL swap - CELL 1- and allot
variable CGEN-CELL
0 CGEN-CELL !
create CGENS SLOT-MAX cells allot
create COWNERS SLOT-MAX cells allot
create CSTATES SLOT-MAX cells allot
SLOT-MAX TYPED-BUFFER CSTORE IR-ARENA:arena
SLOT-MAX TYPED-BUFFER CKEYS IR-ID:ir-module-key

: CGEN@ ( n -- n )
   cells CGENS + @ ;

: CGEN! ( n n -- )
   cells CGENS + ! ;

: COWNER@ ( n -- n )
   cells COWNERS + @ ;

: COWNER! ( n n -- )
   cells COWNERS + ! ;

: CSTATE@ ( n -- n )
   cells CSTATES + @ ;

: CSTATE! ( n n -- )
   cells CSTATES + ! ;

: SLOTS-CLEAR ( -- )
   SLOT-MAX 0 ?do
      0 i CGEN!
   loop ;
SLOTS-CLEAR

: CGEN-NEXT-N ( n -- n )
   dup 0 < over CGEN-MAX >= or if E-IR-CANON-SERIALS throw then
   1+ ;

: TRY-CGEN ( -- n bool )
   CGEN-CELL atomic@ {: current:n :}
   current CGEN-NEXT-N {: next:n :}
   current next CGEN-CELL atomic-cas current =
   if next 0 0= else 0 0 0 <> then ;

: TAKE-CGEN ( -- n )
   begin
      TRY-CGEN dup 0=
   while
      2drop
   repeat
   drop ;

: FIND-C ( n -- n )
   {: g:n :}
   -1
   SLOT-MAX 0 ?do
      g i CGEN@ = if drop i leave then
   loop ;

\ Retire every slot whose owning context has torn down: the arena it names is
\ already unmapped and its generation can never resolve again.
: SWEEP ( -- )
   SLOT-MAX 0 ?do
      i CGEN@ 0 <> if
         i COWNER@ IR-CTX:SERIAL-LIVE? 0= if
            0 i CGEN!
         then
      then
   loop ;

: FREE-SLOT ( -- n )
   -1
   SLOT-MAX 0 ?do
      i CGEN@ 0= if drop i leave then
   loop
   dup 0 < if E-IR-CANON-SLOTS throw then ;

\ Resolve a handle to its slot. A generation this registry never minted and a
\ slot whose owning context has torn down are both stale; a released slot keeps
\ its generation, so it can still say which of the two happened.
: LIVE-SLOT ( IR-CANON:table -- n )
   T>N FIND-C
   dup 0 < if E-IR-CANON-STALE throw then
   dup COWNER@ IR-CTX:SERIAL-LIVE? 0= if
      0 over CGEN! E-IR-CANON-STALE throw
   then
   dup CSTATE@ ST-RELEASED = if E-IR-CANON-RELEASED throw then
   dup CSTATE@ ST-LIVE <> if E-IR-CANON-STATE throw then ;

\ ---- the module under canonicalization ---------------------------------------
\ One canonicalization per process, in the shape IR-VERIFY records the module it
\ is checking: CANON records the views here, every reader below names one, and
\ nothing carries over because CANON overwrites all thirteen and the key before
\ it reads any of them. The compilation context is deliberately not recorded -
\ IR-CTX's rule is that no context handle outlives the call it was given to - so
\ every word that allocates takes it from its caller.
0 constant V-SYP                     \ symbol byte pool
1 constant V-SYR                     \ symbol rows
2 constant V-TYP                     \ type list pool
3 constant V-TYR                     \ type rows
4 constant V-ATP                     \ attribute payload pool
5 constant V-ATR                     \ attribute rows
6 constant V-SRC                     \ source registry
7 constant V-FNP                     \ function attribute pool
8 constant V-FNR                     \ function rows
9 constant V-BLR                     \ block rows
10 constant V-OPP                    \ operation cell pool
11 constant V-VAL                    \ value rows
12 constant V-OPR                    \ operation rows
13 constant VIEW#

VIEW# TYPED-BUFFER MV IR-ARENA:view
1 TYPED-BUFFER MK IR-ID:ir-module-key
1 TYPED-BUFFER OUT IR-ARENA:arena

create MAPC MAP-CELLS cells allot    \ insertion ordinal -> canonical ordinal
create ATC MAP-CELLS cells allot     \ canonical ordinal -> first insertion row
create CNTS TB# cells allot          \ rows the module holds
create CCNTS TB# cells allot         \ canonical rows the stream states
create NBA NAME-MAX allot            \ the two byte buffers a name compare needs
create NBB NAME-MAX allot
create PKA PAIR-MAX cells allot      \ the two keyed-entry buffers a record or
create PVA PAIR-MAX cells allot      \ operation compare needs, canonical and
create PKB PAIR-MAX cells allot      \ sorted by canonical key
create PVB PAIR-MAX cells allot

\ Locals are single-assignment, so the selection's running choice and the
\ assignment's running cursor keep a named cell each. Each is written and read
\ inside one walk, and the walks do not nest.
variable BEST
variable NEXT
variable LAST
variable PN
variable PM

: V@ ( n -- IR-ARENA:view )
   MV @ ;

: V! ( IR-ARENA:view n -- )
   MV ! ;

: K@ ( -- IR-ID:ir-module-key )
   0 MK @ ;

: O@ ( -- IR-ARENA:arena )
   0 OUT @ ;

\ ---- identities and counts ---------------------------------------------------
: SYM ( n -- IR-ID:ir-symbol-id )
   K@ swap IR-ID:PACK-SYMBOL ;

: TYP ( n -- IR-ID:ir-type-id )
   K@ swap IR-ID:PACK-TYPE ;

: ATT ( n -- IR-ID:ir-attr-id )
   K@ swap IR-ID:PACK-ATTR ;

: SRC ( n -- IR-ID:ir-source-id )
   K@ swap IR-ID:PACK-SOURCE ;

: FUN ( n -- IR-ID:ir-fun-id )
   K@ swap IR-ID:PACK-FUN ;

: BLK ( n -- IR-ID:ir-block-id )
   K@ swap IR-ID:PACK-BLOCK ;

: OPI ( n -- IR-ID:ir-op-id )
   K@ swap IR-ID:PACK-OP ;

: VAL ( n -- IR-ID:ir-value-id )
   K@ swap IR-ID:PACK-VALUE ;

: SYMS ( -- n )
   V-SYR V@ IR-SYM:FSYMBOLS ;

: TYS ( -- n )
   V-TYR V@ IR-TYPE:FTYPES ;

: ATS ( -- n )
   V-ATR V@ IR-ATTR:FATTRS ;

: SRCS ( -- n )
   V-SRC V@ IR-SOURCE:FSOURCES ;

: FUNS ( -- n )
   V-FNR V@ IR-FUN:FFUNS ;

: BLKS ( -- n )
   V-BLR V@ IR-FUN:FBLOCKS ;

: OPS ( -- n )
   V-OPR V@ IR-OP:FOPS ;

: VALS ( -- n )
   V-VAL V@ IR-OP:FVALUES ;

\ ---- the canonical maps ------------------------------------------------------
: MAP@ ( n n -- n )
   {: tb:n l:n :}
   tb TB-BASE l + cells MAPC + @ ;

: MAP! ( n n n -- )
   {: tb:n v:n l:n :}
   v tb TB-BASE l + cells MAPC + ! ;

: AT@ ( n n -- n )
   {: tb:n c:n :}
   tb TB-BASE c + cells ATC + @ ;

: AT! ( n n n -- )
   {: tb:n v:n c:n :}
   v tb TB-BASE c + cells ATC + ! ;

: CNT@ ( n -- n )
   cells CNTS + @ ;

: CNT! ( n n -- )
   {: tb:n v:n :}
   v tb cells CNTS + ! ;

: CCNT@ ( n -- n )
   cells CCNTS + @ ;

: CCNT! ( n n -- )
   {: tb:n v:n :}
   v tb cells CCNTS + ! ;

\ ---- comparison primitives ---------------------------------------------------
: NCMP ( n n -- n )
   {: a:n b:n :}
   a b < if -1 exit then
   a b > if 1 exit then
   0 ;

\ Lexicographic byte order, shorter first on a shared prefix.
: BCMP ( ptr u8 n ptr u8 n -- n )
   {: p au:n q bu:n :} \ typed-local-lint: allow-bare-local - p and q keep the ptr u8 byte-span roles
   au bu min 0 ?do
      p i + c@  q i + c@  NCMP dup 0 <> if unloop exit then
      drop
   loop
   au bu NCMP ;

: NAME-CAP-CK ( n -- n )
   dup NAME-MAX > if E-IR-CANON-CAP throw then ;

: PAIR-CAP-CK ( n -- n )
   dup PAIR-MAX > if E-IR-CANON-CAP throw then ;

\ ---- names and string values -------------------------------------------------
: SYM>A ( n -- n )
   {: l:n :}
   V-SYR V@ l SYM IR-SYM:FLEN@ NAME-CAP-CK drop
   V-SYP V@ V-SYR V@ l SYM NBA NAME-MAX IR-SYM:FCOPY ;

: SYM>B ( n -- n )
   {: l:n :}
   V-SYR V@ l SYM IR-SYM:FLEN@ NAME-CAP-CK drop
   V-SYP V@ V-SYR V@ l SYM NBB NAME-MAX IR-SYM:FCOPY ;

: TXT>A ( n -- n )
   {: l:n :}
   V-ATR V@ l ATT IR-ATTR:FTEXT-LEN@ NAME-CAP-CK drop
   V-ATP V@ V-ATR V@ l ATT NBA NAME-MAX IR-ATTR:FTEXT-COPY ;

: TXT>B ( n -- n )
   {: l:n :}
   V-ATR V@ l ATT IR-ATTR:FTEXT-LEN@ NAME-CAP-CK drop
   V-ATP V@ V-ATR V@ l ATT NBB NAME-MAX IR-ATTR:FTEXT-COPY ;

\ ---- type rows ---------------------------------------------------------------
: TY-KIND ( n -- n )
   V-TYR V@ swap TYP IR-TYPE:FKIND@ TK-CODE ;

: TY-W ( n -- n )
   V-TYR V@ swap TYP IR-TYPE:FINT@ drop W-CODE ;

: TY-S ( n -- n )
   V-TYR V@ swap TYP IR-TYPE:FINT@ nip S-CODE ;

: TY-F ( n -- n )
   V-TYR V@ swap TYP IR-TYPE:FFLT@ F-CODE ;

: TY-DM ( n -- n )
   V-TYR V@ swap TYP IR-TYPE:FTOKEN@ DM-CODE ;

: TY-SP ( n -- n )
   V-TYR V@ K@ rot TYP IR-TYPE:FPOINTER@ drop SP-CODE ;

: TY-PTEE ( n -- n )
   V-TYR V@ K@ rot TYP IR-TYPE:FPOINTER@ nip IR-ID:TYPE-LOCAL ;

: TY-PN ( n -- n )
   V-TYR V@ swap TYP IR-TYPE:FARITY@ drop ;

: TY-RN ( n -- n )
   V-TYR V@ swap TYP IR-TYPE:FARITY@ nip ;

: TY-FN? ( n -- bool )
   dup TK-QUOT = swap TK-CREF = or ;

\ One element of a function type's list: parameters first, then results, which
\ is the order IR-TYPE stores them in.
: TY-ELEM ( n n -- n )
   {: l:n i:n :}
   l TY-PN {: pn:n :}
   i pn < if
      V-TYP V@ V-TYR V@ K@ l TYP i IR-TYPE:FPARAM@ IR-ID:TYPE-LOCAL exit
   then
   V-TYP V@ V-TYR V@ K@ l TYP i pn - IR-TYPE:FRESULT@ IR-ID:TYPE-LOCAL ;

: TY-ARITY ( n -- n )
   dup TY-PN swap TY-RN + ;

: TY-READY? ( n -- bool )
   {: l:n :}
   l TY-KIND {: k:n :}
   k TK-PTR = if l TY-PTEE TB-TY swap MAP@ UNSET <> exit then
   k TY-FN? 0= if true exit then
   l TY-ARITY 0 ?do
      l i TY-ELEM TB-TY swap MAP@ UNSET = if false unloop exit then
   loop
   true ;

: TY-CMP ( n n -- n )
   {: a:n b:n :}
   a TY-KIND b TY-KIND NCMP dup 0 <> if exit then
   drop
   a TY-KIND {: k:n :}
   k TK-INT = if
      a TY-W b TY-W NCMP dup 0 <> if exit then
      drop
      a TY-S b TY-S NCMP exit
   then
   k TK-FLT = if a TY-F b TY-F NCMP exit then
   k TK-TOK = if a TY-DM b TY-DM NCMP exit then
   k TK-PTR = if
      a TY-SP b TY-SP NCMP dup 0 <> if exit then
      drop
      a TY-PTEE TB-TY swap MAP@  b TY-PTEE TB-TY swap MAP@  NCMP exit
   then
   k TY-FN? 0= if 0 exit then
   a TY-PN b TY-PN NCMP dup 0 <> if exit then
   drop
   a TY-RN b TY-RN NCMP dup 0 <> if exit then
   drop
   a TY-ARITY 0 ?do
      a i TY-ELEM TB-TY swap MAP@  b i TY-ELEM TB-TY swap MAP@  NCMP
      dup 0 <> if unloop exit then
      drop
   loop
   0 ;

\ ---- attribute rows ----------------------------------------------------------
: AT-KIND ( n -- n )
   V-ATR V@ swap ATT IR-ATTR:FKIND@ AK-CODE ;

: AT-INT ( n -- n )
   V-ATR V@ swap ATT IR-ATTR:FINT@ ;

: AT-BOOL ( n -- n )
   V-ATR V@ swap ATT IR-ATTR:FBOOLEAN@ if 1 else 0 then ;

: AT-SYM ( n -- n )
   V-ATR V@ K@ rot ATT IR-ATTR:FSYM@ IR-ID:SYMBOL-LOCAL ;

: AT-TYPE ( n -- n )
   V-ATR V@ K@ rot ATT IR-ATTR:FTYPE@ IR-ID:TYPE-LOCAL ;

: AT-ITEMS ( n -- n )
   V-ATR V@ swap ATT IR-ATTR:FITEMS@ ;

: AT-ITEM ( n n -- n )
   {: l:n i:n :}
   V-ATP V@ V-ATR V@ l ATT i IR-ATTR:FITEM@ ;

: AT-PAIRS ( n -- n )
   V-ATR V@ swap ATT IR-ATTR:FPAIRS@ ;

: AT-PKEY ( n n -- n )
   {: l:n i:n :}
   V-ATP V@ V-ATR V@ K@ l ATT i IR-ATTR:FKEY@ IR-ID:SYMBOL-LOCAL ;

: AT-PVAL ( n n -- n )
   {: l:n i:n :}
   V-ATP V@ V-ATR V@ K@ l ATT i IR-ATTR:FVAL@ IR-ID:ATTR-LOCAL ;

: AT-EFAM ( n -- n )
   V-ATR V@ swap ATT IR-ATTR:FEFAM@ EF-CODE ;

\ The member of an enum attribute, under the family its row records. Each arm
\ reads through the family's own typed reader, so a stored member outside its
\ family's vocabulary is IR-ATTR's named refusal rather than a raw cell here.
: AT-EMEM ( n -- n )
   {: l:n :}
   l ATT {: id:IR-ID:ir-attr-id :}
   V-ATR V@ {: r:IR-ARENA:view :}
   l AT-EFAM {: f:n :}
   f EF-OVF = if r id IR-ATTR:FOVERFLOW@ OVF-CODE exit then
   f EF-FLO = if r id IR-ATTR:FFLOAT-MODEL@ FLO-CODE exit then
   f EF-CON = if r id IR-ATTR:FCONTRACTION@ CON-CODE exit then
   f EF-FAS = if r id IR-ATTR:FFAST-MATH@ FAS-CODE exit then
   f EF-CMP = if r id IR-ATTR:FCOMPARE@ CMP-CODE exit then
   f EF-ARCH = if r id IR-ATTR:FARCH@ ARCH-CODE exit then
   f EF-ABI = if r id IR-ATTR:FABI@ ABI-CODE exit then
   f EF-END = if r id IR-ATTR:FENDIAN@ END-CODE exit then
   r id IR-ATTR:FPTR-WIDTH@ PTRW-CODE ;

: AT-DIG ( n n -- n )
   {: l:n w:n :}
   V-ATR V@ l ATT IR-ATTR:FDIGEST@ CDIGEST-DIGEST:UNMAKE
   {: w0:n w1:n w2:n w3:n :}
   w 0 = if w0 exit then
   w 1 = if w1 exit then
   w 2 = if w2 exit then
   w3 ;

\ ---- keyed entries -----------------------------------------------------------
\ A record's pairs and an operation's attribute entries are both keyed by a
\ symbol, and design line 479 requires the keys to be canonically sorted. Both
\ are stored sorted by the key's INSERTION ordinal, which is a different
\ permutation from the canonical one, so both are reloaded into canonical
\ numbering and re-sorted here. The sort is by key alone, because a duplicate
\ key is already refused where the entry set is built.
: PAIR-SWAP-A ( n n -- )
   {: x:n y:n :}
   x cells PKA + @  y cells PKA + @  x cells PKA + !  y cells PKA + !
   x cells PVA + @  y cells PVA + @  x cells PVA + !  y cells PVA + ! ;

: PAIR-SWAP-B ( n n -- )
   {: x:n y:n :}
   x cells PKB + @  y cells PKB + @  x cells PKB + !  y cells PKB + !
   x cells PVB + @  y cells PVB + @  x cells PVB + !  y cells PVB + ! ;

: SORT-A ( n -- )
   {: n:n :}
   n 1 ?do
      i PM !
      begin
         PM @ 0 > if
            PM @ cells PKA + @  PM @ 1- cells PKA + @  <
         else
            false
         then
      while
         PM @ PM @ 1- PAIR-SWAP-A
         PM @ 1- PM !
      repeat
   loop ;

: SORT-B ( n -- )
   {: n:n :}
   n 1 ?do
      i PM !
      begin
         PM @ 0 > if
            PM @ cells PKB + @  PM @ 1- cells PKB + @  <
         else
            false
         then
      while
         PM @ PM @ 1- PAIR-SWAP-B
         PM @ 1- PM !
      repeat
   loop ;

\ A record's pairs in canonical numbering and canonical key order.
: REC>A ( n -- n )
   {: l:n :}
   l AT-PAIRS PAIR-CAP-CK {: n:n :}
   n 0 ?do
      TB-SYM  l i AT-PKEY  MAP@ i cells PKA + !
      TB-AT   l i AT-PVAL  MAP@ i cells PVA + !
   loop
   n SORT-A
   n ;

: REC>B ( n -- n )
   {: l:n :}
   l AT-PAIRS PAIR-CAP-CK {: n:n :}
   n 0 ?do
      TB-SYM  l i AT-PKEY  MAP@ i cells PKB + !
      TB-AT   l i AT-PVAL  MAP@ i cells PVB + !
   loop
   n SORT-B
   n ;

: AT-READY? ( n -- bool )
   {: l:n :}
   l AT-KIND AK-REC <> if true exit then
   l AT-PAIRS 0 ?do
      TB-AT  l i AT-PVAL  MAP@ UNSET = if false unloop exit then
   loop
   true ;

: REC-CMP ( n n -- n )
   {: a:n b:n :}
   a REC>A {: an:n :}
   b REC>B {: bn:n :}
   an bn NCMP dup 0 <> if exit then
   drop
   an 0 ?do
      i cells PKA + @  i cells PKB + @  NCMP dup 0 <> if unloop exit then
      drop
      i cells PVA + @  i cells PVB + @  NCMP dup 0 <> if unloop exit then
      drop
   loop
   0 ;

: TXT-CMP ( n n -- n )
   {: a:n b:n :}
   a TXT>A {: au:n :}
   b TXT>B {: bu:n :}
   NBA au NBB bu BCMP ;

: ILIST-CMP ( n n -- n )
   {: a:n b:n :}
   a AT-ITEMS b AT-ITEMS NCMP dup 0 <> if exit then
   drop
   a AT-ITEMS 0 ?do
      a i AT-ITEM  b i AT-ITEM  NCMP dup 0 <> if unloop exit then
      drop
   loop
   0 ;

: DIG-CMP ( n n -- n )
   {: a:n b:n :}
   4 0 ?do
      a i AT-DIG  b i AT-DIG  NCMP dup 0 <> if unloop exit then
      drop
   loop
   0 ;

: ENUM-CMP ( n n -- n )
   {: a:n b:n :}
   a AT-EFAM b AT-EFAM NCMP dup 0 <> if exit then
   drop
   a AT-EMEM b AT-EMEM NCMP ;

: AT-CMP ( n n -- n )
   {: a:n b:n :}
   a AT-KIND b AT-KIND NCMP dup 0 <> if exit then
   drop
   a AT-KIND {: k:n :}
   k AK-INT = if a AT-INT b AT-INT NCMP exit then
   k AK-BOOL = if a AT-BOOL b AT-BOOL NCMP exit then
   k AK-TXT = if a b TXT-CMP exit then
   k AK-SYM = if
      TB-SYM a AT-SYM MAP@  TB-SYM b AT-SYM MAP@  NCMP exit
   then
   k AK-TYPE = if
      TB-TY a AT-TYPE MAP@  TB-TY b AT-TYPE MAP@  NCMP exit
   then
   k AK-ILIST = if a b ILIST-CMP exit then
   k AK-ENUM = if a b ENUM-CMP exit then
   k AK-REC = if a b REC-CMP exit then
   a b DIG-CMP ;

\ ---- source rows -------------------------------------------------------------
: SRC-LEN ( n -- n )
   V-SRC V@ swap SRC IR-SOURCE:FLEN@ ;

: SRC-ROOT? ( n -- bool )
   V-SRC V@ swap SRC IR-SOURCE:FROOT? ;

: SRC-PARENT ( n -- n )
   V-SRC V@ K@ rot SRC IR-SOURCE:FORIGIN@ IR-ID:SOURCE-LOCAL ;

: SRC-DIG ( n n -- n )
   {: l:n w:n :}
   V-SRC V@ l SRC IR-SOURCE:FDIGEST@ CDIGEST-DIGEST:UNMAKE
   {: w0:n w1:n w2:n w3:n :}
   w 0 = if w0 exit then
   w 1 = if w1 exit then
   w 2 = if w2 exit then
   w3 ;

\ The origin cell a canonical row states: zero for a root, otherwise the
\ canonical parent ordinal plus one, which is IR-SOURCE's own encoding read in
\ canonical numbering.
: SRC-ORG ( n -- n )
   {: l:n :}
   l SRC-ROOT? if ORG-NONE exit then
   TB-SRC l SRC-PARENT MAP@ 1+ ;

: SRC-READY? ( n -- bool )
   {: l:n :}
   l SRC-ROOT? if true exit then
   TB-SRC l SRC-PARENT MAP@ UNSET <> ;

: SRC-CMP ( n n -- n )
   {: a:n b:n :}
   a SRC-ORG b SRC-ORG NCMP dup 0 <> if exit then
   drop
   4 0 ?do
      a i SRC-DIG  b i SRC-DIG  NCMP dup 0 <> if unloop exit then
      drop
   loop
   a SRC-LEN b SRC-LEN NCMP ;

\ ---- one selection rule for all four tables ----------------------------------
: RDY? ( n n -- bool )
   {: tb:n l:n :}
   tb TB-SYM = if true exit then
   tb TB-TY = if l TY-READY? exit then
   tb TB-AT = if l AT-READY? exit then
   tb TB-SRC = if l SRC-READY? exit then
   E-IR-CANON-STATE throw ;

: CMP ( n n n -- n )
   {: tb:n a:n b:n :}
   tb TB-SYM = if
      a SYM>A {: au:n :}
      b SYM>B {: bu:n :}
      NBA au NBB bu BCMP exit
   then
   tb TB-TY = if a b TY-CMP exit then
   tb TB-AT = if a b AT-CMP exit then
   tb TB-SRC = if a b SRC-CMP exit then
   E-IR-CANON-STATE throw ;

: FRESH ( n n -- )
   {: tb:n l:n :}
   tb NEXT @ l MAP!
   tb l NEXT @ AT!
   NEXT @ 1+ NEXT ! ;

\ The row just selected either has content no earlier row had, and takes the
\ next canonical ordinal, or has the content of the row selected before it - the
\ keys only ever increase, so an equal key is the immediately preceding one -
\ and shares its ordinal.
: ASSIGN ( n n -- )
   {: tb:n l:n :}
   LAST @ UNSET = if tb l FRESH exit then
   tb l LAST @ CMP 0 <> if tb l FRESH exit then
   tb  tb LAST @ MAP@  l MAP! ;

: PICK-READY ( n -- )
   {: tb:n :}
   -1 BEST !
   tb CNT@ 0 ?do
      tb i MAP@ UNSET = if
         tb i RDY? if
            BEST @ 0 < if
               i BEST !
            else
               tb i BEST @ CMP 0 < if i BEST ! then
            then
         then
      then
   loop
   BEST @ 0 < if E-IR-CANON-ORDER throw then ;

\ Order one table. Every round takes the smallest ready row, so a row is never
\ numbered before anything it references; a round with no ready row left means
\ the stored references contain a cycle, which construction cannot produce and a
\ forged table can, and it is refused rather than looped on.
: ORDER ( n -- )
   {: tb:n :}
   tb CNT@ 0 ?do
      tb UNSET i MAP!
   loop
   0 NEXT !
   UNSET LAST !
   tb CNT@ 0 ?do
      tb PICK-READY
      tb BEST @ ASSIGN
      BEST @ LAST !
   loop
   tb NEXT @ CCNT! ;

\ ---- writing the canonical store ---------------------------------------------
: PUT ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx v:n :}
   c O@ v IR-ARENA:PUSH drop ;

: B>C ( n -- n )
   CELL-BYTES 1- + CELL-BYTES / ;

\ Eight bytes per cell little-endian with a zero-padded tail: the packing
\ IR-SYM and IR-ATTR use for their own pools, repeated here as the stream's
\ convention for a run of bytes.
: PACK8 ( ptr u8 n n -- n )
   {: p u:n j:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   0
   CELL-BYTES 0 ?do
      j i + u < if
         p j i + + c@  i 8 * lshift  or
      then
   loop ;

: PUT-BYTES ( IR-CTX:ctx ptr u8 n -- )
   {: c:IR-CTX:ctx p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c u PUT
   u B>C 0 ?do
      c  p u i CELL-BYTES *  PACK8  PUT
   loop ;

\ A span in canonical numbering: the source's canonical ordinal, then the byte
\ start and length, which no permutation touches.
: PUT-SPAN ( IR-CTX:ctx IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: c:IR-CTX:ctx s:IR-ID:ir-source-id st:n ln:n :}
   c  TB-SRC s IR-ID:SOURCE-LOCAL MAP@  PUT
   c st PUT
   c ln PUT ;

: PUT-HEAD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c CAN-MAGIC PUT
   c K@ KEY-SERIAL PUT
   c TB-SYM CNT@ PUT
   c TB-SYM CCNT@ PUT
   c TB-TY CNT@ PUT
   c TB-TY CCNT@ PUT
   c TB-AT CNT@ PUT
   c TB-AT CCNT@ PUT
   c TB-SRC CNT@ PUT
   c TB-SRC CCNT@ PUT ;

\ The maps, in table order: one cell per row of the module, holding that row's
\ canonical ordinal. They are what a later pass rewrites an identity through.
: PUT-MAPS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   TB# 0 ?do
      i CNT@ 0 ?do
         c  j i MAP@  PUT
      loop
   loop ;

\ ---- the stream: section 6.6's table order -----------------------------------
: PUT-SYMS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c TB-SYM CCNT@ PUT
   TB-SYM CCNT@ 0 ?do
      TB-SYM i AT@ SYM>A {: u:n :}
      c NBA u PUT-BYTES
   loop ;

: PUT-TYPE ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx l:n :}
   l TY-KIND {: k:n :}
   c k PUT
   k TK-INT = if
      c l TY-W PUT
      c l TY-S PUT
      exit
   then
   k TK-FLT = if c l TY-F PUT exit then
   k TK-TOK = if c l TY-DM PUT exit then
   k TK-PTR = if
      c l TY-SP PUT
      c  TB-TY l TY-PTEE MAP@  PUT
      exit
   then
   k TY-FN? 0= if exit then
   c l TY-PN PUT
   c l TY-RN PUT
   l TY-ARITY 0 ?do
      c  TB-TY l i TY-ELEM MAP@  PUT
   loop ;

: PUT-TYPES ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c TB-TY CCNT@ PUT
   TB-TY CCNT@ 0 ?do
      c  TB-TY i AT@  PUT-TYPE
   loop ;

: PUT-ATTR ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx l:n :}
   l AT-KIND {: k:n :}
   c k PUT
   k AK-INT = if c l AT-INT PUT exit then
   k AK-BOOL = if c l AT-BOOL PUT exit then
   k AK-TXT = if
      l TXT>A {: u:n :}
      c NBA u PUT-BYTES
      exit
   then
   k AK-SYM = if c  TB-SYM l AT-SYM MAP@  PUT exit then
   k AK-TYPE = if c  TB-TY l AT-TYPE MAP@  PUT exit then
   k AK-ILIST = if
      c l AT-ITEMS PUT
      l AT-ITEMS 0 ?do
         c  l i AT-ITEM  PUT
      loop
      exit
   then
   k AK-ENUM = if
      c l AT-EFAM PUT
      c l AT-EMEM PUT
      exit
   then
   k AK-REC = if
      l REC>A {: n:n :}
      c n PUT
      n 0 ?do
         c i cells PKA + @ PUT
         c i cells PVA + @ PUT
      loop
      exit
   then
   4 0 ?do
      c  l i AT-DIG  PUT
   loop ;

: PUT-ATTRS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c TB-AT CCNT@ PUT
   TB-AT CCNT@ 0 ?do
      c  TB-AT i AT@  PUT-ATTR
   loop ;

: PUT-SRCS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c TB-SRC CCNT@ PUT
   TB-SRC CCNT@ 0 ?do
      TB-SRC i AT@ {: l:n :}
      c l SRC-LEN PUT
      4 0 ?do
         c  l i SRC-DIG  PUT
      loop
      c l SRC-ORG PUT
   loop ;

: PUT-FUN ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx l:n :}
   l FUN {: f:IR-ID:ir-fun-id :}
   c  TB-SYM  V-FNR V@ K@ f IR-FUN:FSYMBOL@ IR-ID:SYMBOL-LOCAL  MAP@  PUT
   c  TB-TY  V-FNR V@ K@ f IR-FUN:FSIGNATURE@ IR-ID:TYPE-LOCAL  MAP@  PUT
   c  V-FNR V@ f IR-FUN:FLINKAGE@ LNK-CODE  PUT
   c  V-FNR V@ f IR-FUN:FVISIBILITY@ VIS-CODE  PUT
   c  V-FNR V@ f IR-FUN:FCONVENTION@ CC-CODE  PUT
   c  V-FNR V@ f IR-FUN:FBLOCK-COUNT  PUT
   c  V-FNR V@ f IR-FUN:FATTR-COUNT  PUT
   c  V-FNR V@ K@ f IR-FUN:FSPAN@  PUT-SPAN ;

: PUT-FUNS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c FUNS PUT
   FUNS 0 ?do
      c i PUT-FUN
   loop ;

: PUT-BLOCK ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx l:n :}
   l BLK {: b:IR-ID:ir-block-id :}
   c  V-BLR V@ V-FNR V@ K@ b IR-FUN:FPARENT@ IR-ID:FUN-LOCAL  PUT
   c  V-BLR V@ b IR-FUN:FARG-COUNT  PUT
   c  V-BLR V@ b IR-FUN:FOP-COUNT  PUT
   c  V-BLR V@ K@ b IR-FUN:FBLOCK-SPAN@  PUT-SPAN ;

: PUT-BLOCKS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c BLKS PUT
   BLKS 0 ?do
      c i PUT-BLOCK
   loop ;

: PUT-OP ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx l:n :}
   l OPI {: o:IR-ID:ir-op-id :}
   c  TB-SYM  V-OPR V@ K@ o IR-OP:FOPCODE@ IR-ID:SYMBOL-LOCAL  MAP@  PUT
   c  V-OPR V@ o IR-OP:FOPERANDS  PUT
   c  V-OPR V@ o IR-OP:FRESULTS  PUT
   c  V-OPR V@ o IR-OP:FSUCCESSORS  PUT
   c  V-OPR V@ o IR-OP:FATTRS  PUT
   c  V-OPR V@ K@ o IR-OP:FSPAN@  PUT-SPAN ;

: PUT-OPS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c OPS PUT
   OPS 0 ?do
      c i PUT-OP
   loop ;

: PUT-VALUE ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx l:n :}
   l VAL {: v:IR-ID:ir-value-id :}
   c  TB-TY  V-VAL V@ K@ v IR-OP:FVALUE-TYPE@ IR-ID:TYPE-LOCAL  MAP@  PUT
   V-VAL V@ v IR-OP:FVALUE-KIND@ {: dk:IR-OP:def-kind :}
   c dk DK-CODE PUT
   dk IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ if
      c  V-VAL V@ K@ v IR-OP:FVALUE-BLOCK@ IR-ID:BLOCK-LOCAL  PUT
      c  V-VAL V@ v IR-OP:FVALUE-ARG@  PUT
      exit
   then
   c  V-VAL V@ V-OPR V@ K@ v IR-OP:FVALUE-OP@ IR-ID:OP-LOCAL  PUT
   c  V-VAL V@ v IR-OP:FVALUE-POS@  PUT ;

: PUT-VALUES ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c VALS PUT
   VALS 0 ?do
      c i PUT-VALUE
   loop ;

\ ---- the pools ---------------------------------------------------------------
\ Design line 573's "operand/result/successor pools", plus the three windows
\ this substrate's tables add: a block's arguments, a function's attribute list,
\ and an operation's keyed attribute entries. Each pool is emitted in row order,
\ so the window every row stated as a count is recovered by walking the rows.
\ Operand, result, successor and argument cells are value and block ordinals,
\ which keep their order and their numbering; the attribute cells are rewritten,
\ and an operation's entries are sorted by canonical key.
: PUT-OPERAND-POOL ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   OPS 0 ?do
      i OPI {: o:IR-ID:ir-op-id :}
      V-OPR V@ o IR-OP:FOPERANDS 0 ?do
         c  V-OPP V@ V-OPR V@ K@ o i IR-OP:FOPERAND@ IR-ID:VALUE-LOCAL  PUT
      loop
   loop ;

: PUT-RESULT-POOL ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   OPS 0 ?do
      i OPI {: o:IR-ID:ir-op-id :}
      V-OPR V@ o IR-OP:FRESULTS 0 ?do
         c  V-OPP V@ V-OPR V@ K@ o i IR-OP:FRESULT@ IR-ID:VALUE-LOCAL  PUT
      loop
   loop ;

: PUT-SUCC-POOL ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   OPS 0 ?do
      i OPI {: o:IR-ID:ir-op-id :}
      V-OPR V@ o IR-OP:FSUCCESSORS 0 ?do
         c  V-OPP V@ V-OPR V@ K@ o i IR-OP:FSUCCESSOR@ IR-ID:BLOCK-LOCAL  PUT
      loop
   loop ;

: PUT-ARG-POOL ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   BLKS 0 ?do
      i BLK {: b:IR-ID:ir-block-id :}
      V-BLR V@ b IR-FUN:FARG-COUNT 0 ?do
         c  V-BLR V@ V-VAL V@ K@ b i IR-FUN:FARG@ IR-ID:VALUE-LOCAL  PUT
      loop
   loop ;

: PUT-FUN-ATTR-POOL ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   FUNS 0 ?do
      i FUN {: f:IR-ID:ir-fun-id :}
      V-FNR V@ f IR-FUN:FATTR-COUNT 0 ?do
         c  TB-AT
         V-FNP V@ V-FNR V@ K@ f i IR-FUN:FATTR@ IR-ID:ATTR-LOCAL
         MAP@  PUT
      loop
   loop ;

: OP-ENTRIES>A ( n -- n )
   {: l:n :}
   l OPI {: o:IR-ID:ir-op-id :}
   V-OPR V@ o IR-OP:FATTRS PAIR-CAP-CK {: n:n :}
   n 0 ?do
      TB-SYM
      V-OPP V@ V-OPR V@ K@ o i IR-OP:FATTR-KEY@ IR-ID:SYMBOL-LOCAL
      MAP@ i cells PKA + !
      TB-AT
      V-OPP V@ V-OPR V@ K@ o i IR-OP:FATTR@ IR-ID:ATTR-LOCAL
      MAP@ i cells PVA + !
   loop
   n SORT-A
   n ;

: PUT-OP-ATTR-POOL ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   OPS 0 ?do
      i OP-ENTRIES>A {: n:n :}
      n 0 ?do
         c i cells PKA + @ PUT
         c i cells PVA + @ PUT
      loop
   loop ;

: PUT-POOLS ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c PUT-OPERAND-POOL
   c PUT-RESULT-POOL
   c PUT-SUCC-POOL
   c PUT-ARG-POOL
   c PUT-FUN-ATTR-POOL
   c PUT-OP-ATTR-POOL ;

: PUT-STREAM ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c PUT-SYMS
   c PUT-TYPES
   c PUT-ATTRS
   c PUT-SRCS
   c PUT-FUNS
   c PUT-BLOCKS
   c PUT-OPS
   c PUT-VALUES
   c PUT-POOLS ;

\ ---- capacity and the store's ceiling ----------------------------------------
: ROWS-CK ( -- )
   SYMS SYM-MAX > if E-IR-CANON-CAP throw then
   TYS TY-MAX > if E-IR-CANON-CAP throw then
   ATS AT-MAX > if E-IR-CANON-CAP throw then
   SRCS SRC-MAX > if E-IR-CANON-CAP throw then ;

\ Every run of bytes and every keyed list the stream will restate, measured
\ before anything is ordered or allocated. Without this pass a name too long for
\ the compare buffers would be found part way through writing the store, and a
\ refusal has to cost the caller nothing.
: NAMES-CK ( -- )
   SYMS 0 ?do
      V-SYR V@ i SYM IR-SYM:FLEN@ NAME-CAP-CK drop
   loop ;

: WIDTHS-CK ( -- )
   ATS 0 ?do
      i AT-KIND AK-TXT = if
         V-ATR V@ i ATT IR-ATTR:FTEXT-LEN@ NAME-CAP-CK drop
      then
      i AT-KIND AK-REC = if i AT-PAIRS PAIR-CAP-CK drop then
   loop
   OPS 0 ?do
      V-OPR V@ i OPI IR-OP:FATTRS PAIR-CAP-CK drop
   loop ;

: FITS-CK ( -- )
   ROWS-CK
   NAMES-CK
   WIDTHS-CK ;

\ An upper bound on the cells the canonical store needs, so the arena is created
\ with one committed ceiling and never has to be resized while it is written.
\ The three terms are: the header and one map cell per interned row; at most
\ sixteen fixed cells per row of every table, which covers the widest row this
\ file emits (a function states ten and an attribute five); and the storage of
\ every pool the variable-length content comes out of, since a run of bytes,
\ list elements or window cells can never be longer in the stream than it is in
\ the table it was read from.
: STORE-CEIL ( -- n )
   HDR-CELLS
   SYMS + TYS + ATS + SRCS +
   SYMS TYS + ATS + SRCS + FUNS + BLKS + OPS + VALS + 16 * +
   V-SYP V@ IR-ARENA:SIZE +
   V-TYP V@ IR-ARENA:SIZE +
   V-ATP V@ IR-ARENA:SIZE +
   V-FNP V@ IR-ARENA:SIZE +
   V-OPP V@ IR-ARENA:SIZE +
   64 + ;

: TAKE-VIEWS ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY 0 MK !
   m IR-BUILD:FSYM-POOL V-SYP V!
   m IR-BUILD:FSYM-ROWS V-SYR V!
   m IR-BUILD:FTYPE-POOL V-TYP V!
   m IR-BUILD:FTYPE-ROWS V-TYR V!
   m IR-BUILD:FATTR-POOL V-ATP V!
   m IR-BUILD:FATTR-ROWS V-ATR V!
   m IR-BUILD:FSOURCES V-SRC V!
   m IR-BUILD:FFUN-POOL V-FNP V!
   m IR-BUILD:FFUN-ROWS V-FNR V!
   m IR-BUILD:FBLOCK-ROWS V-BLR V!
   m IR-BUILD:FOP-POOL V-OPP V!
   m IR-BUILD:FVALUE-ROWS V-VAL V!
   m IR-BUILD:FOP-ROWS V-OPR V! ;

: TAKE-COUNTS ( -- )
   TB-SYM SYMS CNT!
   TB-TY TYS CNT!
   TB-AT ATS CNT!
   TB-SRC SRCS CNT! ;

\ Symbols first, because a type row's key mentions no other table but an
\ attribute's mentions symbols and types and a source's mentions sources.
: ORDER-ALL ( -- )
   TB-SYM ORDER
   TB-TY ORDER
   TB-AT ORDER
   TB-SRC ORDER ;

: WRITE-ALL ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c PUT-HEAD
   c PUT-MAPS
   c PUT-STREAM ;

public

\ ---- canonicalization --------------------------------------------------------
\ Canonicalize one frozen module and answer the owned canonical table. Every
\ refusal - a module that is not a live frozen one, a module larger than the
\ committed working set, a name or list too wide, a registry with no free slot -
\ runs before the store is created, so a refused canonicalization allocates
\ nothing. The generation is installed last, so a failure part way through
\ writing leaves no half-installed slot behind either: the store it had begun is
\ unreachable and its arena registry slot returns with the context, the same
\ discipline a failure part way through IR-BUILD's table creation keeps.
: CANON ( IR-CTX:ctx IR-BUILD:module -- IR-CANON:table )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   m IR-BUILD:FROZEN? 0= if E-IR-CANON-STALE throw then
   SWEEP
   FREE-SLOT {: slot:n :}
   TAKE-CGEN {: g:n :}
   m TAKE-VIEWS
   TAKE-COUNTS
   FITS-CK
   ORDER-ALL
   c STORE-CEIL IR-ARENA:NEW 0 OUT !
   c WRITE-ALL
   O@ slot CSTORE !
   K@ slot CKEYS !
   c IR-CTX:SERIAL slot COWNER!
   ST-LIVE slot CSTATE!
   g slot CGEN!
   g MINT-T ;

\ Give the canonical table up: the store is retired, which releases its arena
\ registry slot and makes every cell it held unreadable, and the slot records
\ that it was released so a later use of the handle is named.
: RELEASE ( IR-CANON:table -- )
   LIVE-SLOT {: slot:n :}
   slot CSTORE @ IR-ARENA:ABORT
   ST-RELEASED slot CSTATE! ;

: LIVE? ( IR-CANON:table -- bool )
   T>N FIND-C {: slot:n :}
   slot 0 < if 0 0 <> exit then
   slot COWNER@ IR-CTX:SERIAL-LIVE?
   slot CSTATE@ ST-LIVE = and ;

private

\ ---- reading the canonical store ---------------------------------------------
: SCELL@ ( n n -- n )
   {: slot:n k:n :}
   slot CSTORE @ {: a:IR-ARENA:arena :}
   a a k IR-ARENA:NTH IR-ARENA:PEEK ;

: HDR-CK ( n -- n )
   {: slot:n :}
   slot CSTORE @ IR-ARENA:USED HDR-CELLS < if E-IR-CANON-STATE throw then
   slot HC-MAGIC SCELL@ CAN-MAGIC <> if E-IR-CANON-STATE throw then
   slot HC-SERIAL SCELL@ slot CKEYS @ KEY-SERIAL <> if
      E-IR-CANON-STATE throw
   then
   slot ;

: SLOT-OF ( IR-CANON:table -- n )
   LIVE-SLOT HDR-CK ;

: OWN-CK ( n n -- )
   {: slot:n os:n :}
   os slot CKEYS @ KEY-SERIAL <> if E-IR-CANON-OWNER throw then ;

\ One map cell: the ordinal is validated against the count this store recorded
\ for the table it indexes, and the map regions follow the header in table
\ order.
: MAP-AT ( n n n -- n )
   {: slot:n tb:n l:n :}
   slot tb 2 * 2 + SCELL@ {: n:n :}
   l 0 < l n >= or if E-IR-CANON-BOUND throw then
   0
   tb 0 ?do
      slot i 2 * 2 + SCELL@ +
   loop
   {: base:n :}
   slot HDR-CELLS base + l + SCELL@ ;

: MAPS-CELLS ( n -- n )
   {: slot:n :}
   0
   TB# 0 ?do
      slot i 2 * 2 + SCELL@ +
   loop ;

public

\ ---- the canonical ordinal of an identity ------------------------------------
\ What a later pass rewrites a reference through. An identity minted under
\ another module is refused: this table numbers one module's rows.
: SYMBOL-ORD ( IR-CANON:table IR-ID:ir-symbol-id -- n )
   {: t:IR-CANON:table id:IR-ID:ir-symbol-id :}
   t SLOT-OF {: slot:n :}
   slot id IR-ID:SYMBOL-OWNER MID-SERIAL OWN-CK
   slot TB-SYM id IR-ID:SYMBOL-LOCAL MAP-AT ;

: TYPE-ORD ( IR-CANON:table IR-ID:ir-type-id -- n )
   {: t:IR-CANON:table id:IR-ID:ir-type-id :}
   t SLOT-OF {: slot:n :}
   slot id IR-ID:TYPE-OWNER MID-SERIAL OWN-CK
   slot TB-TY id IR-ID:TYPE-LOCAL MAP-AT ;

: ATTR-ORD ( IR-CANON:table IR-ID:ir-attr-id -- n )
   {: t:IR-CANON:table id:IR-ID:ir-attr-id :}
   t SLOT-OF {: slot:n :}
   slot id IR-ID:ATTR-OWNER MID-SERIAL OWN-CK
   slot TB-AT id IR-ID:ATTR-LOCAL MAP-AT ;

: SOURCE-ORD ( IR-CANON:table IR-ID:ir-source-id -- n )
   {: t:IR-CANON:table id:IR-ID:ir-source-id :}
   t SLOT-OF {: slot:n :}
   slot id IR-ID:SOURCE-OWNER MID-SERIAL OWN-CK
   slot TB-SRC id IR-ID:SOURCE-LOCAL MAP-AT ;

\ ---- how many canonical rows each table has ----------------------------------
: SYMBOLS ( IR-CANON:table -- n )
   SLOT-OF HC-CSYMS SCELL@ ;

: TYPES ( IR-CANON:table -- n )
   SLOT-OF HC-CTYPES SCELL@ ;

: ATTRS ( IR-CANON:table -- n )
   SLOT-OF HC-CATTRS SCELL@ ;

: SOURCES ( IR-CANON:table -- n )
   SLOT-OF HC-CSRCS SCELL@ ;

\ ---- the canonical stream ----------------------------------------------------
\ The cells the encoder frames: section 6.6's table order, every reference in
\ canonical numbering, no host address and no arena capacity. Two modules that
\ differ only in the order their tables were interned have the same stream, cell
\ for cell.
: CELLS ( IR-CANON:table -- n )
   SLOT-OF {: slot:n :}
   slot CSTORE @ IR-ARENA:USED HDR-CELLS - slot MAPS-CELLS - ;

: CELL@ ( IR-CANON:table n -- n )
   {: t:IR-CANON:table k:n :}
   t SLOT-OF {: slot:n :}
   k 0 < k t CELLS >= or if E-IR-CANON-BOUND throw then
   slot HDR-CELLS slot MAPS-CELLS + k + SCELL@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
