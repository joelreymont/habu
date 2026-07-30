\ diff.f - structural difference between two frozen modules: the one authority
\ that says whether two modules mean the same thing, and what differs when they
\ do not.
\
\ docs/compiler-ir-design.md section 6.6 names IR:DIFF among the five words the
\ serialization stage publishes, and section 5.6 forbids recovering semantic
\ facts from text. So this file compares the two modules THEMSELVES, field by
\ field through their frozen readers, and only then asks
\ src/compiler/ir/render.f to spell the rows it found a difference in. It never
\ renders the two modules and compares the text: that would be a text diff
\ wearing a semantic name, and it would answer the wrong question, as the
\ paragraph below shows.
\
\ WHY PAIRING BY CONTENT IS THE WHOLE DESIGN. A module's four interned tables -
\ symbols, types, attributes, sources - are SETS of content. Their canonical
\ ordinals are derived from that content, so adding one symbol whose bytes sort
\ near the front renumbers every symbol after it, and with it every type,
\ attribute and operation row that names one. A text diff of two renders would
\ therefore report dozens of changed lines for one added symbol. The truth is
\ that one symbol was added and nothing else changed, and that is what this file
\ reports, because it pairs rows of the interned tables BY CONTENT: a row of the
\ left module that no row of the right module matches is a removal, a row of the
\ right that no row of the left matches is an addition, and rows that match are
\ not mentioned. test/compiler/ir-diff.f measures exactly that: one added symbol
\ is one difference.
\
\ Functions, blocks, operations and values are the opposite case. Their order is
\ the program - block layout, instruction order, which operand is which - so they
\ are paired BY POSITION, and each field of a paired row is compared. Where such
\ a field is a reference into an interned table it is compared by the referent's
\ CONTENT, never by its ordinal, because an ordinal of the left module and an
\ ordinal of the right module are numbers in two different numberings and
\ comparing them would be meaningless.
\
\ WHAT COUNTS AS ONE COMPARISON. Every closed vocabulary is compared through the
\ equality its own declaration derives - IR-TYPE:kind, IR-FUN:linkage,
\ CNUM:overflow and the rest - so this file states no wire codes of its own and a
\ new member of any family cannot silently compare equal to another. Names are
\ compared as bytes through IR-SYM:FEQ?, structures recursively (a pointer by its
\ space and its pointee, a function type by its arity and its elements, a record
\ by its keyed entries, a source by its length, digest and origin), and a keyed
\ list by selecting its entries in canonical key order on each side, so the order
\ the keys were interned in is no part of the comparison.
\
\ WHY THE ENTRY SELECTION USES NO BUFFER. A record's value can be another record,
\ so comparing two records recurses. A pair of scratch arrays holding one record's
\ loaded entries would be overwritten by that recursion, so the j-th entry in
\ canonical key order is SELECTED from the row each time it is needed: keys within
\ one keyed list are unique, so the entry whose canonical key ordinal has exactly
\ j smaller siblings is well defined. That is quadratic in the entries of one
\ list, which PAIR-MAX bounds, and it cannot be clobbered by a nested comparison.
\ The two name buffers are only ever filled and consumed inside one leaf
\ comparison, where nothing nested can be in flight.
\
\ WHAT THE ANSWER IS. DIFF answers two numbers: the length of the report it wrote
\ and the NUMBER OF DIFFERENCES it found. The count is the equality predicate -
\ zero differences means the two modules mean the same thing - so no caller ever
\ has to read the report back to learn the answer, and the report stays what
\ section 6.6 says rendered text is: something for a human. The report is written
\ into a span the caller owns, a span too short is E-IR-DIFF-ROOM by name, and
\ this package holds no registry and creates no arena.
\
\ THE COMMITTED WORKING SET. Two inverse canonical maps, two name buffers and one
\ spelling scratch, with the row ceilings IR-BUILD's production plan commits to. A
\ module pair larger than that, a name longer than NAME-MAX, or a keyed list wider
\ than PAIR-MAX is refused with E-IR-DIFF-CAP before any report byte is written.
\ Two inverse maps are why this file builds its own rather than borrowing the
\ renderer's: the renderer holds the map of the one module it is spelling, and a
\ comparison needs both modules numbered at the same time.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/source.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/attr.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/verify.f
require src/compiler/ir/build.f
require src/compiler/ir/canon.f
require src/compiler/ir/render.f

package IR-DIFF
private

\ ---- the committed working set -----------------------------------------------
256 constant SYM-MAX
128 constant TY-MAX
128 constant AT-MAX
64 constant SRC-MAX
256 constant NAME-MAX
1024 constant TEXT-MAX
64 constant PAIR-MAX

\ ---- the two sides -----------------------------------------------------------
0 constant LEFT
1 constant RIGHT
2 constant SIDE#

\ ---- the four interned tables ------------------------------------------------
0 constant TB-SYM
1 constant TB-TY
2 constant TB-AT
3 constant TB-SRC

: TB-BASE ( n -- n )
   case
      TB-SYM of 0 endof
      TB-TY  of SYM-MAX endof
      TB-AT  of SYM-MAX TY-MAX + endof
      TB-SRC of SYM-MAX TY-MAX + AT-MAX + endof
      E-IR-DIFF-STATE throw
   endcase ;

SYM-MAX TY-MAX + AT-MAX + SRC-MAX + constant MAP-CELLS

-1 constant UNSET

\ ---- each module's frozen views ----------------------------------------------
0 constant V-SYP
1 constant V-SYR
2 constant V-TYP
3 constant V-TYR
4 constant V-ATP
5 constant V-ATR
6 constant V-SRC
7 constant V-SCR
8 constant V-FNP
9 constant V-FNR
10 constant V-BLR
11 constant V-OPP
12 constant V-VAL
13 constant V-OPR
14 constant V-EGP
15 constant V-EGR
16 constant VIEW#

\ ---- state -------------------------------------------------------------------
variable OUT-A
variable OUT-CAP
variable OUT-U
variable NUM-U
variable HITS

SIDE# VIEW# * TYPED-BUFFER MV IR-ARENA:view
SIDE# TYPED-BUFFER MK IR-ID:ir-module-key
SIDE# TYPED-BUFFER MM IR-BUILD:module
SIDE# TYPED-BUFFER CTB IR-CANON:table

create INV SIDE# MAP-CELLS * cells allot
create NA NAME-MAX allot              \ the left row's bytes
create NBB NAME-MAX allot             \ the right row's bytes
create TXB TEXT-MAX allot             \ one spelling from IR-RENDER
create NUMB 32 allot

\ ---- the report sink ---------------------------------------------------------
: OUT-P ( -- ptr ptr u8 )
   OUT-A 0 ptr-field ;

: SINK! ( ptr u8 n -- )
   {: p:ptr room:n :}
   p OUT-A !
   room OUT-CAP !
   0 OUT-U ! ;

: PUT-B ( n -- )
   {: b:n :}
   OUT-U @ OUT-CAP @ >= if E-IR-DIFF-ROOM throw then
   b OUT-P @ OUT-U @ + c!
   OUT-U @ 1+ OUT-U ! ;

: PUT-S ( ptr u8 n -- )
   {: p:ptr u:n :}
   u 0 ?do
      p i + c@ PUT-B
   loop ;

: PUT-NL ( -- )
   $0A PUT-B ;

: PUT-SP ( -- )
   $20 PUT-B ;

: DEC-BUILD ( n -- )
   {: v:n :}
   0 NUM-U !
   v 0= if
      $30 NUMB c!
      1 NUM-U !
      exit
   then
   v begin dup 0 > while
      dup 10 mod $30 + NUMB NUM-U @ + c!
      10 /
      NUM-U @ 1+ NUM-U !
   repeat drop ;

: PUT-U ( n -- )
   DEC-BUILD
   NUM-U @ 0 ?do
      NUMB NUM-U @ 1- i - + c@ PUT-B
   loop ;

\ ---- views, keys, modules and canonical tables -------------------------------
: V@ ( n n -- IR-ARENA:view )
   {: s:n idx:n :}
   s VIEW# * idx + MV @ ;

: V! ( IR-ARENA:view n n -- )
   {: v:IR-ARENA:view s:n idx:n :}
   v s VIEW# * idx + MV ! ;

: K@ ( n -- IR-ID:ir-module-key )
   MK @ ;

: M@ ( n -- IR-BUILD:module )
   MM @ ;

: T@ ( n -- IR-CANON:table )
   CTB @ ;

: SYR ( n -- IR-ARENA:view )
   V-SYR V@ ;

: SYP ( n -- IR-ARENA:view )
   V-SYP V@ ;

: TYR ( n -- IR-ARENA:view )
   V-TYR V@ ;

: TYPV ( n -- IR-ARENA:view )
   V-TYP V@ ;

: ATR ( n -- IR-ARENA:view )
   V-ATR V@ ;

: ATP ( n -- IR-ARENA:view )
   V-ATP V@ ;

: SRV ( n -- IR-ARENA:view )
   V-SRC V@ ;

: SCR ( n -- IR-ARENA:view )
   V-SCR V@ ;

: FNR ( n -- IR-ARENA:view )
   V-FNR V@ ;

: FNP ( n -- IR-ARENA:view )
   V-FNP V@ ;

: BLR ( n -- IR-ARENA:view )
   V-BLR V@ ;

: OPR ( n -- IR-ARENA:view )
   V-OPR V@ ;

: OPP ( n -- IR-ARENA:view )
   V-OPP V@ ;

: VLR ( n -- IR-ARENA:view )
   V-VAL V@ ;

: EGR ( n -- IR-ARENA:view )
   V-EGR V@ ;

: EGP ( n -- IR-ARENA:view )
   V-EGP V@ ;

: TAKE-VIEWS ( IR-BUILD:module IR-CANON:table n -- )
   {: m:IR-BUILD:module t:IR-CANON:table s:n :}
   m IR-BUILD:FROZEN? 0= if E-IR-DIFF-STALE throw then
   m s MM !
   t s CTB !
   m IR-BUILD:FKEY s MK !
   m IR-BUILD:FSYM-POOL s V-SYP V!
   m IR-BUILD:FSYM-ROWS s V-SYR V!
   m IR-BUILD:FTYPE-POOL s V-TYP V!
   m IR-BUILD:FTYPE-ROWS s V-TYR V!
   m IR-BUILD:FATTR-POOL s V-ATP V!
   m IR-BUILD:FATTR-ROWS s V-ATR V!
   m IR-BUILD:FSOURCES s V-SRC V!
   m IR-BUILD:FSCHEMA-ROWS s V-SCR V!
   m IR-BUILD:FFUN-POOL s V-FNP V!
   m IR-BUILD:FFUN-ROWS s V-FNR V!
   m IR-BUILD:FBLOCK-ROWS s V-BLR V!
   m IR-BUILD:FOP-POOL s V-OPP V!
   m IR-BUILD:FVALUE-ROWS s V-VAL V!
   m IR-BUILD:FOP-ROWS s V-OPR V!
   m IR-BUILD:FEDGE-POOL s V-EGP V!
   m IR-BUILD:FEDGE-ROWS s V-EGR V! ;

\ ---- identities from one side's rows -----------------------------------------
: SYM-ID ( n n -- IR-ID:ir-symbol-id )
   {: s:n l:n :}
   s K@ l IR-ID:PACK-SYMBOL ;

: TY-ID ( n n -- IR-ID:ir-type-id )
   {: s:n l:n :}
   s K@ l IR-ID:PACK-TYPE ;

: AT-ID ( n n -- IR-ID:ir-attr-id )
   {: s:n l:n :}
   s K@ l IR-ID:PACK-ATTR ;

: SRC-ID ( n n -- IR-ID:ir-source-id )
   {: s:n l:n :}
   s K@ l IR-ID:PACK-SOURCE ;

: FUN-ID ( n n -- IR-ID:ir-fun-id )
   {: s:n l:n :}
   s K@ l IR-ID:PACK-FUN ;

: BLK-ID ( n n -- IR-ID:ir-block-id )
   {: s:n l:n :}
   s K@ l IR-ID:PACK-BLOCK ;

: OP-ID ( n n -- IR-ID:ir-op-id )
   {: s:n l:n :}
   s K@ l IR-ID:PACK-OP ;

: VAL-ID ( n n -- IR-ID:ir-value-id )
   {: s:n l:n :}
   s K@ l IR-ID:PACK-VALUE ;

\ ---- how many rows one side holds --------------------------------------------
: SYMS ( n -- n )
   SYR IR-SYM:FSYMBOLS ;

: TYS ( n -- n )
   TYR IR-TYPE:FTYPES ;

: ATS ( n -- n )
   ATR IR-ATTR:FATTRS ;

: SRCS ( n -- n )
   SRV IR-SOURCE:FSOURCES ;

: FUNS ( n -- n )
   FNR IR-FUN:FFUNS ;

: BLKS ( n -- n )
   BLR IR-FUN:FBLOCKS ;

: OPS ( n -- n )
   OPR IR-OP:FOPS ;

: VALS ( n -- n )
   VLR IR-OP:FVALUES ;

: EDGE-BLKS ( n -- n )
   EGR IR-VERIFY:FEDGE-BLOCKS ;

\ ---- canonical ordinals and their inverse ------------------------------------
: SYM-ORD ( n n -- n )
   {: s:n l:n :}
   s T@ s l SYM-ID IR-CANON:SYMBOL-ORD ;

: TY-ORD ( n n -- n )
   {: s:n l:n :}
   s T@ s l TY-ID IR-CANON:TYPE-ORD ;

: AT-ORD ( n n -- n )
   {: s:n l:n :}
   s T@ s l AT-ID IR-CANON:ATTR-ORD ;

: SRC-ORD ( n n -- n )
   {: s:n l:n :}
   s T@ s l SRC-ID IR-CANON:SOURCE-ORD ;

: INV@ ( n n n -- n )
   {: s:n tb:n c:n :}
   s MAP-CELLS * tb TB-BASE + c + cells INV + @ ;

: INV! ( n n n n -- )
   {: s:n tb:n c:n v:n :}
   v s MAP-CELLS * tb TB-BASE + c + cells INV + ! ;

: INV-CLEAR ( -- )
   SIDE# MAP-CELLS * 0 ?do
      UNSET i cells INV + !
   loop ;

: INV-KEEP ( n n n n -- )
   {: s:n tb:n c:n l:n :}
   s tb c INV@ UNSET = if s tb c l INV! then ;

: INV-SIDE ( n -- )
   {: s:n :}
   s SYMS 0 ?do
      s TB-SYM  s i SYM-ORD  i INV-KEEP
   loop
   s TYS 0 ?do
      s TB-TY  s i TY-ORD  i INV-KEEP
   loop
   s ATS 0 ?do
      s TB-AT  s i AT-ORD  i INV-KEEP
   loop
   s SRCS 0 ?do
      s TB-SRC  s i SRC-ORD  i INV-KEEP
   loop ;

: INV-FILL ( -- )
   INV-CLEAR
   LEFT INV-SIDE
   RIGHT INV-SIDE ;

\ ---- capacity ----------------------------------------------------------------
: NAME-CK ( n -- n )
   dup NAME-MAX > if E-IR-DIFF-CAP throw then ;

: PAIR-CK ( n -- n )
   dup PAIR-MAX > if E-IR-DIFF-CAP throw then ;

: ROWS-CK ( n -- )
   {: s:n :}
   s SYMS SYM-MAX > if E-IR-DIFF-CAP throw then
   s TYS TY-MAX > if E-IR-DIFF-CAP throw then
   s ATS AT-MAX > if E-IR-DIFF-CAP throw then
   s SRCS SRC-MAX > if E-IR-DIFF-CAP throw then ;

: FITS-CK ( -- )
   LEFT ROWS-CK
   RIGHT ROWS-CK ;

\ ---- names -------------------------------------------------------------------
: NAME>NA ( n n -- n )
   {: s:n l:n :}
   s SYR s l SYM-ID IR-SYM:FLEN@ NAME-CK drop
   s SYP s SYR s l SYM-ID NA NAME-MAX IR-SYM:FCOPY ;

\ One name compared as bytes. The left row's bytes are copied out and the right
\ row is asked whether it holds them, which is IR-SYM's own comparison.
: NAME-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT a NAME>NA {: u:n :}
   RIGHT SYP RIGHT SYR RIGHT b SYM-ID NA u IR-SYM:FEQ? ;

: BYTES-EQ? ( n n -- bool )
   {: au:n bu:n :}
   au bu <> if false exit then
   au 0 ?do
      NA i + c@ NBB i + c@ <> if false unloop exit then
   loop
   true ;

\ ---- type structure ----------------------------------------------------------
: TY-KIND ( n n -- IR-TYPE:kind )
   {: s:n l:n :}
   s TYR s l TY-ID IR-TYPE:FKIND@ ;

: FN-KIND? ( IR-TYPE:kind -- bool )
   {: k:IR-TYPE:kind :}
   k IR--TYPE-KIND:QUOTATION IR--TYPE-KIND:EQ
   k IR--TYPE-KIND:CODE-REF IR--TYPE-KIND:EQ or ;

: TY-PARAMS ( n n -- n )
   {: s:n l:n :}
   s TYR s l TY-ID IR-TYPE:FARITY@ drop ;

: TY-RESULTS ( n n -- n )
   {: s:n l:n :}
   s TYR s l TY-ID IR-TYPE:FARITY@ nip ;

\ One element of a function type's list, parameters first then results, which is
\ the order IR-TYPE stores them in.
: TY-ELEM ( n n n -- n )
   {: s:n l:n j:n :}
   s l TY-PARAMS {: pn:n :}
   j pn < if
      s TYPV s TYR s K@ s l TY-ID j IR-TYPE:FPARAM@ IR-ID:TYPE-LOCAL exit
   then
   s TYPV s TYR s K@ s l TY-ID j pn - IR-TYPE:FRESULT@ IR-ID:TYPE-LOCAL ;

: INT-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT TYR LEFT a TY-ID IR-TYPE:FINT@ {: aw:IR-TYPE:width as:IR-TYPE:sign :}
   RIGHT TYR RIGHT b TY-ID IR-TYPE:FINT@ {: bw:IR-TYPE:width bs:IR-TYPE:sign :}
   aw bw IR--TYPE-WIDTH:EQ
   as bs IR--TYPE-SIGN:EQ and ;

: FLT-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT TYR LEFT a TY-ID IR-TYPE:FFLT@
   RIGHT TYR RIGHT b TY-ID IR-TYPE:FFLT@
   IR--TYPE-FMT:EQ ;

: TOK-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT TYR LEFT a TY-ID IR-TYPE:FTOKEN@
   RIGHT TYR RIGHT b TY-ID IR-TYPE:FTOKEN@
   IR--TYPE-DOMAIN:EQ ;

: PTR-SPACE-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT TYR LEFT K@ LEFT a TY-ID IR-TYPE:FPOINTER@ drop
   RIGHT TYR RIGHT K@ RIGHT b TY-ID IR-TYPE:FPOINTER@ drop
   IR--TYPE-SPACE:EQ ;

: PTEE ( n n -- n )
   {: s:n l:n :}
   s TYR s K@ s l TY-ID IR-TYPE:FPOINTER@ nip IR-ID:TYPE-LOCAL ;

\ Two type rows of two modules denote the same type. The pointer and function
\ arms recurse on stored references; every reference passes IR-TYPE's strict
\ decrease, so the recursion is bounded by the ordinal on either side.
: TY-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT a TY-KIND {: ka:IR-TYPE:kind :}
   RIGHT b TY-KIND {: kb:IR-TYPE:kind :}
   ka kb IR--TYPE-KIND:EQ 0= if false exit then
   ka IR--TYPE-KIND:INT IR--TYPE-KIND:EQ if a b INT-EQ? exit then
   ka IR--TYPE-KIND:FLOAT IR--TYPE-KIND:EQ if a b FLT-EQ? exit then
   ka IR--TYPE-KIND:MEMORY-TOKEN IR--TYPE-KIND:EQ if a b TOK-EQ? exit then
   ka IR--TYPE-KIND:POINTER IR--TYPE-KIND:EQ if
      a b PTR-SPACE-EQ? 0= if false exit then
      LEFT a PTEE  RIGHT b PTEE  recurse
      exit
   then
   ka FN-KIND? 0= if true exit then
   LEFT a TY-PARAMS RIGHT b TY-PARAMS <> if false exit then
   LEFT a TY-RESULTS RIGHT b TY-RESULTS <> if false exit then
   LEFT a TY-PARAMS LEFT a TY-RESULTS + 0 ?do
      LEFT a i TY-ELEM  RIGHT b i TY-ELEM  recurse 0= if false unloop exit then
   loop
   true ;

\ ---- attribute structure -----------------------------------------------------
: AT-KIND ( n n -- IR-ATTR:kind )
   {: s:n l:n :}
   s ATR s l AT-ID IR-ATTR:FKIND@ ;

: AT-EFAM ( n n -- IR-ATTR:efam )
   {: s:n l:n :}
   s ATR s l AT-ID IR-ATTR:FEFAM@ ;

: OVF-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FOVERFLOW@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FOVERFLOW@
   CNUM-OVERFLOW:EQ ;

: FLO-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FFLOAT-MODEL@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FFLOAT-MODEL@
   CNUM-FLOAT--MODEL:EQ ;

: CON-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FCONTRACTION@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FCONTRACTION@
   CNUM-CONTRACTION:EQ ;

: FAS-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FFAST-MATH@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FFAST-MATH@
   CNUM-FAST--MATH:EQ ;

: CMP-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FCOMPARE@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FCOMPARE@
   CNUM-COMPARE:EQ ;

: ARCH-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FARCH@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FARCH@
   CTARGET-ARCH:EQ ;

: ABI-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FABI@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FABI@
   CTARGET-ABI:EQ ;

: END-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FENDIAN@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FENDIAN@
   CTARGET-ENDIAN:EQ ;

: PTRW-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FPTR-WIDTH@
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FPTR-WIDTH@
   CTARGET-PTR--WIDTH:EQ ;

\ The member of an enum attribute, under the family both rows record. Each arm
\ reads through the family's own typed reader and compares through the equality
\ that family derives, so a member outside its family is IR-ATTR's named refusal
\ rather than a raw cell here.
: ENUM-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT a AT-EFAM {: fa:IR-ATTR:efam :}
   RIGHT b AT-EFAM {: fb:IR-ATTR:efam :}
   fa fb IR--ATTR-EFAM:EQ 0= if false exit then
   fa MATCH IR-ATTR:efam
      overflow    OF a b OVF-EQ? ENDOF
      float-model OF a b FLO-EQ? ENDOF
      contraction OF a b CON-EQ? ENDOF
      fast-math   OF a b FAS-EQ? ENDOF
      compare     OF a b CMP-EQ? ENDOF
      arch        OF a b ARCH-EQ? ENDOF
      abi         OF a b ABI-EQ? ENDOF
      endian      OF a b END-EQ? ENDOF
      ptr-width   OF a b PTRW-EQ? ENDOF
   ;MATCH ;

\ A boolean attribute's stored truth as a cell, so the two sides compare through
\ one numeric equality rather than a branch per side.
: BOOL-CELL ( n n -- n )
   {: s:n l:n :}
   s ATR s l AT-ID IR-ATTR:FBOOLEAN@ if 1 else 0 then ;

: TEXT-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FTEXT-LEN@ NAME-CK drop
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FTEXT-LEN@ NAME-CK drop
   LEFT ATP LEFT ATR LEFT a AT-ID NA NAME-MAX IR-ATTR:FTEXT-COPY {: au:n :}
   RIGHT ATP RIGHT ATR RIGHT b AT-ID NBB NAME-MAX IR-ATTR:FTEXT-COPY {: bu:n :}
   au bu BYTES-EQ? ;

: DIG-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FDIGEST@ CDIGEST-DIGEST:UNMAKE
   {: a0:n a1:n a2:n a3:n :}
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FDIGEST@ CDIGEST-DIGEST:UNMAKE
   {: b0:n b1:n b2:n b3:n :}
   a0 b0 = a1 b1 = and a2 b2 = and a3 b3 = and ;

: ILIST-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT a AT-ID IR-ATTR:FITEMS@ {: an:n :}
   RIGHT ATR RIGHT b AT-ID IR-ATTR:FITEMS@ {: bn:n :}
   an bn <> if false exit then
   an 0 ?do
      LEFT ATP LEFT ATR LEFT a AT-ID i IR-ATTR:FITEM@
      RIGHT ATP RIGHT ATR RIGHT b AT-ID i IR-ATTR:FITEM@
      <> if false unloop exit then
   loop
   true ;

: SYMREF-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT K@ LEFT a AT-ID IR-ATTR:FSYM@ IR-ID:SYMBOL-LOCAL
   RIGHT ATR RIGHT K@ RIGHT b AT-ID IR-ATTR:FSYM@ IR-ID:SYMBOL-LOCAL
   NAME-EQ? ;

: TYPEREF-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT ATR LEFT K@ LEFT a AT-ID IR-ATTR:FTYPE@ IR-ID:TYPE-LOCAL
   RIGHT ATR RIGHT K@ RIGHT b AT-ID IR-ATTR:FTYPE@ IR-ID:TYPE-LOCAL
   TY-EQ? ;

\ ---- a record's keyed entries, selected in canonical key order ---------------
: REC-PAIRS ( n n -- n )
   {: s:n l:n :}
   s ATR s l AT-ID IR-ATTR:FPAIRS@ PAIR-CK ;

: REC-KEY ( n n n -- n )
   {: s:n l:n j:n :}
   s ATP s ATR s K@ s l AT-ID j IR-ATTR:FKEY@ IR-ID:SYMBOL-LOCAL ;

: REC-VAL ( n n n -- n )
   {: s:n l:n j:n :}
   s ATP s ATR s K@ s l AT-ID j IR-ATTR:FVAL@ IR-ID:ATTR-LOCAL ;

: REC-KEY-ORD ( n n n -- n )
   {: s:n l:n j:n :}
   s  s l j REC-KEY  SYM-ORD ;

: REC-RANK ( n n n -- n )
   {: s:n l:n c:n :}
   0
   s l REC-PAIRS 0 ?do
      s l i REC-KEY-ORD c < if 1+ then
   loop ;

\ Keys within one record are unique, so the entry whose canonical key ordinal has
\ exactly j smaller siblings is the j-th entry in canonical key order.
: REC-NTH ( n n n -- n n )
   {: s:n l:n j:n :}
   s l REC-PAIRS 0 ?do
      s l  s l i REC-KEY-ORD  REC-RANK j = if
         s l i REC-KEY  s l i REC-VAL  unloop exit
      then
   loop
   E-IR-DIFF-STATE throw ;

\ ---- two attribute rows denote the same attribute ----------------------------
: AT-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT a AT-KIND {: ka:IR-ATTR:kind :}
   RIGHT b AT-KIND {: kb:IR-ATTR:kind :}
   ka kb IR--ATTR-KIND:EQ 0= if false exit then
   ka IR--ATTR-KIND:INT IR--ATTR-KIND:EQ if
      LEFT ATR LEFT a AT-ID IR-ATTR:FINT@
      RIGHT ATR RIGHT b AT-ID IR-ATTR:FINT@ = exit
   then
   ka IR--ATTR-KIND:BOOLEAN IR--ATTR-KIND:EQ if
      LEFT a BOOL-CELL  RIGHT b BOOL-CELL  = exit
   then
   ka IR--ATTR-KIND:TEXT IR--ATTR-KIND:EQ if a b TEXT-EQ? exit then
   ka IR--ATTR-KIND:SYM IR--ATTR-KIND:EQ if a b SYMREF-EQ? exit then
   ka IR--ATTR-KIND:TYPE-REF IR--ATTR-KIND:EQ if a b TYPEREF-EQ? exit then
   ka IR--ATTR-KIND:INT-LIST IR--ATTR-KIND:EQ if a b ILIST-EQ? exit then
   ka IR--ATTR-KIND:ENUM-VAL IR--ATTR-KIND:EQ if a b ENUM-EQ? exit then
   ka IR--ATTR-KIND:DIGEST IR--ATTR-KIND:EQ if a b DIG-EQ? exit then
   LEFT a REC-PAIRS RIGHT b REC-PAIRS <> if false exit then
   LEFT a REC-PAIRS 0 ?do
      LEFT a i REC-NTH {: ak:n av:n :}
      RIGHT b i REC-NTH {: bk:n bv:n :}
      ak bk NAME-EQ? 0= if false unloop exit then
      av bv recurse 0= if false unloop exit then
   loop
   true ;

\ ---- source structure --------------------------------------------------------
: SRC-LEN ( n n -- n )
   {: s:n l:n :}
   s SRV s l SRC-ID IR-SOURCE:FLEN@ ;

: SRC-ROOT? ( n n -- bool )
   {: s:n l:n :}
   s SRV s l SRC-ID IR-SOURCE:FROOT? ;

\ A root flag as a cell, so the two sides compare through one numeric equality.
: SRC-ROOT-CELL ( n n -- n )
   {: s:n l:n :}
   s l SRC-ROOT? if 1 else 0 then ;

: SRC-PARENT ( n n -- n )
   {: s:n l:n :}
   s SRV s K@ s l SRC-ID IR-SOURCE:FORIGIN@ IR-ID:SOURCE-LOCAL ;

: SRC-DIG-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT SRV LEFT a SRC-ID IR-SOURCE:FDIGEST@ CDIGEST-DIGEST:UNMAKE
   {: a0:n a1:n a2:n a3:n :}
   RIGHT SRV RIGHT b SRC-ID IR-SOURCE:FDIGEST@ CDIGEST-DIGEST:UNMAKE
   {: b0:n b1:n b2:n b3:n :}
   a0 b0 = a1 b1 = and a2 b2 = and a3 b3 = and ;

\ Two source rows are the same source: the same bytes, and the same place they
\ were derived from. The origin arm recurses, bounded by IR-SOURCE's own strict
\ decrease of the parent ordinal.
: SRC-EQ? ( n n -- bool )
   {: a:n b:n :}
   LEFT a SRC-LEN RIGHT b SRC-LEN <> if false exit then
   a b SRC-DIG-EQ? 0= if false exit then
   LEFT a SRC-ROOT-CELL RIGHT b SRC-ROOT-CELL <> if false exit then
   LEFT a SRC-ROOT? if true exit then
   LEFT a SRC-PARENT  RIGHT b SRC-PARENT  recurse ;

\ ---- finding a left row in the right module, and back ------------------------
\ The interned tables are sets of content, so a row is looked for by what it
\ holds. The comparators take a left row and a right row, so searching the left
\ table for a right row swaps the roles by searching for the left row that
\ matches; both directions are one scan of at most SYM-MAX rows.
: SYM-IN-RIGHT? ( n -- bool )
   {: a:n :}
   RIGHT SYMS 0 ?do
      a i NAME-EQ? if true unloop exit then
   loop
   false ;

: SYM-IN-LEFT? ( n -- bool )
   {: b:n :}
   LEFT SYMS 0 ?do
      i b NAME-EQ? if true unloop exit then
   loop
   false ;

: TY-IN-RIGHT? ( n -- bool )
   {: a:n :}
   RIGHT TYS 0 ?do
      a i TY-EQ? if true unloop exit then
   loop
   false ;

: TY-IN-LEFT? ( n -- bool )
   {: b:n :}
   LEFT TYS 0 ?do
      i b TY-EQ? if true unloop exit then
   loop
   false ;

: AT-IN-RIGHT? ( n -- bool )
   {: a:n :}
   RIGHT ATS 0 ?do
      a i AT-EQ? if true unloop exit then
   loop
   false ;

: AT-IN-LEFT? ( n -- bool )
   {: b:n :}
   LEFT ATS 0 ?do
      i b AT-EQ? if true unloop exit then
   loop
   false ;

: SRC-IN-RIGHT? ( n -- bool )
   {: a:n :}
   RIGHT SRCS 0 ?do
      a i SRC-EQ? if true unloop exit then
   loop
   false ;

: SRC-IN-LEFT? ( n -- bool )
   {: b:n :}
   LEFT SRCS 0 ?do
      i b SRC-EQ? if true unloop exit then
   loop
   false ;

\ ---- report primitives -------------------------------------------------------
: HIT ( -- )
   HITS @ 1+ HITS ! ;

: PUT-ONLY ( n -- )
   {: s:n :}
   s LEFT = if s" - " PUT-S exit then
   s" + " PUT-S ;

\ Every spelling in the report comes from IR-RENDER, so this file states no
\ spelling of its own.
: PUT-NAME ( n n -- )
   {: s:n l:n :}
   s M@ s l SYM-ID TXB TEXT-MAX IR-RENDER:SYMBOL-TEXT {: u:n :}
   TXB u PUT-S ;

: PUT-TY ( n n -- )
   {: s:n l:n :}
   s M@ s l TY-ID TXB TEXT-MAX IR-RENDER:TYPE-TEXT {: u:n :}
   TXB u PUT-S ;

: PUT-AT ( n n -- )
   {: s:n l:n :}
   s M@ s T@ s l AT-ID TXB TEXT-MAX IR-RENDER:ATTR-TEXT {: u:n :}
   TXB u PUT-S ;

: PUT-SRC ( n n -- )
   {: s:n l:n :}
   $63 PUT-B
   s l SRC-ORD PUT-U
   s"  len " PUT-S
   s l SRC-LEN PUT-U ;

\ ---- the interned tables -----------------------------------------------------
: SYM-LINE ( n n -- )
   {: s:n c:n :}
   HIT
   s" symbol " PUT-S
   s PUT-ONLY
   $73 PUT-B c PUT-U
   PUT-SP
   s  s TB-SYM c INV@  PUT-NAME
   PUT-NL ;

: CK-SYMS ( -- )
   LEFT T@ IR-CANON:SYMBOLS 0 ?do
      LEFT TB-SYM i INV@ SYM-IN-RIGHT? 0= if LEFT i SYM-LINE then
   loop
   RIGHT T@ IR-CANON:SYMBOLS 0 ?do
      RIGHT TB-SYM i INV@ SYM-IN-LEFT? 0= if RIGHT i SYM-LINE then
   loop ;

: TY-LINE ( n n -- )
   {: s:n c:n :}
   HIT
   s" type " PUT-S
   s PUT-ONLY
   $74 PUT-B c PUT-U
   PUT-SP
   s  s TB-TY c INV@  PUT-TY
   PUT-NL ;

: CK-TYPES ( -- )
   LEFT T@ IR-CANON:TYPES 0 ?do
      LEFT TB-TY i INV@ TY-IN-RIGHT? 0= if LEFT i TY-LINE then
   loop
   RIGHT T@ IR-CANON:TYPES 0 ?do
      RIGHT TB-TY i INV@ TY-IN-LEFT? 0= if RIGHT i TY-LINE then
   loop ;

: AT-LINE ( n n -- )
   {: s:n c:n :}
   HIT
   s" attr " PUT-S
   s PUT-ONLY
   $61 PUT-B c PUT-U
   PUT-SP
   s  s TB-AT c INV@  PUT-AT
   PUT-NL ;

: CK-ATTRS ( -- )
   LEFT T@ IR-CANON:ATTRS 0 ?do
      LEFT TB-AT i INV@ AT-IN-RIGHT? 0= if LEFT i AT-LINE then
   loop
   RIGHT T@ IR-CANON:ATTRS 0 ?do
      RIGHT TB-AT i INV@ AT-IN-LEFT? 0= if RIGHT i AT-LINE then
   loop ;

: SRC-LINE ( n n -- )
   {: s:n c:n :}
   HIT
   s" source " PUT-S
   s PUT-ONLY
   s  s TB-SRC c INV@  PUT-SRC
   PUT-NL ;

: CK-SRCS ( -- )
   LEFT T@ IR-CANON:SOURCES 0 ?do
      LEFT TB-SRC i INV@ SRC-IN-RIGHT? 0= if LEFT i SRC-LINE then
   loop
   RIGHT T@ IR-CANON:SOURCES 0 ?do
      RIGHT TB-SRC i INV@ SRC-IN-LEFT? 0= if RIGHT i SRC-LINE then
   loop ;

\ ---- the header --------------------------------------------------------------
: DIALECT-SYM ( n -- n )
   {: s:n :}
   s SCR s K@ IR-SCHEMA:FDIALECT@ IR-ID:SYMBOL-LOCAL ;

: FIELD ( ptr u8 n -- )
   {: p:ptr u:n :}
   HIT
   p u PUT-S ;

: PUT-NUMS ( n n -- )
   {: a:n b:n :}
   s"  - " PUT-S
   a PUT-U
   s"  + " PUT-S
   b PUT-U
   PUT-NL ;

: CK-DIALECT ( -- )
   LEFT DIALECT-SYM RIGHT DIALECT-SYM NAME-EQ? if exit then
   s" header dialect" FIELD
   s"  - " PUT-S
   LEFT LEFT DIALECT-SYM PUT-NAME
   s"  + " PUT-S
   RIGHT RIGHT DIALECT-SYM PUT-NAME
   PUT-NL ;

: CK-HEAD ( -- )
   CK-DIALECT
   LEFT SCR IR-SCHEMA:FMAJOR@ {: amaj:n :}
   RIGHT SCR IR-SCHEMA:FMAJOR@ {: bmaj:n :}
   amaj bmaj <> if
      s" header schema-major" FIELD
      amaj bmaj PUT-NUMS
   then
   LEFT SCR IR-SCHEMA:FMINOR@ {: amin:n :}
   RIGHT SCR IR-SCHEMA:FMINOR@ {: bmin:n :}
   amin bmin <> if
      s" header schema-minor" FIELD
      amin bmin PUT-NUMS
   then ;

\ ---- row labels for the program tables ---------------------------------------
: ROW-LABEL ( ptr u8 n n n -- )
   {: p:ptr u:n letter:n l:n :}
   HIT
   p u PUT-S
   PUT-SP
   letter PUT-B
   l PUT-U
   PUT-SP ;

\ The tail of a line whose two sides are counts. Counting the difference belongs
\ to whatever OPENED the line - ROW-LABEL for a program row, FIELD for a header
\ or table-count line - so exactly one difference is counted per line written.
: COUNT-LINE ( ptr u8 n n n -- )
   {: p:ptr u:n a:n b:n :}
   p u PUT-S
   a b PUT-NUMS ;

\ ---- functions ---------------------------------------------------------------
: FN-NAME ( n n -- n )
   {: s:n l:n :}
   s FNR s K@ s l FUN-ID IR-FUN:FSYMBOL@ IR-ID:SYMBOL-LOCAL ;

: FN-SIG ( n n -- n )
   {: s:n l:n :}
   s FNR s K@ s l FUN-ID IR-FUN:FSIGNATURE@ IR-ID:TYPE-LOCAL ;

: FN-ATTR ( n n n -- n )
   {: s:n l:n j:n :}
   s FNP s FNR s K@ s l FUN-ID j IR-FUN:FATTR@ IR-ID:ATTR-LOCAL ;

: CK-FN-NAME ( n -- )
   {: l:n :}
   LEFT l FN-NAME RIGHT l FN-NAME NAME-EQ? if exit then
   s" fun" $66 l ROW-LABEL
   s" name - " PUT-S
   LEFT LEFT l FN-NAME PUT-NAME
   s"  + " PUT-S
   RIGHT RIGHT l FN-NAME PUT-NAME
   PUT-NL ;

: CK-FN-SIG ( n -- )
   {: l:n :}
   LEFT l FN-SIG RIGHT l FN-SIG TY-EQ? if exit then
   s" fun" $66 l ROW-LABEL
   s" sig - " PUT-S
   LEFT LEFT l FN-SIG PUT-TY
   s"  + " PUT-S
   RIGHT RIGHT l FN-SIG PUT-TY
   PUT-NL ;

: CK-FN-LINKAGE ( n -- )
   {: l:n :}
   LEFT FNR LEFT l FUN-ID IR-FUN:FLINKAGE@ {: a:IR-FUN:linkage :}
   RIGHT FNR RIGHT l FUN-ID IR-FUN:FLINKAGE@ {: b:IR-FUN:linkage :}
   a b IR--FUN-LINKAGE:EQ if exit then
   s" fun" $66 l ROW-LABEL
   s" linkage differs" PUT-S
   PUT-NL ;

: CK-FN-VIS ( n -- )
   {: l:n :}
   LEFT FNR LEFT l FUN-ID IR-FUN:FVISIBILITY@ {: a:IR-FUN:visibility :}
   RIGHT FNR RIGHT l FUN-ID IR-FUN:FVISIBILITY@ {: b:IR-FUN:visibility :}
   a b IR--FUN-VISIBILITY:EQ if exit then
   s" fun" $66 l ROW-LABEL
   s" visibility differs" PUT-S
   PUT-NL ;

: CK-FN-CC ( n -- )
   {: l:n :}
   LEFT FNR LEFT l FUN-ID IR-FUN:FCONVENTION@ {: a:IR-FUN:convention :}
   RIGHT FNR RIGHT l FUN-ID IR-FUN:FCONVENTION@ {: b:IR-FUN:convention :}
   a b IR--FUN-CONVENTION:EQ if exit then
   s" fun" $66 l ROW-LABEL
   s" convention differs" PUT-S
   PUT-NL ;

: CK-FN-ATTR ( n n -- )
   {: l:n j:n :}
   LEFT l j FN-ATTR RIGHT l j FN-ATTR AT-EQ? if exit then
   s" fun" $66 l ROW-LABEL
   s" attr " PUT-S
   j PUT-U
   s"  - " PUT-S
   LEFT LEFT l j FN-ATTR PUT-AT
   s"  + " PUT-S
   RIGHT RIGHT l j FN-ATTR PUT-AT
   PUT-NL ;

: CK-FN-ATTRS ( n -- )
   {: l:n :}
   LEFT FNR LEFT l FUN-ID IR-FUN:FATTR-COUNT {: an:n :}
   RIGHT FNR RIGHT l FUN-ID IR-FUN:FATTR-COUNT {: bn:n :}
   an bn <> if
      s" fun" $66 l ROW-LABEL
      s" attrs" an bn COUNT-LINE
      exit
   then
   an 0 ?do
      l i CK-FN-ATTR
   loop ;

: CK-FN-SPAN ( n -- )
   {: l:n :}
   LEFT FNR LEFT K@ LEFT l FUN-ID IR-FUN:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: asid:IR-ID:ir-source-id ast:n aln:n :}
   RIGHT FNR RIGHT K@ RIGHT l FUN-ID IR-FUN:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: bsid:IR-ID:ir-source-id bst:n bln:n :}
   asid IR-ID:SOURCE-LOCAL bsid IR-ID:SOURCE-LOCAL SRC-EQ?
   ast bst = and aln bln = and if exit then
   s" fun" $66 l ROW-LABEL
   s" span differs" PUT-S
   PUT-NL ;

: CK-FN-BLOCKS ( n -- )
   {: l:n :}
   LEFT FNR LEFT l FUN-ID IR-FUN:FBLOCK-COUNT {: an:n :}
   RIGHT FNR RIGHT l FUN-ID IR-FUN:FBLOCK-COUNT {: bn:n :}
   an bn = if exit then
   s" fun" $66 l ROW-LABEL
   s" blocks" an bn COUNT-LINE ;

: CK-FN ( n -- )
   {: l:n :}
   l CK-FN-NAME
   l CK-FN-SIG
   l CK-FN-LINKAGE
   l CK-FN-VIS
   l CK-FN-CC
   l CK-FN-ATTRS
   l CK-FN-BLOCKS
   l CK-FN-SPAN ;

: CK-FUNS ( -- )
   LEFT FUNS RIGHT FUNS <> if
      s" functions" FIELD
      LEFT FUNS RIGHT FUNS PUT-NUMS
   then
   LEFT FUNS RIGHT FUNS min 0 ?do
      i CK-FN
   loop ;

\ ---- blocks ------------------------------------------------------------------
: CK-BLK-PARENT ( n -- )
   {: l:n :}
   LEFT BLR LEFT FNR LEFT K@ LEFT l BLK-ID IR-FUN:FPARENT@ IR-ID:FUN-LOCAL
   {: a:n :}
   RIGHT BLR RIGHT FNR RIGHT K@ RIGHT l BLK-ID IR-FUN:FPARENT@ IR-ID:FUN-LOCAL
   {: b:n :}
   a b = if exit then
   s" block" $62 l ROW-LABEL
   s" in" a b COUNT-LINE ;

: CK-BLK-ARG ( n n -- )
   {: l:n j:n :}
   LEFT BLR LEFT VLR LEFT K@ LEFT l BLK-ID j IR-FUN:FARG@ IR-ID:VALUE-LOCAL
   {: a:n :}
   RIGHT BLR RIGHT VLR RIGHT K@ RIGHT l BLK-ID j IR-FUN:FARG@ IR-ID:VALUE-LOCAL
   {: b:n :}
   a b = if exit then
   s" block" $62 l ROW-LABEL
   s" arg " PUT-S
   j PUT-U
   a b PUT-NUMS ;

: CK-BLK-ARGS ( n -- )
   {: l:n :}
   LEFT BLR LEFT l BLK-ID IR-FUN:FARG-COUNT {: an:n :}
   RIGHT BLR RIGHT l BLK-ID IR-FUN:FARG-COUNT {: bn:n :}
   an bn <> if
      s" block" $62 l ROW-LABEL
      s" args" an bn COUNT-LINE
      exit
   then
   an 0 ?do
      l i CK-BLK-ARG
   loop ;

: CK-BLK-OPS ( n -- )
   {: l:n :}
   LEFT BLR LEFT l BLK-ID IR-FUN:FOP-COUNT {: an:n :}
   RIGHT BLR RIGHT l BLK-ID IR-FUN:FOP-COUNT {: bn:n :}
   an bn = if exit then
   s" block" $62 l ROW-LABEL
   s" ops" an bn COUNT-LINE ;

: CK-BLK-SPAN ( n -- )
   {: l:n :}
   LEFT BLR LEFT K@ LEFT l BLK-ID IR-FUN:FBLOCK-SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: asid:IR-ID:ir-source-id ast:n aln:n :}
   RIGHT BLR RIGHT K@ RIGHT l BLK-ID IR-FUN:FBLOCK-SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: bsid:IR-ID:ir-source-id bst:n bln:n :}
   asid IR-ID:SOURCE-LOCAL bsid IR-ID:SOURCE-LOCAL SRC-EQ?
   ast bst = and aln bln = and if exit then
   s" block" $62 l ROW-LABEL
   s" span differs" PUT-S
   PUT-NL ;

: CK-BLK ( n -- )
   {: l:n :}
   l CK-BLK-PARENT
   l CK-BLK-ARGS
   l CK-BLK-OPS
   l CK-BLK-SPAN ;

: CK-BLOCKS ( -- )
   LEFT BLKS RIGHT BLKS <> if
      s" blocks" FIELD
      LEFT BLKS RIGHT BLKS PUT-NUMS
   then
   LEFT BLKS RIGHT BLKS min 0 ?do
      i CK-BLK
   loop ;

\ ---- operations --------------------------------------------------------------
: OP-CODE ( n n -- n )
   {: s:n l:n :}
   s OPR s K@ s l OP-ID IR-OP:FOPCODE@ IR-ID:SYMBOL-LOCAL ;

: OP-OPERAND ( n n n -- n )
   {: s:n l:n j:n :}
   s OPP s OPR s K@ s l OP-ID j IR-OP:FOPERAND@ IR-ID:VALUE-LOCAL ;

: OP-RESULT ( n n n -- n )
   {: s:n l:n j:n :}
   s OPP s OPR s K@ s l OP-ID j IR-OP:FRESULT@ IR-ID:VALUE-LOCAL ;

: OP-SUCC ( n n n -- n )
   {: s:n l:n j:n :}
   s OPP s OPR s K@ s l OP-ID j IR-OP:FSUCCESSOR@ IR-ID:BLOCK-LOCAL ;

: OP-ENTRIES ( n n -- n )
   {: s:n l:n :}
   s OPR s l OP-ID IR-OP:FATTRS PAIR-CK ;

: OPE-KEY ( n n n -- n )
   {: s:n l:n j:n :}
   s OPP s OPR s K@ s l OP-ID j IR-OP:FATTR-KEY@ IR-ID:SYMBOL-LOCAL ;

: OPE-VAL ( n n n -- n )
   {: s:n l:n j:n :}
   s OPP s OPR s K@ s l OP-ID j IR-OP:FATTR@ IR-ID:ATTR-LOCAL ;

: OPE-KEY-ORD ( n n n -- n )
   {: s:n l:n j:n :}
   s  s l j OPE-KEY  SYM-ORD ;

: OPE-RANK ( n n n -- n )
   {: s:n l:n c:n :}
   0
   s l OP-ENTRIES 0 ?do
      s l i OPE-KEY-ORD c < if 1+ then
   loop ;

: OPE-NTH ( n n n -- n n )
   {: s:n l:n j:n :}
   s l OP-ENTRIES 0 ?do
      s l  s l i OPE-KEY-ORD  OPE-RANK j = if
         s l i OPE-KEY  s l i OPE-VAL  unloop exit
      then
   loop
   E-IR-DIFF-STATE throw ;

: CK-OP-CODE ( n -- )
   {: l:n :}
   LEFT l OP-CODE RIGHT l OP-CODE NAME-EQ? if exit then
   s" op" $6F l ROW-LABEL
   s" opcode - " PUT-S
   LEFT LEFT l OP-CODE PUT-NAME
   s"  + " PUT-S
   RIGHT RIGHT l OP-CODE PUT-NAME
   PUT-NL ;

: CK-OP-OPERANDS ( n -- )
   {: l:n :}
   LEFT OPR LEFT l OP-ID IR-OP:FOPERANDS {: an:n :}
   RIGHT OPR RIGHT l OP-ID IR-OP:FOPERANDS {: bn:n :}
   an bn <> if
      s" op" $6F l ROW-LABEL
      s" operands" an bn COUNT-LINE
      exit
   then
   an 0 ?do
      LEFT l i OP-OPERAND RIGHT l i OP-OPERAND <> if
         s" op" $6F l ROW-LABEL
         s" operand " PUT-S
         i PUT-U
         LEFT l i OP-OPERAND RIGHT l i OP-OPERAND PUT-NUMS
      then
   loop ;

: CK-OP-RESULTS ( n -- )
   {: l:n :}
   LEFT OPR LEFT l OP-ID IR-OP:FRESULTS {: an:n :}
   RIGHT OPR RIGHT l OP-ID IR-OP:FRESULTS {: bn:n :}
   an bn <> if
      s" op" $6F l ROW-LABEL
      s" results" an bn COUNT-LINE
      exit
   then
   an 0 ?do
      LEFT l i OP-RESULT RIGHT l i OP-RESULT <> if
         s" op" $6F l ROW-LABEL
         s" result " PUT-S
         i PUT-U
         LEFT l i OP-RESULT RIGHT l i OP-RESULT PUT-NUMS
      then
   loop ;

: CK-OP-SUCCS ( n -- )
   {: l:n :}
   LEFT OPR LEFT l OP-ID IR-OP:FSUCCESSORS {: an:n :}
   RIGHT OPR RIGHT l OP-ID IR-OP:FSUCCESSORS {: bn:n :}
   an bn <> if
      s" op" $6F l ROW-LABEL
      s" successors" an bn COUNT-LINE
      exit
   then
   an 0 ?do
      LEFT l i OP-SUCC RIGHT l i OP-SUCC <> if
         s" op" $6F l ROW-LABEL
         s" successor " PUT-S
         i PUT-U
         LEFT l i OP-SUCC RIGHT l i OP-SUCC PUT-NUMS
      then
   loop ;

: CK-OP-ENTRY ( n n -- )
   {: l:n j:n :}
   LEFT l j OPE-NTH {: ak:n av:n :}
   RIGHT l j OPE-NTH {: bk:n bv:n :}
   ak bk NAME-EQ? 0= if
      s" op" $6F l ROW-LABEL
      s" attr " PUT-S
      j PUT-U
      s"  key - " PUT-S
      LEFT ak PUT-NAME
      s"  + " PUT-S
      RIGHT bk PUT-NAME
      PUT-NL
      exit
   then
   av bv AT-EQ? if exit then
   s" op" $6F l ROW-LABEL
   s" attr " PUT-S
   j PUT-U
   s"  value - " PUT-S
   LEFT av PUT-AT
   s"  + " PUT-S
   RIGHT bv PUT-AT
   PUT-NL ;

: CK-OP-ATTRS ( n -- )
   {: l:n :}
   LEFT l OP-ENTRIES {: an:n :}
   RIGHT l OP-ENTRIES {: bn:n :}
   an bn <> if
      s" op" $6F l ROW-LABEL
      s" attrs" an bn COUNT-LINE
      exit
   then
   an 0 ?do
      l i CK-OP-ENTRY
   loop ;

: CK-OP-SPAN ( n -- )
   {: l:n :}
   LEFT OPR LEFT K@ LEFT l OP-ID IR-OP:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: asid:IR-ID:ir-source-id ast:n aln:n :}
   RIGHT OPR RIGHT K@ RIGHT l OP-ID IR-OP:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: bsid:IR-ID:ir-source-id bst:n bln:n :}
   asid IR-ID:SOURCE-LOCAL bsid IR-ID:SOURCE-LOCAL SRC-EQ?
   ast bst = and aln bln = and if exit then
   s" op" $6F l ROW-LABEL
   s" span differs" PUT-S
   PUT-NL ;

: CK-OP ( n -- )
   {: l:n :}
   l CK-OP-CODE
   l CK-OP-OPERANDS
   l CK-OP-RESULTS
   l CK-OP-SUCCS
   l CK-OP-ATTRS
   l CK-OP-SPAN ;

: CK-OPS ( -- )
   LEFT OPS RIGHT OPS <> if
      s" ops" FIELD
      LEFT OPS RIGHT OPS PUT-NUMS
   then
   LEFT OPS RIGHT OPS min 0 ?do
      i CK-OP
   loop ;

\ ---- values ------------------------------------------------------------------
: VAL-KIND ( n n -- IR-OP:def-kind )
   {: s:n l:n :}
   s VLR s l VAL-ID IR-OP:FVALUE-KIND@ ;

: ARG-KIND? ( IR-OP:def-kind -- bool )
   {: dk:IR-OP:def-kind :}
   dk IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ ;

: VAL-TYPE ( n n -- n )
   {: s:n l:n :}
   s VLR s K@ s l VAL-ID IR-OP:FVALUE-TYPE@ IR-ID:TYPE-LOCAL ;

: CK-VAL-TYPE ( n -- )
   {: l:n :}
   LEFT l VAL-TYPE RIGHT l VAL-TYPE TY-EQ? if exit then
   s" value" $76 l ROW-LABEL
   s" type - " PUT-S
   LEFT LEFT l VAL-TYPE PUT-TY
   s"  + " PUT-S
   RIGHT RIGHT l VAL-TYPE PUT-TY
   PUT-NL ;

: CK-VAL-ARG ( n -- )
   {: l:n :}
   LEFT VLR LEFT K@ LEFT l VAL-ID IR-OP:FVALUE-BLOCK@ IR-ID:BLOCK-LOCAL {: ab:n :}
   RIGHT VLR RIGHT K@ RIGHT l VAL-ID IR-OP:FVALUE-BLOCK@ IR-ID:BLOCK-LOCAL {: bb:n :}
   ab bb <> if
      s" value" $76 l ROW-LABEL
      s" block" ab bb COUNT-LINE
   then
   LEFT VLR LEFT l VAL-ID IR-OP:FVALUE-ARG@ {: ai:n :}
   RIGHT VLR RIGHT l VAL-ID IR-OP:FVALUE-ARG@ {: bi:n :}
   ai bi = if exit then
   s" value" $76 l ROW-LABEL
   s" index" ai bi COUNT-LINE ;

: CK-VAL-RESULT ( n -- )
   {: l:n :}
   LEFT VLR LEFT OPR LEFT K@ LEFT l VAL-ID IR-OP:FVALUE-OP@ IR-ID:OP-LOCAL
   {: ao:n :}
   RIGHT VLR RIGHT OPR RIGHT K@ RIGHT l VAL-ID IR-OP:FVALUE-OP@ IR-ID:OP-LOCAL
   {: bo:n :}
   ao bo <> if
      s" value" $76 l ROW-LABEL
      s" op" ao bo COUNT-LINE
   then
   LEFT VLR LEFT l VAL-ID IR-OP:FVALUE-POS@ {: ap:n :}
   RIGHT VLR RIGHT l VAL-ID IR-OP:FVALUE-POS@ {: bp:n :}
   ap bp = if exit then
   s" value" $76 l ROW-LABEL
   s" pos" ap bp COUNT-LINE ;

: CK-VAL ( n -- )
   {: l:n :}
   LEFT l VAL-KIND {: ka:IR-OP:def-kind :}
   RIGHT l VAL-KIND {: kb:IR-OP:def-kind :}
   ka kb IR--OP-DEF--KIND:EQ 0= if
      s" value" $76 l ROW-LABEL
      s" kind differs" PUT-S
      PUT-NL
      exit
   then
   l CK-VAL-TYPE
   ka ARG-KIND? if l CK-VAL-ARG exit then
   l CK-VAL-RESULT ;

: CK-VALUES ( -- )
   LEFT VALS RIGHT VALS <> if
      s" values" FIELD
      LEFT VALS RIGHT VALS PUT-NUMS
   then
   LEFT VALS RIGHT VALS min 0 ?do
      i CK-VAL
   loop ;

\ ---- the derived edge index --------------------------------------------------
: PRED ( n n n -- n )
   {: s:n l:n j:n :}
   s EGP s EGR s K@ s l BLK-ID j IR-VERIFY:FPRED@ IR-ID:BLOCK-LOCAL ;

: CK-EDGE-SUCCS ( n -- )
   {: l:n :}
   LEFT EGR LEFT l BLK-ID IR-VERIFY:FSUCC-COUNT {: an:n :}
   RIGHT EGR RIGHT l BLK-ID IR-VERIFY:FSUCC-COUNT {: bn:n :}
   an bn = if exit then
   s" edge" $62 l ROW-LABEL
   s" succs" an bn COUNT-LINE ;

: CK-EDGE-PREDS ( n -- )
   {: l:n :}
   LEFT EGR LEFT l BLK-ID IR-VERIFY:FPRED-COUNT {: an:n :}
   RIGHT EGR RIGHT l BLK-ID IR-VERIFY:FPRED-COUNT {: bn:n :}
   an bn <> if
      s" edge" $62 l ROW-LABEL
      s" preds" an bn COUNT-LINE
      exit
   then
   an 0 ?do
      LEFT l i PRED RIGHT l i PRED <> if
         s" edge" $62 l ROW-LABEL
         s" pred " PUT-S
         i PUT-U
         LEFT l i PRED RIGHT l i PRED PUT-NUMS
      then
   loop ;

: CK-EDGES ( -- )
   LEFT EDGE-BLKS RIGHT EDGE-BLKS <> if
      s" edges" FIELD
      LEFT EDGE-BLKS RIGHT EDGE-BLKS PUT-NUMS
   then
   LEFT EDGE-BLKS RIGHT EDGE-BLKS min 0 ?do
      i CK-EDGE-SUCCS
      i CK-EDGE-PREDS
   loop ;

: BODY ( -- )
   CK-HEAD
   CK-SYMS
   CK-TYPES
   CK-ATTRS
   CK-SRCS
   CK-FUNS
   CK-BLOCKS
   CK-OPS
   CK-VALUES
   CK-EDGES ;

public

\ ---- comparing two frozen modules --------------------------------------------
\ Compare the two modules and write the differences into the caller's span.
\ Answers the length written and the number of differences found; zero
\ differences is what "these two modules mean the same thing" means, so no caller
\ reads the report back to learn it. Everything that can refuse runs before the
\ first byte: both modules must be live frozen ones, both must fit the committed
\ working set, and building the two inverse maps asks each canonical table for
\ the ordinals of its own module's identities, which is IR-CANON's owner check
\ and so proves each table numbers the module it was passed with.
: DIFF ( IR-BUILD:module IR-CANON:table IR-BUILD:module IR-CANON:table ptr u8 n -- n n )
   {: ma:IR-BUILD:module ta:IR-CANON:table
      mb:IR-BUILD:module tb:IR-CANON:table p:ptr room:n :}
   ma ta LEFT TAKE-VIEWS
   mb tb RIGHT TAKE-VIEWS
   FITS-CK
   INV-FILL
   p room SINK!
   0 HITS !
   BODY
   OUT-U @ HITS @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
