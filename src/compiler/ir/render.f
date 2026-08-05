\ render.f - deterministic diagnostic text for one frozen module: the one
\ authority that says how a module's content is spelled for a human.
\
\ docs/compiler-ir-design.md section 6.6 names IR:RENDER among the five words
\ the serialization stage publishes and adds one rule about it: "IR:RENDER is
\ diagnostic text. It is not parsed by the compiler." Section 5.6 says the same
\ thing from the other side - "deriving schedule facts from rendered text" is
\ prohibited, and "all such facts must exist before rendering". So this file
\ produces text for people and for goldens, and nothing in the compiler is
\ allowed to read it back. tools/render-parse-lint.f enforces that as a
\ repository gate: no source under src/compiler/ other than this stage's own two
\ files may name a word of this package or of IR-DIFF, reopen either package, or
\ require either file, so compiler code cannot even obtain rendered bytes.
\
\ WHAT MAKES THE TEXT DETERMINISTIC. Section 5.7 requires identical inputs to
\ produce identical artifacts, and a module's interned tables are numbered in
\ the order a builder happened to intern them. Rendering those insertion
\ ordinals would make the text depend on build order, which is the one thing a
\ golden must not depend on. Every interned reference this file writes is
\ therefore a CANONICAL ordinal taken from src/compiler/ir/canon.f, the four
\ interned tables are walked in canonical order rather than insertion order, and
\ every keyed list - a record's pairs, an operation's attribute entries - is
\ sorted by canonical key, exactly as the canonical stream sorts it. Two modules
\ built along two admissible insertion orders therefore render byte for byte the
\ same, which test/compiler/ir-render.f measures against ir-canon.f's own
\ reversed-order fixture.
\
\ WHAT IS NOT REORDERED, AND WHY. Functions, blocks, operations and values keep
\ their module order. That order is the program - block layout, instruction
\ order, which operand is which - so a renderer that sorted it would hide the
\ very thing a diagnostic is read for. Their references are still canonical,
\ because they name symbols, types, attributes and sources.
\
\ WHERE EACH SPELLING COMES FROM. Two spellings already have owners and are not
\ restated here. A type's spelling is IR-TYPE:FRENDER, which unfolds a row into
\ its structure ("i32", "ptr<generic,i8>", "( i64 -- i64 )") and never prints an
\ ordinal, so it is already stable under any insertion order. An attribute's
\ spelling is IR-ATTR:FRENDER for every kind whose content is self-contained -
\ integers, booleans, text, integer lists, enum members, digests. The three
\ attribute kinds that store a REFERENCE are spelled here instead, because
\ IR-ATTR spells them with the module-local ordinal it stores ("sym#4") and this
\ file must spell them canonically ("sym s1"): a symbol reference, a type
\ reference, and a record, whose pairs are keyed by symbol and re-sorted by
\ canonical key. Nothing else about a row's shape is duplicated here.
\
\ THE OUTPUT IS THE CALLER'S BYTES. Rendered text is a value, not a resource:
\ every consumer copies it into a report, a file, or a test assertion. So RENDER
\ writes into a byte span the caller already owns and answers the length it
\ wrote, a span too short is E-IR-RENDER-ROOM by name, and this package holds no
\ registry and creates no arena. Nothing here writes to the module either: every
\ reader used is a frozen view reader, so rendering a module twice answers the
\ same bytes and leaves the module and its canonical table exactly as they were.
\
\ HOW THE MODULE AND THE TABLE ARE PROVED TO BE EACH OTHER. RENDER is handed a
\ frozen module and a canonical table, and text built from one module's rows
\ against another module's numbering would be a lie no later check could catch.
\ The proof is the same one IR-ENCODE uses and costs nothing extra: building the
\ inverse of the canonical map asks the table for the ordinal of an identity this
\ module minted, which is IR-CANON's own owner check, so a mismatched pair leaves
\ as E-IR-CANON-OWNER before a byte is written.
\
\ THE COMMITTED WORKING SET. The inverse maps, the name buffer, the spelling
\ scratch and the keyed-entry buffers are package-owned arrays with named
\ ceilings, in the shape IR-CANON's own working set uses, and the row ceilings
\ are the same ones IR-BUILD's production plan commits to. A module planned
\ larger, or a name longer than NAME-MAX, or a keyed list wider than PAIR-MAX, is
\ refused with E-IR-RENDER-CAP before any text is written rather than rendered
\ partially. A type or attribute whose delegated spelling is longer than
\ TEXT-MAX is the owning table's own named RANGE refusal, because that table owns
\ the spelling.
\
\ THE PER-ITEM WORDS, AND WHO USES THEM. SYMBOL-TEXT, TYPE-TEXT and ATTR-TEXT
\ spell one row into a caller span. They exist so src/compiler/ir/diff.f can name
\ the rows it found a difference in without spelling anything itself: this
\ package stays the one authority for turning IR content into text, and the diff
\ stays the one authority for deciding what differs. Like the staging areas in
\ IR-TYPE and IR-CTX, the output sink is one package-owned cursor under the
\ single-task compilation discipline, so one text is written at a time.

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

package IR-RENDER
private

\ ---- the committed working set -----------------------------------------------
\ The row ceilings are IR-BUILD's committed production ceilings, the same ones
\ IR-CANON sizes its working set to.
256 constant SYM-MAX
128 constant TY-MAX
128 constant AT-MAX
64 constant SRC-MAX
256 constant NAME-MAX                \ bytes of one symbol name or string value
1024 constant TEXT-MAX               \ bytes of one delegated type or attribute spelling
64 constant PAIR-MAX                 \ keyed entries in one record or operation

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
      E-IR-RENDER-STATE throw
   endcase ;

SYM-MAX TY-MAX + AT-MAX + SRC-MAX + constant MAP-CELLS

-1 constant UNSET

\ The one integer whose sign cannot be flipped to print it, so the decimal
\ emitter states its digits directly, exactly as IR-ATTR's does.
$8000000000000000 constant INT-MIN

\ ---- the module's frozen views -----------------------------------------------
0 constant V-SYP                     \ symbol name pool
1 constant V-SYR                     \ symbol rows
2 constant V-TYP                     \ type list pool
3 constant V-TYR                     \ type rows
4 constant V-ATP                     \ attribute payload pool
5 constant V-ATR                     \ attribute rows
6 constant V-SRC                     \ source registry
7 constant V-SCR                     \ dialect schema rows
8 constant V-FNP                     \ function attribute pool
9 constant V-FNR                     \ function rows
10 constant V-BLR                    \ block rows
11 constant V-OPP                    \ operation cell pool
12 constant V-VAL                    \ value rows
13 constant V-OPR                    \ operation rows
14 constant V-EGP                    \ derived edge pool
15 constant V-EGR                    \ derived edge rows
16 constant VIEW#

\ ---- attribute kinds, mirroring IR-ATTR's storage codes ----------------------
0 constant AK-INT
1 constant AK-BOOL
2 constant AK-TXT
3 constant AK-SYM
4 constant AK-TYPE
5 constant AK-ILIST
6 constant AK-ENUM
7 constant AK-REC
8 constant AK-DIG

\ ---- closed vocabularies as words --------------------------------------------
\ Each mapping is an exhaustive MATCH, so a new member of any family fails to
\ compile here rather than rendering as some other member.
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

: LNK-STR ( IR-FUN:linkage -- ptr u8 n )
   MATCH IR-FUN:linkage
      defined     OF s" defined" ENDOF
      replaceable OF s" replaceable" ENDOF
      imported    OF s" imported" ENDOF
   ;MATCH ;

: VIS-STR ( IR-FUN:visibility -- ptr u8 n )
   MATCH IR-FUN:visibility
      hidden   OF s" hidden" ENDOF
      exported OF s" exported" ENDOF
   ;MATCH ;

: CC-STR ( IR-FUN:convention -- ptr u8 n )
   MATCH IR-FUN:convention
      habu   OF s" habu" ENDOF
      c-abi  OF s" c-abi" ENDOF
      kernel OF s" kernel" ENDOF
   ;MATCH ;

: ARG-KIND? ( IR-OP:def-kind -- bool )
   MATCH IR-OP:def-kind
      op-result OF false ENDOF
      blk-arg   OF true ENDOF
   ;MATCH ;

\ ---- the output sink ---------------------------------------------------------
variable OUT-A
variable OUT-CAP
variable OUT-U
variable NUM-U
variable PAIR-I

VIEW# TYPED-BUFFER MV IR-ARENA:view
1 TYPED-BUFFER MK IR-ID:ir-module-key
1 TYPED-BUFFER CTB IR-CANON:table

create INV MAP-CELLS cells allot      \ canonical ordinal -> first module row
create NB NAME-MAX allot              \ one symbol name or string value
create TXB TEXT-MAX allot             \ one delegated spelling
create NUMB 32 allot                  \ decimal digits, built reversed
create PK PAIR-MAX cells allot        \ keyed entries in canonical numbering,
create PV PAIR-MAX cells allot        \ sorted by canonical key

\ ---- the sink's typed pointer ------------------------------------------------
\ A plain `variable @` reads back an untyped cell, so the byte store below would
\ not certify; `0 ptr-field` gives the checked ptr u8 view (the lib-wide
\ *-BUF-A idiom).
: OUT-P ( -- ptr ptr u8 )
   OUT-A 0 ptr-field ;

: SINK! ( ptr u8 n -- )
   {: p:ptr room:n :}
   p OUT-A !
   room OUT-CAP !
   0 OUT-U ! ;

: PUT-B ( n -- )
   {: b:n :}
   OUT-U @ OUT-CAP @ >= if E-IR-RENDER-ROOM throw then
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

: PUT-N ( n -- )
   {: v:n :}
   v INT-MIN = if s" -9223372036854775808" PUT-S exit then
   v 0 < if
      $2D PUT-B
      0 v - PUT-U
      exit
   then
   v PUT-U ;

: HEXDIG ( n -- n )
   dup 10 < if $30 + else 10 - $61 + then ;

: PUT-H16 ( n -- )
   {: v:n :}
   16 0 ?do
      v 15 i - 4 * rshift $F and HEXDIG PUT-B
   loop ;

\ ---- views, key and canonical table ------------------------------------------
: V@ ( n -- IR-ARENA:view )
   MV @ ;

: V! ( IR-ARENA:view n -- )
   MV ! ;

: K@ ( -- IR-ID:ir-module-key )
   0 MK @ ;

: T@ ( -- IR-CANON:table )
   0 CTB @ ;

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
   m IR-BUILD:FSCHEMA-ROWS V-SCR V!
   m IR-BUILD:FFUN-POOL V-FNP V!
   m IR-BUILD:FFUN-ROWS V-FNR V!
   m IR-BUILD:FBLOCK-ROWS V-BLR V!
   m IR-BUILD:FOP-POOL V-OPP V!
   m IR-BUILD:FVALUE-ROWS V-VAL V!
   m IR-BUILD:FOP-ROWS V-OPR V!
   m IR-BUILD:FEDGE-POOL V-EGP V!
   m IR-BUILD:FEDGE-ROWS V-EGR V! ;

\ ---- identities from module rows ---------------------------------------------
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

\ ---- how many rows the module holds ------------------------------------------
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

\ ---- the canonical ordinal of one module row ---------------------------------
: SYM-ORD ( n -- n )
   T@ swap SYM IR-CANON:SYMBOL-ORD ;

: TY-ORD ( n -- n )
   T@ swap TYP IR-CANON:TYPE-ORD ;

: AT-ORD ( n -- n )
   T@ swap ATT IR-CANON:ATTR-ORD ;

: SRC-ORD ( n -- n )
   T@ swap SRC IR-CANON:SOURCE-ORD ;

\ ---- the inverse of the canonical map ----------------------------------------
\ Walking a table in canonical order needs the map the other way round. The
\ source registry does not deduplicate, so two rows can share one canonical
\ ordinal; the first row wins, which is deterministic and reads the same bytes.
: INV@ ( n n -- n )
   {: tb:n c:n :}
   tb TB-BASE c + cells INV + @ ;

: INV! ( n n n -- )
   {: tb:n v:n c:n :}
   v tb TB-BASE c + cells INV + ! ;

: INV-CLEAR ( -- )
   MAP-CELLS 0 ?do
      UNSET i cells INV + !
   loop ;

: INV-KEEP ( n n n -- )
   {: tb:n c:n l:n :}
   tb c INV@ UNSET = if tb l c INV! then ;

: INV-FILL ( -- )
   INV-CLEAR
   SYMS 0 ?do
      TB-SYM i SYM-ORD i INV-KEEP
   loop
   TYS 0 ?do
      TB-TY i TY-ORD i INV-KEEP
   loop
   ATS 0 ?do
      TB-AT i AT-ORD i INV-KEEP
   loop
   SRCS 0 ?do
      TB-SRC i SRC-ORD i INV-KEEP
   loop ;

\ ---- capacity ----------------------------------------------------------------
: NAME-CK ( n -- n )
   dup NAME-MAX > if E-IR-RENDER-CAP throw then ;

: PAIR-CK ( n -- n )
   dup PAIR-MAX > if E-IR-RENDER-CAP throw then ;

: ROWS-CK ( -- )
   SYMS SYM-MAX > if E-IR-RENDER-CAP throw then
   TYS TY-MAX > if E-IR-RENDER-CAP throw then
   ATS AT-MAX > if E-IR-RENDER-CAP throw then
   SRCS SRC-MAX > if E-IR-RENDER-CAP throw then ;

\ Every run of bytes and every keyed list the text will restate, measured before
\ anything is written, so a refusal costs the caller no half-written report.
: WIDTHS-CK ( -- )
   SYMS 0 ?do
      V-SYR V@ i SYM IR-SYM:FLEN@ NAME-CK drop
   loop
   ATS 0 ?do
      V-ATR V@ i ATT IR-ATTR:FKIND@ AK-CODE {: k:n :}
      k AK-TXT = if
         V-ATR V@ i ATT IR-ATTR:FTEXT-LEN@ NAME-CK drop
      then
      k AK-REC = if
         V-ATR V@ i ATT IR-ATTR:FPAIRS@ PAIR-CK drop
      then
   loop
   OPS 0 ?do
      V-OPR V@ i OPI IR-OP:FATTRS PAIR-CK drop
   loop ;

: FITS-CK ( -- )
   ROWS-CK
   WIDTHS-CK ;

\ ---- reference tokens --------------------------------------------------------
\ One letter and one ordinal, so a reference reads back to the row that states
\ it. The four interned tables use canonical ordinals; the program tables use
\ their module order, which is the program itself.
: SREF ( n -- )
   $73 PUT-B SYM-ORD PUT-U ;

: TREF ( n -- )
   $74 PUT-B TY-ORD PUT-U ;

: AREF ( n -- )
   $61 PUT-B AT-ORD PUT-U ;

: CREF ( n -- )
   $63 PUT-B SRC-ORD PUT-U ;

: FREF ( n -- )
   $66 PUT-B PUT-U ;

: BREF ( n -- )
   $62 PUT-B PUT-U ;

: OREF ( n -- )
   $6F PUT-B PUT-U ;

: VREF ( n -- )
   $76 PUT-B PUT-U ;

\ ---- names and delegated spellings -------------------------------------------
: NAME>NB ( n -- n )
   {: l:n :}
   V-SYR V@ l SYM IR-SYM:FLEN@ NAME-CK drop
   V-SYP V@ V-SYR V@ l SYM NB NAME-MAX IR-SYM:FCOPY ;

: PUT-NAME ( n -- )
   NAME>NB {: u:n :}
   $22 PUT-B
   NB u PUT-S
   $22 PUT-B ;

: PUT-TY-SPELL ( n -- )
   {: l:n :}
   V-TYP V@ V-TYR V@ l TYP TXB TEXT-MAX IR-TYPE:FRENDER {: u:n :}
   TXB u PUT-S ;

: PUT-AT-DELEGATE ( n -- )
   {: l:n :}
   V-ATP V@ V-ATR V@ l ATT TXB TEXT-MAX IR-ATTR:FRENDER {: u:n :}
   TXB u PUT-S ;

\ ---- keyed entries in canonical key order ------------------------------------
\ A record's pairs and an operation's attribute entries are both keyed by a
\ symbol and stored sorted by the key's INSERTION ordinal, which is a different
\ permutation from the canonical one. Both are reloaded into canonical numbering
\ and re-sorted here, the same way the canonical stream sorts them, so the text
\ does not depend on the order the keys were interned in.
: PAIR-SWAP ( n n -- )
   {: x:n y:n :}
   x cells PK + @  y cells PK + @  x cells PK + !  y cells PK + !
   x cells PV + @  y cells PV + @  x cells PV + !  y cells PV + ! ;

: PAIR-SORT ( n -- )
   {: n:n :}
   n 1 ?do
      i PAIR-I !
      begin
         PAIR-I @ 0 > if
            PAIR-I @ cells PK + @  PAIR-I @ 1- cells PK + @  <
         else
            false
         then
      while
         PAIR-I @ PAIR-I @ 1- PAIR-SWAP
         PAIR-I @ 1- PAIR-I !
      repeat
   loop ;

: REC-PAIRS ( n -- n )
   {: l:n :}
   V-ATR V@ l ATT IR-ATTR:FPAIRS@ PAIR-CK {: n:n :}
   n 0 ?do
      V-ATP V@ V-ATR V@ K@ l ATT i IR-ATTR:FKEY@ IR-ID:SYMBOL-LOCAL
      SYM-ORD i cells PK + !
      V-ATP V@ V-ATR V@ K@ l ATT i IR-ATTR:FVAL@ IR-ID:ATTR-LOCAL
      AT-ORD i cells PV + !
   loop
   n PAIR-SORT
   n ;

: OP-ENTRIES ( n -- n )
   {: l:n :}
   l OPI {: o:IR-ID:ir-op-id :}
   V-OPR V@ o IR-OP:FATTRS PAIR-CK {: n:n :}
   n 0 ?do
      V-OPP V@ V-OPR V@ K@ o i IR-OP:FATTR-KEY@ IR-ID:SYMBOL-LOCAL
      SYM-ORD i cells PK + !
      V-OPP V@ V-OPR V@ K@ o i IR-OP:FATTR@ IR-ID:ATTR-LOCAL
      AT-ORD i cells PV + !
   loop
   n PAIR-SORT
   n ;

\ One loaded entry as "sN=aM": the buffers already hold canonical ordinals, so
\ this writes them straight rather than mapping again.
: PUT-ENTRY ( n -- )
   {: i:n :}
   $73 PUT-B i cells PK + @ PUT-U
   $3D PUT-B
   $61 PUT-B i cells PV + @ PUT-U ;

: PUT-ENTRIES ( n -- )
   {: n:n :}
   n 0 ?do
      i 0 > if PUT-SP then
      i PUT-ENTRY
   loop ;

\ ---- attribute spellings -----------------------------------------------------
: PUT-REC ( n -- )
   {: l:n :}
   s" rec(" PUT-S
   l REC-PAIRS PUT-ENTRIES
   $29 PUT-B ;

: PUT-AT-SPELL ( n -- )
   {: l:n :}
   V-ATR V@ l ATT IR-ATTR:FKIND@ AK-CODE {: k:n :}
   k AK-SYM = if
      s" sym " PUT-S
      V-ATR V@ K@ l ATT IR-ATTR:FSYM@ IR-ID:SYMBOL-LOCAL SREF
      exit
   then
   k AK-TYPE = if
      s" type " PUT-S
      V-ATR V@ K@ l ATT IR-ATTR:FTYPE@ IR-ID:TYPE-LOCAL TREF
      exit
   then
   k AK-REC = if l PUT-REC exit then
   l PUT-AT-DELEGATE ;


\ ---- source spans ------------------------------------------------------------
: PUT-SPAN ( IR-SOURCE:span -- )
   IR--SOURCE-SPAN:UNMAKE
   {: sid:IR-ID:ir-source-id st:n ln:n :}
   s"  span " PUT-S
   sid IR-ID:SOURCE-LOCAL CREF
   PUT-SP st PUT-U
   PUT-SP ln PUT-U ;

\ ---- the sections ------------------------------------------------------------
\ One line per row, fields in a fixed order, so a reader finds a row by its
\ token and a golden diff points at the row that moved.
: PUT-HEAD ( -- )
   s" module dialect " PUT-S
   V-SCR V@ K@ IR-SCHEMA:FDIALECT@ IR-ID:SYMBOL-LOCAL {: l:n :}
   l SREF
   PUT-SP l PUT-NAME
   s"  schema " PUT-S
   V-SCR V@ IR-SCHEMA:FMAJOR@ PUT-U
   PUT-SP
   V-SCR V@ IR-SCHEMA:FMINOR@ PUT-U
   PUT-NL ;

: PUT-COUNT ( ptr u8 n n -- )
   {: p:ptr u:n n:n :}
   p u PUT-S
   PUT-SP
   n PUT-U
   PUT-NL ;

: PUT-SYMS ( -- )
   s" symbols" T@ IR-CANON:SYMBOLS PUT-COUNT
   T@ IR-CANON:SYMBOLS 0 ?do
      s" sym " PUT-S
      $73 PUT-B i PUT-U
      PUT-SP
      TB-SYM i INV@ PUT-NAME
      PUT-NL
   loop ;

: PUT-TYPES ( -- )
   s" types" T@ IR-CANON:TYPES PUT-COUNT
   T@ IR-CANON:TYPES 0 ?do
      s" type " PUT-S
      $74 PUT-B i PUT-U
      PUT-SP
      TB-TY i INV@ PUT-TY-SPELL
      PUT-NL
   loop ;

: PUT-ATTRS ( -- )
   s" attrs" T@ IR-CANON:ATTRS PUT-COUNT
   T@ IR-CANON:ATTRS 0 ?do
      s" attr " PUT-S
      $61 PUT-B i PUT-U
      PUT-SP
      TB-AT i INV@ PUT-AT-SPELL
      PUT-NL
   loop ;

: PUT-SRC-ORIGIN ( n -- )
   {: l:n :}
   V-SRC V@ l SRC IR-SOURCE:FROOT? if s" root" PUT-S exit then
   s" from " PUT-S
   V-SRC V@ K@ l SRC IR-SOURCE:FORIGIN@ IR-ID:SOURCE-LOCAL CREF ;

: PUT-SRC-DIGEST ( n -- )
   {: l:n :}
   s"  digest " PUT-S
   V-SRC V@ l SRC IR-SOURCE:FDIGEST@ CDIGEST-DIGEST:UNMAKE
   {: w0:n w1:n w2:n w3:n :}
   w0 PUT-H16
   w1 PUT-H16
   w2 PUT-H16
   w3 PUT-H16 ;

: PUT-SRCS ( -- )
   s" sources" T@ IR-CANON:SOURCES PUT-COUNT
   T@ IR-CANON:SOURCES 0 ?do
      TB-SRC i INV@ {: l:n :}
      s" source " PUT-S
      $63 PUT-B i PUT-U
      PUT-SP l PUT-SRC-ORIGIN
      s"  len " PUT-S
      V-SRC V@ l SRC IR-SOURCE:FLEN@ PUT-U
      s"  depth " PUT-S
      V-SRC V@ l SRC IR-SOURCE:FDEPTH PUT-U
      l PUT-SRC-DIGEST
      PUT-NL
   loop ;

: PUT-FUN-ATTRS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   s"  attrs" PUT-S
   V-FNR V@ f IR-FUN:FATTR-COUNT 0 ?do
      PUT-SP
      V-FNP V@ V-FNR V@ K@ f i IR-FUN:FATTR@ IR-ID:ATTR-LOCAL AREF
   loop ;

: PUT-FUN ( n -- )
   {: l:n :}
   l FUN {: f:IR-ID:ir-fun-id :}
   s" fun " PUT-S
   l FREF
   s"  name " PUT-S
   V-FNR V@ K@ f IR-FUN:FSYMBOL@ IR-ID:SYMBOL-LOCAL SREF
   s"  sig " PUT-S
   V-FNR V@ K@ f IR-FUN:FSIGNATURE@ IR-ID:TYPE-LOCAL TREF
   PUT-SP V-FNR V@ f IR-FUN:FLINKAGE@ LNK-STR PUT-S
   PUT-SP V-FNR V@ f IR-FUN:FVISIBILITY@ VIS-STR PUT-S
   PUT-SP V-FNR V@ f IR-FUN:FCONVENTION@ CC-STR PUT-S
   f PUT-FUN-ATTRS
   s"  blocks " PUT-S
   V-FNR V@ f IR-FUN:FBLOCK-COUNT PUT-U
   V-FNR V@ K@ f IR-FUN:FSPAN@ PUT-SPAN
   PUT-NL ;

: PUT-FUNS ( -- )
   s" functions" FUNS PUT-COUNT
   FUNS 0 ?do
      i PUT-FUN
   loop ;

: PUT-BLOCK-ARGS ( IR-ID:ir-block-id -- )
   {: b:IR-ID:ir-block-id :}
   s"  args" PUT-S
   V-BLR V@ b IR-FUN:FARG-COUNT 0 ?do
      PUT-SP
      V-BLR V@ V-VAL V@ K@ b i IR-FUN:FARG@ IR-ID:VALUE-LOCAL VREF
   loop ;

: PUT-BLOCK ( n -- )
   {: l:n :}
   l BLK {: b:IR-ID:ir-block-id :}
   s" block " PUT-S
   l BREF
   s"  in " PUT-S
   V-BLR V@ V-FNR V@ K@ b IR-FUN:FPARENT@ IR-ID:FUN-LOCAL FREF
   b PUT-BLOCK-ARGS
   s"  ops " PUT-S
   V-BLR V@ b IR-FUN:FOP-COUNT PUT-U
   V-BLR V@ K@ b IR-FUN:FBLOCK-SPAN@ PUT-SPAN
   PUT-NL ;

: PUT-BLOCKS ( -- )
   s" blocks" BLKS PUT-COUNT
   BLKS 0 ?do
      i PUT-BLOCK
   loop ;

: PUT-OPERANDS ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   s"  operands" PUT-S
   V-OPR V@ o IR-OP:FOPERANDS 0 ?do
      PUT-SP
      V-OPP V@ V-OPR V@ K@ o i IR-OP:FOPERAND@ IR-ID:VALUE-LOCAL VREF
   loop ;

: PUT-RESULTS ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   s"  results" PUT-S
   V-OPR V@ o IR-OP:FRESULTS 0 ?do
      PUT-SP
      V-OPP V@ V-OPR V@ K@ o i IR-OP:FRESULT@ IR-ID:VALUE-LOCAL VREF
   loop ;

: PUT-SUCCS ( IR-ID:ir-op-id -- )
   {: o:IR-ID:ir-op-id :}
   s"  successors" PUT-S
   V-OPR V@ o IR-OP:FSUCCESSORS 0 ?do
      PUT-SP
      V-OPP V@ V-OPR V@ K@ o i IR-OP:FSUCCESSOR@ IR-ID:BLOCK-LOCAL BREF
   loop ;

: PUT-OP-ATTRS ( n -- )
   {: l:n :}
   s"  attrs" PUT-S
   l OP-ENTRIES {: n:n :}
   n 0 ?do
      PUT-SP
      i PUT-ENTRY
   loop ;

: PUT-OP ( n -- )
   {: l:n :}
   l OPI {: o:IR-ID:ir-op-id :}
   s" op " PUT-S
   l OREF
   s"  code " PUT-S
   V-OPR V@ K@ o IR-OP:FOPCODE@ IR-ID:SYMBOL-LOCAL SREF
   o PUT-OPERANDS
   o PUT-RESULTS
   o PUT-SUCCS
   l PUT-OP-ATTRS
   V-OPR V@ K@ o IR-OP:FSPAN@ PUT-SPAN
   PUT-NL ;

: PUT-OPS ( -- )
   s" ops" OPS PUT-COUNT
   OPS 0 ?do
      i PUT-OP
   loop ;

: PUT-VALUE ( n -- )
   {: l:n :}
   l VAL {: v:IR-ID:ir-value-id :}
   s" value " PUT-S
   l VREF
   PUT-SP
   V-VAL V@ v IR-OP:FVALUE-KIND@ ARG-KIND? if
      s" arg " PUT-S
      V-VAL V@ K@ v IR-OP:FVALUE-BLOCK@ IR-ID:BLOCK-LOCAL BREF
      s"  index " PUT-S
      V-VAL V@ v IR-OP:FVALUE-ARG@ PUT-U
   else
      s" result " PUT-S
      V-VAL V@ V-OPR V@ K@ v IR-OP:FVALUE-OP@ IR-ID:OP-LOCAL OREF
      s"  pos " PUT-S
      V-VAL V@ v IR-OP:FVALUE-POS@ PUT-U
   then
   s"  type " PUT-S
   V-VAL V@ K@ v IR-OP:FVALUE-TYPE@ IR-ID:TYPE-LOCAL TREF
   PUT-NL ;

: PUT-VALUES ( -- )
   s" values" VALS PUT-COUNT
   VALS 0 ?do
      i PUT-VALUE
   loop ;

\ The derived block-edge index of design line 573's "optional derived indices".
: PUT-EDGE ( n -- )
   {: l:n :}
   l BLK {: b:IR-ID:ir-block-id :}
   s" edge " PUT-S
   l BREF
   s"  succs " PUT-S
   V-EGR V@ b IR-VERIFY:FSUCC-COUNT PUT-U
   s"  preds" PUT-S
   V-EGR V@ b IR-VERIFY:FPRED-COUNT 0 ?do
      PUT-SP
      V-EGP V@ V-EGR V@ K@ b i IR-VERIFY:FPRED@ IR-ID:BLOCK-LOCAL BREF
   loop
   PUT-NL ;

: PUT-EDGES ( -- )
   s" edges" V-EGR V@ IR-VERIFY:FEDGE-BLOCKS PUT-COUNT
   V-EGR V@ IR-VERIFY:FEDGE-BLOCKS 0 ?do
      i PUT-EDGE
   loop ;

\ ---- one whole module --------------------------------------------------------
: OPEN ( IR-BUILD:module IR-CANON:table -- )
   {: m:IR-BUILD:module t:IR-CANON:table :}
   m IR-BUILD:FROZEN? 0= if E-IR-RENDER-STALE throw then
   m TAKE-VIEWS
   t 0 CTB ! ;

: BODY ( -- )
   PUT-HEAD
   PUT-SYMS
   PUT-TYPES
   PUT-ATTRS
   PUT-SRCS
   PUT-FUNS
   PUT-BLOCKS
   PUT-OPS
   PUT-VALUES
   PUT-EDGES ;

public

\ ---- rendering ---------------------------------------------------------------
\ Write one frozen module's diagnostic text into the caller's span and answer the
\ length written. Everything that can refuse runs before the first byte: the
\ module must be a live frozen one, it must fit the committed working set, and
\ building the inverse map asks the canonical table for the ordinals of this
\ module's own identities, which is IR-CANON's owner check and so proves the two
\ are each other's. Past that point a span too short is the only refusal left,
\ and it is named.
: RENDER ( IR-BUILD:module IR-CANON:table ptr u8 n -- n )
   {: m:IR-BUILD:module t:IR-CANON:table p:ptr room:n :}
   m t OPEN
   FITS-CK
   INV-FILL
   p room SINK!
   BODY
   OUT-U @ ;

\ ---- one row's spelling ------------------------------------------------------
\ What src/compiler/ir/diff.f names a differing row with. Each answers the length
\ written, and none of them reads the caller's bytes back.
: SYMBOL-TEXT ( IR-BUILD:module IR-ID:ir-symbol-id ptr u8 n -- n )
   {: m:IR-BUILD:module id:IR-ID:ir-symbol-id p:ptr room:n :}
   m IR-BUILD:FROZEN? 0= if E-IR-RENDER-STALE throw then
   m TAKE-VIEWS
   p room SINK!
   id IR-ID:SYMBOL-LOCAL PUT-NAME
   OUT-U @ ;

: TYPE-TEXT ( IR-BUILD:module IR-ID:ir-type-id ptr u8 n -- n )
   {: m:IR-BUILD:module id:IR-ID:ir-type-id p:ptr room:n :}
   m IR-BUILD:FROZEN? 0= if E-IR-RENDER-STALE throw then
   m TAKE-VIEWS
   p room SINK!
   id IR-ID:TYPE-LOCAL PUT-TY-SPELL
   OUT-U @ ;

: ATTR-TEXT ( IR-BUILD:module IR-CANON:table IR-ID:ir-attr-id ptr u8 n -- n )
   {: m:IR-BUILD:module t:IR-CANON:table id:IR-ID:ir-attr-id p:ptr room:n :}
   m t OPEN
   p room SINK!
   id IR-ID:ATTR-LOCAL PUT-AT-SPELL
   OUT-U @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
