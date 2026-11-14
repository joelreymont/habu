\ sumtype.f: package TYPE-DECL, the NEWTYPE, SUMTYPE and PRODUCT declaration
\ grammar (ENUM is enum-decl.f; the user grammar is docs/forth.md "Structures
\ And Enums"). Loaded unchecked in the checker prefix, right after render.f.
\ Every failure is transactional: the registry marks restore, TDECL-DIAG
\ (render.f) reports, and the declaration counts as a multi-error reject or
\ throws its code. The seven global entries are at the foot of the file.

using SCHEMA-REG
using TFAM

package TYPE-DECL

\ --- named reject codes (7101-7106 live in type-family.f, 7103 in type-schema.f).

public

7107 constant E-TDECL-SYNTAX    \ malformed declaration (name/arity/terminator/token)
7108 constant E-TDECL-ARITY     \ malformed type parameter header
7109 constant E-TDECL-PAYLOAD   \ unknown variant payload type
7110 constant E-TDECL-NAME      \ reserved or colliding family/variant name
7116 constant E-TDECL-POLICY    \ unknown or not-yet-supported layout policy (7111-7115 = checker.f E-CTOR/E-EXPORT)
7117 constant E-TDECL-RECURSIVE \ direct self-family payload under a non-boxed policy
7118 constant E-TDECL-CAP       \ a declared name past TF-NAME-MAX
7119 constant E-TDECL-DERIVE    \ unknown, deferred, or kind-gated DERIVE clause
7204 constant E-TDECL-UNRESOLVED \ quiet replay: a nominal payload declaration may arrive

private

create DECL-PARAM-NAMES TFAM-DECL-PARAM-COUNT allot
variable DECL-PARAM-N
: DECL-PARAM-CLEAR ( -- )
   0 DECL-PARAM-N !
   TFAM-DECL-PARAM-COUNT 0 ?DO 0 DECL-PARAM-NAMES i + c! LOOP ;
DECL-PARAM-CLEAR

public

\ Declaration-local names bind by list position. The registry and generated
\ effects still use positional schema indices and the canonical alphabet.
: DECL-PARAM ( n -- n bool )
   {: char:n :}
   0 BEGIN dup DECL-PARAM-N @ < WHILE
      DECL-PARAM-NAMES over + c@ char = IF RES-TRUE EXIT THEN
      1+
   REPEAT drop 0 RES-FALSE ;

: DECL-HEADER ( ptr u8 n -- ptr u8 n n bool )
   {: a:ptr u:n :}
   DECL-PARAM-CLEAR
   a u BYTE-SPAN? 0= IF a 0 0 RES-FALSE EXIT THEN
   0 BEGIN dup u < IF a over + c@ 60 <> ELSE RES-FALSE THEN WHILE 1+ REPEAT
   {: tail:n :}
   tail u = IF a u 0 RES-TRUE EXIT THEN
   u tail - 2 - {: chars:n :}
   chars 1 < chars 1 and 0= or IF a u 0 RES-FALSE EXIT THEN
   a u 1- + c@ 62 <> IF a u 0 RES-FALSE EXIT THEN
   chars 1+ 2 / {: count:n :}
   count TFAM-DECL-PARAM-COUNT > IF a u 0 RES-FALSE EXIT THEN
   count 0 ?DO
      a tail 1+ + i 2 * + c@ {: char:n :}
      char TFAM-DECL-CHAR>PARAM nip 0= IF a u 0 RES-FALSE unloop EXIT THEN
      char DECL-PARAM nip IF a u 0 RES-FALSE unloop EXIT THEN
      char DECL-PARAM-NAMES i + c!
      i 1+ DECL-PARAM-N !
      i count 1- < IF
         a tail 2 + + i 2 * + c@ 44 <> IF a u 0 RES-FALSE unloop EXIT THEN
      THEN
   LOOP
   a tail count RES-TRUE ;

private

\ Keep names already parsed so a later duplicate still rejects when an earlier
\ missing type prevented a row.
PTR-U8-TABLE-VARIABLE TDECL-SEEN-A
PTR-VARIABLE TDECL-SEEN-U
SCRATCH-VARIABLE TDECL-SEEN-A-CAP
SCRATCH-VARIABLE TDECL-SEEN-U-CAP
variable TDECL-SEEN-N
: TDECL-SEEN-CLEAR ( -- ) 0 TDECL-SEEN-N ! ;
: TDECL-SEEN-NAME ( n -- ptr ptr u8 ) TDECL-SEEN-A @ swap ptr-field ;
: TDECL-SEEN-LEN ( n -- ptr n ) cells TDECL-SEEN-U @ + ;
: TDECL-SEEN-ENSURE ( n -- ) {: need:n :}
   TDECL-SEEN-A TDECL-SEEN-A-CAP need 1 ARENA-ROWS-ENSURE
   TDECL-SEEN-U TDECL-SEEN-U-CAP need 1 ARENA-ROWS-ENSURE ;

: TDECL-SEEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 BEGIN dup TDECL-SEEN-N @ < WHILE
      dup TDECL-SEEN-NAME @  over TDECL-SEEN-LEN @
      a u CORE-STR= IF drop RES-TRUE EXIT THEN
      1+
   REPEAT drop RES-FALSE ;
: TDECL-SEEN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   TDECL-SEEN-N @ 1 + TDECL-SEEN-ENSURE
   a TDECL-SEEN-N @ TDECL-SEEN-NAME !
   u TDECL-SEEN-N @ TDECL-SEEN-LEN !
   TDECL-SEEN-N @ 1 + TDECL-SEEN-N ! ;

\ --- declaration context, set before TDECL-RUN, read by bodies and diagnostics.
PTR-VARIABLE TDK-A   variable TDK-U   \ decl kind token ("newtype"/"sumtype")
PTR-VARIABLE TDN-A   variable TDN-U   \ family name token
PTR-VARIABLE TDS-A   variable TDS-U   \ source token naming the family (DEFFAMILY)
PTR-VARIABLE TDB-A   variable TDB-U   \ body (SUMTYPE token buffer / arity token)
PTR-VARIABLE TDT-A   variable TDT-U   \ offending token (diagnostics)
PTR-VARIABLE TDW-A   variable TDW-U   \ short reason (diagnostics)
variable TDECL-CUR-FAM              \ family id being declared (-1 outside a body)

: TDECL-TOK! ( ptr u8 n -- ) TDT-U ! TDT-A ! ;
: TDECL-WHY! ( ptr u8 n -- ) TDW-U ! TDW-A ! ;

public

: TDECL-THROW ( ptr u8 n ptr u8 n n -- ) {: ta:ptr tu:n wa:ptr wu:n code:n :}
   ta tu TDECL-TOK!
   wa wu TDECL-WHY!
   code throw ;

\ --- transactional mark/restore over every store a declaration can grow.
\ The family and variant stores go back through TFAM-REWIND (type-family.f), not
\ here: their indexes chain rows by id, and a chained row whose id is out of
\ range kills the next lookup, so unchaining must precede the counters. Writing
\ the counters here left a rejected SUMTYPE's family chained past the store's
\ end; the next lookup of its tail died 76 `tfam: bad family id` while every
\ high-water mark was right. The schema store's marks go back through
\ SCHEMA-REG:REWIND, which scrubs what it retires.

private

variable TDM-TFAM   variable TDM-STR   variable TDM-PK
variable TDM-SUMV   variable TDM-LAY
variable TDM-SCH    variable TDM-ROOT
variable TDV-QUOT-N
variable TDV-QUOT-MISS

public

: QUOT-ROLLBACK ( -- ) 0 TDV-QUOT-N !  0 TDV-QUOT-MISS ! ;

private

public

variable TDECL-FAM-REG   \ family id registered by the LAST successful declaration (-1 = none)

\ PF-N / PF-COMMIT-N are NOT snapshot here. PRODUCT records fields inside the
\ one declaration transaction, whose event participant owns the field frame
\ through PREPARE, COMMIT, RELEASE or ROLLBACK. The checker-scope frame
\ independently owns the enclosing family/schema/layout marks. Thus either
\ participant can reject and the transaction restores the complete declaration;
\ SUMTYPE/ENUM/NEWTYPE never mutate PF directly.

private

: TDECL-MARK ( -- )
   TFAM-N @ TDM-TFAM !   TF-STR-U @ TDM-STR !   TF-PK-N @ TDM-PK !
   SUMV-N @ TDM-SUMV !   LAY-N @ TDM-LAY !
   SCH-N @ TDM-SCH !     SCH-ROOT-N @ TDM-ROOT ! ;
: TDECL-RESTORE ( -- )
   TDM-TFAM @  TDM-STR @  TDM-PK @  TDM-SUMV @  TDM-LAY @  TFAM-REWIND
   TDM-SCH @ TDM-ROOT @ SCHEMA-REG:REWIND
   QUOT-ROLLBACK ;

\ A DEFFAMILY packet points at the name whatever the refusal, as the source
\ token TDS: the folded DEFTYPE header the body checks has no place.
: TDECL-REPORT-TOK ( -- ptr u8 n )
   TDS-U @ 0 > IF TDS-A @ TDS-U @ EXIT THEN
   TDT-A @ TDT-U @ ;

: TDECL-REPORT ( -- )
   TDK-A @ TDK-U @  TDN-A @ TDN-U @  TDECL-REPORT-TOK  TDW-A @ TDW-U @
   TDECL-DIAG ;

\ A throw rolls the registries back; E-TDECL-UNRESOLVED propagates unreported, any
\ other renders, then counts in a multi-error load or refuses (DEF-STOPPED).
: TDECL-RUN ( [ -- ] -- )
   READ-PENDING                     \ no type entry is read inside the declaration
   TDECL-MARK
   -1 TDECL-FAM-REG !               \ set by a successful declaration only
   catch {: rc:n :}
   TDECL-SEEN-CLEAR
   rc 0= IF EXIT THEN
   TDECL-RESTORE
   rc E-TDECL-UNRESOLVED = IF rc throw THEN
   SIG-UNRES-CLEAR
   TDECL-REPORT
   MULTI-ERR? IF 1 REJECTION:TALLY +! EXIT THEN
   rc DEF-STOPPED !
   rc CHECKER-REFUSE ;

\ The declared name is read first by the body's name gate and by the report. A
\ length that describes no memory (BYTE-SPAN?) is recorded as no name, so the
\ declaration is refused as a missing name and the report reads none of it.
: TDECL-CTX! ( ptr u8 n ptr u8 n ptr u8 n -- )   \ kind, name, body
   TDB-U ! TDB-A !
   2dup BYTE-SPAN? 0= IF drop 0 THEN
   TDN-U ! TDN-A !
   TDK-U ! TDK-A !
   0 TDS-U !  NULL-PTR TDS-A !
   0 TDT-U !  NULL-PTR TDT-A !
   SIG-UNRES-CLEAR
   TDECL-SEEN-CLEAR
   -1 TDECL-CUR-FAM !
   s" declaration failed" TDECL-WHY! ;

\ Shared unterminated-sum reject: report the declaration-shaped E-BAD-DECLARATION
\ packet (name + partial body) through TDECL-RUN so every path -- native SUMTYPE,
\ the verify-source recording pass, and the check.f collector -- agrees.
\ Defined here (before TDECL-DEFSUM) so every context that sees TDECL-DEFSUM
\ also sees TDECL-DEFSUM-NOEND.
: TDECL-NOEND-BODY ( -- )
   TDN-A @ TDN-U @ s" missing ;SUMTYPE" E-TDECL-SYNTAX TDECL-THROW ;

public

: TDECL-DEFSUM-NOEND ( ptr u8 n ptr u8 n -- )   \ name, partial body -> declaration packet
   {: na:ptr nu:n ba:ptr bu:n :}
   s" sumtype" na nu ba bu TDECL-CTX!
   [: TDECL-NOEND-BODY ;] TDECL-RUN ;

\ --- name gate: reserved signature/type tokens, control words, and grammar
\ keywords may not name a family or variant (docs/forth.md "Structures And Enums"). The family
\ list is TYPE-NAME:FAMILY-RESERVED? (type-family.f), which every family definer
\ asks, so the definers cannot drift apart on it.

private

: TDECL-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TF-GRAMMAR-KEYWORD? ;

: TDECL-VAR-SCOPE? ( n -- bool )
   SUMV-FAM@ TFAM-PKG$ {: pa:ptr pu:n :}
   pu 0= IF RES-TRUE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   pa pu CHECKER-AUTH-PACKAGE$ CORE-STR=CI ;

: TDECL-VAR-TAKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SUMV-N @ 0 ?do
      i SUMV-NAME$ a u CORE-STR= IF
         i TDECL-VAR-SCOPE? IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

\ A family, variant or field name longer than TF-NAME-MAX has no room in the
\ spellings derived from it (type-family.f TF-CTOR-BUF).
: TDECL-REQUIRE-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF a u s" missing name" E-TDECL-SYNTAX TDECL-THROW THEN
   u TF-NAME-MAX > IF a u TF-NAME-LONG$ E-TDECL-CAP TDECL-THROW THEN
   a u TF-CANON? 0= IF
      a u s" name must be a lowercase family tail" E-TFAM-CASE TDECL-THROW
   THEN
   a u TYPE-NAME:FAMILY-RESERVED? IF a u s" reserved name" E-TDECL-NAME TDECL-THROW THEN ;

\ A FAMILY name must remain valid grammar and must not collide with an in-scope
\ sum variant. Exact same-package collisions fall through to TFAM-DECL's
\ E-TFAM-DUP so the diagnostic says "duplicate family". A package family may
\ share a tail with a global or another package family; TFAM-RESOLVE preserves
\ those exact identities with lexical lookup rather than rejecting the declaration.
: TDECL-REQUIRE-FAMILY-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TDECL-REQUIRE-NAME
   a u TDECL-VAR-TAKEN? IF
      a u s" collides with a sum variant" E-TDECL-NAME TDECL-THROW
   THEN ;

\ A VARIANT name lives in no scope of its own: any collision with a family
\ the declaring scope resolves (TYPE-NAME:FAMILY-TAKEN?) is a reserved name,
\ in every scope.
: TDECL-REQUIRE-VARIANT-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TDECL-REQUIRE-NAME
   a u TYPE-NAME:FAMILY-TAKEN? 0= IF EXIT THEN
   a u s" collides with a type family" E-TDECL-NAME TDECL-THROW ;

\ --- package scope: a declaration registers in the active checker package with
\ the active visibility (TFAM-ACTIVE-PKG$, type-family.f); top level is the
\ global scope, public.
: TDECL-VIS ( -- n )
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF CHECKER-PACKAGE-PUBLIC EXIT THEN
   CHECKER-AUTH-PACKAGE-MODE@ ;

\ TFAM-DECL's one failure left here is a duplicate, unless a compile callback
\ declares: the window refusal comes first, under the general reason.
: TDECL-FAMILY ( n n -- n ) {: ar:n kind:n :}
   TDN-A @ TDN-U @ TDECL-TOK!
   WRITE-WINDOW-CK                     \ src/core/checker.f WRITE-WINDOW
   s" duplicate family" TDECL-WHY!
   TFAM-ACTIVE-PKG$ TDECL-VIS TDN-A @ TDN-U @ ar kind TFAM-DECL ;

\ --- body cursor over the buffered declaration tokens (checker sig lexer).
: TDECL-CURSOR! ( ptr u8 n -- ) {: a:ptr u:n :}
   a SB!  u SL !  0 SI !
   PKRESET ;
: TDECL-NEXT ( -- ptr u8 n ) NEXT-SIG-TOK ;

\ --- variant payload elements -> schema nodes (type-schema.f header): positional
\ letter params within arity, n/f/r and concrete CT cell types, and `ptr T`.
\ Comments are not supported inside a declaration block; stray tokens reject.
variable TDECL-FAM-ARITY

: TDECL-HEADER! ( -- n )
   TDN-U @ 0= IF TDN-A @ 0 TDECL-REQUIRE-NAME THEN
   TDN-A @ TDN-U @ DECL-HEADER 0= IF
      drop 2drop
      TDN-A @ TDN-U @ s" invalid type parameter header" E-TDECL-ARITY TDECL-THROW
   THEN
   {: ar:n :}
   \ The bare name is empty only after the split, and an empty token has no place.
   dup 0= IF
      2drop TDN-A @ TDN-U @ s" missing name" E-TDECL-SYNTAX TDECL-THROW
   THEN
   TDN-U ! TDN-A !
   TDS-U @ 0 > IF TDN-U @ TDS-U ! THEN
   ar TDECL-FAM-ARITY !
   ar ;

: TDECL-LETTER ( ptr u8 n -- n ) {: a:ptr u:n :}
   a c@ {: c:n :}
   c 110 = IF CC-N SCHEMA-CON EXIT THEN
   c 102 = IF CC-BOOL SCHEMA-CON EXIT THEN
   c 114 = IF CC-R SCHEMA-CON EXIT THEN
   c DECL-PARAM 0= IF
      drop a u s" unknown payload type" E-TDECL-PAYLOAD TDECL-THROW
   THEN
   SCHEMA-PARAM ;

\ A direct self-family payload (its tail is the family being declared,
\ TDN-A/TDN-U) makes the family recursive, which only a boxed policy could lay
\ out; boxed rejects at the POLICY clause, so a self-reference always rejects
\ here as recursive. This catches the bare spelling; TDECL-PAY-FAM? repeats the
\ reject on the resolved id (TDECL-CUR-FAM) for a qualified one. Only the direct
\ case: mutual recursion (A -> B -> A) is not detected.
: TDECL-SELF-REF? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TDN-A @ TDN-U @ CORE-STR= ;

\ A bare family payload is one logical schema root whose physical width is the
\ referenced closed width: an arity-0 layout family (sum/enum/product) or cell
\ family (nominal scalar, the reference a signature admits). Resolution is
\ signature scope (TFAM-SIG-RESOLVE): a private family in another package never
\ resolves. A resolved id equal to the declared family is the qualified
\ self-reference and rejects recursive. A family UNSTORED-REASON names, linear
\ payloads and evidence families are rejected.
: TDECL-PAY-FAM? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id UNSTORED-REASON IF a u 2swap E-TDECL-PAYLOAD TDECL-THROW THEN 2drop
   id TDECL-CUR-FAM @ = IF
      a u s" invalid layout policy for recursive sum" E-TDECL-RECURSIVE TDECL-THROW
   THEN
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 RES-FALSE EXIT THEN
   id TFAM-ARITY@ 0 <> IF 0 RES-FALSE EXIT THEN
   id TFAM-CONCRETE-LINEAR? IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TDECL-ABSENT-MAY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE IF drop RES-FALSE EXIT THEN drop
   a u TFAM-MAY-ARRIVE? ;
: TDECL-PAY-ELEM ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= IF a u s" missing ;VARIANT" E-TDECL-SYNTAX TDECL-THROW THEN
   a u DELIM? IF a u s" bad payload token" E-TDECL-SYNTAX TDECL-THROW THEN
   a u TDECL-KEYWORD? IF a u s" bad payload token" E-TDECL-SYNTAX TDECL-THROW THEN
   a u s" ptr" CORE-STR= IF
      TDECL-NEXT RECURSE
      SIG-UNRES @ IF drop 0 EXIT THEN
      SCHEMA-PTR EXIT THEN
   a u TDECL-SELF-REF? IF
      a u s" invalid layout policy for recursive sum" E-TDECL-RECURSIVE TDECL-THROW
   THEN
   u 1 = IF a u TDECL-LETTER EXIT THEN
   a u CON-OF dup 0 <> IF SCHEMA-CON EXIT THEN drop
   a u TDECL-PAY-FAM? IF 0 0 SCHEMA-APP EXIT THEN drop
   a u TDECL-ABSENT-MAY? IF a u SIG-UNRES! 0 EXIT THEN
   a u s" unknown payload type" E-TDECL-PAYLOAD TDECL-THROW ;

\ --- a variant payload may also be a closed parametric application
\ `fam<arg,...>` (each argument a payload element) or a quotation type
\ `[ din -- dout | rin -- rout ]`. An application's schema is an SCH-APP of the
\ family id and argument roots; construct, MATCH and layout resolve each
\ argument against the declaring family's parameters (type-family.f TFC-*). A
\ quotation is one xt cell whose SCH-QUOT schema records all four rows, empty
\ sides included. The quotation parser is shared with STRUCTURE fields;
\ applications are variant-only, and PRODUCT fields keep the scalar grammar
\ (TDECL-PAY-ELEM).

\ Reentrant scratch for ONE nesting level's argument nodes. Each TDECL-VPAY-ARGS
\ invocation saves a base, pushes its fully-parsed argument node ids above it,
\ then copies [base,top) into CONTIGUOUS schema roots (every deeper nested root
\ already sits below argbase) and rewinds. The nodes cannot accumulate directly on
\ the schema-root pool because a nested application interleaves its own arg roots
\ between this level's arguments.
$100 constant TDV-ARG-CAP
create TDV-ARG-SCR TDV-ARG-CAP cells allot
variable TDV-ARG-N

: TDV-ARG+ ( n -- ) {: node:n :}
   TDV-ARG-N @ TDV-ARG-CAP >= IF
      TDN-A @ TDN-U @ s" type application nesting too deep" E-TDECL-PAYLOAD TDECL-THROW
   THEN
   node TDV-ARG-N @ cells TDV-ARG-SCR + !
   TDV-ARG-N @ 1 + TDV-ARG-N ! ;

defer TDV-ELEM-XT ( ptr u8 n -- n )   \ forward ref for the ELEM<->ARGS<->QUOT recursion

\ resolve a payload token as a layout/cell family in signature scope (the head of
\ a parametric application). Mirrors TDECL-PAY-FAM?'s kind gate but keeps arity
\ open -- the arity match happens in TDV-ARGS-BUILD once the args are counted.
: TDECL-VPAY-FAM? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id UNSTORED-REASON IF a u 2swap E-TDECL-PAYLOAD TDECL-THROW THEN 2drop
   id C2-READ-FAM @ = IF 0 RES-FALSE EXIT THEN
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TDV-ARGS-BUILD ( n n -- n ) {: fam:n base:n :}
   TDV-ARG-N @ base - {: cnt:n :}
   cnt fam TFAM-ARITY@ <> IF
      base TDV-ARG-N !
      TDN-A @ TDN-U @ s" type family applied to wrong number of arguments"
      E-TDECL-PAYLOAD TDECL-THROW
   THEN
   SIG-UNRES @ IF base TDV-ARG-N ! 0 EXIT THEN
   SCHEMA-ROOT-N@ {: argbase:n :}
   base BEGIN dup TDV-ARG-N @ < WHILE
      dup cells TDV-ARG-SCR + @ SCHEMA-ROOT+ drop
      1 +
   REPEAT drop
   base TDV-ARG-N !
   fam argbase cnt SCHEMA-APP ;

\ A qualified or applied self-reference resolves to the just-registered row, so
\ recursion is rejected by id.
: TDECL-VPAY-ARGS ( n -- n ) {: fam:n :}
   fam TDECL-CUR-FAM @ = IF
      TDN-A @ TDN-U @ s" invalid layout policy for recursive sum" E-TDECL-RECURSIVE TDECL-THROW
   THEN
   TDV-ARG-N @ {: base:n :}
   BEGIN
      TDECL-NEXT
      2dup s" >" CORE-STR= IF 2drop fam base TDV-ARGS-BUILD EXIT THEN
      2dup DELIM? IF s" bad type application" E-TDECL-SYNTAX TDECL-THROW THEN
      TDV-ELEM-XT TDV-ARG+
      TDECL-NEXT
      2dup s" ," CORE-STR= IF 2drop ELSE
      2dup s" >" CORE-STR= IF 2drop fam base TDV-ARGS-BUILD EXIT ELSE
         s" bad type application" E-TDECL-SYNTAX TDECL-THROW
      THEN THEN
   AGAIN ;

\ Each quotation side is an ordered row of zero or more payload elements.
\ Element ids collect in a reentrant scratch and land as one contiguous run of
\ schema roots per side, after every element's own argument roots, so each
\ SCH-ROW range stays contiguous. No `|` clause stores hasr 0 with empty rin and
\ rout rows; an explicit clause pins both.
$100 constant TDV-QUOT-CAP
create TDV-QUOT-SCR TDV-QUOT-CAP cells allot
: TDV-QUOT+ ( n [ ptr u8 n n -- ] -- ) {: node:n fail :}
   TDV-QUOT-N @ TDV-QUOT-CAP >= IF
      s" quotation effect side too long" E-TDECL-PAYLOAD fail execute THEN
   node 0= IF -1 TDV-QUOT-MISS ! THEN
   node TDV-QUOT-N @ cells TDV-QUOT-SCR + !
   TDV-QUOT-N @ 1 + TDV-QUOT-N ! ;
: TDV-QUOT-BUILD-ROW ( n -- n ) {: base:n :}
   TDV-QUOT-MISS @ IF base TDV-QUOT-N ! 0 EXIT THEN
   SCHEMA-ROOT-N@ {: estart:n :}
   TDV-QUOT-N @ base - {: cnt:n :}
   base BEGIN dup TDV-QUOT-N @ < WHILE
      dup cells TDV-QUOT-SCR + @ SCHEMA-ROOT+ drop
      1 +
   REPEAT drop
   base TDV-QUOT-N !
   estart cnt SCHEMA-ROW ;
: TDV-QUOT-EMPTY-ROW ( -- n ) SCHEMA-ROOT-N@ 0 SCHEMA-ROW ;
: TDV-QUOT-FINISH ( n n n n n n -- n )
   {: din:n dout:n rin:n rout:n hasr:n outer:n :}
   TDV-QUOT-MISS @ {: missing:n :}
   outer TDV-QUOT-MISS !
   missing 0 <> IF 0 EXIT THEN
   din dout rin rout hasr SCHEMA-QUOT ;
: TDV-QUOT-SIDE ( [ -- ptr u8 n ] [ ptr u8 n -- n ] [ ptr u8 n n -- ] -- n ptr u8 n )
   {: next elem fail :}
   TDV-QUOT-N @ {: base:n :}
   BEGIN next execute 2dup DELIM? 0= WHILE
      elem execute fail TDV-QUOT+
   REPEAT
   {: sep:ptr sepu:n :}
   base TDV-QUOT-BUILD-ROW sep sepu ;
: TDV-QUOT-EXPECT ( ptr u8 n ptr u8 n [ ptr u8 n n -- ] -- )
   {: a:ptr u:n ea:ptr eu:n fail :}
   a u ea eu CORE-STR= IF EXIT THEN
   s" malformed quotation payload" E-TDECL-SYNTAX fail execute ;

public

\ The quotation grammar is shared by variant payloads and STRUCTURE fields.
\ The caller supplies its token stream, element resolver and declaration error
\ reporter; the resulting schema and ordered effect rows are identical.
: PARSE-QUOT ( [ -- ptr u8 n ] [ ptr u8 n -- n ] [ ptr u8 n n -- ] -- n )
   {: next elem fail :}
   TDV-QUOT-MISS @ {: outer:n :}
   0 TDV-QUOT-MISS !
   next elem fail TDV-QUOT-SIDE {: din:n sep:ptr sepu:n :}
   sep sepu s" --" fail TDV-QUOT-EXPECT
   next elem fail TDV-QUOT-SIDE {: dout:n end:ptr endu:n :}
   end endu s" |" CORE-STR= IF
      next elem fail TDV-QUOT-SIDE {: rin:n rsep:ptr rsepu:n :}
      rsep rsepu s" --" fail TDV-QUOT-EXPECT
      next elem fail TDV-QUOT-SIDE {: rout:n rend:ptr rendu:n :}
      rend rendu s" ]" fail TDV-QUOT-EXPECT
      din dout rin rout -1 outer TDV-QUOT-FINISH
   ELSE
      end endu s" ]" fail TDV-QUOT-EXPECT
      din dout TDV-QUOT-EMPTY-ROW TDV-QUOT-EMPTY-ROW 0 outer TDV-QUOT-FINISH
   THEN ;

private

: TDECL-QUOT-FAIL ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   TDN-A @ TDN-U @ a u code TDECL-THROW ;
: TDECL-VPAY-QUOT ( -- n )
   [: TDECL-NEXT ;] [: TDV-ELEM-XT ;] [: TDECL-QUOT-FAIL ;] PARSE-QUOT
   SIG-UNRES @ IF drop 0 THEN ;

\ a resolved family head followed by `<` is a parametric application; any other
\ next token is pushed back and reported as not-an-application so the head falls
\ through to the bare arity-0 grammar (or its reject).
: TDECL-VPAY-APP? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u TDECL-VPAY-FAM? 0= IF drop 0 RES-FALSE EXIT THEN
   {: fam:n :}
   TDECL-NEXT 2dup s" <" CORE-STR= IF 2drop fam TDECL-VPAY-ARGS RES-TRUE EXIT THEN
   PK! 0 RES-FALSE ;

\ An absent head has no arity or layout to build. Consume its complete
\ application, checking each independently known argument with the ordinary
\ variant payload parser before the declaration is rolled back.
: TDECL-VPAY-LOOSE-APP ( -- )
   BEGIN
      TDECL-NEXT
      2dup s" >" CORE-STR= IF 2drop EXIT THEN
      2dup DELIM? IF s" bad type application" E-TDECL-SYNTAX TDECL-THROW THEN
      TDV-ELEM-XT drop
      TDECL-NEXT
      2dup s" ," CORE-STR= IF 2drop ELSE
      2dup s" >" CORE-STR= IF 2drop EXIT ELSE
         s" bad type application" E-TDECL-SYNTAX TDECL-THROW
      THEN THEN
   AGAIN ;

: TDECL-VPAY-ELEM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" [" CORE-STR= IF TDECL-VPAY-QUOT EXIT THEN
   a u s" ptr" CORE-STR= IF
      TDECL-NEXT RECURSE
      SIG-UNRES @ IF drop 0 EXIT THEN
      SCHEMA-PTR EXIT THEN
   a u TDECL-VPAY-APP? IF EXIT THEN drop
   u 1 = IF a u TDECL-PAY-ELEM EXIT THEN
   a u CON-OF dup 0 <> IF drop a u TDECL-PAY-ELEM EXIT THEN drop
   a u TDECL-ABSENT-MAY? IF
      a u SIG-UNRES!
      TDECL-NEXT 2dup s" <" CORE-STR= IF 2drop TDECL-VPAY-LOOSE-APP
      ELSE PK! THEN
      0 EXIT
   THEN
   a u TDECL-PAY-ELEM ;
: TDV-ELEM-INSTALL ( -- ) [: TDECL-VPAY-ELEM ;] is TDV-ELEM-XT ;
TDV-ELEM-INSTALL

\ --- READ-TYPE reads one type by the signature grammar (SIG-TYPE) and stores
\ the checker's term as a schema node. A definer's type has no letters: its
\ parameters come from EFFECT:NAMED. A type variable, a row variable and a
\ fresh name are letters, and a view's region slots take only those.

: READ-LETTER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u FRESH-ATOM-TOK? IF RES-TRUE EXIT THEN
   a u ROW-LEAD? IF RES-TRUE EXIT THEN
   a u SIG-VAR-SLOT 0 < IF RES-FALSE EXIT THEN
   a u SIG-FAM? nip 0= ;

\ The first letter in the signature text, a 0 for none.
: READ-LETTER ( -- ptr u8 n )
   0 SI !  PKRESET
   BEGIN NEXT-SIG-TOK dup 0 <> WHILE
      2dup READ-LETTER? IF EXIT THEN 2drop
   REPEAT ;

\ Stages node on the argument scratch; false when it is full.
: READ-STAGE ( n -- bool ) {: node:n :}
   TDV-ARG-N @ TDV-ARG-CAP >= IF RES-FALSE EXIT THEN
   node TDV-ARG-N @ cells TDV-ARG-SCR + !
   TDV-ARG-N @ 1 + TDV-ARG-N !  RES-TRUE ;

\ Roots the nodes staged from base on as one contiguous run, the last staged
\ first when flip, unstages them and answers the run's start.
: READ-ROOTS ( n bool -- n ) {: base:n flip:bool :}
   SCHEMA-ROOT-N@ {: start:n :}
   TDV-ARG-N @ base - 0 ?do
      flip IF TDV-ARG-N @ 1 - i - ELSE base i + THEN
      cells TDV-ARG-SCR + @ SCHEMA-ROOT+ drop
   loop
   base TDV-ARG-N !  start ;

defer TERM-SCHEMA-XT ( n -- n )

\ Stages term t's node; false when it has none or the scratch is full.
: READ-STAGE-TERM ( n -- bool )
   TERM-SCHEMA-XT dup 0= IF drop RES-FALSE EXIT THEN READ-STAGE ;

: READ-APP ( n -- n ) {: t:n :}
   TDV-ARG-N @ {: base:n :}
   t PARAM>ARGC 0 ?do
      t i PARAM>ARG READ-STAGE-TERM 0= IF base TDV-ARG-N ! 0 unloop EXIT THEN
   loop
   t PARAM>FAM  base RES-FALSE READ-ROOTS  t PARAM>ARGC SCHEMA-APP ;

\ A quotation side, whose top cell the term holds first, as a SCH-ROW node of
\ its cells deepest first.
: READ-SIDE ( n -- n ) {: row:n :}
   TDV-ARG-N @ {: base:n :}
   row R-RES BEGIN dup TAG S-PUSH = WHILE
      dup P>TYPE READ-STAGE-TERM 0= IF drop base TDV-ARG-N ! 0 EXIT THEN
      P>REST R-RES
   REPEAT drop
   TDV-ARG-N @ base - {: k:n :}
   base RES-TRUE READ-ROOTS k SCHEMA-ROW ;

\ A quotation whose data sides share their tail and whose return sides are
\ one empty row: `| -- ` is neutral, so it stores as no clause.
: READ-QUOT ( n -- n ) {: q:n :}
   q Q>RIN R-RES {: rin:n :}
   rin TAG S-PUSH =  rin q Q>ROUT R-RES <> or IF 0 EXIT THEN
   q Q>DIN ROW-TAIL q Q>DOUT ROW-TAIL <> IF 0 EXIT THEN
   q Q>DIN READ-SIDE dup 0= IF EXIT THEN {: din:n :}
   q Q>DOUT READ-SIDE dup 0= IF EXIT THEN {: dout:n :}
   din dout TDV-QUOT-EMPTY-ROW TDV-QUOT-EMPTY-ROW 0 SCHEMA-QUOT ;

\ The schema node of a closed type's term, 0 for one no node holds: an atom,
\ a binder or a variable.
: TERM-SCHEMA ( n -- n )
   T-RES {: t:n :}
   t TAG T-CON = IF t PAY SCHEMA-CON EXIT THEN
   t TAG T-PTR = IF
      t PTR>INNER TERM-SCHEMA-XT dup 0= IF EXIT THEN SCHEMA-PTR EXIT
   THEN
   t TAG T-PARAM = IF t READ-APP EXIT THEN
   t TAG T-QUOT = IF t READ-QUOT EXIT THEN
   0 ;
: TERM-SCHEMA-INSTALL ( -- ) [: TERM-SCHEMA ;] is TERM-SCHEMA-XT ;
TERM-SCHEMA-INSTALL

\ The parse's reject: the token SGBAD latched and why.
: READ-BAD ( [ ptr u8 n ptr u8 n n -- ] -- ) {: fail :}
   SGBAD-A @ SGBAD-U @
   SGBAD-UNKNOWN? IF s" unknown type" E-TDECL-PAYLOAD fail execute EXIT THEN
   SGBAD-ARITY? IF
      s" type family applied to wrong number of arguments"
      E-TDECL-SYNTAX fail execute EXIT
   THEN
   s" bad type" E-TDECL-SYNTAX fail execute ;

public

\ The one type the text spells, read by the signature grammar where the
\ definer runs. A letter in it runs LETTER with the letter; any other reject,
\ a name that may arrive later and text past the type included, runs FAIL
\ with the token, the reason and the code.
: READ-TYPE ( ptr u8 n [ ptr u8 n ptr u8 n n -- ] [ ptr u8 n -- ] -- n )
   {: a:ptr u:n fail letter :}
   a SB!  a u BYTE-SPAN? IF u ELSE 0 THEN SL !
   READ-LETTER {: la:ptr lu:n :}
   lu 0 <> IF la lu letter execute 0 EXIT THEN
   SIG-UNRES-CLEAR  SGBAD-CLEAR  0 TDV-ARG-N !
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET  0 SQUOT-N !  0 BIND-DEPTH !
   0 SIG-LEVEL !  0 SGHASR !  0 RR-SHARED !  0 SI !
   NEXT-SIG-TOK {: ta:ptr tu:n :}
   tu 0= IF ta 0 s" missing type" E-TDECL-SYNTAX fail execute 0 EXIT THEN
   ta tu SIG-TYPE {: term:n :}
   NEXT-SIG-TOK {: ea:ptr eu:n :}
   SGBAD @ IF fail READ-BAD 0 EXIT THEN
   CHECKER-SIG-UNRES-TAKE IF
      s" unknown type" E-TDECL-PAYLOAD fail execute 0 EXIT
   THEN 2drop
   eu 0 <> IF ea eu s" one type expected" E-TDECL-SYNTAX fail execute 0 EXIT THEN
   term TERM-SCHEMA dup 0 <> IF EXIT THEN drop
   a 0 s" no stored type holds it" E-TDECL-SYNTAX fail execute 0 ;

private

variable TDV-TAG   variable TDV-N    variable TDV-MAX
variable TDV-SS    variable TDV-PC   variable TDV-PW
PTR-VARIABLE TDV-NA    variable TDV-NU

: TDECL-VARIANT-NAME ( -- )
   TDECL-NEXT {: a:ptr u:n :}
   a u TDECL-REQUIRE-VARIANT-NAME
   a TDV-NA !  u TDV-NU ! ;

: TDECL-VARIANT-CLOSE ( n -- ) {: fam:n :}
   TDV-NA @ TDV-NU @ TDECL-TOK!
   s" duplicate variant" TDECL-WHY!
   fam TDV-NA @ TDV-NU @ TDV-TAG @ TDV-SS @ TDV-PC @ TDV-PW @ SUMV-ADD drop
   TDV-PW @ TDV-MAX @ max TDV-MAX !
   TDV-TAG @ 1 + TDV-TAG !
   TDV-N @ 1 + TDV-N ! ;

\ Application arguments and quotation effect rows push their roots into the
\ schema-root pool while the element parses, so element nodes collect here and
\ land as one contiguous block after every argument root (TDECL-VARIANT-FINISH).
\ Variants never nest, so one scratch suffices.
$100 constant TDV-PAY-CAP
create TDV-PAY-SCR TDV-PAY-CAP cells allot
variable TDV-PAY-N
: TDV-PAY+ ( n -- ) {: node:n :}
   TDV-PAY-N @ TDV-PAY-CAP >= IF
      TDN-A @ TDN-U @ s" too many variant payload elements" E-TDECL-PAYLOAD TDECL-THROW THEN
   node TDV-PAY-N @ cells TDV-PAY-SCR + !
   TDV-PAY-N @ 1 + TDV-PAY-N ! ;
\ Every SCH-PARAM in a landed variant's payload subtree (plain element,
\ application argument, quotation side) must lie within the declaring family's
\ arity (TFAM-SCH-ARITY); an out-of-arity parameter rejects inside TDECL-RUN's
\ transaction. This is the authoritative check, beside PRODUCT's PF-SCHEMA-OK?;
\ the TDECL-LETTER parse gate is not. The letter renders through the shared
\ declaration alphabet.
create TDV-BADLETTER 1 allot
: TDECL-VPAY-ARITY ( n n n -- ) {: fam:n base:n count:n :}
   0 BEGIN dup count < WHILE
      base over + SCHEMA-ROOT@ fam swap TFAM-SCH-ARITY
      dup 0 >= IF
         TFAM-DECL-PARAM>CHAR 0= IF
            drop TDN-A @ TDN-U @ s" payload parameter has no declaration spelling"
            E-TDECL-PAYLOAD TDECL-THROW
         THEN
         TDV-BADLETTER c!
         TDV-BADLETTER 1 s" payload parameter out of family arity"
         E-TDECL-PAYLOAD TDECL-THROW
      THEN
      drop 1 +
   REPEAT drop ;
: TDECL-VARIANT-FINISH ( n -- ) {: fam:n :}
   SIG-UNRES @ IF
      TDV-NA @ TDV-NU @ TDECL-SEEN? IF
         TDV-NA @ TDV-NU @ s" duplicate variant" E-TFAM-DUP TDECL-THROW THEN
   THEN
   TDV-NA @ TDV-NU @ TDECL-SEEN+
   SIG-UNRES @ IF EXIT THEN
   SCHEMA-ROOT-N@ TDV-SS !
   0 TDV-PC !
   0 BEGIN dup TDV-PAY-N @ < WHILE
      dup cells TDV-PAY-SCR + @ SCHEMA-ROOT+ drop
      TDV-PC @ 1 + TDV-PC !
      1 +
   REPEAT drop
   fam TDV-SS @ TDV-PC @ TDECL-VPAY-ARITY
   fam TDECL-VARIANT-CLOSE ;
: TDECL-VARIANT ( n -- ) {: fam:n :}
   TDECL-VARIANT-NAME
   0 TDV-PW !  0 TDV-ARG-N !  0 TDV-PAY-N !
   BEGIN
      TDECL-NEXT
      2dup s" ;variant" CORE-STR=CI IF 2drop fam TDECL-VARIANT-FINISH EXIT THEN
      TDECL-VPAY-ELEM
      SIG-UNRES @ IF drop ELSE
         dup fam swap TFAM-SCH-WIDTH TDV-PW @ + TDV-PW ! TDV-PAY+
      THEN
   AGAIN ;

: TDECL-SUM-VARIANTS ( n -- ) {: fam:n :}
   BEGIN
      TDECL-NEXT
      dup 0= IF 2drop EXIT THEN
      2dup s" variant" CORE-STR=CI 0= IF
         s" unexpected token in sum declaration" E-TDECL-SYNTAX TDECL-THROW
      THEN
      2drop fam TDECL-VARIANT
   AGAIN ;

\ A PUBLIC family derives its constructor package once and records it in every
\ variant's SV.CTOR-PKG, keyed by family id so same-tail families in different
\ packages get disjoint namespaces. A PRIVATE family's slot stays empty: a
\ private PRODUCT spells its generated words with TF-CTOR-PRIV$ into the
\ declaring package's private wordlist; a private SUM or ENUM publishes no word
\ and constructs through `construct`. The name is interned after the escaped
\ spelling is built, so the transient package/tail pointers are consumed before
\ any pool grow.

public

: TDECL-CTOR-PUBLISH ( n n n -- ) {: fam:n vstart:n count:n :}
   fam TFAM-GEN-PUBLIC? 0= IF EXIT THEN
   TDN-A @ TDN-U @ TDECL-TOK!
   s" constructor package name exceeds 255 bytes" TDECL-WHY!
   fam TFAM-PKG$ fam TFAM-NAME$ TF-CTOR-PKG$ {: ca:ptr cu:n :}
   s" declaration failed" TDECL-WHY!
   ca cu TF-INTERN {: coff:n :}
   count 0 DO  vstart i +  coff cu SUMV-CTOR-PKG!  LOOP ;

\ `POLICY <name>` follows the arity (sum/product) or the name (enum), before the
\ first VARIANT/FIELD; a missing clause leaves TL-STACK-CELL-TAG. stack-cell-tag
\ and packed-tag are accepted: packed keeps the stack representation and bakes a
\ LAY memory descriptor at close (TDECL-LAYOUT-DESC). niche-null is accepted for
\ one shape, checked once the variants are read; boxed rejects as not supported
\ until codegen lowers it. Any other token, `custom` included (the LAY registry
\ tolerates it), is an unknown policy.

private

: TDECL-POLICY-DEFERRED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" boxed" CORE-STR=CI ;
: TDECL-POLICY-SET ( ptr u8 n n -- ) {: a:ptr u:n fam:n :}
   a u s" stack-cell-tag" CORE-STR=CI IF fam TL-STACK-CELL-TAG TFAM-LAYOUT! EXIT THEN
   a u s" packed-tag" CORE-STR=CI IF fam TL-PACKED-TAG TFAM-LAYOUT! EXIT THEN
   a u s" niche-null" CORE-STR=CI IF fam TL-NICHE TFAM-LAYOUT! EXIT THEN
   a u TDECL-POLICY-DEFERRED? IF
      a u s" layout policy not yet supported" E-TDECL-POLICY TDECL-THROW
   THEN
   a u s" unknown layout policy" E-TDECL-POLICY TDECL-THROW ;
\ Bake the packed memory descriptor at declaration close, when the variant /
\ field counts that PACKED-DESC reads are final. stack-cell-tag families bake
\ no LAY row (nothing consumes one). Runs inside TDECL-RUN's transaction, so a
\ later close-stage reject rolls LAY-N back with the family row.

public

: TDECL-LAYOUT-DESC ( n -- ) {: fam:n :}
   fam TFAM-LAYOUT-POLICY@ TL-PACKED-TAG = 0= IF EXIT THEN
   fam TL-PACKED-TAG fam PACKED-DESC LAY-ADD drop ;
: TDECL-POLICY ( n -- ) {: fam:n :}
   TDECL-NEXT {: a:ptr u:n :}
   a u s" policy" CORE-STR=CI 0= IF a u PK! EXIT THEN     \ no clause: push the token back for the body loop
   TDECL-NEXT {: pa:ptr pu:n :}
   pu 0= IF TDN-A @ TDN-U @ s" missing layout policy name" E-TDECL-POLICY TDECL-THROW THEN
   pa pu fam TDECL-POLICY-SET ;

\ A niche-null value is one cell whose zero is the empty first variant, so the
\ family is that variant and one holding a single `ptr` field.
: TDECL-NICHE-MESSAGE$ ( -- ptr u8 n )
   s" niche-null needs an empty variant, then one ptr field" ;

private

: TDECL-NICHE-THROW ( -- )
   TDN-A @ TDN-U @ TDECL-NICHE-MESSAGE$ E-TDECL-POLICY TDECL-THROW ;
: TDECL-NICHE-SHAPE? ( n -- bool ) {: vstart:n :}
   TDV-N @ 2 <> IF RES-FALSE EXIT THEN
   vstart SUMV-SCH-COUNT@ 0 <> IF RES-FALSE EXIT THEN
   vstart 1 + SUMV-SCH-COUNT@ 1 <> IF RES-FALSE EXIT THEN
   vstart 1 + SUMV-SCH-START@ SCHEMA-ROOT@ SCHEMA-PTR? ;
: TDECL-NICHE-REQUIRE ( n n -- ) {: fam:n vstart:n :}
   fam TFAM-NICHE? 0= IF EXIT THEN
   vstart TDECL-NICHE-SHAPE? IF EXIT THEN
   TDECL-NICHE-THROW ;

\ `DERIVE <feature>...` follows the POLICY position, before the variants/fields.
\ eq and hash need a PUBLIC arity-0 family, init a product; order rejects as not
\ supported; an unknown first token rejects, and the list ends at the first
\ non-feature token by pushback. With eq or hash, payload roles are checked per
\ variant/field after the block parses (TDECL-DERIVE-REQUIRE): integer scalars
\ compare with `=`; pointer payloads have no derived equality (no checked
\ pointer-equality surface exists); other scalars reject, linear ones because
\ comparing consumes them; a family-typed field compares through its family's
\ EQ and hashes through its HASH, so that family derives what the outer does.

: TDECL-DERIVE-GUARD ( ptr u8 n n -- ) {: a:ptr u:n fam:n :}
   fam TFAM-PUBLIC? 0= IF
      a u s" derive requires a public family" E-TDECL-DERIVE TDECL-THROW THEN
   fam TFAM-ARITY@ 0 <> IF
      a u s" derive requires a concrete (arity 0) family" E-TDECL-DERIVE TDECL-THROW THEN ;
: TDECL-DERIVE-SET ( ptr u8 n n -- ) {: a:ptr u:n fam:n :}
   a u s" eq" CORE-STR=CI IF
      a u fam TDECL-DERIVE-GUARD  fam TFAM-DERIVE-EQ! EXIT THEN
   a u s" hash" CORE-STR=CI IF
      a u fam TDECL-DERIVE-GUARD  fam TFAM-DERIVE-HASH! EXIT THEN
   a u s" init" CORE-STR=CI IF
      fam TFAM-PRODUCT? 0= IF
         a u s" derive init requires a product" E-TDECL-DERIVE TDECL-THROW THEN
      fam TFAM-DERIVE-INIT! EXIT THEN
   a u s" order" CORE-STR=CI IF
      a u s" derive feature not yet supported" E-TDECL-DERIVE TDECL-THROW THEN
   a u s" unknown derive feature" E-TDECL-DERIVE TDECL-THROW ;
: TDECL-DERIVE-MORE? ( n -- bool ) {: fam:n :}
   TDECL-NEXT {: a:ptr u:n :}
   u 0= IF RES-FALSE EXIT THEN
   a u s" eq" CORE-STR=CI
   a u s" hash" CORE-STR=CI or
   a u s" init" CORE-STR=CI or
   a u s" order" CORE-STR=CI or
   0= IF a u PK! RES-FALSE EXIT THEN
   a u fam TDECL-DERIVE-SET
   RES-TRUE ;
: TDECL-DERIVE ( n -- ) {: fam:n :}
   TDECL-NEXT {: a:ptr u:n :}
   a u s" derive" CORE-STR=CI 0= IF a u PK! EXIT THEN
   TDECL-NEXT {: fa:ptr fu:n :}
   fu 0= IF TDN-A @ TDN-U @ s" missing derive feature" E-TDECL-DERIVE TDECL-THROW THEN
   fa fu fam TDECL-DERIVE-SET          \ the FIRST token must be a feature (unknown rejects);
   BEGIN fam TDECL-DERIVE-MORE? 0= UNTIL ;   \ later tokens end the order-free list by pushback

\ a DERIVE-marked family must not declare a variant spelled like a generated
\ derived tail: the ctor package would hold two words with one name.
public

: TDECL-DERIVE-COLLIDE ( n n n -- ) {: fam:n vstart:n count:n :}
   fam TFAM-DERIVE-ANY? 0= IF EXIT THEN
   vstart count + vstart ?do
      i SUMV-NAME$ TFAM-DERIVED-TAIL? IF
         i SUMV-NAME$
         s" variant name collides with a derived word" E-TDECL-NAME TDECL-THROW THEN
   loop ;

\ payload-role gate: every payload/field schema node must be
\ derivably comparable, else the DECLARATION rejects naming the exact role.

private

: TDECL-DERIVE-FIELD-OK ( n n -- ) {: fam:n node:n :}
   node SCHEMA-A@ {: ff:n :}
   ff TFAM-GEN-PUBLIC? 0= IF
      ff TFAM-PKG$ fam TFAM-PKG$ CORE-STR=CI 0= IF
         ff TFAM-NAME$
         s" field family's derived words are private to its package"
         E-TDECL-DERIVE TDECL-THROW THEN THEN
   fam TFAM-DERIVE-EQ? ff TFAM-DERIVE-EQ? 0= and IF
      ff TFAM-NAME$
      s" field family must also derive eq" E-TDECL-DERIVE TDECL-THROW THEN
   fam TFAM-DERIVE-HASH? ff TFAM-DERIVE-HASH? 0= and IF
      ff TFAM-NAME$
      s" field family must also derive hash" E-TDECL-DERIVE TDECL-THROW THEN ;

: TDECL-DERIVE-NODE-OK ( n n -- ) {: fam:n node:n :}
   node SCHEMA-CON? IF
      node SCHEMA-A@ CT-INT? 0= IF
         node SCHEMA-A@ CT-NAME$
         s" payload type has no derived equality" E-TDECL-DERIVE TDECL-THROW THEN
      EXIT THEN
   node SCHEMA-PTR? IF
      TDN-A @ TDN-U @
      s" pointer payloads have no derived equality" E-TDECL-DERIVE TDECL-THROW THEN
   node SCHEMA-APP? IF fam node TDECL-DERIVE-FIELD-OK EXIT THEN
   TDN-A @ TDN-U @
   s" payload role has no derived equality" E-TDECL-DERIVE TDECL-THROW ;

: TDECL-DERIVE-VARIANT ( n n -- ) {: fam:n vid:n :}
   vid SUMV-PAY-N 0 ?do
      fam vid i SUMV-PAY-ROOT SCHEMA-ROOT@ TDECL-DERIVE-NODE-OK
   loop ;

public

: TDECL-DERIVE-REQUIRE ( n n n -- ) {: fam:n vstart:n count:n :}
   fam TFAM-DERIVE-EQ? fam TFAM-DERIVE-HASH? or 0= IF EXIT THEN
   vstart count + vstart ?do fam i TDECL-DERIVE-VARIANT loop ;

\ --- registration entry points (verify-source and the definers below).

private

: CHECKER-DEFSUM-BODY ( -- )
   TDECL-HEADER! {: ar:n :}
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-CURSOR!
   ar TK-SUM TDECL-FAMILY {: fam:n :}
   fam TDECL-CUR-FAM !                    \ payload self-references reject by id
   fam TDECL-POLICY
   fam TDECL-DERIVE
   SUMV-N @ {: vstart:n :}
   0 TDV-TAG !  0 TDV-N !  0 TDV-MAX !
   fam TDECL-SUM-VARIANTS
   SIG-UNRES @ IF E-TDECL-UNRESOLVED throw THEN
   TDV-N @ 0= IF TDN-A @ TDN-U @ s" empty sum" E-TDECL-SYNTAX TDECL-THROW THEN
   fam vstart TDECL-NICHE-REQUIRE
   fam vstart TDV-N @ TDECL-DERIVE-COLLIDE
   fam vstart TDV-N @ TDECL-DERIVE-REQUIRE
   fam vstart TDV-N @ TFAM-VAR-RANGE!
   fam TDV-MAX @ TFAM-SLOTS!
   fam TDECL-LAYOUT-DESC
   fam vstart TDV-N @ TDECL-CTOR-PUBLISH
   fam TDECL-FAM-REG ! ;


public

: TDECL-DEFSUM ( ptr u8 n ptr u8 n -- )   \ name, buffered body tokens
   {: na:ptr nu:n ba:ptr bu:n :}
   s" sumtype" na nu ba bu TDECL-CTX!
   [: CHECKER-DEFSUM-BODY ;] TDECL-RUN ;

\ A PRODUCT is a single-shape TK-PRODUCT record (MAKE and UNMAKE: TDECL-PROD-PLAN). Field tails are
\ their own namespace, one-letter names legal; linear instantiations are refused at the sig and bind layers.

private

variable TDP-N   \ product field count (schema roots)
variable TDP-W   \ cumulative product cell width / next field's cell OFFSET
variable TDP-EVT


public

defer TDECL-TXN-XT ( [ -- ] -- )
variable TDECL-TXN-ARMED
\ The declaration owner installs its runner once, at load; a second install is refused,
\ so the public word cannot replace the transaction runner afterwards.
: TXN-INSTALL ( [ [ -- ] -- ] -- )
   TDECL-TXN-ARMED @ IF s" sumtype: declaration transaction already installed" 76 die THEN
   is TDECL-TXN-XT  -1 TDECL-TXN-ARMED ! ;
defer TDECL-EVENT-CURRENT-XT ( -- n )
defer TDECL-EVENT-DECL-XT ( n n -- n )
defer TDECL-EVENT-ARITY-XT ( n n n -- n )
defer TDECL-EVENT-FIELD-XT ( n n ptr u8 n n n n n n n n -- n )
defer TDECL-EVENT-ARMED ( -- bool )


private

: TDECL-REQUIRE-FIELD-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF a u s" missing field name" E-TDECL-SYNTAX TDECL-THROW THEN
   u TF-NAME-MAX > IF a u TF-NAME-LONG$ E-TDECL-CAP TDECL-THROW THEN
   a u DELIM? IF a u s" bad field name" E-TDECL-SYNTAX TDECL-THROW THEN
   a u TF-CANON? 0= IF
      a u s" field name must be a lowercase tail" E-TFAM-CASE TDECL-THROW
   THEN
   a u TDECL-TOK!
   s" reserved field name" TDECL-WHY!
   a u PF-NAME-REQUIRE ;

\ A PRODUCT field may be typed as an arity-0 sum or enum family of registry
\ width 1 (arity 0: no argument can be linear). Its schema is an SCH-APP holding
\ the resolved family id, so MAKE consumes it as its family, UNMAKE reproduces
\ it, and a swapped same-width enum field is a checker reject. Resolution is
\ signature scope (TFAM-SIG-RESOLVE, in TFAM-RESOLVE's order; an ambiguity is
\ unresolved). Anything else falls through to the payload grammar, which admits
\ concrete non-linear layout/cell families (self-references reject recursive)
\ and rejects parametric, linear and unknown types (E-TDECL-PAYLOAD).
: TDECL-FIELD-FAM? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF 0 RES-FALSE EXIT THEN
   id TFAM-ARITY@ 0 <> IF 0 RES-FALSE EXIT THEN
   id TFAM-WIDTH@ 1 <> IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TDECL-FIELD-ELEM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u TDECL-FIELD-FAM? IF 0 0 SCHEMA-APP EXIT THEN drop
   a u TDECL-PAY-ELEM ;

\ The `field` keyword is already consumed. PRODUCT fields use the canonical CELL
\ payload ABI of stack-cell-tag and packed-tag; other policies stay refused in PF.
: TDECL-PRODUCT-FIELD ( n -- ) {: fam:n :}
   TDECL-NEXT {: fna:ptr fnu:n :}
   fna fnu TDECL-REQUIRE-FIELD-NAME
   SCHEMA-ROOT-N@ {: ss:n :}
   TDECL-NEXT TDECL-FIELD-ELEM {: node:n :}
   SIG-UNRES @ IF
      fna fnu TDECL-SEEN? IF
         fna fnu s" duplicate field" E-TFAM-DUP TDECL-THROW THEN
   THEN
   fna fnu TDECL-SEEN+
   SIG-UNRES @ IF EXIT THEN
   node SCHEMA-ROOT+ drop    \ one field type (family/letter/con/ptr T)
   fam ss SCHEMA-ROOT@ TFAM-SCH-WIDTH {: fw:n :}
   fna fnu TDECL-TOK!
   s" duplicate field" TDECL-WHY!
   TDP-EVT @ fam fna fnu ss
   TDP-W @ fw TDP-W @ cells fw cells CELL PF-FLAGS-NONE
   TDECL-EVENT-FIELD-XT TDP-EVT !
   fw TDP-W @ + TDP-W !
   TDP-N @ 1 + TDP-N ! ;

: TDECL-PRODUCT-FIELDS ( n -- ) {: fam:n :}
   BEGIN
      TDECL-NEXT
      dup 0= IF 2drop EXIT THEN
      2dup s" field" CORE-STR=CI 0= IF
         s" unexpected token in product declaration" E-TDECL-SYNTAX TDECL-THROW
      THEN
      2drop fam TDECL-PRODUCT-FIELD
   AGAIN ;

\ the two generated-word rows: field schemas are appended one root per field,
\ so the ctor payload range is [rstart, rstart+field-count) — both rows share
\ it; the rows' PAYCELLS is the CELL width sum (TDP-W), not the field count.
: TDECL-PRODUCT-ROWS ( n n -- ) {: fam:n rstart:n :}
   SUMV-N @ {: vstart:n :}
   fam s" make"   0 rstart TDP-N @ TDP-W @ SUMV-ADD drop
   fam s" unmake" 1 rstart TDP-N @ TDP-W @ SUMV-ADD drop
   fam vstart 2 TFAM-VAR-RANGE!
   fam vstart 2 TDECL-CTOR-PUBLISH ;

: TDECL-PRODUCT-RECORD ( n -- ) {: fam:n :}
   TDECL-EVENT-CURRENT-XT TDP-EVT !
   TDP-EVT @ fam TDECL-EVENT-DECL-XT TDP-EVT !
   TDP-EVT @ fam TDECL-FAM-ARITY @ TDECL-EVENT-ARITY-XT TDP-EVT !
   PF-N @ {: fstart:n :}
   SCHEMA-ROOT-N@ {: rstart:n :}
   0 TDP-N !   0 TDP-W !
   fam TDECL-PRODUCT-FIELDS
   SIG-UNRES @ IF E-TDECL-UNRESOLVED throw THEN
   TDP-N @ 0= IF TDN-A @ TDN-U @ s" empty product" E-TDECL-SYNTAX TDECL-THROW THEN
   fam fstart TDP-N @ TFAM-FLD-RANGE!
   fam TDP-W @ TFAM-SLOTS!
   fam TDECL-LAYOUT-DESC
   fam rstart TDECL-PRODUCT-ROWS
   fam fam TFAM-VAR-START@ 1 TDECL-DERIVE-REQUIRE ;

\ Both entries run the body inside one declaration transaction: field rows go
\ through DECL-EVENT, which only an open transaction admits. TDECL-RUN stays
\ outside it, so a reject it swallows under a multi-error load is already rolled
\ back (DECL-EVENT rows, field rows, constructors), as DECL-FRONT's driver
\ stands outside its transaction (generated-declaration.f). Each transaction step
\ is a named word because hb-stage0 compiles this file and cannot nest
\ quotations.
: TDECL-PRODUCT-TXN ( [ -- ] -- )
   TDECL-EVENT-ARMED 0= TDECL-TXN-ARMED @ 0= or IF
      s" product: declaration event transaction not installed" 76 die
   THEN
   TDECL-TXN-XT ;

: CHECKER-DEFPRODUCT-BODY ( -- )
   TDECL-HEADER! {: ar:n :}
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-CURSOR!
   ar TK-PRODUCT TDECL-FAMILY {: fam:n :}
   fam TDECL-CUR-FAM !                    \ field self-references reject by id
   fam TDECL-POLICY
   fam TFAM-NICHE? IF TDECL-NICHE-THROW THEN
   fam TDECL-DERIVE
   fam TDECL-PRODUCT-RECORD
   fam TDECL-FAM-REG ! ;

: TDECL-DEFPRODUCT-TXN ( -- ) [: CHECKER-DEFPRODUCT-BODY ;] TDECL-PRODUCT-TXN ;


public

: TDECL-DEFPRODUCT ( ptr u8 n ptr u8 n -- )   \ name, buffered body tokens
   {: na:ptr nu:n ba:ptr bu:n :}
   s" product" na nu ba bu TDECL-CTX!
   [: TDECL-DEFPRODUCT-TXN ;] TDECL-RUN ;


private

: CHECKER-DEFFAMILY-BODY ( -- )
   TDECL-HEADER! {: ar:n :}
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   ar TK-CELL TDECL-FAMILY TDECL-FAM-REG ! ;


public

\ The family header and the source token that names it, the same bytes up to
\ case: a DEFTYPE name folds to its header, a NEWTYPE name is its own token.
: TDECL-DEFFAMILY ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n ta:ptr tu:n :}
   s" newtype" na nu s" " TDECL-CTX!
   ta tu 2dup BYTE-SPAN? 0= IF drop 0 THEN
   TDS-U ! TDS-A !
   [: CHECKER-DEFFAMILY-BODY ;] TDECL-RUN ;

defer TDECL-NAME-PREFLIGHT-XT ( ptr u8 n -- )
defer TDECL-CAPACITY-PREFLIGHT-XT ( ptr u8 n n -- )

private

CAST: AS-STORAGE-RUN ( n -- [ ptr u8 n n n ptr u8 n -- n ] )

: TDECL-CHILD-RUN ( ptr u8 n n n ptr u8 n -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-STORAGE-RUN execute ;

CAST: AS-OPEN ( n -- [ ptr u8 n n -- ] )

: TDECL-DEFINE-RUN ( ptr u8 n n -- )
   data-base PROVIDED-XT:DEFINE-CELL + @ AS-OPEN execute ;

CAST: AS-ITEM ( n -- [ ptr u8 n n n -- ] )

\ Appends to the open definition the item of ITEM-ABI op OP, spelled A U,
\ with argument V.
: TDGEN-ITEM ( ptr u8 n n n -- )
   data-base PROVIDED-XT:ITEM-CELL + @ AS-ITEM execute ;

\ Makes the child word NAME through CHILD-RUN, of a CHILD-ABI SHAPE and PARAM.
: TDECL-CHILD ( ptr u8 n n n -- )
   {: na:ptr nu:n shape:n param:n :}
   na nu shape param NULL-PTR 0 TDECL-CHILD-RUN drop ;

public

\ Makes the stored child NAME over TYPE, as TDECL-CHILD makes a child, and
\ answers its effect handle.
: TDECL-STORED-CHILD ( ptr u8 n ptr u8 n -- n )
   {: na:ptr nu:n ta:ptr tu:n :}
   na nu CHILD-ABI:STORED 0 ta tu TDECL-CHILD-RUN ;

\ Makes the stored child NAME over type node NODE, as TDECL-STORED-CHILD does.
: TDECL-NODE-CHILD ( ptr u8 n n -- n ) {: na:ptr nu:n node:n :}
   na nu CHILD-ABI:STORED node NULL-PTR 0 TDECL-CHILD-RUN ;

\ The structures.f field words, OFF bytes into a record.
: TDECL-CELL-CHILD ( ptr u8 n n -- ) {: na:ptr nu:n off:n :}
   na nu CHILD-ABI:CELL-OFF off TDECL-CHILD ;

: TDECL-PTR-CHILD ( ptr u8 n n -- ) {: na:ptr nu:n off:n :}
   na nu CHILD-ABI:PTR-OFF off TDECL-CHILD ;

: TDECL-BYTE-CHILD ( ptr u8 n n -- ) {: na:ptr nu:n off:n :}
   na nu CHILD-ABI:BYTE-OFF off TDECL-CHILD ;


private

: TDECL-NAME-PREFLIGHT-MISSING ( ptr u8 n -- )
   2drop s" sumtype: generated name preflight not installed" 76 die ;

: TDECL-CAPACITY-PREFLIGHT-MISSING ( ptr u8 n n -- )
   2drop drop s" sumtype: generated capacity preflight not installed" 76 die ;

: TDECL-PREFLIGHT-DEFAULTS ( -- )
   [: TDECL-NAME-PREFLIGHT-MISSING ;] is TDECL-NAME-PREFLIGHT-XT
   [: TDECL-CAPACITY-PREFLIGHT-MISSING ;] is TDECL-CAPACITY-PREFLIGHT-XT
   [: 0 0= 0= ;] is TDECL-EVENT-ARMED ;
TDECL-PREFLIGHT-DEFAULTS

\ Two TF-NAME-MAX tails joined by ':' or '-', the second maybe ending in @ or !.
TF-NAME-MAX 2 * 2 + constant TDGEN-CAP
create TDGEN-BUF TDGEN-CAP allot
variable TDGEN-U

\ TDGEN-BUF holds the name being made. This plan arena grows before publication,
\ so a large family cannot fail halfway through. Each row keeps relocation-safe
\ offsets for its name. The symbol table rejects collisions within the plan;
\ post-xref name and capacity hooks validate live visibility and every
\ plan-determined fixed registry.
$1000 constant TDPLAN-CAP-INIT
4 constant TDPLAN-ROW-CAP-INIT
6 cells constant TDPLAN-REC
$7FFFFFFFFFFFFFFF constant TDPLAN-BYTE-MAX
TDPLAN-BYTE-MAX TDPLAN-REC / constant TDPLAN-ROW-MAX
0 cells constant TDPLAN.SYM-OFF
1 cells constant TDPLAN.NAME-OFF-OFF
2 cells constant TDPLAN.NAME-U-OFF
3 cells constant TDPLAN.BUILT-OFF
4 cells constant TDPLAN.FAM-OFF
5 cells constant TDPLAN.VID-OFF
\ The builder kinds TDPLAN.BUILT holds.
1 constant TDPLAN-TAG
2 constant TDPLAN-EQ
3 constant TDPLAN-HASH
4 constant TDPLAN-CTOR
5 constant TDPLAN-MAKE
6 constant TDPLAN-UNMAKE
create TDPLAN-BOOT TDPLAN-CAP-INIT allot
create TDPLAN-ROW-BOOT TDPLAN-ROW-CAP-INIT TDPLAN-REC * allot
PERSISTED-PTR-VARIABLE TDPLAN-P       TDPLAN-BOOT TDPLAN-P !
PERSISTED-PTR-VARIABLE TDPLAN-ROW-P   TDPLAN-ROW-BOOT TDPLAN-ROW-P !
variable TDPLAN-CAP         TDPLAN-CAP-INIT TDPLAN-CAP !
variable TDPLAN-ROW-CAP     TDPLAN-ROW-CAP-INIT TDPLAN-ROW-CAP !
variable TDPLAN-U

public

variable TDPLAN-N

private

variable TDPLAN-SYM-HI        \ the largest symbol a row of this plan holds

: TDPLAN-BYTE-NEED ( n -- n ) {: add:n :}
   add 0 < add TDPLAN-BYTE-MAX > or IF
      s" sumtype: generated declaration plan capacity overflow" 76 die
   THEN
   TDPLAN-U @ 0 < IF
      s" sumtype: generated declaration plan capacity overflow" 76 die
   THEN
   TDPLAN-U @ TDPLAN-BYTE-MAX add - > IF
      s" sumtype: generated declaration plan capacity overflow" 76 die
   THEN
   TDPLAN-U @ add + ;

: TDPLAN-GROW-CAP ( n n -- n ) {: need:n cap:n :}
   need 0 <= need TDPLAN-BYTE-MAX > or IF
      s" sumtype: generated declaration plan capacity overflow" 76 die
   THEN
   cap 0 <= cap TDPLAN-BYTE-MAX > or IF
      s" sumtype: generated declaration plan capacity overflow" 76 die
   THEN
   cap TDPLAN-BYTE-MAX 2 / <= IF cap 2 * need max EXIT THEN
   need ;

: TDPLAN-ENSURE ( n -- ) {: need:n :}
   need TDPLAN-CAP @ <= IF EXIT THEN
   need TDPLAN-CAP @ TDPLAN-GROW-CAP {: cap:n :}
   TDPLAN-P TDPLAN-CAP @ cap REG-GROW1
   cap TDPLAN-CAP ! ;

: TDPLAN-ROWS>BYTES ( n -- n )
   dup 0 < over TDPLAN-ROW-MAX > or IF
      s" sumtype: generated declaration row capacity overflow" 76 die
   THEN
   TDPLAN-REC * ;

: TDPLAN-NEXT-ROW-CAP ( -- n )
   TDPLAN-N @ TDPLAN-ROW-MAX >= IF
      s" sumtype: generated declaration row capacity overflow" 76 die
   THEN
   TDPLAN-N @ 1 + {: need:n :}
   TDPLAN-ROW-CAP @ 0 <= TDPLAN-ROW-CAP @ TDPLAN-ROW-MAX > or IF
      s" sumtype: generated declaration row capacity overflow" 76 die
   THEN
   TDPLAN-ROW-CAP @ TDPLAN-ROW-MAX 2 / <= IF
      TDPLAN-ROW-CAP @ 2 * need max EXIT
   THEN
   need ;

: TDPLAN-ROW-ENSURE ( -- )
   TDPLAN-N @ TDPLAN-ROW-CAP @ < IF EXIT THEN
   TDPLAN-NEXT-ROW-CAP {: cap:n :}
   TDPLAN-ROW-P TDPLAN-ROW-CAP @ TDPLAN-ROWS>BYTES
      cap TDPLAN-ROWS>BYTES REG-GROW1
   cap TDPLAN-ROW-CAP ! ;


public

: TDPLAN-BEGIN ( -- )
   0 TDPLAN-U !
   0 TDPLAN-N !
   0 TDPLAN-SYM-HI ! ;


private

: TDPLAN-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   u TDPLAN-BYTE-NEED {: need:n :}
   need TDPLAN-ENSURE
   TDPLAN-U @ {: off:n :}
   u 0 ?do a i + c@ TDPLAN-P @ off + i + c! loop
   need TDPLAN-U ! ;

: TDPLAN-ROW ( n -- ptr n )
   dup 0 < over TDPLAN-ROW-CAP @ >= or IF
      s" sumtype: generated declaration row capacity overflow" 76 die
   THEN
   TDPLAN-REC * TDPLAN-ROW-P @ + ;

: TDPLAN.SYM ( ptr a -- ptr a ) TDPLAN.SYM-OFF + ;
: TDPLAN.NAME-OFF ( ptr a -- ptr a ) TDPLAN.NAME-OFF-OFF + ;
: TDPLAN.NAME-U ( ptr a -- ptr a ) TDPLAN.NAME-U-OFF + ;
: TDPLAN.BUILT ( ptr a -- ptr a ) TDPLAN.BUILT-OFF + ;
: TDPLAN.FAM ( ptr a -- ptr a ) TDPLAN.FAM-OFF + ;
: TDPLAN.VID ( ptr a -- ptr a ) TDPLAN.VID-OFF + ;

: TDPLAN-SYM@ ( n -- n )
   TDPLAN-ROW TDPLAN.SYM @ ;

\ A symbol above every planned one cannot repeat one, and a freshly interned
\ name always is, so the duplicate walk runs only for a symbol at or below the
\ plan's largest; a plan of fresh names is checked in constant time per row.
: TDPLAN-SYM+ ( n -- ) {: sym:n :}
   sym TDPLAN-SYM-HI @ <= IF
      0
      BEGIN dup TDPLAN-N @ < WHILE
         dup TDPLAN-SYM@ sym = IF
            TDGEN-BUF TDGEN-U @ s" generated declaration name already defined" E-TDECL-NAME TDECL-THROW
         THEN
         1 +
      REPEAT
      drop
   THEN
   sym TDPLAN-SYM-HI @ max TDPLAN-SYM-HI !
   TDPLAN-ROW-ENSURE
   sym TDPLAN-N @ TDPLAN-ROW TDPLAN.SYM !
   TDPLAN-N @ 1 + TDPLAN-N ! ;

: TDPLAN-NAME+ ( -- n )
   TDGEN-BUF TDGEN-U @ {: na:ptr nu:n :}
   na nu CHECKER-DEFINED-HERE? IF
      na nu s" generated declaration already defined" E-TDECL-NAME TDECL-THROW
   THEN
   na nu CHECKER-RECORD-SYM dup TDPLAN-SYM+
   TDPLAN-N @ 1 - TDPLAN-ROW {: r:ptr :}
   TDPLAN-U @ r TDPLAN.NAME-OFF !
   nu r TDPLAN.NAME-U !
   na nu TDPLAN-APP ;

: TDPLAN-CTOR-NAME+ ( n -- n ) {: vid:n :}
   TDPLAN-NAME+ dup vid swap SUMV-CTOR-SYM! ;

\ A built row records its name, family and kind; its kind's builder makes the
\ word at evaluation.
: TDPLAN-BUILT+ ( n n -- ) {: fam:n kind:n :}
   TDPLAN-NAME+ drop
   TDPLAN-N @ 1 - TDPLAN-ROW {: r:ptr :}
   kind r TDPLAN.BUILT !  fam r TDPLAN.FAM !  0 r TDPLAN.VID ! ;

\ A built row whose name is variant VID's constructor symbol.
: TDPLAN-CTOR-BUILT+ ( n n n -- ) {: vid:n fam:n kind:n :}
   vid TDPLAN-CTOR-NAME+ drop
   TDPLAN-N @ 1 - TDPLAN-ROW {: r:ptr :}
   kind r TDPLAN.BUILT !  fam r TDPLAN.FAM !  vid r TDPLAN.VID ! ;

: TDPLAN-SPAN$ ( n n -- ptr u8 n ) {: off:n u:n :}
   off 0 < u 0 < or off TDPLAN-U @ > or IF
      s" sumtype: invalid generated declaration span" 76 die
   THEN
   u TDPLAN-U @ off - > IF
      s" sumtype: invalid generated declaration span" 76 die
   THEN
   TDPLAN-P @ off + u ;


public

: TDPLAN-NAME$ ( n -- ptr u8 n )
   TDPLAN-ROW {: r:ptr :}
   r TDPLAN.NAME-OFF @ r TDPLAN.NAME-U @ TDPLAN-SPAN$ ;


private

: TDPLAN-KIND@ ( n -- n ) TDPLAN-ROW TDPLAN.BUILT @ ;
: TDPLAN-FAM@ ( n -- n ) TDPLAN-ROW TDPLAN.FAM @ ;
: TDPLAN-VID@ ( n -- n ) TDPLAN-ROW TDPLAN.VID @ ;

: TDPLAN-PROD? ( n -- bool ) {: kind:n :}
   kind TDPLAN-MAKE =  kind TDPLAN-UNMAKE = or ;

\ The CHILD-ABI shape of a MAKE or UNMAKE row's word.
: TDPLAN-SHAPE ( n -- n )
   TDPLAN-MAKE = IF CHILD-ABI:MAKE ELSE CHILD-ABI:UNMAKE THEN ;

\ Registers row N's MAKE or UNMAKE effect, as CHILD-RUN would.
: TDPLAN-PROD-REG ( n -- ) {: row:n :}
   row TDPLAN-KIND@ TDPLAN-SHAPE  row TDPLAN-FAM@
   TFAM-CHILD-ROWS {: din:n dout:n val:n :}
   row TDPLAN-NAME$ din dout CHECKER-CHILD-ROWS drop ;

: TDPLAN-PREFLIGHT-NAMES ( -- )
   TDPLAN-N @ 0 ?do i TDPLAN-NAME$ TDECL-NAME-PREFLIGHT-XT loop ;

\ FAM's value type; derive requires arity 0.
: TDECL-SELF-NODE ( n -- n ) {: fam:n :}
   fam 0 0 SCHEMA-APP ;

\ The effect a derived word of KIND over FAM takes.
: TDECL-KIND-EFFECT ( n n -- n ) {: fam:n kind:n :}
   fam TDECL-SELF-NODE  EMPTY-EFFECT over EFFECT-IN
   kind TDPLAN-EQ = IF
      over EFFECT-IN  CC-BOOL SCHEMA-CON EFFECT-OUT
   ELSE CC-N SCHEMA-CON EFFECT-OUT THEN
   nip ;

\ FAM applied to its own parameters, fam<p0,..>.
: TDECL-FAM-NODE ( n -- n ) {: fam:n :}
   SCHEMA-ROOT-N@ {: start:n :}
   fam TFAM-ARITY@ {: ar:n :}
   ar 0 ?do i SCHEMA-PARAM SCHEMA-ROOT+ drop loop
   fam start ar SCHEMA-APP ;

\ Variant VID's constructor: its payload in declaration order to FAM's value.
: TDECL-CTOR-EFFECT ( n n -- n ) {: fam:n vid:n :}
   EMPTY-EFFECT
   vid SUMV-PAY-N 0 ?do
      vid i SUMV-PAY-ROOT SCHEMA-ROOT@ EFFECT-IN
   loop
   fam TDECL-FAM-NODE EFFECT-OUT ;

\ The effect built row ROW's word takes, a transient node.
: TDECL-DRV-EFFECT ( n -- n ) {: row:n :}
   row TDPLAN-FAM@  row TDPLAN-KIND@ {: fam:n kind:n :}
   TRANSIENT-ON
   [: kind TDPLAN-CTOR = IF fam row TDPLAN-VID@ TDECL-CTOR-EFFECT
      ELSE fam kind TDECL-KIND-EFFECT THEN ;]
   [: TRANSIENT-OFF ;] finally ;

\ Each row registers the effect its kind gives.
: TDPLAN-PREFLIGHT-ROWS ( -- )
   TDPLAN-N @ 0 ?do
      i TDPLAN-KIND@ TDPLAN-PROD? IF
         i TDPLAN-PROD-REG
      ELSE
         i TDPLAN-NAME$  i TDECL-DRV-EFFECT EFFECT-SIG 2drop
         CHECKER-CHILD-ROWS drop
      THEN
   loop ;

\ A generated definition takes no declaration location: its rows are registered
\ with the declaring registrar's latch paused (NAV-PAUSE).
: TDPLAN-PREFLIGHT-DEFINITIONS ( -- )
   NAV-PAUSE {: on:n new:n :}
   [: TDPLAN-PREFLIGHT-ROWS ;] catch {: rc:n :}
   on new NAV-RESUME
   rc 0 <> IF rc throw THEN ;

: TDPLAN-PREFLIGHT-CHECKER ( -- )
   CHECKER-CANDIDATE-SCOPE-START
   [: TDPLAN-PREFLIGHT-DEFINITIONS ;] catch {: rc:n :}
   CHECKER-CANDIDATE-SCOPE-DONE
   rc 0 <> IF rc throw THEN ;

: TDPLAN-PREFLIGHT ( -- )
   TDPLAN-PREFLIGHT-NAMES
   TDPLAN-N @ 0= IF s" sumtype: empty generated declaration plan" 76 die THEN
   0 TDPLAN-NAME$ TDPLAN-N @ TDECL-CAPACITY-PREFLIGHT-XT
   TDPLAN-PREFLIGHT-CHECKER ;

: TDGEN-CLEAR ( -- ) 0 TDGEN-U ! ;
: TDGEN-C, ( n -- )
   TDGEN-BUF TDGEN-U @ + c!
   TDGEN-U @ 1 + TDGEN-U ! ;
: TDGEN-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ TDGEN-C, loop ;
: TDGEN-UPPER ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ TF-UPPER-C TDGEN-C, loop ;
PTR-VARIABLE TDECL-BUF-P
SCRATCH-VARIABLE TDECL-BUF-CAP   \ in cells
variable TDECL-U
: TDECL-BUF ( -- ptr u8 ) TDECL-BUF-P @ ;

\ --- derived typed equality. `DERIVE eq` builds checked words into the family's
\ constructor package from items, their engine words found where this file is
\ compiled: no pending window, no trust rows, no engine lowering, so equality is semantic and
\ layout-policy agnostic. Derived eq consumes both operands.
\  - TAG ( fam -- n ): declaration-order tag, which any checked MATCH already
\    observes. Enums and sums only.
\  - EQ ( fam fam -- bool ): payload-free families compare tags; payload sums
\    compare diagonally (outer MATCH binds one value's payloads to locals,
\    inner MATCH the other's; the same-variant arm compares with `=`,
\    integer widening at the bind making every CT-INT payload an n; cross arms
\    drop and answer false); products UNMAKE both values, bind fields top-down
\    and compare field-wise. A family-typed slot binds as its family and
\    compares through that family's EQ.
\ A derived word is "PKG-FAMILY:TAIL" for a public family and "FAMILY-TAIL" for a
\ private or OPAQUE one, as a constructor is spelled. Neither stamps a
\ constructor package (TDECL-CTOR-PUBLISH exits early), so SUMV-CTOR-PKG$ would
\ name nothing. A private or OPAQUE family's words bind in the declaring package,
\ so only a declaration in that package derives through them
\ (TDECL-DERIVE-FIELD-OK).
: TDGEN-DRV-REF ( n ptr u8 n -- ) {: fam:n ta:ptr tu:n :}
   fam TFAM-GEN-PUBLIC? 0= IF
      fam TFAM-NAME$ ta tu TF-CTOR-PRIV$ TDGEN-APP EXIT THEN
   fam TFAM-VAR-START@ SUMV-CTOR-PKG$ TDGEN-APP
   58 TDGEN-C,
   ta tu TDGEN-UPPER ;
: TDGEN-DRV-NAME ( n ptr u8 n -- ) {: fam:n ta:ptr tu:n :}   \ the name alone
   TDGEN-CLEAR  fam ta tu TDGEN-DRV-REF ;
: TDGEN-KW ( ptr u8 n -- ) 0 ITEM-ABI:KEYWORD TDGEN-ITEM ;
: TDGEN-OF ( ptr u8 n -- )                \ a variant's `of`
   0 ITEM-ABI:OPERAND TDGEN-ITEM  s" of" TDGEN-KW ;
\ The index the next local takes: the definition's locals before it.
: TDGEN-LOCALS ( -- n ) data-base BODY-ITEMS:LOCALS-CELL + @ ;
: TDGEN-LOCAL ( n -- ) {: node:n :}      \ a local of type node NODE
   s" DERIVE" node ITEM-ABI:LOCAL TDGEN-ITEM ;
: TDGEN-LOCAL@ ( n -- ) {: ix:n :}
   s" DERIVE" ix ITEM-ABI:LOCAL-REF TDGEN-ITEM ;
: TDGEN-SELF-NODE ( n -- n ) {: fam:n :}  \ TDECL-SELF-NODE, transient
   TRANSIENT-ON
   [: fam TDECL-SELF-NODE ;] [: TRANSIENT-OFF ;] finally ;
: TDGEN-MATCH-OPEN ( n -- ) {: fam:n :}
   s" match" TDGEN-KW
   fam TFAM-NAME$  fam TDGEN-SELF-NODE  ITEM-ABI:TYPE TDGEN-ITEM ;
\ A public family's derived TAIL by its exact name, which no part named as its
\ constructor package captures; a private or OPAQUE one's where it binds.
: TDGEN-DRV-CALL ( n ptr u8 n -- ) {: fam:n ta:ptr tu:n :}
   TDGEN-CLEAR  fam ta tu TDGEN-DRV-REF
   fam TFAM-GEN-PUBLIC? IF ITEM-ABI:CALL-EXACT ELSE ITEM-ABI:CALL THEN {: op:n :}
   TDGEN-BUF TDGEN-U @ 0 op TDGEN-ITEM ;
: TDGEN-DROPS ( n -- ) {: k:n :}          \ drop x k
   k 0 ?do postpone drop loop ;
\ A slot's local type: a family slot's node, else `n`, which an integer
\ payload widens to at the bind.
: TDGEN-SLOT-TYPE ( n -- n ) {: node:n :}
   node SCHEMA-APP? IF node EXIT THEN
   TRANSIENT-ON
   [: CC-N SCHEMA-CON ;] [: TRANSIENT-OFF ;] finally ;
: TDGEN-BINDS ( n -- n ) {: vid:n :}   \ "{: .. :}" over vid's payload slots; the first's index
   TDGEN-LOCALS {: base:n :}
   vid SUMV-PAY-N {: m:n :}
   m 0= IF base EXIT THEN
   s" {:" TDGEN-KW
   m 0 ?do vid i SUMV-PAY-ROOT SCHEMA-ROOT@ TDGEN-SLOT-TYPE TDGEN-LOCAL loop
   s" :}" TDGEN-KW
   base ;
\ Reads slot I's local, BASE the index of the first its binds declared: a sum
\ binds its slots in order, TDGEN-UNBIND a product's fields top-down.
: TDGEN-SLOT@ ( n n n n -- ) {: fam:n vid:n base:n i:n :}
   fam TFAM-PRODUCT? IF base vid SUMV-PAY-N 1 - i - + ELSE base i + THEN
   TDGEN-LOCAL@ ;
: TDGEN-CMP1 ( n n n n n -- ) {: fam:n vid:n pb:n qb:n i:n :}   \ p q, then = or FAM:EQ
   fam vid pb i TDGEN-SLOT@  fam vid qb i TDGEN-SLOT@
   vid i SUMV-PAY-ROOT SCHEMA-ROOT@ {: node:n :}
   node SCHEMA-APP? IF node SCHEMA-A@ s" eq" TDGEN-DRV-CALL EXIT THEN
   postpone = ;
: TDGEN-CMP ( n n n n -- ) {: fam:n vid:n pb:n qb:n :}  \ p0 q0 = p1 q1 = and .. (m=0 -> true)
   vid SUMV-PAY-N {: m:n :}
   m 0= IF 0 literal postpone 0= EXIT THEN
   m 0 ?do
      fam vid pb qb i TDGEN-CMP1
      i 0 > IF postpone and THEN
   loop ;
: TDGEN-TAG-BODY ( n -- ) {: fam:n :}   \ match fam v of <drops> <tag> endof .. ;match
   fam TDGEN-MATCH-OPEN
   fam TFAM-VAR-START@ {: vstart:n :}
   vstart fam TFAM-VAR-COUNT@ + vstart ?do
      i SUMV-NAME$ TDGEN-OF
      i SUMV-PAY-N TDGEN-DROPS
      i SUMV-TAG@ literal
      s" endof" TDGEN-KW
   loop
   s" ;match" TDGEN-KW ;
\ One inner arm: the diagonal compares with the outer arm's locals from QB,
\ any other variant drops and answers false.
: TDGEN-EQ-ARM ( n n n n -- ) {: fam:n vout:n vin:n qb:n :}
   vin SUMV-NAME$ TDGEN-OF
   vin vout = IF
      vin TDGEN-BINDS {: pb:n :}
      fam vin pb qb TDGEN-CMP
   ELSE
      vin SUMV-PAY-N TDGEN-DROPS
      1 literal postpone 0=
   THEN
   s" endof" TDGEN-KW ;
: TDGEN-EQ-OUT ( n n n n -- ) {: fam:n vstart:n k:n vout:n :}   \ vout's outer arm
   vout SUMV-NAME$ TDGEN-OF
   vout TDGEN-BINDS {: qb:n :}
   fam TDGEN-MATCH-OPEN
   vstart k + vstart ?do fam vout i qb TDGEN-EQ-ARM loop
   s" ;match" TDGEN-KW  s" endof" TDGEN-KW ;
: TDGEN-EQ-DIAG ( n -- ) {: fam:n :}   \ diagonal double-match body for payload sums
   fam TDGEN-MATCH-OPEN
   fam TFAM-VAR-START@ {: vstart:n :}
   fam TFAM-VAR-COUNT@ {: k:n :}
   vstart k + vstart ?do fam vstart k i TDGEN-EQ-OUT loop
   s" ;match" TDGEN-KW ;
: TDGEN-UNBIND1 ( n n n -- ) {: fam:n mk:n k:n :}   \ field k's bind
   s" {:" TDGEN-KW
   mk k SUMV-PAY-ROOT SCHEMA-ROOT@ TDGEN-SLOT-TYPE TDGEN-LOCAL
   s" :}" TDGEN-KW ;
: TDGEN-UNBIND ( n -- n ) {: fam:n :}   \ PKG:UNMAKE + top-down field binds; the first's index
   fam s" unmake" TDGEN-DRV-CALL
   TDGEN-LOCALS {: base:n :}
   fam TFAM-VAR-START@ {: mk:n :}
   mk SUMV-PAY-N {: m:n :}
   m 0 ?do fam mk m 1 - i - TDGEN-UNBIND1 loop
   base ;
: TDGEN-EQ-PROD ( n -- ) {: fam:n :}   \ UNMAKE both values, field-wise compare
   fam TDGEN-UNBIND {: qb:n :}
   fam TDGEN-UNBIND {: pb:n :}
   fam fam TFAM-VAR-START@ pb qb TDGEN-CMP ;
: TDGEN-EQ-TAGS ( n -- ) {: fam:n :}      \ payload-free family: tag equality (O(V))
   fam s" tag" TDGEN-DRV-CALL  postpone swap
   fam s" tag" TDGEN-DRV-CALL  postpone = ;

\ Derived hash: FNV-1a over whole cells, h = BASIS, then per cell
\ h = (h xor v) * PRIME. Sums and enums fold the tag, then each bound payload
\ scalar; products fold the fields. A family-typed slot folds its family's
\ HASH, so equal values always hash equal. Hash values are an in-memory
\ contract only: never persist them across engine versions.
$cbf29ce484222325 constant DRV-FNV-BASIS   \ FNV-1a 64-bit offset basis
$100000001b3 constant DRV-FNV-PRIME        \ FNV-1a 64-bit prime
: TDGEN-HMIX ( -- )                       \ xor PRIME *
   postpone xor  DRV-FNV-PRIME literal  postpone * ;
: TDGEN-HASH-FOLD1 ( n n n n -- ) {: fam:n vid:n base:n i:n :}   \ slot i [FAM:HASH] mix
   fam vid base i TDGEN-SLOT@
   vid i SUMV-PAY-ROOT SCHEMA-ROOT@ {: node:n :}
   node SCHEMA-APP? IF node SCHEMA-A@ s" hash" TDGEN-DRV-CALL THEN
   TDGEN-HMIX ;
: TDGEN-HASH-FOLDS ( n n n -- ) {: fam:n vid:n base:n :}   \ fold every slot's local
   vid SUMV-PAY-N 0 ?do fam vid base i TDGEN-HASH-FOLD1 loop ;
: TDGEN-HASH-ARM ( n n -- ) {: fam:n vid:n :}   \ bind payloads, fold tag then payloads
   vid TDGEN-BINDS {: base:n :}
   DRV-FNV-BASIS literal
   vid SUMV-TAG@ literal  TDGEN-HMIX
   fam vid base TDGEN-HASH-FOLDS ;
: TDGEN-HASH-BODY ( n -- ) {: fam:n :}   \ sum/enum: MATCH ladder of per-arm folds
   fam TDGEN-MATCH-OPEN
   fam TFAM-VAR-START@ {: vstart:n :}
   vstart fam TFAM-VAR-COUNT@ + vstart ?do
      i SUMV-NAME$ TDGEN-OF
      fam i TDGEN-HASH-ARM
      s" endof" TDGEN-KW
   loop
   s" ;match" TDGEN-KW ;
: TDGEN-HASH-PROD ( n -- ) {: fam:n :}   \ product: UNMAKE, fold fields (no tag)
   fam TDGEN-UNBIND {: base:n :}
   DRV-FNV-BASIS literal
   fam fam TFAM-VAR-START@ base TDGEN-HASH-FOLDS ;
: TDECL-TAG-WORD ( n -- ) {: fam:n :}
   fam s" tag" TDGEN-DRV-NAME
   fam TDPLAN-TAG TDPLAN-BUILT+ ;
: TDECL-EQ-WORD ( n -- ) {: fam:n :}
   fam s" eq" TDGEN-DRV-NAME
   fam TDPLAN-EQ TDPLAN-BUILT+ ;
: TDECL-HASH-WORD ( n -- ) {: fam:n :}
   fam s" hash" TDGEN-DRV-NAME
   fam TDPLAN-HASH TDPLAN-BUILT+ ;
: TDECL-DRV-WORDS ( n -- ) {: fam:n :}
   fam TFAM-DERIVE-ANY? 0= IF EXIT THEN
   fam TFAM-PRODUCT? 0= IF fam TDECL-TAG-WORD THEN   \ discriminant rides any derive
   fam TFAM-DERIVE-EQ? IF fam TDECL-EQ-WORD THEN
   fam TFAM-DERIVE-HASH? IF fam TDECL-HASH-WORD THEN ;

: TDECL-BUILT-OPEN ( n -- ) {: row:n :}
   row TDPLAN-NAME$  row TDECL-DRV-EFFECT
   TDECL-DEFINE-RUN ;
: TDECL-BUILT-END ( -- ) postpone ; ;
: TDECL-TAG-BUILD ( n n -- ) {: fam:n row:n :}
   row TDECL-BUILT-OPEN
   fam TDGEN-TAG-BODY
   TDECL-BUILT-END ;
: TDECL-EQ-BUILD ( n n -- ) {: fam:n row:n :}
   row TDECL-BUILT-OPEN
   fam TFAM-PRODUCT? IF fam TDGEN-EQ-PROD ELSE
   fam TFAM-SLOTS@ 0= IF fam TDGEN-EQ-TAGS ELSE fam TDGEN-EQ-DIAG THEN THEN
   TDECL-BUILT-END ;
: TDECL-HASH-BUILD ( n n -- ) {: fam:n row:n :}
   row TDECL-BUILT-OPEN
   fam TFAM-PRODUCT? IF fam TDGEN-HASH-PROD ELSE fam TDGEN-HASH-BODY THEN
   TDECL-BUILT-END ;
: TDECL-CTOR-BUILD ( n n -- ) {: fam:n row:n :}
   row TDECL-BUILT-OPEN
   s" construct" TDGEN-KW
   fam TFAM-NAME$  fam TDGEN-SELF-NODE  ITEM-ABI:TYPE TDGEN-ITEM
   row TDPLAN-VID@ SUMV-NAME$ 0 ITEM-ABI:OPERAND TDGEN-ITEM
   TDECL-BUILT-END ;
: TDECL-DRV-BUILD ( n n -- ) {: fam:n row:n :}
   row TDPLAN-KIND@ {: kind:n :}
   kind TDPLAN-CTOR = IF fam row TDECL-CTOR-BUILD EXIT THEN
   kind TDPLAN-TAG = IF fam row TDECL-TAG-BUILD EXIT THEN
   kind TDPLAN-EQ = IF fam row TDECL-EQ-BUILD EXIT THEN
   kind TDPLAN-HASH = IF fam row TDECL-HASH-BUILD EXIT THEN
   kind TDPLAN-PROD? IF
      row TDPLAN-NAME$ kind TDPLAN-SHAPE fam TDECL-CHILD EXIT
   THEN
   s" sumtype: no builder for this plan row" 76 die ;

: TDECL-GEN-EVAL ( n -- ) {: fam:n :}
   TDPLAN-PREFLIGHT
   TDPLAN-N @ 0 ?do fam i TDECL-DRV-BUILD loop ;

\ A product's MAKE or UNMAKE row of KIND for variant VID. Its word is a cast: a
\ product value is its field cells in slot order, with no pads and no tag.
: TDECL-PROD-ROW ( n n n -- ) {: fam:n vid:n kind:n :}
   fam vid SUMV-NAME$ TDGEN-DRV-NAME
   vid fam kind TDPLAN-CTOR-BUILT+ ;

\ Variant VID's constructor row.
: TDECL-CTOR-ROW ( n n -- ) {: fam:n vid:n :}
   fam vid TDPLAN-CTOR TDECL-PROD-ROW ;

: TDECL-PROD-PLAN ( n -- ) {: fam:n :}   \ make (row 0) + unmake (row 1)
   fam TFAM-VAR-START@ {: vstart:n :}
   TDPLAN-BEGIN
   fam vstart TDPLAN-MAKE TDECL-PROD-ROW
   fam vstart 1 + TDPLAN-UNMAKE TDECL-PROD-ROW
   fam TDECL-DRV-WORDS ;

\ A live declaration's switch runs the `private` / `public` keyword's
\ operation. A replay defines no word, and its pre-pass (verify-source.f)
\ opens the package in the checker alone: the engine has no package open
\ there. A replay switches the checker's mode only.
: TDECL-GEN-PRIVATE ( bool -- ) {: replay:bool :}
   replay IF CHECKER-PRIVATE EXIT THEN
   CHECKER-RESOLVE:PRIVATE-SECTION ;
: TDECL-GEN-RESTORE ( n bool -- ) {: fam:n replay:bool :}
   fam TFAM-PUBLIC? 0= IF replay TDECL-GEN-PRIVATE EXIT THEN
   replay IF CHECKER-PUBLIC EXIT THEN
   CHECKER-RESOLVE:PUBLIC-SECTION ;

\ Q plans, preflights and builds (or replays) a family's generated rows. A
\ private or OPAQUE family's rows (not TFAM-GEN-PUBLIC?) go into the declaring
\ package's private wordlist, so Q runs between a switch to private and a
\ restore of the declaration's section, on either exit. Everything that interns
\ a name runs inside the switch: the name preflight asks the wordlist the name
\ will land in (xref.f TARGET-RECORD). Q takes and returns the family, which
\ each caller's generator takes; catch takes only a
\ stack-preserving quotation.
: TDECL-GEN-PLACED ( n bool [ n -- n ] -- ) {: fam:n replay:bool q :}
   fam TFAM-GEN-PUBLIC? IF fam q execute drop EXIT THEN
   replay TDECL-GEN-PRIVATE
   fam q catch {: rc:n :}
   fam replay TDECL-GEN-RESTORE
   rc 0 <> IF rc throw THEN
   drop ;

: TDECL-PROD-WORDS-BODY ( n -- ) {: fam:n :}
   fam RES-FALSE [: dup TDECL-PROD-PLAN dup TDECL-GEN-EVAL ;] TDECL-GEN-PLACED ;

\ The ( fam -- ) product generator structure-make.f calls: it declares its
\ variant rows through SUMV-ADD before generating, so the committed view is its
\ own view.

public

: TDECL-PROD-WORDS ( n -- ) {: fam:n :}
   fam TDECL-PROD-WORDS-BODY ;

\ --- generated ADDRESS accessors (`DERIVE addr`).
\ One child word per field plus AT / BYTES / CELLS, in the family's own
\ spelling, each made by the engine's CHILD-RUN from its CHILD-ABI shape and
\ parameter: no text, so no word of the declaring package can capture one. A
\ field's parameter is its committed field id, AT's the family id; BYTES and
\ CELLS are numbers. Generation runs at commit: a field id is an authority only
\ once TYPE-FIELD has committed the row, which the runner's commit order
\ (DECL-EVENT before the address surface) guarantees.

private

: TDECL-ADDR-NAME ( n ptr u8 n -- ptr u8 n ) {: fam:n ta:ptr tu:n :}
   TDGEN-CLEAR  fam ta tu TDGEN-DRV-REF  TDGEN-BUF TDGEN-U @ ;

\ Runs XT on each child of FAM, with its name, shape and parameter.
: TDECL-ADDR-EACH ( n [ ptr u8 n n n -- ] -- ) {: fam:n xt :}
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fam  fs i + PF-NAME$  TDECL-ADDR-NAME
      CHILD-ABI:FIELD  fs i +  xt execute
   loop
   fam s" at" TDECL-ADDR-NAME  CHILD-ABI:INDEX fam  xt execute
   fam s" bytes" TDECL-ADDR-NAME
   CHILD-ABI:VALUE  fam TFAM-SLOTS@ CELL *  xt execute
   fam s" cells" TDECL-ADDR-NAME
   CHILD-ABI:VALUE  fam TFAM-SLOTS@  xt execute ;

\ A name the checker already holds in this scope is the declaration's error,
\ asked of every child before any is made.
: TDECL-ADDR-FREE ( ptr u8 n n n -- ) {: na:ptr nu:n shape:n param:n :}
   na nu CHECKER-DEFINED-HERE? IF
      na nu s" generated declaration already defined" E-TDECL-NAME TDECL-THROW
   THEN ;

: TDECL-ADDR-LIVE-FREE ( ptr u8 n n n -- ) {: na:ptr nu:n shape:n param:n :}
   na nu shape param TDECL-ADDR-FREE
   na nu TDECL-NAME-PREFLIGHT-XT ;

: TDECL-ADDR-MAKE ( ptr u8 n n n -- ) {: na:ptr nu:n shape:n param:n :}
   na nu shape param TDECL-CHILD ;

\ Replay makes no word: it registers each child's effect, as CHILD-RUN would.
: TDECL-ADDR-REG ( ptr u8 n n n -- ) {: na:ptr nu:n shape:n param:n :}
   shape param TFAM-CHILD-ROWS {: din:n dout:n val:n :}
   na nu din dout CHECKER-CHILD-ROWS drop ;

: TDECL-ADDR-ROOM ( n -- ) {: fam:n :}
   fam s" at" TDECL-ADDR-NAME  fam TFAM-FLD-COUNT@ 3 +
   TDECL-CAPACITY-PREFLIGHT-XT ;

: TDECL-ADDR-LIVE ( n -- n )
   dup [: TDECL-ADDR-LIVE-FREE ;] TDECL-ADDR-EACH
   dup TDECL-ADDR-ROOM
   dup [: TDECL-ADDR-MAKE ;] TDECL-ADDR-EACH ;

: TDECL-ADDR-REPLAY-REG ( n -- n )
   dup [: TDECL-ADDR-FREE ;] TDECL-ADDR-EACH
   dup [: TDECL-ADDR-REG ;] TDECL-ADDR-EACH ;

public

: TDECL-ADDR-WORDS ( n -- )
   RES-FALSE [: TDECL-ADDR-LIVE ;] TDECL-GEN-PLACED ;

\ Replay (verify-source.f's declaration pre-pass) registers the checked
\ effects of a generated set without emitting code, so a file that uses its
\ own record accessors resolves on the pre-scan.
: TDECL-ADDR-REPLAY ( n -- )
   RES-TRUE [: TDECL-ADDR-REPLAY-REG ;] TDECL-GEN-PLACED ;

\ Declares DEFTYPE's family, tail TA TU named by surface NA NU, and makes each
\ converter, >NAME then NAME>N. A family refused under a multi-error load
\ leaves -1 and has no converters.
: TDECL-DEFTYPE ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n ta:ptr tu:n :}
   ta tu na nu TDECL-DEFFAMILY
   TDECL-FAM-REG @ {: fam:n :}
   fam 0 < IF EXIT THEN
   TDGEN-CLEAR  [char] > TDGEN-C,  na nu TDGEN-APP
   TDGEN-BUF TDGEN-U @  CHILD-ABI:NOMINAL-IN fam  TDECL-CHILD
   TDGEN-CLEAR  na nu TDGEN-APP  s" >N" TDGEN-APP
   TDGEN-BUF TDGEN-U @  CHILD-ABI:NOMINAL-OUT fam  TDECL-CHILD ;

\ Registers converter NA NU's effect, of SHAPE, over the last declared family.
: TDECL-NOMINAL-REPLAY ( ptr u8 n n -- ) {: na:ptr nu:n shape:n :}
   na nu shape TDECL-FAM-REG @ TDECL-ADDR-REG ;

\ --- initialized-field accessors (`DERIVE init`).
\ A getter FIELD@ and a setter FIELD! per field, in the family's own spelling,
\ each made by the engine's CHILD-RUN from its CHILD-ABI shape and committed
\ field id: no text, so no word of the declaring package can capture one.

private

: TDINIT-TERM ( n -- n ) TFAM-FRESH-TERM ;

: TDINIT-REFUSE ( n -- )
   TFAM-NAME$ s" derive init requires a canonical fixed-cell record"
   E-TDECL-DERIVE TDECL-THROW ;

\ INIT-RECORD? proves the whole fixed canonical layout; INIT-FIELD? then proves
\ every id and field extent against that same instantiation. These queries read
\ committed rows, so this runs only after the field transaction's commit.
: TDINIT-REQUIRE ( n -- ) {: fam:n :}
   fam TFAM-DERIVE-INIT? 0= fam TFAM-PRODUCT? 0= or IF fam TDINIT-REFUSE THEN
   fam TFAM-PKG$ nip 0= IF fam TDINIT-REFUSE THEN
   fam TFAM-ARITY@ TFAM-DECL-PARAM-COUNT 4 - > IF fam TDINIT-REFUSE THEN
   fam TDINIT-TERM {: term:n :}
   term INIT-RECORD? 0= IF 2drop drop fam TDINIT-REFUSE THEN
   2drop drop
   fam TFAM-FLD-COUNT@ 0 ?do
      fam TFAM-FLD-START@ i + term INIT-FIELD? 0= IF
         2drop 2drop unloop fam TDINIT-REFUSE THEN
      2drop 2drop
   loop ;

: TDINIT-NAME ( n n bool -- ptr u8 n ) {: fam:n fid:n store:bool :}
   fam  fid store TFAM-INIT-MEMBER$  TDECL-ADDR-NAME ;

\ Runs XT on each accessor of FAM, with its name, shape and field id.
: TDINIT-EACH ( n [ ptr u8 n n n -- ] -- ) {: fam:n xt :}
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fam fs i + RES-FALSE TDINIT-NAME  CHILD-ABI:INIT-GET fs i +  xt execute
      fam fs i + RES-TRUE TDINIT-NAME  CHILD-ABI:INIT-SET fs i +  xt execute
   loop ;

: TDINIT-ROOM ( n -- ) {: fam:n :}
   fam  fam TFAM-FLD-START@ RES-FALSE TDINIT-NAME
   fam TFAM-FLD-COUNT@ 2 * TDECL-CAPACITY-PREFLIGHT-XT ;

: TDINIT-CHECK ( n [ ptr u8 n n n -- ] -- n )
   over swap TDINIT-EACH
   dup TDINIT-ROOM ;

: TDINIT-LIVE ( n -- n )
   [: TDECL-ADDR-LIVE-FREE ;] TDINIT-CHECK
   dup [: TDECL-ADDR-MAKE ;] TDINIT-EACH ;

: TDINIT-REPLAY-REG ( n -- n )
   [: TDECL-ADDR-FREE ;] TDINIT-CHECK
   dup [: TDECL-ADDR-REG ;] TDINIT-EACH ;

public

: TDECL-INIT-WORDS ( n -- )
   dup TDINIT-REQUIRE  RES-FALSE [: TDINIT-LIVE ;] TDECL-GEN-PLACED ;
: TDECL-INIT-REPLAY ( n -- )
   dup TDINIT-REQUIRE  RES-TRUE [: TDINIT-REPLAY-REG ;] TDECL-GEN-PLACED ;

\ PRODUCT's outer declaration runner arms the same commit participant as
\ STRUCTURE after its constructors exist; structure-make.f installs the
\ participant after this pre-hook file has loaded.
defer TDECL-INIT-ARM-XT ( n -- )

private

: TDECL-INIT-ARM-MISSING ( n -- )
   drop s" sumtype: initialized field participant not installed" 76 die ;
: TDECL-INIT-ARM-DEFAULT ( -- )
   [: TDECL-INIT-ARM-MISSING ;] is TDECL-INIT-ARM-XT ;
TDECL-INIT-ARM-DEFAULT

private

\ A PUBLIC family always publishes generated words: into the reserved
\ constructor namespace, or, when OPAQUE, into the declaring package's private
\ wordlist (TDECL-GEN-PLACED). A PRIVATE PRODUCT publishes into that private
\ wordlist too. A private SUM or ENUM publishes nothing: it constructs through
\ the checker-owned `construct` token, which needs no dictionary word.
: TDECL-GENERATES? ( n -- bool ) {: fam:n :}
   fam TFAM-PUBLIC? IF RES-TRUE EXIT THEN
   fam TFAM-PRODUCT? ;

: TDECL-GEN-PLAN ( n -- n )
   {: fam:n :}
   fam TDECL-GENERATES? 0= IF fam EXIT THEN
   fam TFAM-PRODUCT? IF
      fam TDECL-PROD-PLAN  fam EXIT THEN
   \ Parametric families publish too: a constructor's parametric result expands
   \ to hidden fields wherever its width is known, linear included (checker.f
   \ PUSH-LOGICAL), and stays one logical cell only while its width reads an
   \ open argument.
   TDPLAN-BEGIN
   fam TFAM-VAR-COUNT@ 0 ?do
      fam  fam TFAM-VAR-START@ i +  TDECL-CTOR-ROW
   loop
   fam TDECL-DRV-WORDS
   fam ;

\ Replay publishes checked effects from the same constructor bodies as a live
\ declaration. It emits no code and creates no runtime dictionary entries.
: TDECL-REPLAY-PLAN ( n -- n )
   TDECL-GEN-PLAN TDPLAN-PREFLIGHT-DEFINITIONS ;


public

: TDECL-CTOR-REPLAY ( n -- )
   dup TDECL-GENERATES? 0= IF drop EXIT THEN
   RES-TRUE [: TDECL-REPLAY-PLAN ;] TDECL-GEN-PLACED ;

: TDECL-CTOR-WORDS-BODY ( n -- ) {: fam:n :}
   fam TDECL-GENERATES? 0= IF EXIT THEN
   fam TDECL-GEN-PLAN TDECL-GEN-EVAL ;

\ Every declaration announces its family through TDECL-FAM-REG; that ambient
\ read lives only in the adapters below and in TDECL-DEFTYPE and
\ TDECL-NOMINAL-REPLAY. A SUMTYPE refused under a multi-error load leaves -1
\ (TDECL-RUN's continue path) and generates nothing; PRODUCT generates inside
\ the body TDECL-RUN guards, so a refused one never reaches generation.

private

: TDECL-CTOR-WORDS ( -- )
   TDECL-FAM-REG @ {: fam:n :}
   fam 0 < IF EXIT THEN
   fam TDECL-CTOR-WORDS-BODY ;

: TDECL-PRODUCT-INIT-ARM ( -- )
   TDECL-FAM-REG @ {: fam:n :}
   fam 0 < IF EXIT THEN
   fam TFAM-DERIVE-INIT? IF fam TDECL-INIT-ARM-XT THEN ;

\ A generation fault is the declaration's own: report it at the family name,
\ not at the last field the body armed.
: TDECL-PRODUCT-GENERATE ( -- )
   TDN-A @ TDN-U @ TDECL-TOK!  s" declaration failed" TDECL-WHY!
   TDECL-CTOR-WORDS TDECL-PRODUCT-INIT-ARM ;

public

\ The replay twin, for a tool that registered a SUMTYPE or PRODUCT from tokens
\ it already lexed (verify-source.f): the checked effects of the words the
\ definer would generate, and no word. These include a PRODUCT's
\ initialized-field accessors when it derives init (TDECL-PRODUCT-INIT-ARM);
\ its rows are committed once the registration returns, which is all
\ TDINIT-REQUIRE reads.
: TDECL-CTOR-WORDS-REPLAY ( -- )
   TDECL-FAM-REG @ {: fam:n :}
   fam 0 < IF EXIT THEN
   fam TDECL-CTOR-REPLAY
   fam TFAM-DERIVE-INIT? IF fam TDECL-INIT-REPLAY THEN ;

: TDECL-NEWTYPE ( -- )
   parse-name 2dup TDECL-DEFFAMILY ;


private

: TDECL-CLEAR ( -- ) 0 TDECL-U ! ;
: TDECL-BUF-ENSURE ( n -- ) {: need:n :}
   TDECL-BUF-P TDECL-BUF-CAP  need CELL + 1 - CELL /  1
   ARENA-ROWS-ENSURE ;
: TDECL-C, ( n -- )
   TDECL-U @ 1 + {: need:n :}
   need TDECL-BUF-ENSURE
   TDECL-BUF TDECL-U @ + c!
   need TDECL-U ! ;
: TDECL-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ TDECL-C, loop ;
: TDECL-TOKEN+ ( ptr u8 n -- )
   TDECL-U @ 0 > IF 32 TDECL-C, THEN
   TDECL-APP ;

: SUMTYPE-COLLECT ( -- bool )   \ buffer tokens; false = input ended unterminated
   BEGIN
      parse-name
      dup 0= IF 2drop RES-FALSE EXIT THEN
      2dup s" ;sumtype" CORE-STR=CI IF 2drop RES-TRUE EXIT THEN
      TDECL-TOKEN+
   AGAIN ;


public

\ Under a multi-error load TDECL-RUN counts a refusal and returns, so each
\ refusal exits before the transaction opens.
: TDECL-SUMTYPE ( -- )
   parse-name {: na:ptr nu:n :}
   TDECL-CLEAR
   SUMTYPE-COLLECT 0= IF
      na nu TDECL-BUF TDECL-U @ TDECL-DEFSUM-NOEND EXIT
   THEN
   na TDN-A !  nu TDN-U !
   TDECL-TXN-ARMED @ 0= IF s" sumtype: declaration transaction not installed" 76 die THEN
   [: TDN-A @ TDN-U @ TDECL-BUF TDECL-U @ TDECL-DEFSUM TDECL-CTOR-WORDS ;]
      TDECL-TXN-XT ;

private

: PRODUCT-COLLECT ( -- bool )   \ buffer tokens; false = input ended unterminated
   BEGIN
      parse-name
      dup 0= IF 2drop RES-FALSE EXIT THEN
      2dup s" ;product" CORE-STR=CI IF 2drop RES-TRUE EXIT THEN
      TDECL-TOKEN+
   AGAIN ;

: TDECL-PRODUCT-NOEND-BODY ( -- )
   TDN-A @ TDN-U @ s" missing ;PRODUCT" E-TDECL-SYNTAX TDECL-THROW ;

: TDECL-PRODUCT-GEN-TXN ( -- )
   [: CHECKER-DEFPRODUCT-BODY TDECL-PRODUCT-GENERATE ;] TDECL-PRODUCT-TXN ;


public

: TDECL-DEFPRODUCT-NOEND ( ptr u8 n ptr u8 n -- )   \ name, partial body -> declaration packet
   {: na:ptr nu:n ba:ptr bu:n :}
   s" product" na nu ba bu TDECL-CTX!
   [: TDECL-PRODUCT-NOEND-BODY ;] TDECL-RUN ;

: TDECL-PRODUCT ( -- )
   parse-name {: na:ptr nu:n :}
   TDECL-CLEAR
   PRODUCT-COLLECT
   s" product" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
   0= IF [: TDECL-PRODUCT-NOEND-BODY ;] TDECL-RUN EXIT THEN
   [: TDECL-PRODUCT-GEN-TXN ;] TDECL-RUN ;

;package

using TYPE-DECL

\ --- the global surface, the only names in this file a user program can spell:
\ global so a declaration registers from top level (test/type-decl-suite.f).
: NEWTYPE ( -- ) TDECL-NEWTYPE ;
: SUMTYPE ( -- ) TDECL-SUMTYPE ;
: PRODUCT ( -- ) TDECL-PRODUCT ;

\ The preverify and all-errors replay paths (verify-source.f) reach
\ CHECKER-DEFFAMILY and CHECKER-NOMINAL-REPLAY; DEFTYPE reaches CHECKER-DEFTYPE.
: CHECKER-DEFFAMILY ( ptr u8 n ptr u8 n -- ) TDECL-DEFFAMILY ;
NOMINAL-PROVIDER
: CHECKER-DEFTYPE ( ptr u8 n ptr u8 n -- ) TDECL-DEFTYPE ;
NOMINAL-PROVIDER
: CHECKER-NOMINAL-REPLAY ( ptr u8 n n -- ) TDECL-NOMINAL-REPLAY ;
: CHECKER-DEFSUM ( ptr u8 n ptr u8 n -- ) TDECL-DEFSUM ;
NOMINAL-PROVIDER
: CHECKER-DEFSUM-NOEND ( ptr u8 n ptr u8 n -- ) TDECL-DEFSUM-NOEND ;
: CHECKER-DEFPRODUCT-NOEND ( ptr u8 n ptr u8 n -- ) TDECL-DEFPRODUCT-NOEND ;
: CHECKER-DEFPRODUCT ( ptr u8 n ptr u8 n -- ) TDECL-DEFPRODUCT ;
NOMINAL-PROVIDER

\ The block openers parse their own body tokens up to the ;NAME closer, so their
\ cell effect is ( -- ). The axiom rows keep them checker-known: the sealed
\ checker refuses a word it has no row for. UNSAFE-TOK? rejects the openers inside checked bodies
\ (they parse the live input and mutate the type registry at run time, which
\ ( -- ) does not express), so the axioms add no checked-code capability. ENUM
\ has no row: it is an ordinary checked ( -- ) definition over ENUM-DECL:ED-RUN,
\ like STRUCTURE.
PRIM: NEWTYPE PRIM;
PRIM: SUMTYPE PRIM;
PRIM: PRODUCT PRIM;

;using
;using
;using
