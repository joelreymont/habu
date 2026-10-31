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
7118 constant E-TDECL-CAP       \ declaration body past TDECL-CAP, or a declared name past TF-NAME-MAX
7119 constant E-TDECL-DERIVE    \ unknown, deferred, or kind-gated DERIVE clause
7204 constant E-TDECL-UNRESOLVED \ quiet replay: a nominal payload declaration may arrive
7210 constant E-TDECL-UNCHECKED \ a constructor-generating declaration while checking is suspended

private

7133 constant E-TDECL-PROVIDER  \ payload provider result contradicts the family's own schema/slot metadata
                                \ (7120-7132 already belong to the checker, layout-buffer, field, and cast blocks)

$1000 constant TDECL-CAP        \ buffered declaration body bytes

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
   a u CK-NAME-SPAN? 0= IF a 0 0 RES-FALSE EXIT THEN
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

\ The body cap bounds its field/variant names. Keep names already parsed so a
\ later duplicate still rejects when an earlier missing type prevented a row.
TDECL-CAP 4 / constant TDECL-SEEN-CAP
TDECL-SEEN-CAP PTR-U8-TABLE TDECL-SEEN-A
create TDECL-SEEN-U TDECL-SEEN-CAP cells allot
variable TDECL-SEEN-N
: TDECL-SEEN-A-FIELD ( n -- ptr ptr u8 ) cells TDECL-SEEN-A + 0 ptr-field ;
: TDECL-SEEN-CLEAR ( -- )
   TDECL-SEEN-N @ 0 ?do
      NULL-PTR i TDECL-SEEN-A-FIELD !
      0 i cells TDECL-SEEN-U + !
   loop
   0 TDECL-SEEN-N ! ;

\ The backing `allot` storage can contain pointers from the host's prior heap.
\ A build captures DATA even when no declaration reads those scratch entries.
TDECL-SEEN-CAP TDECL-SEEN-N !
TDECL-SEEN-CLEAR

: TDECL-SEEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 BEGIN dup TDECL-SEEN-N @ < WHILE
      dup TDECL-SEEN-A-FIELD @
      over cells TDECL-SEEN-U + @ a u CORE-STR= IF drop RES-TRUE EXIT THEN
      1+
   REPEAT drop RES-FALSE ;
: TDECL-SEEN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a TDECL-SEEN-N @ TDECL-SEEN-A-FIELD !
   u TDECL-SEEN-N @ cells TDECL-SEEN-U + !
   TDECL-SEEN-N @ 1 + TDECL-SEEN-N ! ;

\ --- declaration context, set before TDECL-RUN, read by bodies and diagnostics.
PTR-VARIABLE TDK-A   variable TDK-U   \ decl kind token ("newtype"/"sumtype")
PTR-VARIABLE TDN-A   variable TDN-U   \ family name token
PTR-VARIABLE TDB-A   variable TDB-U   \ body (SUMTYPE token buffer / arity token)
PTR-VARIABLE TDT-A   variable TDT-U   \ offending token (diagnostics)
PTR-VARIABLE TDW-A   variable TDW-U   \ short reason (diagnostics)
variable TDECL-OVERSIZE             \ a collection buffer capped an over-cap body
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

variable TDECL-FAM-REG   REG-PROTECT   \ family id registered by the LAST successful sum (-1 = none)

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

: TDECL-REPORT ( -- )
   TDK-A @ TDK-U @  TDN-A @ TDN-U @  TDT-A @ TDT-U @  TDW-A @ TDW-U @
   TDECL-DIAG ;

\ TDECL-RUN ( [ -- ] -- ) : run one declaration body quotation transactionally. On
\ any throw the registries roll back and the failure is reported; a multi-error
\ load counts the reject and continues, otherwise the named code propagates as
\ the refusal the report rendered (checker.f CHECKER-REFUSE). TDECL-OVERSIZE
\ belongs to this declaration: a body that refuses before TDECL-REQUIRE-FIT
\ reads it would otherwise leave it set for the next replay entry.
: TDECL-RUN ( [ -- ] -- )
   TDECL-MARK
   -1 TDECL-FAM-REG !               \ set by a successful sum registration only
   catch {: rc:n :}
   0 TDECL-OVERSIZE !
   TDECL-SEEN-CLEAR
   rc 0= IF EXIT THEN
   TDECL-RESTORE
   rc E-TDECL-UNRESOLVED = IF rc throw THEN
   SIG-UNRES-CLEAR
   TDECL-REPORT
   MULTI-ERR? IF 1 MULTI-ERR-N +! EXIT THEN
   rc CHECKER-REFUSE ;

\ The declared name is read first by the body's name gate and by the report. A
\ length no name has (CK-NAME-SPAN?) is recorded as no name, so the declaration
\ is refused as a missing name and the report reads none of it.
: TDECL-CTX! ( ptr u8 n ptr u8 n ptr u8 n -- )   \ kind, name, body
   TDB-U ! TDB-A !
   2dup CK-NAME-SPAN? 0= IF drop 0 THEN
   TDN-U ! TDN-A !
   TDK-U ! TDK-A !
   0 TDT-U !  NULL-PTR TDT-A !
   SIG-UNRES-CLEAR
   TDECL-SEEN-CLEAR
   -1 TDECL-CUR-FAM !
   s" declaration failed" TDECL-WHY! ;

\ Reject an over-cap declaration body through the declaration packet:
\ either a collection buffer capped an over-long body (TDECL-OVERSIZE, native
\ TDECL-C,) or the collected body already exceeds TDECL-CAP (verify-source /
\ check-core, whose larger buffers hand the full body across). Runs at the head
\ of each body under TDECL-RUN, name already in the TDN context; TDECL-RUN
\ clears the flag.
: TDECL-REQUIRE-FIT ( -- )
   TDECL-OVERSIZE @ 0 <>  TDB-U @ TDECL-CAP >  or IF
      TDN-A @ TDN-U @ s" declaration too long" E-TDECL-CAP TDECL-THROW
   THEN ;

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
   TDN-U ! TDN-A !
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
\ self-reference and rejects recursive. Linear payloads and evidence families
\ are refused.
: TDECL-PAY-FAM? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
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
\ LAY memory descriptor at close (TDECL-LAYOUT-DESC). niche-null and boxed
\ reject as not supported until codegen lowers them. Any other token, `custom`
\ included (the LAY registry tolerates it), is an unknown policy.

private

: TDECL-POLICY-DEFERRED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" niche-null" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" boxed" CORE-STR=CI ;
: TDECL-POLICY-SET ( ptr u8 n n -- ) {: a:ptr u:n fam:n :}
   a u s" stack-cell-tag" CORE-STR=CI IF fam TL-STACK-CELL-TAG TFAM-LAYOUT! EXIT THEN
   a u s" packed-tag" CORE-STR=CI IF fam TL-PACKED-TAG TFAM-LAYOUT! EXIT THEN
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

\ `DERIVE <feature>...` follows the POLICY position, before the variants/fields.
\ eq and hash need a PUBLIC arity-0 family, init a product; order rejects as not
\ supported; an unknown first token rejects, and the list ends at the first
\ non-feature token by pushback. With eq or hash, payload roles are checked per
\ variant/field after the block parses (TDECL-DERIVE-REQUIRE): integer scalars
\ compare with `=`; pointer payloads have no derived equality (no checked
\ pointer-equality surface exists); other scalars reject, linear ones because
\ comparing consumes them; a family-typed field requires that family to derive
\ too (its words are the field comparator).

private

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

: TDECL-DERIVE-NODE-OK ( n -- ) {: node:n :}
   node SCHEMA-CON? IF
      node SCHEMA-A@ CT-INT? 0= IF
         node SCHEMA-A@ CT-NAME$
         s" payload type has no derived equality" E-TDECL-DERIVE TDECL-THROW THEN
      EXIT THEN
   node SCHEMA-PTR? IF
      TDN-A @ TDN-U @
      s" pointer payloads have no derived equality" E-TDECL-DERIVE TDECL-THROW THEN
   node SCHEMA-APP? IF
      node SCHEMA-A@ TFAM-DERIVE-ANY? 0= IF
         node SCHEMA-A@ TFAM-NAME$
         s" field family must also derive" E-TDECL-DERIVE TDECL-THROW THEN
      EXIT THEN
   TDN-A @ TDN-U @
   s" payload role has no derived equality" E-TDECL-DERIVE TDECL-THROW ;

: TDECL-DERIVE-VARIANT ( n -- ) {: vid:n :}
   vid SUMV-PAY-N 0 ?do
      vid i SUMV-PAY-ROOT SCHEMA-ROOT@ TDECL-DERIVE-NODE-OK
   loop ;

public

: TDECL-DERIVE-REQUIRE ( n n n -- ) {: fam:n vstart:n count:n :}
   fam TFAM-DERIVE-EQ? fam TFAM-DERIVE-HASH? or 0= IF EXIT THEN
   vstart count + vstart ?do i TDECL-DERIVE-VARIANT loop ;

\ --- registration entry points (verify-source and the definers below).

private

: CHECKER-DEFSUM-BODY ( -- )
   TDECL-REQUIRE-FIT
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

\ A PRODUCT is a single-shape TK-PRODUCT record: no tag, no variants. Each
\ `FIELD name type` adds one PF-* row (fam, tail, schema root, slot) and one cell
\ of width, so TFAM-WIDTH@ is the field cell count. Field tails are their own
\ namespace: one-letter names are legal (the family RESERVED gate does not
\ apply); TYPE-FIELD-OWNER:ADD enforces lowercase and no duplicate.
\ The generated surface is `make` ( fields -- fam<..> ) and `unmake`
\ ( fam<..> -- fields ), two SUMV rows sharing the sum publish and protection
\ path, compiled with empty bodies: a product bundle is its field cells in slot
\ order. Each is an ordinary stored-effect call unless the instantiation has a
\ layout argument (checker.f SEED-TERM?): one wider than a cell, or a direct
\ layout-family argument of a closed non-linear instantiation, one cell wide
\ included (onebox<oneleaf>). There MAKE takes the construct step seeded from
\ the declared output and UNMAKE the reversed step seeded from its live input
\ (checker.f CTOR-STEP-XT, type-family.f TFAM-UNMAKE-STEP?). Linear
\ instantiations are refused at the sig and argument-bind layers. Only the
\ engine PRODUCT definer generates dictionary words. MATCH and `construct`
\ accept sum/enum only: product rows are never matchable variants and private
\ products have no construction surface.

private

variable TDP-N   \ product field count (schema roots)
variable TDP-W   \ cumulative product cell width / next field's cell OFFSET
variable TDP-EVT


public

\ TDECL-TXN-ARMED carries REG-PROTECT and the two ARMED flags below do not: its
\ only writer is TXN-INSTALL, a compiled reference in this file, and REG-PROTECT
\ closes interpret and the tick, not compile mode.
defer TDECL-TXN-XT ( [ -- ] -- )   REG-PROTECT
variable TDECL-TXN-ARMED   REG-PROTECT
\ The declaration owner installs its runner once, at load; a second install is refused,
\ so the public word cannot replace the transaction runner afterwards.
: TXN-INSTALL ( [ [ -- ] -- ] -- )
   TDECL-TXN-ARMED @ IF s" sumtype: declaration transaction already installed" 76 die THEN
   is TDECL-TXN-XT  -1 TDECL-TXN-ARMED ! ;
defer TDECL-EVENT-CURRENT-XT ( -- n )   REG-PROTECT
defer TDECL-EVENT-DECL-XT ( n n -- n )   REG-PROTECT
defer TDECL-EVENT-ARITY-XT ( n n n -- n )   REG-PROTECT
defer TDECL-EVENT-FIELD-XT ( n n ptr u8 n n n n n n n n -- n )   REG-PROTECT
defer TDECL-EVENT-ARMED ( -- bool )   REG-PROTECT


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
\ signature scope (TFAM-SIG-RESOLVE: own package first, else the unique public;
\ qualified PKG:tail accepted; ambiguity is unresolved). Anything else falls
\ through to the payload grammar, which admits concrete non-linear layout/cell
\ families (self-references reject recursive) and rejects parametric, linear
\ and unknown types (E-TDECL-PAYLOAD).
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
   TDECL-REQUIRE-FIT
   TDECL-HEADER! {: ar:n :}
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-CURSOR!
   ar TK-PRODUCT TDECL-FAMILY {: fam:n :}
   fam TDECL-CUR-FAM !                    \ field self-references reject by id
   fam TDECL-POLICY
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
   ar TK-CELL TDECL-FAMILY drop ;


public

: TDECL-DEFFAMILY ( ptr u8 n -- )   \ family header
   {: na:ptr nu:n :}
   s" newtype" na nu s" " TDECL-CTX!
   [: CHECKER-DEFFAMILY-BODY ;] TDECL-RUN ;

\ Only the engine's SUMTYPE word below generates dictionary words; TDECL-DEFSUM
\ stays metadata-only, so preverify and all-errors dispatch never mutate the
\ tool dictionary and checker rollback stays complete. Each PUBLIC variant
\ becomes, from SUMV metadata only,
\    : <CTOR-PKG>:<VARIANT> ( payload.. -- family<a,..> )
\       construct <family> <variant> ;
\ Before any generated source reaches the evaluator, the generator renders every
\ constructor and derived word, rejects duplicate, live or visibility-invalid
\ symbols, grows all plan-determined registry storage and seals the checker's
\ constructor queue. It checks the dependency-ordered source in one candidate
\ scope, each earlier row pending for later ones (a product's derived HASH calls
\ its UNMAKE; checker.f CK-PENDING-SYM), discards the provisional checker rows
\ and rewinds the queue. Only the accepted plan crosses INCLUDE-EVALUATE, once.
\ Generated text never contains TRUST, TRUSTED: or set-check.
\ include.f binds TDECL-EVAL-XT and xref.f binds TDECL-PROT-WID-XT; a `defer`
\ keeps the call effect-known but cannot be probed for its binding, so an ARMED
\ flag records it: a stage builder that omits those files leaves it 0 and the
\ generator refuses.
defer TDECL-EVAL-XT ( ptr u8 n -- )   REG-PROTECT
defer TDECL-PROT-WID-XT ( ptr u8 n -- )   REG-PROTECT
defer TDECL-NAME-PREFLIGHT-XT ( ptr u8 n -- )   REG-PROTECT
defer TDECL-CAPACITY-PREFLIGHT-XT ( ptr u8 n n -- )   REG-PROTECT
\ TDECL-EVAL-ARMED and TDECL-PROT-WID-ARMED carry no REG-PROTECT. Their writers
\ (include.f, xref.f, test/bootstrap-wide-*-src.f) arm at top level: this file
\ loads unchecked, so nothing it defines except a `defer` is checker-known, and
\ include.f and xref.f load after the checker hook. Measured: the store inside a
\ checked word in include.f answers `undefined word 'TDECL-EVAL-ARMED'`, rc 70;
\ with REG-PROTECT the top-level store in test/bootstrap-wide-memory-src.f
\ answers `hb: internal engine word`, rc 70. The bare name is still hidden:
\ `0 TDECL-EVAL-ARMED !` from user source is E-UNDEFINED, and the qualified
\ spelling dies rc 76 naming its owner. Retiring this needs a checker-known
\ arming surface: a checked half of this file with the flags private behind an
\ ARM word.
variable TDECL-EVAL-ARMED
variable TDECL-PROT-WID-ARMED


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

$4000 constant TDGEN-CAP   \ wide initialized accessors render up to 32 cell operations
create TDGEN-BUF TDGEN-CAP allot
variable TDGEN-U
PTR-VARIABLE TDGEN-NA   variable TDGEN-NU  \ word-name span inside TDGEN-BUF

\ The per-word renderer is bounded by TDGEN-CAP; this plan arena grows before
\ publication, so a large family cannot fail halfway through. Each row keeps
\ relocation-safe offsets for its name and checker definition span. The symbol
\ table rejects collisions within the plan; post-xref name and capacity hooks
\ validate live visibility and every plan-determined fixed registry.
\ A row also carries the field-projection arming a generated address accessor
\ needs (TDECL-ADDR-FIELD-WORD). The sealed window is single-shot and keyed on
\ the accessor's name, and every generated definition is read twice (candidate
\ preflight, evaluator), so the authority rides the row that names the word.
\ FID -1 means no arming: every constructor and derived row.
$1000 constant TDPLAN-CAP-INIT
4 constant TDPLAN-ROW-CAP-INIT
7 cells constant TDPLAN-REC
$7FFFFFFFFFFFFFFF constant TDPLAN-BYTE-MAX
TDPLAN-BYTE-MAX TDPLAN-REC / constant TDPLAN-ROW-MAX
0 cells constant TDPLAN.SYM-OFF
1 cells constant TDPLAN.DEF-OFF-OFF
2 cells constant TDPLAN.DEF-U-OFF
3 cells constant TDPLAN.NAME-OFF-OFF
4 cells constant TDPLAN.NAME-U-OFF
5 cells constant TDPLAN.FID-OFF
6 cells constant TDPLAN.FOFF-OFF
-1 constant TDPLAN-NO-ARM
create TDPLAN-BOOT TDPLAN-CAP-INIT allot
create TDPLAN-ROW-BOOT TDPLAN-ROW-CAP-INIT TDPLAN-REC * allot
PERSISTED-PTR-VARIABLE TDPLAN-P       TDPLAN-BOOT TDPLAN-P !
PERSISTED-PTR-VARIABLE TDPLAN-ROW-P   TDPLAN-ROW-BOOT TDPLAN-ROW-P !
variable TDPLAN-CAP         TDPLAN-CAP-INIT TDPLAN-CAP !
variable TDPLAN-ROW-CAP     TDPLAN-ROW-CAP-INIT TDPLAN-ROW-CAP !
variable TDPLAN-U

public

variable TDPLAN-N   REG-PROTECT

private

variable TDPLAN-SYM-HI        \ the largest symbol a row of this plan holds


public

defer TDECL-OWNER-SNAPSHOT-XT ( -- )   REG-PROTECT

private

: TDECL-OWNER-SNAPSHOT-DEFAULT ( -- ) ;
: TDECL-OWNER-SNAPSHOT-DEFAULTS ( -- )
   [: TDECL-OWNER-SNAPSHOT-DEFAULT ;] is TDECL-OWNER-SNAPSHOT-XT ;
TDECL-OWNER-SNAPSHOT-DEFAULTS

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
   0 TDPLAN-SYM-HI !
   CTOR-PEND-CLEAR ;


private

: TDPLAN-C, ( n -- )
   1 TDPLAN-BYTE-NEED {: need:n :}
   need TDPLAN-ENSURE
   TDPLAN-P @ TDPLAN-U @ + c!
   need TDPLAN-U ! ;

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
: TDPLAN.DEF-OFF ( ptr a -- ptr a ) TDPLAN.DEF-OFF-OFF + ;
: TDPLAN.DEF-U ( ptr a -- ptr a ) TDPLAN.DEF-U-OFF + ;
: TDPLAN.NAME-OFF ( ptr a -- ptr a ) TDPLAN.NAME-OFF-OFF + ;
: TDPLAN.NAME-U ( ptr a -- ptr a ) TDPLAN.NAME-U-OFF + ;
: TDPLAN.FID ( ptr a -- ptr a ) TDPLAN.FID-OFF + ;
: TDPLAN.FOFF ( ptr a -- ptr a ) TDPLAN.FOFF-OFF + ;

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
            TDGEN-NA @ TDGEN-NU @ s" generated declaration name already defined" E-TDECL-NAME TDECL-THROW
         THEN
         1 +
      REPEAT
      drop
   THEN
   sym TDPLAN-SYM-HI @ max TDPLAN-SYM-HI !
   TDPLAN-ROW-ENSURE
   sym TDPLAN-N @ TDPLAN-ROW TDPLAN.SYM !
   TDPLAN-N @ 1 + TDPLAN-N ! ;

: TDPLAN-WORD+ ( -- )
   TDPLAN-N @ 1 - TDPLAN-ROW {: r:ptr :}
   TDGEN-U @ 3 < IF s" sumtype: malformed generated declaration" 76 die THEN
   TDGEN-NA @ TDGEN-BUF - {: noff:n :}
   noff 0 < noff TDGEN-U @ > or
   TDGEN-NU @ 0 < or
   TDGEN-NU @ TDGEN-U @ noff - > or IF
      s" sumtype: malformed generated declaration name span" 76 die
   THEN
   2 TDPLAN-BYTE-NEED r TDPLAN.DEF-OFF !
   TDGEN-U @ 3 - r TDPLAN.DEF-U !
   noff TDPLAN-BYTE-NEED r TDPLAN.NAME-OFF !
   TDGEN-NU @ r TDPLAN.NAME-U !
   TDPLAN-NO-ARM r TDPLAN.FID !
   0 r TDPLAN.FOFF !
   TDGEN-BUF TDGEN-U @ TDPLAN-APP
   10 TDPLAN-C, ;

\ The committed field id and the baked byte offset of the row just planned. The
\ id is the window's whole authority (the checker derives family, offset, extent,
\ role and schema from it), and the offset is cross-checked against it, so an
\ accessor whose text and arming disagree is refused rather than published.
: TDPLAN-ARM+ ( n n -- ) {: fid:n off:n :}
   TDPLAN-N @ 1 - TDPLAN-ROW {: r:ptr :}
   fid r TDPLAN.FID !
   off r TDPLAN.FOFF ! ;

: TDPLAN-NAME+ ( -- n )
   TDGEN-NA @ TDGEN-NU @ CHECKER-DEFINED-HERE? IF
      TDGEN-NA @ TDGEN-NU @ s" generated declaration already defined" E-TDECL-NAME TDECL-THROW
   THEN
   TDGEN-NA @ TDGEN-NU @ CHECKER-RECORD-SYM dup TDPLAN-SYM+ ;

: TDPLAN-CTOR-NAME+ ( n -- n ) {: vid:n :}
   TDPLAN-NAME+ dup vid swap SUMV-CTOR-SYM! ;

: TDPLAN-SUM-CTOR+ ( n -- )
   TDPLAN-CTOR-NAME+ drop
   TDPLAN-WORD+ ;

: TDPLAN-PROD-CTOR+ ( n -- )
   TDPLAN-CTOR-NAME+ CTOR-PEND-SYM+
   TDPLAN-WORD+ ;

: TDPLAN-DERIVED+ ( -- )
   TDPLAN-NAME+ drop
   TDPLAN-WORD+ ;

: TDPLAN-SPAN$ ( n n -- ptr u8 n ) {: off:n u:n :}
   off 0 < u 0 < or off TDPLAN-U @ > or IF
      s" sumtype: invalid generated declaration span" 76 die
   THEN
   u TDPLAN-U @ off - > IF
      s" sumtype: invalid generated declaration span" 76 die
   THEN
   TDPLAN-P @ off + u ;


public

: TDPLAN-DEF$ ( n -- ptr u8 n )
   TDPLAN-ROW {: r:ptr :}
   r TDPLAN.DEF-OFF @ r TDPLAN.DEF-U @ TDPLAN-SPAN$ ;

: TDPLAN-NAME$ ( n -- ptr u8 n )
   TDPLAN-ROW {: r:ptr :}
   r TDPLAN.NAME-OFF @ r TDPLAN.NAME-U @ TDPLAN-SPAN$ ;


private

\ The whole row text, ": NAME ( sig ) body ;", as the evaluator reads it;
\ TDPLAN-DEF$ is the same span without ': ' and ';', the candidate form CHECK!
\ takes. Both come from the recorded definition span so they cannot drift.
: TDPLAN-FULL$ ( n -- ptr u8 n )
   TDPLAN-ROW {: r:ptr :}
   r TDPLAN.DEF-OFF @ 2 -  r TDPLAN.DEF-U @ 3 +  TDPLAN-SPAN$ ;

\ Arms the sealed field-projection window for one generated definition.
\ FIELD-PROJ! binds TYPE-DECL's private row (checker.f), so this caller is
\ checked code. ARENA (arena.f) arms its NAME-NEW and NAME-RELEASE through it
\ too. sumtype.f loads before check-hook.f, so FIELD-ARM has no checker effect
\ and the boot seal marks it internal: only the compiled calls here and in
\ arena.f reach it.
public
: FIELD-ARM ( ptr u8 n n n -- ) FIELD-PROJ! ;
private

\ Arms the window for this row, if it carries one, before each of the two
\ readings of its text: the window is single-shot and disarms at the
\ `field-project` token even on a reject.
: TDPLAN-ARM ( n -- ) {: i:n :}
   i TDPLAN-ROW {: r:ptr :}
   r TDPLAN.FID @ TDPLAN-NO-ARM = IF EXIT THEN
   i TDPLAN-NAME$ r TDPLAN.FID @ r TDPLAN.FOFF @ FIELD-ARM ;

: TDPLAN-PREFLIGHT-NAMES ( -- )
   TDPLAN-N @ 0 ?do i TDPLAN-NAME$ TDECL-NAME-PREFLIGHT-XT loop ;

: TDPLAN-PREFLIGHT-ROWS ( -- )
   TDPLAN-N @ 0 ?do
      i TDPLAN-ARM
      i TDPLAN-DEF$ CHECK! -1 <> IF 70 throw THEN
   loop
   CTOR-PEND-REQUIRE-DONE ;

\ A generated definition takes no declaration location: its rows are checked
\ with the declaring registrar's latch paused (NAV-PAUSE).
: TDPLAN-PREFLIGHT-DEFINITIONS ( -- )
   NAV-PAUSE {: on:n new:n :}
   [: TDPLAN-PREFLIGHT-ROWS ;] catch {: rc:n :}
   on new NAV-RESUME
   rc 0 <> IF rc throw THEN ;

: TDPLAN-PREFLIGHT-CHECKER ( -- )
   CTOR-PEND-REWIND
   CHECKER-CANDIDATE-SCOPE-START
   [: TDPLAN-PREFLIGHT-DEFINITIONS ;] catch {: rc:n :}
   CHECKER-CANDIDATE-SCOPE-DONE
   CTOR-PEND-REWIND
   rc 0 <> IF rc throw THEN ;

: TDPLAN-PREFLIGHT ( -- )
   TDPLAN-PREFLIGHT-NAMES
   TDPLAN-N @ 0= IF s" sumtype: empty generated declaration plan" 76 die THEN
   0 TDPLAN-NAME$ TDPLAN-N @ TDECL-CAPACITY-PREFLIGHT-XT
   TDPLAN-PREFLIGHT-CHECKER ;

: TDGEN-CLEAR ( -- ) 0 TDGEN-U ! ;
: TDGEN-C, ( n -- )
   TDGEN-U @ TDGEN-CAP >= IF s" sumtype: generated constructor too long" 76 die THEN
   TDGEN-BUF TDGEN-U @ + c!
   TDGEN-U @ 1 + TDGEN-U ! ;
: TDGEN-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ TDGEN-C, loop ;
: TDGEN-UPPER ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ TF-UPPER-C TDGEN-C, loop ;
: TDGEN-DEC ( n -- ) {: v:n :}           \ non-negative decimal (tag literal)
   v 10 >= IF v 10 / RECURSE THEN
   v 10 mod 48 + TDGEN-C, ;
: TDGEN-LETTER ( n -- )                   \ positional param -> shared spelling
   TFAM-DECL-PARAM>CHAR 0= IF
      drop s" sumtype: declaration parameter index out of range" 76 die
   THEN
   TDGEN-C, ;

\ A family-application field renders as PKG:tail when the family lives in a
\ package (the generated eval runs while the declaring package is still open,
\ so own-private fields resolve too), else as the bare tail.
: TDGEN-FAM-REF ( n -- ) {: fam:n :}
   fam TFAM-PKG$ {: pa:ptr pu:n :}
   pu 0 > IF pa pu TDGEN-APP 58 TDGEN-C, THEN
   fam TFAM-NAME$ TDGEN-APP ;

\ A quotation side renders each element followed by a space, so an empty side
\ emits nothing. Deferred because SCH-QUOT in TDGEN-SCH reaches it before its
\ body, which calls TDGEN-SCH back, is defined.
defer TDGEN-QUOT-ROW ( n -- )   \ ( rownode -- )

: TDGEN-SCH ( n -- ) {: node:n :}
   node SCHEMA-PARAM? IF node SCHEMA-A@ TDGEN-LETTER EXIT THEN
   node SCHEMA-CON?   IF node SCHEMA-A@ CT-NAME$ TDGEN-APP EXIT THEN
   node SCHEMA-PTR?   IF s" ptr " TDGEN-APP node SCHEMA-A@ RECURSE EXIT THEN
   node SCHEMA-QUOT? IF               \ "[ din -- dout | rin -- rout ]" (row per side)
      s" [ " TDGEN-APP
      node SCHEMA-QUOT-DIN@ TDGEN-QUOT-ROW
      s" -- " TDGEN-APP
      node SCHEMA-QUOT-DOUT@ TDGEN-QUOT-ROW
      node SCHEMA-QUOT-HASR@ 0= 0= IF
         s" | " TDGEN-APP
         node SCHEMA-QUOT-RIN@ TDGEN-QUOT-ROW
         s" -- " TDGEN-APP
         node SCHEMA-QUOT-ROUT@ TDGEN-QUOT-ROW
      THEN
      s" ] " TDGEN-APP
      EXIT
   THEN
   node SCHEMA-APP? IF               \ "fam" or "fam<arg,arg,..>" (RECURSE per arg)
      node SCHEMA-A@ TDGEN-FAM-REF
      node SCHEMA-C@ {: c:n :}
      c 0 > IF
         60 TDGEN-C,
         0 BEGIN dup c < WHILE
            dup 0 > IF 44 TDGEN-C, THEN
            node SCHEMA-B@ over + SCHEMA-ROOT@ RECURSE
            1 +
         REPEAT drop
         62 TDGEN-C,
      THEN
      EXIT
   THEN
   s" sumtype: unsupported constructor payload schema" 76 die ;

: TDGEN-QUOT-ROW-IMPL ( n -- ) {: rownode:n :}
   0 BEGIN dup rownode SCHEMA-ROW-COUNT@ < WHILE
      rownode over SCHEMA-ROW-ELEM@ TDGEN-SCH  32 TDGEN-C,  1 +
   REPEAT drop ;
: TDGEN-QUOT-ROW-INSTALL ( -- ) [: TDGEN-QUOT-ROW-IMPL ;] is TDGEN-QUOT-ROW ;
TDGEN-QUOT-ROW-INSTALL

\ --- payload provider. Everything below renders constructor and DERIVE text from
\ a PROVIDER: one opaque context cell and three quotation capabilities.
\   qn  [ n n n -- n ]    ( ctx fam vid -- count )       payload element count
\   qr  [ n n n n -- n ]  ( ctx fam vid index -- root )  declaration-order schema root
\   qc  [ n n n -- n ]    ( ctx fam vid -- cells )       payload cell width
\ The SUMTYPE / ENUM / PRODUCT definers pass TDECL-SUMV-PROVIDER, which reads the
\ committed SUMV rows; a front end whose declaration is not yet published passes
\ capabilities over its own live declaration token.
\ The provider is asked once, before any text renders; its answers are validated
\ together and copied into the snapshot every render word and DERIVE walker
\ reads. The validated view is the rendered view, and no untrusted quotation runs
\ while a render cursor is live. Validation proves the answers are consistent
\ and fit the family (count within payload slots, roots exist and are at least
\ one cell wide, cell width equals the roots' widths and fits the slots). It
\ does not prove the provider described the right payload: the provider is the
\ authority for which payload a variant carries, and its caller owns the
\ obligation that the view it supplies is the view that will be committed.
$40 constant TDPV-INIT                   \ initial snapshot slots (variants, roots)
create TDPV-CNT-BOOT   TDPV-INIT cells allot
create TDPV-CELLS-BOOT TDPV-INIT cells allot
create TDPV-OFF-BOOT   TDPV-INIT cells allot
create TDPV-NODE-BOOT  TDPV-INIT cells allot
PERSISTED-PTR-VARIABLE TDPV-CNT-P    TDPV-CNT-BOOT TDPV-CNT-P !
PERSISTED-PTR-VARIABLE TDPV-CELLS-P  TDPV-CELLS-BOOT TDPV-CELLS-P !
PERSISTED-PTR-VARIABLE TDPV-OFF-P    TDPV-OFF-BOOT TDPV-OFF-P !
PERSISTED-PTR-VARIABLE TDPV-NODE-P   TDPV-NODE-BOOT TDPV-NODE-P !
variable TDPV-VAR-CAP   TDPV-INIT TDPV-VAR-CAP !
variable TDPV-NODE-CAP  TDPV-INIT TDPV-NODE-CAP !
variable TDPV-FAM   -1 TDPV-FAM !        \ family the snapshot describes (-1 = none)
variable TDPV-BASE                       \ its first variant row
variable TDPV-VARS                       \ variants captured
variable TDPV-NODES                      \ nodes captured

create TDECL-BUF TDECL-CAP allot
variable TDECL-U

\ Every file boundary and image capture leaves the declaration scratch as a cold
\ engine holds it, so a cached load and a fresh one capture the same bytes.
: TDECL-SCRATCH-SNAPSHOT-RESET ( -- )
   CTOR-PEND-COUNT @ 0 <> IF
      s" sumtype: snapshot inside generated declaration" 76 die
   THEN
   CTOR-PEND-P CTOR-PEND-BOOT CTOR-PEND-CAP @ CTOR-PEND-ROWS>BYTES ARENA-BOOT-RESTORE
   CTOR-PEND-CAP-INIT CTOR-PEND-CAP !
   CTOR-PEND-BOOT 0 CTOR-PEND-CAP-INIT CTOR-PEND-REC * CELL / ARENA-CELLS-ZERO
   CTOR-PEND-CLEAR
   TDPLAN-P TDPLAN-BOOT TDPLAN-CAP @ ARENA-BOOT-RESTORE
   TDPLAN-ROW-P TDPLAN-ROW-BOOT TDPLAN-ROW-CAP @ TDPLAN-ROWS>BYTES ARENA-BOOT-RESTORE
   TDPLAN-CAP-INIT TDPLAN-CAP !
   TDPLAN-ROW-CAP-INIT TDPLAN-ROW-CAP !
   0 TDPLAN-U !
   0 TDPLAN-N !
   0 TDPLAN-SYM-HI !
   TDPV-CNT-P TDPV-CNT-BOOT TDPV-VAR-CAP @ cells ARENA-BOOT-RESTORE
   TDPV-CELLS-P TDPV-CELLS-BOOT TDPV-VAR-CAP @ cells ARENA-BOOT-RESTORE
   TDPV-OFF-P TDPV-OFF-BOOT TDPV-VAR-CAP @ cells ARENA-BOOT-RESTORE
   TDPV-NODE-P TDPV-NODE-BOOT TDPV-NODE-CAP @ cells ARENA-BOOT-RESTORE
   TDPV-INIT TDPV-VAR-CAP !
   TDPV-INIT TDPV-NODE-CAP !
   -1 TDPV-FAM !
   0 TDPV-VARS !
   0 TDPV-NODES !
   NULL-PTR TDK-A !  0 TDK-U !
   NULL-PTR TDN-A !  0 TDN-U !
   NULL-PTR TDB-A !  0 TDB-U !
   NULL-PTR TDT-A !  0 TDT-U !
   NULL-PTR TDW-A !  0 TDW-U !
   NULL-PTR TDV-NA !  0 TDV-NU !
   NULL-PTR TDGEN-NA !  0 TDGEN-NU !
   -1 TDECL-CUR-FAM !
   DECL-PARAM-CLEAR
   0 TDP-N !  0 TDP-W !  0 TDP-EVT !
   0 TDM-TFAM !  0 TDM-STR !  0 TDM-PK !  0 TDM-SUMV !
   0 TDM-LAY !  0 TDM-SCH !  0 TDM-ROOT !
   -1 TDECL-FAM-REG !
   0 TDV-TAG !  0 TDV-N !  0 TDV-MAX !
   0 TDV-SS !  0 TDV-PC !  0 TDV-PW !
   TDV-PAY-SCR 0 TDV-PAY-CAP ARENA-CELLS-ZERO  0 TDV-PAY-N !
   TDV-ARG-SCR 0 TDV-ARG-CAP ARENA-CELLS-ZERO  0 TDV-ARG-N !
   TDV-QUOT-SCR 0 TDV-QUOT-CAP ARENA-CELLS-ZERO  QUOT-ROLLBACK
   TDGEN-BUF 0 TDGEN-CAP CELL / ARENA-CELLS-ZERO  0 TDGEN-U !
   TDPLAN-BOOT 0 TDPLAN-CAP-INIT CELL / ARENA-CELLS-ZERO
   TDPLAN-ROW-BOOT 0 TDPLAN-ROW-CAP-INIT TDPLAN-REC * CELL / ARENA-CELLS-ZERO
   TDPV-CNT-BOOT 0 TDPV-INIT ARENA-CELLS-ZERO
   TDPV-CELLS-BOOT 0 TDPV-INIT ARENA-CELLS-ZERO
   TDPV-OFF-BOOT 0 TDPV-INIT ARENA-CELLS-ZERO
   TDPV-NODE-BOOT 0 TDPV-INIT ARENA-CELLS-ZERO
   0 TDPV-BASE !
   TDECL-BUF 0 TDECL-CAP CELL / ARENA-CELLS-ZERO  0 TDECL-U !
   TDECL-OWNER-SNAPSHOT-XT ;

: TDECL-SCRATCH-SNAPSHOT-INSTALL ( -- )
   [: TDECL-SCRATCH-SNAPSHOT-RESET ;] is REG-LATE-SCRATCH-SNAP-XT ;
TDECL-SCRATCH-SNAPSHOT-INSTALL

: TDPV-THROW ( n ptr u8 n -- )   \ family + reason -> the named provider reject
   {: fam:n ra:ptr ru:n :}
   fam TFAM-NAME$ ra ru E-TDECL-PROVIDER TDECL-THROW ;

: TDPV-GROW-CAP ( n n -- n ) {: need:n cap:n :}
   cap 2 * need max ;
: TDPV-VAR-ENSURE ( n -- ) {: need:n :}
   need TDPV-VAR-CAP @ <= IF EXIT THEN
   need TDPV-VAR-CAP @ TDPV-GROW-CAP {: cap:n :}
   TDPV-CNT-P   TDPV-VAR-CAP @ cells cap cells REG-GROW1
   TDPV-CELLS-P TDPV-VAR-CAP @ cells cap cells REG-GROW1
   TDPV-OFF-P   TDPV-VAR-CAP @ cells cap cells REG-GROW1
   cap TDPV-VAR-CAP ! ;
: TDPV-NODE-ENSURE ( n -- ) {: need:n :}
   need TDPV-NODE-CAP @ <= IF EXIT THEN
   need TDPV-NODE-CAP @ TDPV-GROW-CAP {: cap:n :}
   TDPV-NODE-P TDPV-NODE-CAP @ cells cap cells REG-GROW1
   cap TDPV-NODE-CAP ! ;

\ Each capability is called once per answer and every answer is checked before
\ the next is asked for. The count is bounded against the family's payload
\ slots directly, so the arena write and the loop do not rest on any property
\ of the element widths.
: TDPV-COUNT ( n [ n n n -- n ] n n -- n )   \ ctx qn fam slots -- validated count
   {: ctx:n qn fam:n slots:n :}
   ctx fam TDPV-BASE @ TDPV-VARS @ + qn execute {: k:n :}
   k 0 < IF fam s" payload provider returned a negative count" TDPV-THROW THEN
   k slots > IF
      fam s" payload provider count exceeds the family payload slots" TDPV-THROW THEN
   k ;

: TDPV-NODE+ ( n [ n n n n -- n ] n n -- n )   \ ctx qr fam j -- width : append the next validated schema node
   {: ctx:n qr fam:n j:n :}
   ctx fam TDPV-BASE @ TDPV-VARS @ + j qr execute {: root:n :}
   root 0 < root SCHEMA-ROOT-N@ >= or IF
      fam s" payload provider returned an unknown schema root" TDPV-THROW THEN
   root SCHEMA-ROOT@ {: node:n :}
   TDPV-NODES @ 1 + TDPV-NODE-ENSURE
   node TDPV-NODE-P @ TDPV-NODES @ cells + !
   TDPV-NODES @ 1 + TDPV-NODES !
   fam node TFAM-SCH-WIDTH ;

: TDPV-NODES+ ( n [ n n n n -- n ] n n -- n )   \ ctx qr fam k -- width : the variant's whole root run
   {: ctx:n qr fam:n k:n :}
   0
   k 0 ?do ctx qr fam i TDPV-NODE+ + loop ;

\ `cw` is never spelled `cells` here: that is the core cell-size operator, and a
\ local of that name would silently shadow it in the array indexing below.
: TDPV-CELLS ( n [ n n n -- n ] n n n -- n )   \ ctx qc fam slots width -- validated cell width
   {: ctx:n qc fam:n slots:n w:n :}
   ctx fam TDPV-BASE @ TDPV-VARS @ + qc execute {: cw:n :}
   cw w <> IF
      fam s" payload provider cell width contradicts its own schema" TDPV-THROW THEN
   cw slots > IF
      fam s" payload provider cell width exceeds the family payload slots" TDPV-THROW THEN
   cw ;

: TDPV-VARIANT+ ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n n -- )   \ one variant's whole view
   {: ctx:n qn qr qc fam:n slots:n :}
   TDPV-VARS @ 1 + TDPV-VAR-ENSURE
   TDPV-NODES @ TDPV-OFF-P @ TDPV-VARS @ cells + !
   ctx qn fam slots TDPV-COUNT {: k:n :}
   ctx qr fam k TDPV-NODES+ {: w:n :}
   ctx qc fam slots w TDPV-CELLS {: cw:n :}
   k  TDPV-CNT-P @   TDPV-VARS @ cells + !
   cw TDPV-CELLS-P @ TDPV-VARS @ cells + !
   TDPV-VARS @ 1 + TDPV-VARS ! ;

public

: TDPV-CAPTURE ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- )
   {: ctx:n qn qr qc fam:n :}
   -1 TDPV-FAM !
   fam TFAM-VAR-START@ TDPV-BASE !
   0 TDPV-VARS !   0 TDPV-NODES !
   fam TFAM-SLOTS@ {: slots:n :}
   fam TFAM-VAR-COUNT@ {: vars:n :}
   vars 0 ?do ctx qn qr qc fam slots TDPV-VARIANT+ loop
   fam TDPV-FAM ! ;

\ Every snapshot reader names the family it expects and the captured variant
\ range, and fails closed with the named code outside either. The family check
\ is needed because a rolled-back declaration releases its variant rows, so a
\ later family can reuse the same row numbers.

private

: TDPV-SLOT ( n n -- n ) {: fam:n vid:n :}   \ variant row -> snapshot index
   fam TDPV-FAM @ <> IF
      fam s" payload snapshot is not this family's" TDPV-THROW THEN
   vid TDPV-BASE @ - {: i:n :}
   i 0 < i TDPV-VARS @ >= or IF
      fam s" payload snapshot has no such variant" TDPV-THROW THEN
   i ;
: TDPV-N@ ( n n -- n ) {: fam:n vid:n :}     \ payload element count
   TDPV-CNT-P @ fam vid TDPV-SLOT cells + @ ;
: TDPV-CELLS@ ( n n -- n ) {: fam:n vid:n :} \ payload cell width
   TDPV-CELLS-P @ fam vid TDPV-SLOT cells + @ ;
: TDPV-NODE@ ( n n n -- n ) {: fam:n vid:n j:n :}   \ declaration-order schema node
   fam vid TDPV-SLOT {: i:n :}
   j 0 < j TDPV-CNT-P @ i cells + @ >= or IF
      fam s" payload snapshot has no such payload element" TDPV-THROW THEN
   TDPV-NODE-P @ TDPV-OFF-P @ i cells + @ j + cells + @ ;

\ Each capability takes the full ( ctx fam vid .. ) list every provider gets and
\ answers from the variant row alone; ctx and fam are unused here.
: TDECL-SUMV-N ( n n n -- n ) {: ctx:n fam:n vid:n :}
   vid SUMV-PAY-N ;
: TDECL-SUMV-ROOT ( n n n n -- n ) {: ctx:n fam:n vid:n j:n :}
   vid j SUMV-PAY-ROOT ;
: TDECL-SUMV-CELLS ( n n n -- n ) {: ctx:n fam:n vid:n :}
   vid SUMV-PAYCELLS@ ;

public

: TDECL-SUMV-PROVIDER ( -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] )   \ ctx + the three committed capabilities
   0
   [: TDECL-SUMV-N ;]
   [: TDECL-SUMV-ROOT ;]
   [: TDECL-SUMV-CELLS ;] ;


private

: TDGEN-PAYLOAD ( n n -- ) {: fam:n vid:n :}
   fam vid TDPV-N@ {: k:n :}              \ declared inputs, one per schema root
   k 0 ?do
      fam vid i TDPV-NODE@ TDGEN-SCH
      32 TDGEN-C,
   loop ;

\ Qualified for the reason TDGEN-FAM-REF qualifies every payload family: the
\ generated word is interned into the ctor package, so its signature text has no
\ scope in which a bare tail means this family. A bare output type certified at
\ generation (the checker context was still the declaring package) and resolved
\ the GLOBAL family when the same text was re-parsed at AOT intake.
: TDGEN-OUT-TYPE ( n -- ) {: fam:n :}    \ PKG:family<a,b,..>
   fam TDGEN-FAM-REF
   fam TFAM-ARITY@ {: ar:n :}
   ar 0= IF EXIT THEN
   60 TDGEN-C,
   ar 0 ?do
      i 0 > IF 44 TDGEN-C, THEN
      i TDGEN-LETTER
   loop
   62 TDGEN-C, ;

\ ": PKG:VARIANT " when the family's generated words are public, ": FAMILY-MEMBER "
\ for a private or OPAQUE family (TFAM-GEN-PUBLIC?), with the name span recorded
\ either way. Those families' words go into the declaring package's private
\ wordlist, where a name carrying a colon can never land (TF-CTOR-PKG$ /
\ TF-CTOR-PRIV$, type-family.f).
: TDGEN-NAME ( n -- ) {: vid:n :}
   s" : " TDGEN-APP
   TDGEN-U @ {: n0:n :}
   vid SUMV-FAM@ {: fam:n :}
   fam TFAM-GEN-PUBLIC? IF
      vid SUMV-CTOR-PKG$ TDGEN-APP
      58 TDGEN-C,
      vid SUMV-NAME$ TDGEN-UPPER
   ELSE
      fam TFAM-NAME$ vid SUMV-NAME$ TF-CTOR-PRIV$ TDGEN-APP
   THEN
   TDGEN-BUF n0 + TDGEN-NA !
   TDGEN-U @ n0 - TDGEN-NU !
   32 TDGEN-C, ;

: TDGEN-CONSTRUCT-BODY ( n n -- ) {: fam:n vid:n :}
   s" construct " TDGEN-APP
   fam TFAM-NAME$ TDGEN-APP  32 TDGEN-C,
   vid SUMV-NAME$ TDGEN-APP
   s"  ;" TDGEN-APP ;

: TDECL-GEN-EVAL-BODY ( -- )
   TDECL-EVAL-ARMED @ 0= IF s" sumtype: constructor eval hook not installed" 76 die THEN
   TDPLAN-PREFLIGHT
   TDPLAN-P @ TDPLAN-U @ TDECL-EVAL-XT
   CTOR-PEND-REQUIRE-DONE ;

\ The constructor queue is an authority, not scratch state. This inner boundary
\ proves exact consumption on success and clears name/capacity/candidate/evaluator
\ failures; TDECL-CTOR-WORDS-BODY starts the outer owner before plan rendering.
: TDECL-GEN-EVAL ( -- )
   [: TDECL-GEN-EVAL-BODY ;] catch {: rc:n :}
   CTOR-PEND-CLEAR
   rc 0 <> IF rc throw THEN ;


public

: TDECL-CTOR-WORD ( n n -- ) {: fam:n vid:n :}
   TDGEN-CLEAR
   vid TDGEN-NAME
   s" ( " TDGEN-APP
   fam vid TDGEN-PAYLOAD
   s" -- " TDGEN-APP
   fam TDGEN-OUT-TYPE
   s"  ) " TDGEN-APP
   fam vid TDGEN-CONSTRUCT-BODY
   vid TDPLAN-SUM-CTOR+ ;


private

\ A PRIVATE or OPAQUE family protects no wordlist: its words live in the
\ declaring package's private wordlist, and protecting that wid would refuse
\ every later definition the package makes (in a sealed engine
\ EMIT-STORE-DEF-NAME, habu2.f, exits 84 ENGINE-ERROR:SEAL-PACKAGE). The words
\ stay protected by name through TFAM-CTOR-WORD? and the checker's CTOR-WORD?-XT
\ undefine guard.
: TDECL-CTOR-PROT-WID ( n -- ) {: vid:n :}
   vid SUMV-FAM@ TFAM-GEN-PUBLIC? 0= IF EXIT THEN
   TDECL-PROT-WID-ARMED @ 0= IF s" sumtype: protected-wid hook not installed" 76 die THEN
   TDGEN-CLEAR
   vid TDGEN-NAME
   TDGEN-NA @ TDGEN-NU @ TDECL-PROT-WID-XT ;

\ `: PKG:MAKE ( fields -- fam<..> ) ;` and `: PKG:UNMAKE ( fam<..> -- fields ) ;`:
\ empty bodies under a k=0 pending window, because a product bundle is its field
\ cells in slot order (no pads, no tag); the declared sig, rendered from the PF
\ field schemas, is the checker's truth. mk nonzero means make.
: TDECL-PROD-WORD ( n n n -- ) {: fam:n vid:n mk:n :}
   TDGEN-CLEAR
   vid TDGEN-NAME
   s" ( " TDGEN-APP
   mk 0 <> IF
      fam vid TDGEN-PAYLOAD
      s" -- " TDGEN-APP
      fam TDGEN-OUT-TYPE
      s"  ) ;" TDGEN-APP
   ELSE
      fam TDGEN-OUT-TYPE
      s"  -- " TDGEN-APP
      fam vid TDGEN-PAYLOAD
      s" ) ;" TDGEN-APP
   THEN
   vid TDPLAN-PROD-CTOR+ ;

\ --- derived typed equality. `DERIVE eq` generates ordinary checked words into
\ the family's constructor package: no pending window, no trust rows, no engine
\ lowering, so equality is semantic and layout-policy agnostic. Derived eq
\ consumes both operands.
\  - TAG ( fam -- n ): declaration-order tag, which any checked MATCH already
\    observes. Enums and sums only.
\  - EQ ( fam fam -- bool ): payload-free families compare tags; payload sums
\    compare diagonally (outer MATCH binds one value's payloads to q-locals,
\    inner MATCH the other's to p-locals; the same-variant arm compares with `=`,
\    integer widening at the bind making every CT-INT payload an n; cross arms
\    drop and answer false); products UNMAKE both values, bind fields top-down
\    (an enum-typed field goes through its family's PKG:TAG) and compare
\    field-wise.
\ The scalar `=`/`0=` wall on layout values stays (test/type-decl-suite.f
\ TD12-ZEQ); extend/undefine protection recognizes the fixed tails through
\ TFAM-DERIVED-AT? (products: eq only).
\ A derived word is "PKG-FAMILY:TAIL" for a public family and "FAMILY-TAIL" for a
\ private or OPAQUE one, as TDGEN-NAME spells a constructor. Neither stamps a
\ constructor package (TDECL-CTOR-PUBLISH exits early), so SUMV-CTOR-PKG$ would
\ name nothing.
: TDGEN-DRV-REF ( n ptr u8 n -- ) {: fam:n ta:ptr tu:n :}
   fam TFAM-GEN-PUBLIC? 0= IF
      fam TFAM-NAME$ ta tu TF-CTOR-PRIV$ TDGEN-APP EXIT THEN
   fam TFAM-VAR-START@ SUMV-CTOR-PKG$ TDGEN-APP
   58 TDGEN-C,
   ta tu TDGEN-UPPER ;
: TDGEN-DRV-NAME ( n ptr u8 n -- ) {: fam:n ta:ptr tu:n :}   \ ": PKG:TAIL " span-recorded
   s" : " TDGEN-APP
   TDGEN-U @ {: n0:n :}
   fam ta tu TDGEN-DRV-REF
   TDGEN-BUF n0 + TDGEN-NA !
   TDGEN-U @ n0 - TDGEN-NU !
   32 TDGEN-C, ;
: TDGEN-DROPS ( n -- ) {: k:n :}          \ "drop " x k
   k 0 ?do s" drop " TDGEN-APP loop ;
: TDGEN-LOCAL1 ( n n -- ) {: c:n i:n :}   \ "<c><i>:n "
   c TDGEN-C,  i TDGEN-DEC  s" :n " TDGEN-APP ;
: TDGEN-BINDS ( n n n -- ) {: fam:n vid:n c:n :}   \ "{: c0:n .. :} " over vid's payload slots
   fam vid TDPV-N@ {: m:n :}
   m 0= IF EXIT THEN
   s" {: " TDGEN-APP
   m 0 ?do c i TDGEN-LOCAL1 loop
   s" :} " TDGEN-APP ;
: TDGEN-CMP ( n -- ) {: m:n :}            \ "p0 q0 = p1 q1 = and .. " (m=0 -> true)
   m 0= IF s" 0 0= " TDGEN-APP EXIT THEN
   m 0 ?do
      112 TDGEN-C,  i TDGEN-DEC  32 TDGEN-C,
      113 TDGEN-C,  i TDGEN-DEC  32 TDGEN-C,
      s" = " TDGEN-APP
      i 0 > IF s" and " TDGEN-APP THEN
   loop ;
: TDGEN-MATCH-OPEN ( n -- ) {: fam:n :}   \ "match fam "
   s" match " TDGEN-APP  fam TFAM-NAME$ TDGEN-APP  32 TDGEN-C, ;
: TDGEN-TAG-BODY ( n -- ) {: fam:n :}   \ "match fam v of <drops> <tag> endof .. ;match ;"
   fam TDGEN-MATCH-OPEN
   fam TFAM-VAR-START@ {: vstart:n :}
   vstart fam TFAM-VAR-COUNT@ + vstart ?do
      i SUMV-NAME$ TDGEN-APP
      s"  of " TDGEN-APP
      fam i TDPV-N@ TDGEN-DROPS
      i SUMV-TAG@ TDGEN-DEC
      s"  endof " TDGEN-APP
   loop
   s" ;match ;" TDGEN-APP ;
: TDGEN-EQ-ARM ( n n n -- ) {: fam:n vout:n vin:n :}   \ one inner arm: diagonal cmp / off-diag drop+false
   vin SUMV-NAME$ TDGEN-APP  s"  of " TDGEN-APP
   vin vout = IF
      fam vin 112 TDGEN-BINDS
      fam vin TDPV-N@ TDGEN-CMP
   ELSE
      fam vin TDPV-N@ TDGEN-DROPS
      s" 1 0= " TDGEN-APP
   THEN
   s" endof " TDGEN-APP ;
: TDGEN-EQ-ROW ( n n n n -- ) {: fam:n vstart:n k:n vout:n :}   \ vout's inner arms
   vstart k + vstart ?do fam vout i TDGEN-EQ-ARM loop ;
: TDGEN-EQ-DIAG ( n -- ) {: fam:n :}   \ diagonal double-match body for payload sums
   fam TDGEN-MATCH-OPEN
   fam TFAM-VAR-START@ {: vstart:n :}
   fam TFAM-VAR-COUNT@ {: k:n :}
   vstart k + vstart ?do
      i SUMV-NAME$ TDGEN-APP  s"  of " TDGEN-APP
      fam i 113 TDGEN-BINDS
      fam TDGEN-MATCH-OPEN
      fam vstart k i TDGEN-EQ-ROW
      s" ;match endof " TDGEN-APP
   loop
   s" ;match ;" TDGEN-APP ;
: TDGEN-UNBIND1 ( n n n n -- ) {: fam:n mk:n k:n c:n :}   \ field k's bind
   fam mk k TDPV-NODE@ dup SCHEMA-APP? IF
      dup SCHEMA-A@ s" tag" TDGEN-DRV-REF  32 TDGEN-C, THEN
   drop
   s" {: " TDGEN-APP  c k TDGEN-LOCAL1  s" :} " TDGEN-APP ;
: TDGEN-UNBIND ( n n -- ) {: fam:n c:n :}   \ "PKG:UNMAKE " + top-down field binds
   fam s" unmake" TDGEN-DRV-REF  32 TDGEN-C,
   fam TFAM-VAR-START@ {: mk:n :}
   fam mk TDPV-N@ {: m:n :}
   m 0 ?do fam mk m 1 - i - c TDGEN-UNBIND1 loop ;
: TDGEN-EQ-PROD ( n -- ) {: fam:n :}   \ UNMAKE both values, field-wise compare
   fam 113 TDGEN-UNBIND
   fam 112 TDGEN-UNBIND
   fam fam TFAM-VAR-START@ TDPV-N@ TDGEN-CMP
   59 TDGEN-C, ;
: TDGEN-EQ-TAGS ( n -- ) {: fam:n :}      \ payload-free family: tag equality (O(V))
   fam s" tag" TDGEN-DRV-REF  s"  swap " TDGEN-APP
   fam s" tag" TDGEN-DRV-REF  s"  = ;" TDGEN-APP ;

\ Derived hash: FNV-1a over whole cells, h = BASIS, then per cell
\ h = (h xor v) * PRIME. Sums and enums fold the tag, then each bound payload
\ scalar; products fold the fields (enum-typed fields through their family's
\ PKG:TAG, as derived eq does), so equal values always hash equal. Hash values
\ are an in-memory contract only: never persist them across engine versions.
$cbf29ce484222325 constant DRV-FNV-BASIS   \ FNV-1a 64-bit offset basis
$100000001b3 constant DRV-FNV-PRIME        \ FNV-1a 64-bit prime
: TDGEN-HEXD ( n -- ) {: d:n :}
   d 10 < IF d 48 + ELSE d 87 + THEN TDGEN-C, ;
: TDGEN-HEX ( n -- ) {: v:n :}            \ "$" + 16 nibbles + " "
   36 TDGEN-C,
   0 15 ?do v  i 4 *  rshift  15 and  TDGEN-HEXD  -1 +loop
   32 TDGEN-C, ;
: TDGEN-HMIX ( -- )                       \ "xor $PRIME * "
   s" xor " TDGEN-APP  DRV-FNV-PRIME TDGEN-HEX  s" * " TDGEN-APP ;
: TDGEN-HASH-FOLDS ( n -- ) {: m:n :}     \ fold locals p0..pm-1
   m 0 ?do 112 TDGEN-C,  i TDGEN-DEC  32 TDGEN-C,  TDGEN-HMIX loop ;
: TDGEN-HASH-ARM ( n n -- ) {: fam:n vid:n :}   \ bind payloads, fold tag then payloads
   fam vid 112 TDGEN-BINDS
   DRV-FNV-BASIS TDGEN-HEX
   vid SUMV-TAG@ TDGEN-DEC  32 TDGEN-C,  TDGEN-HMIX
   fam vid TDPV-N@ TDGEN-HASH-FOLDS ;
: TDGEN-HASH-BODY ( n -- ) {: fam:n :}   \ sum/enum: MATCH ladder of per-arm folds
   fam TDGEN-MATCH-OPEN
   fam TFAM-VAR-START@ {: vstart:n :}
   vstart fam TFAM-VAR-COUNT@ + vstart ?do
      i SUMV-NAME$ TDGEN-APP  s"  of " TDGEN-APP
      fam i TDGEN-HASH-ARM
      s" endof " TDGEN-APP
   loop
   s" ;match ;" TDGEN-APP ;
: TDGEN-HASH-PROD ( n -- ) {: fam:n :}   \ product: UNMAKE, fold fields (no tag)
   fam 112 TDGEN-UNBIND
   DRV-FNV-BASIS TDGEN-HEX
   fam fam TFAM-VAR-START@ TDPV-N@ TDGEN-HASH-FOLDS
   59 TDGEN-C, ;
: TDECL-TAG-WORD ( n -- ) {: fam:n :}
   TDGEN-CLEAR
   fam s" tag" TDGEN-DRV-NAME
   s" ( " TDGEN-APP  fam TDGEN-OUT-TYPE  s"  -- n ) " TDGEN-APP
   fam TDGEN-TAG-BODY
   TDPLAN-DERIVED+ ;
: TDECL-EQ-WORD ( n -- ) {: fam:n :}
   TDGEN-CLEAR
   fam s" eq" TDGEN-DRV-NAME
   s" ( " TDGEN-APP
   fam TDGEN-OUT-TYPE  32 TDGEN-C,  fam TDGEN-OUT-TYPE
   s"  -- bool ) " TDGEN-APP
   fam TFAM-PRODUCT? IF fam TDGEN-EQ-PROD ELSE
   fam TFAM-SLOTS@ 0= IF fam TDGEN-EQ-TAGS ELSE fam TDGEN-EQ-DIAG THEN THEN
   TDPLAN-DERIVED+ ;
: TDECL-HASH-WORD ( n -- ) {: fam:n :}
   TDGEN-CLEAR
   fam s" hash" TDGEN-DRV-NAME
   s" ( " TDGEN-APP  fam TDGEN-OUT-TYPE  s"  -- n ) " TDGEN-APP
   fam TFAM-PRODUCT? IF fam TDGEN-HASH-PROD ELSE fam TDGEN-HASH-BODY THEN
   TDPLAN-DERIVED+ ;
: TDECL-DRV-WORDS ( n -- ) {: fam:n :}
   fam TFAM-DERIVE-ANY? 0= IF EXIT THEN
   fam TFAM-PRODUCT? 0= IF fam TDECL-TAG-WORD THEN   \ discriminant rides any derive
   fam TFAM-DERIVE-EQ? IF fam TDECL-EQ-WORD THEN
   fam TFAM-DERIVE-HASH? IF fam TDECL-HASH-WORD THEN ;

: TDECL-PROD-PLAN ( n -- ) {: fam:n :}   \ make (row 0) + unmake (row 1)
   fam TFAM-VAR-START@ {: vstart:n :}
   TDPLAN-BEGIN
   fam vstart 1 TDECL-PROD-WORD
   fam vstart 1 + 0 TDECL-PROD-WORD
   fam TDECL-DRV-WORDS ;

\ A live declaration's `private` / `public` switch reaches the checker through
\ the engine (DECLARATIONS PRIVATE-OFF). A replay defines no word, and its
\ pre-pass (verify-source.f, tools/check-core.f) opens the package in the
\ checker alone, so an engine `private` there dies `private at
\ tools/check.f:1`. A replay switches the checker's mode only.
: TDECL-GEN-PRIVATE ( bool -- ) {: replay:bool :}
   replay IF CHECKER-PRIVATE EXIT THEN
   s" private" TDECL-EVAL-XT ;
: TDECL-GEN-RESTORE ( n bool -- ) {: fam:n replay:bool :}
   fam TFAM-PUBLIC? 0= IF replay TDECL-GEN-PRIVATE EXIT THEN
   replay IF CHECKER-PUBLIC EXIT THEN
   s" public" TDECL-EVAL-XT ;

\ Q renders, preflights and evaluates (or replays) a family's generated rows. A
\ private or OPAQUE family's rows (not TFAM-GEN-PUBLIC?) go into the declaring
\ package's private wordlist, so Q runs between a switch to private and a
\ restore of the declaration's section, on either exit. Everything that interns
\ a name runs inside the switch: the name preflight asks the wordlist the name
\ will land in (xref.f TARGET-RECORD), and CHECKER-RECORD-SYM interns a plan's
\ constructor authorities per section, so a plan rendered in another section
\ queues authorities CTOR-PEND-MATCH? never finds. Q takes and returns the
\ family: a quotation cannot read this word's locals, and catch takes only a
\ stack-preserving quotation.
: TDECL-GEN-PLACED ( n bool [ n -- n ] -- ) {: fam:n replay:bool q :}
   fam TFAM-GEN-PUBLIC? IF fam q execute drop EXIT THEN
   replay TDECL-GEN-PRIVATE
   fam q catch {: rc:n :}
   fam replay TDECL-GEN-RESTORE
   rc 0 <> IF rc throw THEN
   drop ;

: TDECL-PROD-WORDS-BODY ( n -- ) {: fam:n :}
   fam RES-FALSE [: dup TDECL-PROD-PLAN TDECL-GEN-EVAL ;] TDECL-GEN-PLACED
   fam TFAM-VAR-START@ TDECL-CTOR-PROT-WID ;

\ The ( fam -- ) product generator structure-make.f calls: it declares its
\ variant rows through SUMV-ADD before generating, so the committed view is its
\ own view.

public

: TDECL-PROD-WORDS ( n -- ) {: fam:n :}
   TDECL-SUMV-PROVIDER fam TDPV-CAPTURE
   fam TDECL-PROD-WORDS-BODY ;

\ --- generated ADDRESS accessors (`DERIVE addr`).
\ One word per field plus AT / BYTES / CELLS, in the family's own spelling:
\
\   : PKG-BUF:DATA ( ptr PKG:buf -- ptr ptr u8 ) 0 field-project ;
\   : PKG-BUF:AT   ( ptr PKG:buf n -- ptr PKG:buf ) 3 * record-at ;
\   : PKG-BUF:BYTES ( -- n ) 24 ;
\
\ The bodies are ordinary checked text: no TRUST row, no set-check, no pending
\ window. Outside the armed window `field-project` is `+` and `record-at` is
\ `cells +`; the checker replaces the first only while this plan's window is
\ live, and the second only on a layout pointee, which the plain rows fence. The
\ committed field id is the whole authority: the checker re-derives family,
\ offset, extent, role and schema from it and refuses a body whose baked offset,
\ arity, role or output type disagrees.
\ Generation runs at commit: a field id is an authority only once TYPE-FIELD has
\ committed the row, which the runner's commit order (DECL-EVENT before the
\ address surface) guarantees.

private

: TDGEN-ADDR-IN ( n -- ) {: fam:n :}      \ "( ptr PKG:family<..> "
   s" ( ptr " TDGEN-APP  fam TDGEN-OUT-TYPE  32 TDGEN-C, ;

\ ( ptr F -- ptr T ) <byte-offset> field-project
: TDECL-ADDR-FIELD-WORD ( n n -- ) {: fam:n fid:n :}
   TDGEN-CLEAR
   fam  fid PF-NAME$  TDGEN-DRV-NAME
   fam TDGEN-ADDR-IN
   s" -- ptr " TDGEN-APP  fid PF-SCH@ SCHEMA-ROOT@ TDGEN-SCH
   s" ) " TDGEN-APP
   fid PF-BYTE-OFF@ TDGEN-DEC
   s"  field-project ;" TDGEN-APP
   TDPLAN-DERIVED+
   fid fid PF-BYTE-OFF@ TDPLAN-ARM+ ;

\ Strides by the family's committed width. The multiply is baked because the row
\ cannot scale: it keeps the pointee and adds cells, as `cells +` does on any
\ other pointer.
: TDECL-ADDR-AT-WORD ( n -- ) {: fam:n :}
   TDGEN-CLEAR
   fam s" at" TDGEN-DRV-NAME
   fam TDGEN-ADDR-IN
   s" n -- ptr " TDGEN-APP  fam TDGEN-OUT-TYPE
   s"  ) " TDGEN-APP
   fam TFAM-SLOTS@ TDGEN-DEC
   s"  * record-at ;" TDGEN-APP
   TDPLAN-DERIVED+ ;

: TDECL-ADDR-SIZE-WORD ( n ptr u8 n n -- ) {: fam:n ta:ptr tu:n v:n :}
   TDGEN-CLEAR
   fam ta tu TDGEN-DRV-NAME
   s" ( -- n ) " TDGEN-APP
   v TDGEN-DEC
   s"  ;" TDGEN-APP
   TDPLAN-DERIVED+ ;

: TDECL-ADDR-PLAN ( n -- ) {: fam:n :}
   TDPLAN-BEGIN
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fam  fs i +  TDECL-ADDR-FIELD-WORD
   loop
   fam TDECL-ADDR-AT-WORD
   fam s" bytes" fam TFAM-SLOTS@ CELL * TDECL-ADDR-SIZE-WORD
   fam s" cells" fam TFAM-SLOTS@        TDECL-ADDR-SIZE-WORD ;

\ Evaluation is row by row, unlike the constructor set's single evaluator call,
\ because each armed row must be armed again for the evaluator's reading. The
\ plan is checked whole first (TDPLAN-PREFLIGHT), so a set that cannot certify
\ reaches no evaluator.
: TDECL-ADDR-EVAL ( -- )
   TDECL-EVAL-ARMED @ 0= IF s" sumtype: accessor eval hook not installed" 76 die THEN
   TDPLAN-PREFLIGHT
   TDPLAN-N @ 0 ?do
      i TDPLAN-ARM
      i TDPLAN-FULL$ TDECL-EVAL-XT
   loop ;

: TDECL-ADDR-BODY ( n -- )
   RES-FALSE [: dup TDECL-ADDR-PLAN TDECL-ADDR-EVAL ;] TDECL-GEN-PLACED ;

\ Replay takes only the first reading: each row is armed and checked, so the
\ accessor's effect registers under its own name, and no row reaches
\ TDECL-EVAL-XT. The arming is live: this participant commits after DECL-EVENT
\ advances the committed field watermark on both paths, so the baked field ids
\ are committed ids.
: TDECL-ADDR-REPLAY-BODY ( n -- )
   RES-TRUE [: dup TDECL-ADDR-PLAN TDPLAN-PREFLIGHT-DEFINITIONS ;] TDECL-GEN-PLACED ;

public

\ The window is disarmed on every exit: a rejected row leaves its arming live
\ otherwise, and an arming outlives no generation.
: TDECL-ADDR-WORDS ( n -- )
   [: dup TDECL-ADDR-BODY ;] catch {: rc:n :}
   FIELD-PROJ-CLEAR
   rc 0 <> IF rc throw THEN
   drop ;

\ Replay (check-core.f's nominal pass, verify-source.f) registers the checked
\ effects of a generated set without emitting code; the address surface replays
\ from the same plan the live path evaluates, so a file that uses its own record
\ accessors resolves on the pre-scan.
: TDECL-ADDR-REPLAY ( n -- )
   [: dup TDECL-ADDR-REPLAY-BODY ;] catch {: rc:n :}
   FIELD-PROJ-CLEAR
   rc 0 <> IF rc throw THEN
   drop ;

\ Initialized fields publish through a trusted path: a checked TDPLAN row is
\ ": NAME ..." and CHECK! judges its body, and neither describes the fixed
\ representation crossing here. The rows render as complete TRUSTED:
\ definitions and replay their declared effects through the same registrar as
\ source TRUSTED: declarations.
private

variable TDINIT-SIG-OFF
variable TDINIT-SIG-U

TRUSTED: TDINIT-MARK-LAST ( -- ) ndict@ 1 - int-mark ;
TRUSTED: TDINIT-DECLARE ( ptr u8 n ptr u8 n -- ) TRUST-DECL ;

: TDINIT-TERM ( n -- n ) {: fam:n :}
   PARAM-SCR-N @ {: base:n :}
   fam TFAM-ARITY@ 0 ?do FRESH MK-VAR PARAM-SCR+ loop
   base fam TFAM-NAME$ fam MK-PARAM ;

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

: TDINIT-H ( n -- ) {: fam:n :}
   s" mut-view<" TDGEN-APP
   fam TFAM-ARITY@ dup TDGEN-LETTER 44 TDGEN-C,
   1 + dup TDGEN-LETTER 44 TDGEN-C,
   1 + dup TDGEN-LETTER s" ,init<" TDGEN-APP
   1 + TDGEN-LETTER 44 TDGEN-C,
   fam TDGEN-OUT-TYPE s" >>" TDGEN-APP ;

: TDINIT-F ( n -- ) PF-SCH@ SCHEMA-ROOT@ TDGEN-SCH ;

: TDINIT-NAME-START ( -- )
   TDGEN-CLEAR
   s" TRUSTED: " TDGEN-APP
   TDGEN-BUF TDGEN-U @ + TDGEN-NA ! ;

: TDINIT-NAME-END ( -- )
   TDGEN-U @ TDGEN-NA @ TDGEN-BUF - - TDGEN-NU !
   32 TDGEN-C, ;

: TDINIT-HELP-NAME ( n n -- ) {: fam:n fid:n :}
   TDINIT-NAME-START
   fam fid TFAM-INIT-HELPER$ TDGEN-APP
   TDINIT-NAME-END ;

: TDINIT-RECEIVER-NAME ( n -- ) {: fam:n :}
   TDINIT-NAME-START
   fam TFAM-INIT-RECEIVER$ TDGEN-APP
   TDINIT-NAME-END ;

: TDINIT-PUB-NAME ( n n bool -- ) {: fam:n fid:n store:bool :}
   TDINIT-NAME-START
   fam fid store TFAM-INIT-MEMBER$ TDGEN-DRV-REF
   TDINIT-NAME-END ;

\ The recorded span is the effect between the parentheses, the text TRUSTED:
\ hands TRUST-DECL (habu2.f C-SIG-INNER$), so a replay row declares it as is.
: TDINIT-SIG-BEGIN ( -- )
   s" ( " TDGEN-APP
   TDGEN-U @ TDINIT-SIG-OFF ! ;

: TDINIT-SIG-END ( -- )
   TDGEN-U @ TDINIT-SIG-OFF @ - TDINIT-SIG-U !
   s"  ) " TDGEN-APP ;

: TDINIT-RAW-ROW ( n -- ) {: cellsn:n :}
   cellsn 0 ?do s" n " TDGEN-APP loop ;

\ The receiver is one logical, two-cell mutable view. Split it through its
\ exact original type before a trusted body binds the address and bound.
: TDINIT-RECEIVER ( n -- ) {: fam:n :}
   fam TDINIT-RECEIVER-NAME
   TDINIT-SIG-BEGIN
   fam TDINIT-H s"  -- ptr u8 n" TDGEN-APP
   TDINIT-SIG-END
   s" ;" TDGEN-APP ;

\ A helper has the precise original F as input and exactly its committed
\ number of raw cells as output. Its empty body changes only stack grouping.
\ It lives in the author's private wordlist and is marked DNAME-INT before any
\ public accessor can refer to it.
: TDINIT-HELP ( n n -- ) {: fam:n fid:n :}
   fam fid TDINIT-HELP-NAME
   TDINIT-SIG-BEGIN
   fid TDINIT-F
   s"  -- " TDGEN-APP
   fid PF-CELLS@ TDINIT-RAW-ROW
   TDINIT-SIG-END
   s" ;" TDGEN-APP ;

: TDINIT-GET ( n n -- ) {: fam:n fid:n :}
   fam fid RES-FALSE TDINIT-PUB-NAME
   TDINIT-SIG-BEGIN
   fam TDINIT-H s"  -- " TDGEN-APP
   fam TDINIT-H 32 TDGEN-C, fid TDINIT-F
   TDINIT-SIG-END
   fam TFAM-INIT-RECEIVER$ TDGEN-APP
   s"  {: p:ptr b:n :} p b " TDGEN-APP
   fid PF-CELLS@ 0 ?do
      s" p " TDGEN-APP
      fid PF-BYTE-OFF@ i CELL * + TDGEN-DEC
      s"  + cell-view @ " TDGEN-APP
   loop
   s" ;" TDGEN-APP ;

: TDINIT-SET ( n n -- ) {: fam:n fid:n :}
   fam fid RES-TRUE TDINIT-PUB-NAME
   TDINIT-SIG-BEGIN
   fam TDINIT-H 32 TDGEN-C, fid TDINIT-F
   s"  -- " TDGEN-APP fam TDINIT-H
   TDINIT-SIG-END
   fam fid TFAM-INIT-HELPER$ TDGEN-APP
   s"  {: " TDGEN-APP
   fid PF-CELLS@ 0 ?do
      99 TDGEN-C, i TDGEN-DEC s" :n " TDGEN-APP
   loop
   s" :} " TDGEN-APP
   fam TFAM-INIT-RECEIVER$ TDGEN-APP
   s"  {: p:ptr b:n :} " TDGEN-APP
   fid PF-CELLS@ 0 ?do
      99 TDGEN-C, i TDGEN-DEC s"  p " TDGEN-APP
      fid PF-BYTE-OFF@ i CELL * + TDGEN-DEC
      s"  + cell-view ! " TDGEN-APP
   loop
   s" p b ;" TDGEN-APP ;

: TDINIT-NAME-PREFLIGHT ( -- )
   TDGEN-NA @ TDGEN-NU @ TDECL-NAME-PREFLIGHT-XT ;

\ The receiver and helpers are private whatever the family's placement; the
\ public GET/SET members follow the family (TDINIT-ROW).
: TDINIT-HELP-NAME-CHECK ( n bool -- ) {: fam:n replay:bool :}
   replay TDECL-GEN-PRIVATE
   [: TDINIT-NAME-PREFLIGHT ;] catch {: rc:n :}
   fam replay TDECL-GEN-RESTORE
   rc 0 <> IF rc throw THEN ;

: TDINIT-HELP-EVAL ( n -- ) {: fam:n :}
   RES-FALSE TDECL-GEN-PRIVATE
   [: TDGEN-BUF TDGEN-U @ TDECL-EVAL-XT TDINIT-MARK-LAST ;] catch {: rc:n :}
   fam RES-FALSE TDECL-GEN-RESTORE
   rc 0 <> IF rc throw THEN ;

: TDINIT-HELP-REPLAY ( n -- ) {: fam:n :}
   RES-TRUE TDECL-GEN-PRIVATE
   NAV-PAUSE {: on:n new:n :}
   [: TDGEN-NA @ TDGEN-NU @
      TDGEN-BUF TDINIT-SIG-OFF @ + TDINIT-SIG-U @ TDINIT-DECLARE ;]
      catch {: rc:n :}
   on new NAV-RESUME
   fam RES-TRUE TDECL-GEN-RESTORE
   rc 0 <> IF rc throw THEN ;

\ An initialized-field row is generated, so it is declared paused (NAV-PAUSE).
: TDINIT-REPLAY-ROW ( -- )
   NAV-PAUSE {: on:n new:n :}
   [: TDGEN-NA @ TDGEN-NU @
      TDGEN-BUF TDINIT-SIG-OFF @ + TDINIT-SIG-U @ TDINIT-DECLARE ;]
      catch {: rc:n :}
   on new NAV-RESUME
   rc 0 <> IF rc throw THEN ;

\ Placed as the family's generated words are.
: TDINIT-ROW ( n bool -- ) {: fam:n replay:bool :}
   replay IF fam replay [: TDINIT-REPLAY-ROW ;] TDECL-GEN-PLACED EXIT THEN
   fam replay [: TDGEN-BUF TDGEN-U @ TDECL-EVAL-XT ;] TDECL-GEN-PLACED ;

: TDINIT-PREFLIGHT ( n bool -- ) {: fam:n replay:bool :}
   fam TDINIT-REQUIRE
   fam TFAM-FLD-START@ {: fs:n :}
   fam TDINIT-RECEIVER fam replay TDINIT-HELP-NAME-CHECK
   TDGEN-NA @ TDGEN-NU @ fam TFAM-FLD-COUNT@ 3 * 1 +
       TDECL-CAPACITY-PREFLIGHT-XT
   fs fam TFAM-FLD-COUNT@ + fs ?do
      fam i TDINIT-HELP fam replay TDINIT-HELP-NAME-CHECK
      fam i TDINIT-GET fam replay [: TDINIT-NAME-PREFLIGHT ;] TDECL-GEN-PLACED
      fam i TDINIT-SET fam replay [: TDINIT-NAME-PREFLIGHT ;] TDECL-GEN-PLACED
   loop ;

: TDINIT-GENERATE ( n bool -- ) {: fam:n replay:bool :}
   fam replay TDINIT-PREFLIGHT
   fam TFAM-FLD-START@ {: fs:n :}
   fam TDINIT-RECEIVER
   replay IF fam TDINIT-HELP-REPLAY ELSE fam TDINIT-HELP-EVAL THEN
   fs fam TFAM-FLD-COUNT@ + fs ?do
      fam i TDINIT-HELP
      replay IF fam TDINIT-HELP-REPLAY ELSE fam TDINIT-HELP-EVAL THEN
      fam i TDINIT-GET fam replay TDINIT-ROW
      fam i TDINIT-SET fam replay TDINIT-ROW
   loop ;

public

: TDECL-INIT-WORDS ( n -- ) RES-FALSE TDINIT-GENERATE ;
: TDECL-INIT-REPLAY ( n -- ) RES-TRUE TDINIT-GENERATE ;

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

\ TDECL-GEN-BODY generates one family through the caller's provider;
\ TDECL-CTOR-WORDS-BODY is its failure boundary. Rendering can fail after
\ constructors have entered the authority queue, before TDECL-GEN-EVAL, so the
\ boundary owns the whole render/preflight/evaluate/protect lifetime and clears
\ the queue before every result escapes. Provider and family id travel on the
\ data stack because a quotation cannot read its caller's locals.

private

\ A PUBLIC family always publishes generated words: into the reserved
\ constructor namespace, or, when OPAQUE, into the declaring package's private
\ wordlist (TDECL-GEN-PLACED). A PRIVATE PRODUCT publishes into that private
\ wordlist too. A private SUM or ENUM publishes nothing: it constructs through
\ the checker-owned `construct` token, which needs no dictionary word.
: TDECL-GENERATES? ( n -- bool ) {: fam:n :}
   fam TFAM-PUBLIC? IF RES-TRUE EXIT THEN
   fam TFAM-PRODUCT? ;

: TDECL-GEN-PLAN ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n )
   {: ctx:n qn qr qc fam:n :}
   fam TDECL-GENERATES? 0= IF ctx qn qr qc fam EXIT THEN
   ctx qn qr qc fam TDPV-CAPTURE          \ the whole payload view, once, validated
   fam TFAM-PRODUCT? IF
      fam TDECL-PROD-PLAN  ctx qn qr qc fam EXIT THEN
   \ Parametric families publish too: a constructor's parametric result expands
   \ to hidden fields wherever its width is known, linear included (checker.f
   \ PUSH-LOGICAL), and stays one logical cell only while its width reads an
   \ open argument.
   TDPLAN-BEGIN
   fam TFAM-VAR-COUNT@ 0 ?do
      fam  fam TFAM-VAR-START@ i +  TDECL-CTOR-WORD
   loop
   fam TDECL-DRV-WORDS
   ctx qn qr qc fam ;

: TDECL-GEN-BODY ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n )
   dup TDECL-GENERATES? 0= IF EXIT THEN
   TDECL-GEN-PLAN
   TDECL-GEN-EVAL
   dup TFAM-VAR-START@ TDECL-CTOR-PROT-WID ;

\ Replay publishes checked effects from the same constructor bodies as a live
\ declaration. It emits no code and creates no runtime dictionary entries.
: TDECL-REPLAY-PLAN ( n -- n ) {: family:n :}
   TDECL-SUMV-PROVIDER family TDECL-GEN-PLAN
   TDPLAN-PREFLIGHT-DEFINITIONS
   {: ctx:n qn qr qc fam:n :}
   fam ;
: TDECL-REPLAY-BODY ( n -- n ) {: family:n :}
   family RES-TRUE [: TDECL-REPLAY-PLAN ;] TDECL-GEN-PLACED
   family ;


public

: TDECL-CTOR-REPLAY ( n -- )
   dup TDECL-GENERATES? 0= IF drop EXIT THEN
   [: TDECL-REPLAY-BODY ;] catch {: rc:n :}
   CTOR-PEND-CLEAR
   rc 0 <> IF rc throw THEN
   drop ;

: TDECL-CTOR-WORDS-BODY ( n [ n n n -- n ] [ n n n n -- n ] [ n n n -- n ] n -- n )
   \ `catch` restores the DEPTH of both stacks and never their contents, so the
   \ five cells the body was handed come back stale: the family this word
   \ answers with is kept in a local BEFORE the catch, and the cells the
   \ generation left are dropped.
   dup {: fam:n :}
   [: TDECL-GEN-BODY ;] catch {: rc:n :}
   CTOR-PEND-CLEAR
   rc 0 <> IF rc throw THEN
   2drop 2drop drop
   fam ;

\ The SUMTYPE / ENUM / PRODUCT definers announce their family through
\ TDECL-FAM-REG; that ambient read lives only in the two adapters below, which
\ also choose the committed payload view (TDECL-SUMV-PROVIDER). A SUMTYPE
\ refused under a multi-error load leaves -1 (TDECL-RUN's continue path) and
\ generates nothing; PRODUCT generates inside the body TDECL-RUN guards, so a
\ refused one never reaches generation.

private

: TDECL-CTOR-WORDS ( -- )
   TDECL-FAM-REG @ {: fam:n :}
   fam 0 < IF EXIT THEN
   TDECL-SUMV-PROVIDER fam TDECL-CTOR-WORDS-BODY drop ;

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

\ SUMTYPE, PRODUCT, STRUCTURE and ENUM generate constructor words, which the
\ checker certifies: with the hook suspended (`0 set-check`) a product's
\ constructor plan is never consumed and a sum's constructors carry rows without
\ authority. Each live definer refuses such a declaration through its
\ declaration packet, after collecting the body and before registering
\ anything: SUMTYPE and PRODUCT below, STRUCTURE and ENUM in DECL-FRONT:RUN. A
\ NEWTYPE generates no word and is admitted. The replay entries (CHECKER-DEFSUM,
\ *-REPLAY) define no word and stay open: verify-source.f runs them hookless in
\ the fixpoint's certify child (tools/build-fixpoint-certify.f).
\ test/sumtype-unchecked-e2e.f pins this.
: TDECL-UNCHECKED$ ( -- ptr u8 n )
   s" checking is suspended; declare it where checking is on" ;

: TDECL-NEWTYPE ( -- )
   parse-name TDECL-DEFFAMILY ;


private

: TDECL-UNCHECKED-BODY ( -- )
   TDN-A @ TDN-U @ TDECL-UNCHECKED$ E-TDECL-UNCHECKED TDECL-THROW ;

: TDECL-CLEAR ( -- ) 0 TDECL-U !  0 TDECL-OVERSIZE ! ;
: TDECL-C, ( n -- )                     \ over-cap: cap + flag (never raw-die); the
   TDECL-U @ 1 + TDECL-CAP > IF drop -1 TDECL-OVERSIZE ! EXIT THEN   \ body reaches the checker
   TDECL-BUF TDECL-U @ + c!
   TDECL-U @ 1 + TDECL-U ! ;
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
   check@ 0= IF
      s" sumtype" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
      [: TDECL-UNCHECKED-BODY ;] TDECL-RUN EXIT
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

: TDECL-PRODUCT ( -- )
   parse-name {: na:ptr nu:n :}
   TDECL-CLEAR
   PRODUCT-COLLECT
   s" product" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
   0= IF [: TDECL-PRODUCT-NOEND-BODY ;] TDECL-RUN EXIT THEN
   check@ 0= IF [: TDECL-UNCHECKED-BODY ;] TDECL-RUN EXIT THEN
   [: TDECL-PRODUCT-GEN-TXN ;] TDECL-RUN ;

;package

using TYPE-DECL

\ --- the global surface: these seven names are the only part of this file a
\ user program can spell. They stay global because a user declaration must
\ register without opening any package (test/type-decl-suite.f declares from
\ top level). Each is a one-line entry into package TYPE-DECL, as ENUM is in
\ enum-decl.f; test/internal-word-gate.f SEAL-CASES pins that the surface
\ stops there.
: NEWTYPE ( -- ) TDECL-NEWTYPE ;
: SUMTYPE ( -- ) TDECL-SUMTYPE ;
: PRODUCT ( -- ) TDECL-PRODUCT ;

\ Reached by the preverify and all-errors replay paths (verify-source.f,
\ tools/check-core.f) and by checked declarers built on them
\ (src/lib/type/deftype.f DEFTYPE). Their PRIM: rows live in checker.f and stay
\ PRIM: because the names stay global.
: CHECKER-DEFFAMILY ( ptr u8 n -- ) TDECL-DEFFAMILY ;
NOMINAL-PROVIDER
: CHECKER-DEFSUM ( ptr u8 n ptr u8 n -- ) TDECL-DEFSUM ;
NOMINAL-PROVIDER
: CHECKER-DEFSUM-NOEND ( ptr u8 n ptr u8 n -- ) TDECL-DEFSUM-NOEND ;
: CHECKER-DEFPRODUCT ( ptr u8 n ptr u8 n -- ) TDECL-DEFPRODUCT ;
NOMINAL-PROVIDER

\ The block openers parse their own body tokens up to the ;NAME closer, so their
\ cell effect is ( -- ). The axiom rows keep them checker-known: the seal-time
\ marking pass (internal-mark.f) fails closed on any engine-prefix word without
\ a checker-known effect. UNSAFE-TOK? rejects the openers inside checked bodies
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
