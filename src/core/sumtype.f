split-str
\ sumtype.f: package TYPE-DECL, the NEWTYPE and SUMTYPE declaration
\ grammar (ENUM is enum-decl.f; the user grammar is docs/forth.md "Structures
\ And Enums"). Loaded unchecked in the checker prefix.
\ Every failure is transactional: the registry marks restore, TDECL-DIAG
\ (render.f) reports, and the declaration counts as a multi-error reject or
\ throws its code. The replay entries are at the foot of the file; NEWTYPE
\ (structure-decl.f) and SUMTYPE (sumtype-front.f) load after the declaration
\ runner.

package TYPE-DECL
EXPORT CHECK:CHECKER-CANDIDATE-SCOPE-START
EXPORT CHECK:CHECKER-CANDIDATE-SCOPE-DONE
EXPORT CHECK:TDECL-DIAG
EXPORT CHECK:STGR-REASON$
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-DECLARATION
EXPORT CHECK:DEF-NAME-BAD?
EXPORT CHECK:DEF-NAME-REJECT?
EXPORT TFAM:DRV-ADDR
EXPORT TFAM:DRV-EQ
EXPORT TFAM:DRV-HASH
EXPORT TFAM:DRV-INIT
EXPORT TFAM:DRV-TAG
EXPORT TFAM:INIT-FIELD?
EXPORT TFAM:INIT-RECORD?
EXPORT TFAM:LAY-ADD
EXPORT TFAM:LAY-N
EXPORT TFAM:MEMBER-SYM!
EXPORT TFAM:PACKED-DESC
EXPORT TFAM:PF-ADD
EXPORT TFAM:PF-COMMIT
EXPORT TFAM:PF-FLAG-BUFFER
EXPORT TFAM:PF-FLAGS-NONE
EXPORT TFAM:PF-FLAGS@
EXPORT TFAM:PF-KIND-OK?
EXPORT TFAM:PF-NAME$
EXPORT TFAM:PF-NAME-REQUIRE
EXPORT TFAM:PF-NODE-KIND?
EXPORT TFAM:PF-SCH@
EXPORT TFAM:PF.CELLS
EXPORT TFAM:PF.FLAGS
EXPORT TFAM:PK-CELL
EXPORT TFAM:PK-REGION
EXPORT TFAM:PK-SCOPE
EXPORT TFAM:PK-TYPE
EXPORT TFAM:READ-PENDING
EXPORT TFAM:SR-AMBIG
EXPORT TFAM:SR-FOUND
EXPORT TFAM:SUMV-ADD
EXPORT TFAM:SUMV-CTOR-PKG!
EXPORT TFAM:SUMV-CTOR-PKG$
EXPORT TFAM:SUMV-CTOR-SYM!
EXPORT TFAM:SUMV-FAM@
EXPORT TFAM:SUMV-FROM-CTOR-SYM
EXPORT TFAM:SUMV-N
EXPORT TFAM:SUMV-N@
EXPORT TFAM:SUMV-NAME$
EXPORT TFAM:SUMV-PAY-N
EXPORT TFAM:SUMV-PAY-ROOT
EXPORT TFAM:SUMV-SCH-COUNT@
EXPORT TFAM:SUMV-SCH-START@
EXPORT TFAM:SUMV-TAG@
EXPORT TFAM:TF-CANON?
EXPORT TFAM:TF-CTOR-PKG$
EXPORT TFAM:TF-CTOR-PRIV$
EXPORT TFAM:TF-GRAMMAR-KEYWORD?
EXPORT TFAM:TF-INTERN
EXPORT TFAM:TF-PK-N
EXPORT TFAM:TF-STR-U
EXPORT TFAM:TF-UPPER-C
EXPORT TFAM:TFAM-ACTIVE-PKG$
EXPORT TFAM:TFAM-ADDR-FIXED-TAIL?
EXPORT TFAM:TFAM-ARITY@
EXPORT TFAM:TFAM-CELL?
EXPORT TFAM:TFAM-CHILD-ROWS
EXPORT TFAM:TFAM-CONCRETE-LINEAR?
EXPORT TFAM:TFAM-CTOR-PKG?
EXPORT TFAM:TFAM-DECL
EXPORT TFAM:TFAM-DECL-CHAR>PARAM
EXPORT TFAM:TFAM-DECL-PARAM-COUNT
EXPORT TFAM:TFAM-DECL-PARAM>CHAR
EXPORT TFAM:TFAM-DERIVE-ADDR!
EXPORT TFAM:TFAM-DERIVE-ADDR?
EXPORT TFAM:TFAM-DERIVE-ANY?
EXPORT TFAM:TFAM-DERIVE-EQ!
EXPORT TFAM:TFAM-DERIVE-EQ?
EXPORT TFAM:TFAM-DERIVE-HASH!
EXPORT TFAM:TFAM-DERIVE-HASH?
EXPORT TFAM:TFAM-DERIVE-INIT!
EXPORT TFAM:TFAM-DERIVE-INIT?
EXPORT TFAM:TFAM-DERIVE-TAG!
EXPORT TFAM:TFAM-DERIVE-TAG?
EXPORT TFAM:TFAM-DERIVED-TAIL?
EXPORT TFAM:TFAM-ENUM?
EXPORT TFAM:TFAM-FLD-COUNT@
EXPORT TFAM:TFAM-FLD-START@
EXPORT TFAM:TFAM-FRESH-TERM
EXPORT TFAM:TFAM-GEN-PUBLIC?
EXPORT TFAM:TFAM-INIT-MEMBER$
EXPORT TFAM:TFAM-KIND!
EXPORT TFAM:TFAM-LAYOUT!
EXPORT TFAM:TFAM-LAYOUT-POLICY@
EXPORT TFAM:TFAM-LAYOUT?
EXPORT TFAM:TFAM-MAY-ARRIVE?
EXPORT TFAM:TFAM-N
EXPORT TFAM:TFAM-NAME$
EXPORT TFAM:TFAM-NICHE?
EXPORT TFAM:TFAM-NONVALUE-PARAM?
EXPORT TFAM:TFAM-OPAQUE!
EXPORT TFAM:TFAM-OPAQUE?
EXPORT TFAM:TFAM-PK@
EXPORT TFAM:TFAM-PKG$
EXPORT TFAM:TFAM-PRODUCT?
EXPORT TFAM:TFAM-REWIND
EXPORT TFAM:TFAM-SCH-ARITY
EXPORT TFAM:TFAM-SCH-WIDTH
EXPORT TFAM:TFAM-SIG-RESOLVE
EXPORT TFAM:TFAM-SLOTS!
EXPORT TFAM:TFAM-SLOTS@
EXPORT TFAM:TFAM-SUM?
EXPORT TFAM:TFAM-USED-MASK
EXPORT TFAM:TFAM-VAR-COUNT@
EXPORT TFAM:TFAM-VAR-RANGE!
EXPORT TFAM:TFAM-VAR-START@
EXPORT TFAM:TFCL-NODE?
EXPORT TFAM:TK-CELL
EXPORT TFAM:TK-ENUM
EXPORT TFAM:TK-PRODUCT
EXPORT TFAM:TK-SUM
EXPORT TFAM:TL-NICHE
EXPORT TFAM:TL-PACKED-TAG
EXPORT TFAM:TL-STACK-CELL-TAG
EXPORT SCHEMA-REG:EFFECT-IN
EXPORT SCHEMA-REG:EFFECT-OUT
EXPORT SCHEMA-REG:EMPTY-EFFECT
EXPORT SCHEMA-REG:REWIND
EXPORT SCHEMA-REG:SCH-N
EXPORT SCHEMA-REG:SCH-ROOT-N
EXPORT SCHEMA-REG:SCHEMA-A@
EXPORT SCHEMA-REG:SCHEMA-APP
EXPORT SCHEMA-REG:SCHEMA-APP?
EXPORT SCHEMA-REG:SCHEMA-CON
EXPORT SCHEMA-REG:SCHEMA-CON?
EXPORT SCHEMA-REG:SCHEMA-PARAM
EXPORT SCHEMA-REG:SCHEMA-PTR
EXPORT SCHEMA-REG:SCHEMA-PTR?
EXPORT SCHEMA-REG:SCHEMA-QUOT
EXPORT SCHEMA-REG:SCHEMA-ROOT+
EXPORT SCHEMA-REG:SCHEMA-ROOT-N@
EXPORT SCHEMA-REG:SCHEMA-ROOT@
EXPORT SCHEMA-REG:SCHEMA-ROW
EXPORT SCHEMA-REG:SCHEMA-XT
EXPORT SCHEMA-REG:TRANSIENT-OFF
EXPORT SCHEMA-REG:TRANSIENT-ON

public

7107 constant E-TDECL-SYNTAX    \ malformed declaration (name/arity/terminator/token)
7108 constant E-TDECL-ARITY     \ malformed type parameter header
7109 constant E-TDECL-PAYLOAD   \ unknown variant payload type
7110 constant E-TDECL-NAME      \ reserved or colliding family/variant name
7116 constant E-TDECL-POLICY    \ unknown or not-yet-supported layout policy (7111-7115 = checker.f E-CTOR/E-EXPORT)
7117 constant E-TDECL-RECURSIVE \ direct self-family payload under a non-boxed policy
7118 constant E-TDECL-CAP       \ a record past MAKE's items, or a type nested too deep
7119 constant E-TDECL-DERIVE    \ unknown, deferred, or kind-gated DERIVE clause
7120 constant E-TDECL-SEALED    \ a family or generated name that would change a sealed package
7204 constant E-TDECL-UNRESOLVED \ quiet replay: a nominal payload declaration may arrive

-9880 constant E-ENUM-TAG        \ a number that is not one of an enum's tags (FAM:TAG>)

private

create DECL-PARAM-NAMES TFAM-DECL-PARAM-COUNT allot
variable DECL-PARAM-N
\ A header parameter's state: PARAM-UNUSED, ANY-VALUE while only pointees and
\ quotation elements use it, else the kind a use fixed; and the field setting it,
\ at an offset in FIELD-NAMES.
-2 constant PARAM-UNUSED
create PARAM-USES TFAM-DECL-PARAM-COUNT cells allot
create PARAM-FIELD-OS TFAM-DECL-PARAM-COUNT cells allot
create PARAM-FIELD-US TFAM-DECL-PARAM-COUNT cells allot
\ The names of the fields that set parameters, then the open field's name.
PTR-VARIABLE FIELD-NAMES-P
SCRATCH-VARIABLE FIELD-NAMES-CAP   \ FIELD-NAMES-P's room, in cells
variable FIELD-NAMES-U             \ bytes the parameters' fields take
variable OPEN-FIELD-O
variable OPEN-FIELD-U
: FIELD-NAMES ( -- ^u8 ) FIELD-NAMES-P @ ;
: USE-SLOT ( n -- ^n ) cells PARAM-USES + ;
: FIELD-O-SLOT ( n -- ^n ) cells PARAM-FIELD-OS + ;
: FIELD-U-SLOT ( n -- ^n ) cells PARAM-FIELD-US + ;
\ Forgets the header parameters: a declaration opens with none until its
\ header lists them.
: DECL-PARAM-CLEAR ( -- )
   0 DECL-PARAM-N !  0 FIELD-NAMES-U !  0 OPEN-FIELD-O !  0 OPEN-FIELD-U !
   TFAM-DECL-PARAM-COUNT 0 ?DO
      0 DECL-PARAM-NAMES i + c!
      PARAM-UNUSED i USE-SLOT !
      0 i FIELD-O-SLOT !  0 i FIELD-U-SLOT !
   LOOP ;
DECL-PARAM-CLEAR

\ Declaration-local names bind by list position. The registry and generated
\ effects still use positional schema indices and the canonical alphabet.
: DECL-PARAM ( n -- n bool )
   {: char:n :}
   0 BEGIN dup DECL-PARAM-N @ < WHILE
      DECL-PARAM-NAMES over + c@ char = IF RES-TRUE EXIT THEN
      1+
   REPEAT drop 0 RES-FALSE ;

EXPORT CHECK:ARENA-ROWS-ENSURE
EXPORT CHECK:BIND-DEPTH
EXPORT CHECK:C2-READ-FAM
EXPORT CHECK:CHECKER-AUTH-PACKAGE$
EXPORT CHECK:CHECKER-AUTH-PACKAGE-ACTIVE?
EXPORT CHECK:CHECKER-AUTH-PACKAGE-MODE@
EXPORT CHECK:CHECKER-CHILD-ROWS
EXPORT CHECK:CHECKER-DEFINED-HERE?
EXPORT CHECK:CHECKER-GLOBAL
EXPORT CHECK:CHECKER-PRIVATE
EXPORT CHECK:CHECKER-PUBLIC
EXPORT CHECK:CHECKER-REC-SYM
EXPORT CHECK:CHECKER-RECORD-SYM
EXPORT CHECK:CHECKER-SIG-UNRES-TAKE
EXPORT CHECK:CON-OF
EXPORT CHECK:CT-INT?
EXPORT CHECK:CTL-STATABLE
EXPORT CHECK:DELIM?
EXPORT CHECK:EFFECT-ROWS
EXPORT CHECK:FAM-RESET
EXPORT CHECK:FRESH-ATOM-TOK?
EXPORT CHECK:MULTI-ERR?
EXPORT CHECK:NAV-PAUSE
EXPORT CHECK:NAV-RESUME
EXPORT CHECK:NEXT-SIG-TOK
EXPORT CHECK:NMAP-RESET
EXPORT CHECK:NOMINAL-PROVIDER
EXPORT CHECK:OPAQUE-XT?
EXPORT CHECK:P>REST
EXPORT CHECK:P>TYPE
EXPORT CHECK:PARAM>ARG
EXPORT CHECK:PARAM>ARGC
EXPORT CHECK:PARAM>FAM
EXPORT CHECK:PAY
EXPORT CHECK:PK!
EXPORT CHECK:PKRESET
EXPORT CHECK:PTR>INNER
EXPORT CHECK:Q>DIN
EXPORT CHECK:Q>DOUT
EXPORT CHECK:Q>FACTS
EXPORT CHECK:Q>RIN
EXPORT CHECK:Q>ROUT
EXPORT CHECK:R-RES
EXPORT CHECK:REG-GROW1
EXPORT CHECK:ROW-LEAD?
EXPORT CHECK:ROW-TAIL
EXPORT CHECK:ROWMAP-RESET
EXPORT CHECK:RR-SHARED
EXPORT CHECK:SB!
EXPORT CHECK:SGBAD
EXPORT CHECK:SGBAD-A
EXPORT CHECK:SGBAD-AMBIG?
EXPORT CHECK:SGBAD-ARITY?
EXPORT CHECK:SGBAD-CLEAR
EXPORT CHECK:SGBAD-U
EXPORT CHECK:SGBAD-UNKNOWN?
EXPORT CHECK:SGHASR
EXPORT CHECK:SI
EXPORT CHECK:SIG-CARET-BARE?
EXPORT CHECK:SIG-CARET-POINTEE
EXPORT CHECK:SIG-CARET?
EXPORT CHECK:SIG-FAM?
EXPORT CHECK:SIG-LEVEL
EXPORT CHECK:SIG-TYPE
EXPORT CHECK:SIG-UNRES
EXPORT CHECK:SIG-UNRES!
EXPORT CHECK:SIG-UNRES-CLEAR
EXPORT CHECK:SIG-VAR-SLOT
EXPORT CHECK:SL
EXPORT CHECK:SQUOT-N
EXPORT CHECK:STATED-FLAG
EXPORT CHECK:T-RES
EXPORT CHECK:TAG
EXPORT CHECK:TDECL-TITLE
EXPORT CHECK:TFAM-WIDTH-SLOT-XT
EXPORT CHECK:UNSTORED-REASON
EXPORT CHECK:UPKG-HELP
EXPORT CHECK:WRITE-WINDOW-CK

\ A use any value kind fits: a pointee or a quotation element.
-1 constant ANY-VALUE

\ The open field's name, kept after the names the parameters hold.
: DECL-FIELD! ( str -- ) {: a:^ u:n :}
   FIELD-NAMES-P FIELD-NAMES-CAP
   FIELD-NAMES-U @ u + CELL / 1 +  1 ARENA-ROWS-ENSURE
   FIELD-NAMES-U @ OPEN-FIELD-O !
   u 0 ?DO a i + c@ FIELD-NAMES OPEN-FIELD-O @ + i + c! LOOP u OPEN-FIELD-U ! ;

: PARAM-FIELD ( n -- str ) {: idx:n :}
   FIELD-NAMES idx FIELD-O-SLOT @ +  idx FIELD-U-SLOT @ ;
\ Parameter idx keeps the open field's name, which stays when the next opens.
: PARAM-FIELD! ( n -- ) {: idx:n :}
   OPEN-FIELD-O @ idx FIELD-O-SLOT !  OPEN-FIELD-U @ idx FIELD-U-SLOT !
   OPEN-FIELD-O @ OPEN-FIELD-U @ + FIELD-NAMES-U ! ;
: NONVALUE? ( n -- bool ) dup PK-SCOPE = swap PK-REGION = or ;
\ A pointee or quotation element fits a value kind; a scope or region fits
\ only itself.
: USE-CONFLICT? ( n n -- bool ) {: was:n want:n :}
   was PARAM-UNUSED = IF RES-FALSE EXIT THEN
   was ANY-VALUE = IF want NONVALUE? EXIT THEN
   want was = IF RES-FALSE EXIT THEN
   want ANY-VALUE = IF was NONVALUE? EXIT THEN
   RES-TRUE ;

\ Records header parameter idx used as want; answers its state and the field
\ that set it: before this use and true on a conflict, else after it and false.
: DECL-PARAM-USE ( n n -- n str bool ) {: idx:n want:n :}
   idx USE-SLOT @ {: was:n :}
   was want USE-CONFLICT? IF was idx PARAM-FIELD RES-TRUE EXIT THEN
   was PARAM-UNUSED = was ANY-VALUE = or want was <> and IF
      want idx USE-SLOT !  idx PARAM-FIELD! THEN
   idx USE-SLOT @ idx PARAM-FIELD RES-FALSE ;

\ A header's family name: its bytes before the first `<`.
: DECL-FAMILY-NAME ( str -- str ) {: a:^ u:n :}
   a  0 BEGIN dup u < IF a over + c@ 60 <> ELSE RES-FALSE THEN WHILE 1+ REPEAT ;

: DECL-HEADER ( str -- str n bool )
   {: a:^ u:n :}
   DECL-PARAM-CLEAR
   a u DECL-FAMILY-NAME nip {: tail:n :}
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

\ Keep names already parsed so a later duplicate still rejects when an earlier
\ missing type prevented a row.
PTR-U8-TABLE-VARIABLE TDECL-SEEN-A
PTR-VARIABLE TDECL-SEEN-U
SCRATCH-VARIABLE TDECL-SEEN-A-CAP
SCRATCH-VARIABLE TDECL-SEEN-U-CAP
variable TDECL-SEEN-N
: TDECL-SEEN-CLEAR ( -- ) 0 TDECL-SEEN-N ! ;
: TDECL-SEEN-NAME ( n -- ^^u8 ) TDECL-SEEN-A @ swap ptr-field ;
: TDECL-SEEN-LEN ( n -- ^n ) cells TDECL-SEEN-U @ + ;
: TDECL-SEEN-ENSURE ( n -- ) {: need:n :}
   TDECL-SEEN-A TDECL-SEEN-A-CAP need 1 ARENA-ROWS-ENSURE
   TDECL-SEEN-U TDECL-SEEN-U-CAP need 1 ARENA-ROWS-ENSURE ;

: TDECL-SEEN? ( str -- bool ) {: a:^ u:n :}
   0 BEGIN dup TDECL-SEEN-N @ < WHILE
      dup TDECL-SEEN-NAME @  over TDECL-SEEN-LEN @
      a u CORE-STR= IF drop RES-TRUE EXIT THEN
      1+
   REPEAT drop RES-FALSE ;
: TDECL-SEEN+ ( str -- ) {: a:^ u:n :}
   TDECL-SEEN-N @ 1 + TDECL-SEEN-ENSURE
   a TDECL-SEEN-N @ TDECL-SEEN-NAME !
   u TDECL-SEEN-N @ TDECL-SEEN-LEN !
   TDECL-SEEN-N @ 1 + TDECL-SEEN-N ! ;

\ --- declaration context, set before TDECL-RUN, read by bodies and diagnostics.
PTR-VARIABLE TDK-A   variable TDK-U   \ decl kind token ("newtype"/"sumtype")
PTR-VARIABLE TDN-A   variable TDN-U   \ family name token
PTR-VARIABLE TDS-A   variable TDS-U   \ source token the record is placed at
PTR-VARIABLE TDB-A   variable TDB-U   \ body (SUMTYPE token buffer / arity token)
PTR-VARIABLE TDT-A   variable TDT-U   \ offending token (diagnostics)
PTR-VARIABLE TDW-A   variable TDW-U   \ short reason (diagnostics)
PTR-VARIABLE TDF-A   variable TDF-U   \ what to write instead (diagnostics)
PTR-VARIABLE TDA-A   variable TDA-U   \ copy of the token TDECL-THROW or TDECL-AMBIG armed with
variable TDECL-CODE                 \ the code TDW and TDF explain (0 = none armed)
variable TDECL-CUR-FAM              \ family id being declared (-1 outside a body)
\ A collected body, its tokens one blank apart.
PTR-VARIABLE TDECL-BUF-P
SCRATCH-VARIABLE TDECL-BUF-CAP   \ in cells
variable TDECL-U
: TDECL-BUF ( -- ^u8 ) TDECL-BUF-P @ ;

: TDECL-TOK! ( str -- ) TDT-U ! TDT-A ! ;
\ The fixes several rejections share.
: TDECL-PARAM-FIX$ ( -- str )
   s" list the parameter in the family header, as in `box<a>`, before a payload names it" ;
: TDECL-SELF-FIX$ ( -- str )
   s" drop the payload that names the family being declared, or declare it as a `STRUCTURE` or an `ENUM`, which may name it beneath `^`" ;
: TDECL-TYPE-FIX$ ( -- str )
   s" write a payload type here: `n`, `^u8`, a header parameter or a family declared before" ;
: TDECL-APP-FIX$ ( -- str )
   s" write a type application as `family<type,type>`, as in `SPAN:span<^u8>`" ;
: TDECL-PTR-FIX$ ( -- str )
   s" spell a pointer `^T`, `^` joined to the type it points at, as in `^u8`" ;
: TDECL-VARIANT-FIX$ ( -- str ) s" give each variant of this declaration its own name" ;
: TDECL-FIELD-FIX$ ( -- str ) s" give each field of this declaration its own name" ;
: TDECL-GEN-FIX$ ( -- str )
   s" rename the type so no word it generates repeats a word already defined here" ;

\ TDECL-THROW's own copy of its word, mapped on first use and grown to fit: the
\ rollback a throw in a transaction participant runs first scrubs the stores
\ the word may point into before the record reads it.
$1000 constant TDA-FIRST                 \ bytes of the first mapping
PERSISTED-PTR-VARIABLE TDA-P   NULL-PTR TDA-P !
variable TDA-CAP   0 TDA-CAP !          \ bytes mapped at TDA-P, 0 for none

: TDA-KEEP ( str -- ) {: a:^ u:n :}
   TDA-P TDA-CAP TDA-FIRST u ARENA-ROOM
   a TDA-P @ u ARENA-COPY
   TDA-P @ TDA-A !  u TDA-U ! ;

\ The armed reason is the one store every declaration catcher reads, whichever
\ front end runs the declaration; it explains only the code it was armed with.
: TDECL-ARM ( str str n -- )   \ reason, fix, code
   TDECL-CODE ! TDF-U ! TDF-A ! TDW-U ! TDW-A !
   0 TDA-U !  NULL-PTR TDA-A ! ;
: TDECL-ARMED ( -- n ) TDECL-CODE @ ;
: TDECL-WHY@ ( -- str str ) TDW-A @ TDW-U @  TDF-A @ TDF-U @ ;
\ The token TDECL-THROW armed code with, as a `word` note; empty when another
\ code is armed or the record is placed at that token, in any case: DEFTYPE
\ throws its name folded. A generated word that collides is placed at the
\ token that generated it.
: TDECL-WORD-NOTE ( str n -- str ) {: pa:^ pu:n code:n :}
   TDECL-CODE @ code <>  TDA-U @ 0= or IF s" " EXIT THEN
   TDA-A @ TDA-U @ pa pu CORE-STR=CI IF s" " EXIT THEN
   TDA-A @ TDA-U @ ;
\ The word a sealed record names: the generated word that would change the
\ sealed package, else the token a u.
: TDECL-SEALED-WORD ( str -- str ) {: a:^ u:n :}
   a u E-TDECL-SEALED TDECL-WORD-NOTE dup 0= IF 2drop a u THEN ;

: TDECL-THROW ( str str str n -- )
   {: ta:^ tu:n wa:^ wu:n fa:^ fu:n code:n :}
   wa wu fa fu code TDECL-ARM
   ta tu TDECL-TOK!
   ta tu TDA-KEEP
   code throw ;

\ Places a SUMTYPE's record at its name (TDN); a STRUCTURE's or ENUM's stands
\ at its front end's recorded token, a MUTUAL member's close at `;MUTUAL`. NA
\ NU, which has no source token, is its `word` note.
: TDECL-THROW-AT ( str str str n -- )   \ name, reason, fix, code
   {: na:^ nu:n wa:^ wu:n fa:^ fu:n code:n :}
   wa wu fa fu code TDECL-ARM
   TDN-A @ TDN-U @ TDECL-TOK!
   na nu TDA-KEEP
   code throw ;

\ A rejection's label is composed here, grown to fit, and read when the record
\ prints, so the next rejection may reuse the buffer.
$80 constant LABEL-FIRST                \ bytes of the first mapping
PERSISTED-PTR-VARIABLE LABEL-P   NULL-PTR LABEL-P !
variable LABEL-CAP   0 LABEL-CAP !      \ bytes mapped at LABEL-P, 0 for none
variable LABEL-U

\ A label: LABEL-START empties it, LABEL+ appends text, LABEL$ is the text.
: LABEL-START ( -- ) 0 LABEL-U ! ;
: LABEL+ ( str -- ) {: a:^ u:n :}
   LABEL-P LABEL-CAP LABEL-FIRST LABEL-U @ u + ARENA-ROOM
   a LABEL-P @ LABEL-U @ + u ARENA-COPY
   LABEL-U @ u + LABEL-U ! ;
: LABEL$ ( -- str ) LABEL-P @ LABEL-U @ ;

\ Arms E-USING-AMBIGUOUS for the family tail a u, public in more than one used
\ package, keeping a copy of it; answers the code for the caller to throw.
: TDECL-AMBIG ( str -- n ) {: a:^ u:n :}
   s" ambiguous type family" s" " E-USING-AMBIGUOUS TDECL-ARM
   a u TDA-KEEP
   E-USING-AMBIGUOUS ;

\ The record of a TDECL-AMBIG arming in a declaration of a kind and family,
\ placed at the token ta tu. The caller ends it.
: TDECL-AMBIG-DIAG ( str str str -- )
   {: ka:^ ku:n fa:^ fu:n ta:^ tu:n :}
   s" E-USING-AMBIGUOUS" ka ku fa fu TDECL-TITLE
   ta tu REJECTION:AT  TDW-A @ TDW-U @ REJECTION:EMIT-STR
   s" type family"  TDA-A @ TDA-U @  TDA-A @ TDA-U @ TFAM-USED-MASK  UPKG-HELP ;

\ --- transactional mark/restore over every store a declaration can grow.
\ The family and variant stores go back through TFAM-REWIND, not here: their
\ indexes chain rows by id, so unchaining must precede lowering the counters, or
\ a chain keeps a row past the store's end. The schema store's marks go back
\ through SCHEMA-REG:REWIND, which scrubs what it retires.

variable TDM-TFAM   variable TDM-STR   variable TDM-PK
variable TDM-SUMV   variable TDM-LAY
variable TDM-SCH    variable TDM-ROOT
variable TDV-QUOT-N
variable TDV-QUOT-MISS

: QUOT-ROLLBACK ( -- ) 0 TDV-QUOT-N !  0 TDV-QUOT-MISS ! ;

variable TDECL-FAM-REG   \ family id the LAST successful NEWTYPE or DEFTYPE registered (-1 = none)

\ PF-N and PF-COMMIT-N are not marked: SUMTYPE and NEWTYPE make no field row.

: TDECL-MARK ( -- )
   TFAM-N @ TDM-TFAM !   TF-STR-U @ TDM-STR !   TF-PK-N @ TDM-PK !
   SUMV-N @ TDM-SUMV !   LAY-N @ TDM-LAY !
   SCH-N @ TDM-SCH !     SCH-ROOT-N @ TDM-ROOT ! ;
: TDECL-RESTORE ( -- )
   TDM-TFAM @  TDM-STR @  TDM-PK @  TDM-SUMV @  TDM-LAY @  TFAM-REWIND
   TDM-SCH @ TDM-ROOT @ REWIND
   QUOT-ROLLBACK ;

\ One row per token copied into TDECL-BUF: its offset there and the address it
\ was copied from.
PTR-VARIABLE TDECL-TOK-OFF
PTR-U8-TABLE-VARIABLE TDECL-TOK-SRC
SCRATCH-VARIABLE TDECL-TOK-OFF-CAP
SCRATCH-VARIABLE TDECL-TOK-SRC-CAP
variable TDECL-TOK-N
: TDECL-OFF ( n -- ^n ) cells TDECL-TOK-OFF @ + ;
: TDECL-ORIGIN ( n -- ^^u8 ) TDECL-TOK-SRC @ swap ptr-field ;

\ The last row whose token starts at or before offset off; -1 for an empty table.
: TDECL-ROW ( n -- n ) {: off:n :}
   TDECL-TOK-N @ 1 -
   BEGIN dup 0 > WHILE
      dup TDECL-OFF @ off <= IF EXIT THEN
      1 -
   REPEAT ;

\ The bytes span a u of TDECL-BUF was copied from, true, when it lies in the
\ copy; else a u and false. The empty span at the copy's end maps to just past
\ its last token.
: TDECL-ORIGIN-SPAN ( str -- str bool ) {: a:^ u:n :}
   a TDECL-BUF - {: off:n :}
   off 0 <  off TDECL-U @ > or IF a u RES-FALSE EXIT THEN
   off TDECL-ROW {: r:n :}
   r 0 < IF a u RES-FALSE EXIT THEN
   r TDECL-ORIGIN @  off r TDECL-OFF @ - +  u RES-TRUE ;

\ A span in TDECL-BUF becomes the bytes its token was copied from when those
\ lie in armed text, which outlasts the declaration; any other span comes back
\ unchanged.
: TDECL-SRC ( str -- str ) {: a:^ u:n :}
   a u TDECL-ORIGIN-SPAN 0= IF EXIT THEN {: s:^ su:n :}
   s REJECTION:>SOURCE nip 0= IF a u EXIT THEN
   s su ;

\ The token a rejection written now places: the bytes a u was copied from when
\ REJECTION holds them, else a u.
: TDECL-PLACED-TOK ( str -- str ) {: a:^ u:n :}
   a u TDECL-ORIGIN-SPAN 0= IF EXIT THEN
   a u REJECTION:KEPT ;

\ A DEFFAMILY record points at the name whatever the rejection, as the source
\ token TDS: the folded DEFTYPE header the body checks has no place. A
\ declaration with no name points at its keyword, as TDS.
: TDECL-REPORT-TOK ( -- str )
   TDS-U @ 0 > IF TDS-A @ TDS-U @ EXIT THEN
   TDT-A @ TDT-U @ TDECL-PLACED-TOK ;

\ TDECL-RUN reports only a code TDECL-CODE was armed with.
: TDECL-REPORT ( -- )
   TDECL-ARMED E-USING-AMBIGUOUS = IF
      TDK-A @ TDK-U @  TDN-A @ TDN-U @  TDECL-REPORT-TOK  TDECL-AMBIG-DIAG EXIT
   THEN
   TDECL-ARMED E-TDECL-SEALED = IF
      TDECL-REPORT-TOK {: ta:^ tu:n :} ta tu TDECL-SEALED-WORD ta tu REJECTION:SEALED EXIT
   THEN
   TDECL-ARMED TFAM:E-TDECL-DEF-NAME = IF TDA-A @ TDA-U @ TDECL-REPORT-TOK DEF-NAME-REJECT? drop EXIT THEN
   TDK-A @ TDK-U @  TDN-A @ TDN-U @  TDECL-REPORT-TOK  2dup TDECL-ARMED TDECL-WORD-NOTE
   {: pa:^ pu:n na:^ nu:n :}
   pa pu  TDW-A @ TDW-U @  TDF-A @ TDF-U @  na nu  TDECL-DIAG ;

\ TDN names only the running declaration: a STRUCTURE or ENUM after it reads none.
: TDECL-NAME-END ( -- ) NULL-PTR TDN-A !  0 TDN-U ! ;
\ Rethrows E-TDECL-UNRESOLVED, a status in 1..255 and a code no reason is armed
\ with; any other is rendered before the rollback scrubs what its record reads.
: TDECL-RUN ( [ -- ] -- )
   READ-PENDING                     \ no type entry is read inside the declaration
   TDECL-MARK
   -1 TDECL-FAM-REG !               \ set by a successful declaration only
   catch {: rc:n :}
   TDECL-SEEN-CLEAR
   rc 0= IF TDECL-NAME-END EXIT THEN
   rc E-TDECL-UNRESOLVED = IF TDECL-RESTORE TDECL-NAME-END rc throw THEN
   rc 1 >=  rc 255 <= and  TDECL-CODE @ rc <> or IF
      TDECL-RESTORE  SIG-UNRES-CLEAR  TDECL-NAME-END rc throw THEN
   TDECL-REPORT
   TDECL-RESTORE
   SIG-UNRES-CLEAR
   TDECL-NAME-END
   MULTI-ERR? IF REJECTION:KEEP EXIT THEN
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The declared name is read first by the body's name gate and by the report.
: TDECL-CTX! ( str str str -- )   \ kind, name, body
   TDB-U ! TDB-A !
   TDN-U ! TDN-A !
   TDK-U ! TDK-A !
   0 TDS-U !  NULL-PTR TDS-A !
   0 TDT-U !  NULL-PTR TDT-A !
   SIG-UNRES-CLEAR
   TDECL-SEEN-CLEAR
   -1 TDECL-CUR-FAM !
   s" " s" " 0 TDECL-ARM ;
: TDECL-PLACE! ( str -- )
   TDS-U ! TDS-A ! ;

\ The unterminated-sum rejection: the declaration-shaped E-BAD-DECLARATION
\ record (name + partial body) through TDECL-RUN, on the load and a replay
\ alike.
: TDECL-NOEND-BODY ( -- )
   TDN-A @ TDN-U @ s" missing ;SUMTYPE"
      s" end the declaration with `;SUMTYPE` after its last variant"
      E-TDECL-SYNTAX TDECL-THROW ;

\ --- name gate: reserved signature/type tokens, control words, and grammar
\ keywords may not name a family or variant (docs/forth.md "Structures And Enums"). The family
\ list is TYPE-NAME:FAMILY-RESERVED? (type-family.f), which every family definer
\ asks, so the definers cannot drift apart on it.

private

: TDECL-KEYWORD? ( str -- bool ) {: a:^ u:n :}
   a u TF-GRAMMAR-KEYWORD? ;

: TDECL-VAR-SCOPE? ( n -- bool )
   SUMV-FAM@ TFAM-PKG$ {: pa:^ pu:n :}
   pu 0= IF RES-TRUE EXIT THEN
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   pa pu CHECKER-AUTH-PACKAGE$ CORE-STR=CI ;

: TDECL-VAR-TAKEN? ( str -- bool ) {: a:^ u:n :}
   SUMV-N @ 0 ?do
      i SUMV-NAME$ a u CORE-STR= IF
         i TDECL-VAR-SCOPE? IF RES-TRUE unloop EXIT THEN
      THEN
   loop RES-FALSE ;

\ A missing name is rejected before, at the keyword that wants it.
: TDECL-REQUIRE-NAME ( str -- ) {: a:^ u:n :}
   a u DEF-NAME-BAD? IF a u s" " s" " TFAM:E-TDECL-DEF-NAME TDECL-THROW THEN
   a u TF-CANON? 0= IF
      a u s" name must be a lowercase family tail"
         s" write the name as a lowercase tail with no package qualifier, as in `point`"
         TFAM:E-TFAM-CASE TDECL-THROW
   THEN
   a u TYPE-NAME:FAMILY-RESERVED? IF a u s" reserved name"
      s" pick another name: a built-in type such as `n` cannot name a family or variant"
      E-TDECL-NAME TDECL-THROW THEN ;

\ A FAMILY name must remain valid grammar and must not collide with an in-scope
\ sum variant. Exact same-package collisions fall through to TFAM-DECL's
\ E-TFAM-DUP so the diagnostic says "duplicate family". A package family may
\ share a tail with a global or another package family; TFAM-RESOLVE preserves
\ those exact identities with lexical lookup rather than rejecting the declaration.
: TDECL-REQUIRE-FAMILY-NAME ( str -- ) {: a:^ u:n :}
   a u TDECL-REQUIRE-NAME
   a u TDECL-VAR-TAKEN? IF
      a u s" collides with a sum variant"
         s" rename the family: a variant declared in this scope already has this name"
         E-TDECL-NAME TDECL-THROW
   THEN ;

\ A DERIVE-marked family must not declare a variant spelled like a generated
\ derived tail: the constructor package would hold two words with one name.
: TDECL-COLLIDE-WHY ( -- str str )
   s" variant name collides with a derived word"
   s" rename the variant: under `DERIVE` the names `eq`, `hash` and `tag` are taken" ;

: TDECL-COLLIDE-THROW ( str -- )
   TDECL-COLLIDE-WHY E-TDECL-NAME TDECL-THROW ;

\ A variant name lives in no scope of its own, so a family the declaring scope
\ resolves takes it in every scope.
: TDECL-REQUIRE-VARIANT-NAME ( str -- ) {: a:^ u:n :}
   a u TDECL-REQUIRE-NAME
   a u TYPE-NAME:FAMILY-TAKEN? 0= IF EXIT THEN
   a u s" collides with a type family"
      s" rename the variant: a type family in this scope already has this name"
      E-TDECL-NAME TDECL-THROW ;

\ --- package scope: a declaration registers in the active checker package
\ (TFAM-ACTIVE-PKG$, type-family.f). `global` places words, not types: a type
\ declared at top level or in a `global` section is public.
: TDECL-VIS ( -- n )
   CHECKER-AUTH-PACKAGE-MODE@ {: mode:n :}
   mode CHECKER-PACKAGE-NONE =  mode CHECKER-PACKAGE-GLOBAL =  or IF
      CHECKER-PACKAGE-PUBLIC EXIT
   THEN mode ;

\ TFAM-DECL's one failure left here is a duplicate, unless a compile callback
\ declares: the window rejection comes first, under the general reason.
: TDECL-FAMILY ( n n -- n ) {: ar:n kind:n :}
   TDN-A @ TDN-U @ TDECL-TOK!
   WRITE-WINDOW-CK                     \ src/core/checker.f WRITE-WINDOW
   s" duplicate family"
      s" rename the family: this package already declares a family with this name"
      TFAM:E-TFAM-DUP TDECL-ARM
   TFAM-ACTIVE-PKG$ TDECL-VIS TDN-A @ TDN-U @ ar kind TFAM-DECL ;

\ --- body cursor over the buffered declaration tokens (checker sig lexer).
: TDECL-CURSOR! ( str -- ) {: a:^ u:n :}
   a SB!  u SL !  0 SI !
   PKRESET ;
: TDECL-NEXT ( -- str ) NEXT-SIG-TOK ;

defer TDECL-NAME-PREFLIGHT-XT ( str -- )

\ --- variant payload elements -> schema nodes (type-schema.f header): positional
\ letter params within arity, n/f/r and concrete CT cell types, and `^T`.
\ Comments are not supported inside a declaration block; stray tokens reject.

: TDECL-HEADER! ( str -- n ) {: fa:^ fu:n :}   \ fix for a missing name
   TDN-U @ 0= IF TDN-A @ 0 s" missing name" fa fu E-TDECL-SYNTAX TDECL-THROW THEN
   \ The seal is judged before the parameters, at the family name alone.
   TDN-U @ TDS-U @ {: whole:n span:n :}
   TDN-A @ whole DECL-FAMILY-NAME nip {: cut:n :}
   cut 0 > IF
      cut TDN-U !  span 0 > IF cut TDS-U ! THEN
      TDN-A @ cut TDECL-NAME-PREFLIGHT-XT
      whole TDN-U !  span TDS-U !
   THEN
   TDN-A @ TDN-U @ DECL-HEADER 0= IF
      drop 2drop
      TDN-A @ TDN-U @ s" invalid type parameter header"
         s" name each type parameter once, by a letter other than `f`, `n` or `r`, as in `box<a>` or `result<a,b>`"
         E-TDECL-ARITY TDECL-THROW
   THEN
   {: ar:n :}
   \ The bare name is empty only after the split, and an empty token has no place.
   dup 0= IF
      2drop TDN-A @ TDN-U @ s" missing name"
         s" write the family name before its parameters, as in `box<a>`"
         E-TDECL-SYNTAX TDECL-THROW
   THEN
   TDN-U ! TDN-A !
   TDS-U @ 0 > IF TDN-U @ TDS-U ! THEN
   ar ;

: TDECL-LETTER ( str -- n ) {: a:^ u:n :}
   a c@ {: c:n :}
   c 110 = IF CC-N SCHEMA-CON EXIT THEN
   c 102 = IF CC-BOOL SCHEMA-CON EXIT THEN
   c 114 = IF CC-R SCHEMA-CON EXIT THEN
   c DECL-PARAM 0= IF
      drop a u s" unknown payload type"
         TDECL-PARAM-FIX$ E-TDECL-PAYLOAD TDECL-THROW
   THEN
   SCHEMA-PARAM ;

\ A direct self-family payload (its tail is the family being declared,
\ TDN-A/TDN-U) makes the family recursive, which only a boxed policy could lay
\ out; boxed rejects at the POLICY clause, so a self-reference always rejects
\ here as recursive. This catches the bare spelling; TDECL-PAY-FAM? repeats the
\ reject on the resolved id (TDECL-CUR-FAM) for a qualified one. Only the direct
\ case: mutual recursion (A -> B -> A) is not detected.
: TDECL-SELF-REF? ( str -- bool ) {: a:^ u:n :}
   a u TDN-A @ TDN-U @ CORE-STR= ;

\ Rejects the tail a u when its outcome sr is SR-AMBIG.
: TDECL-AMBIG-REJECT ( n str -- ) {: sr:n a:^ u:n :}
   sr SR-AMBIG <> IF EXIT THEN
   a u TDECL-TOK!  a u TDECL-AMBIG throw ;

\ A bare family payload is one logical schema root whose physical width is the
\ referenced closed width: an arity-0 layout family (sum/enum/product) or cell
\ family (nominal scalar, the reference a signature admits). Resolution is
\ signature scope (TFAM-SIG-RESOLVE): a private family in another package never
\ resolves. A resolved id equal to the declared family is the qualified
\ self-reference and rejects recursive. A family UNSTORED-REASON names, linear
\ payloads and evidence families are rejected.
: TDECL-PAY-FAM? ( str -- n bool ) {: a:^ u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE {: id:n sr:n :}
   sr SR-FOUND <> IF sr a u TDECL-AMBIG-REJECT 0 RES-FALSE EXIT THEN
   a u id UNSTORED-REASON IF E-TDECL-PAYLOAD TDECL-THROW THEN 2drop 2drop 2drop
   id TDECL-CUR-FAM @ = IF
      a u s" invalid layout policy for recursive sum"
         TDECL-SELF-FIX$ E-TDECL-RECURSIVE TDECL-THROW
   THEN
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 RES-FALSE EXIT THEN
   id TFAM-ARITY@ 0 <> IF 0 RES-FALSE EXIT THEN
   id TFAM-CONCRETE-LINEAR? IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TDECL-ABSENT-MAY? ( str -- bool ) {: a:^ u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE nip SR-FOUND = IF RES-FALSE EXIT THEN
   a u TFAM-MAY-ARRIVE? ;
: TDECL-NO-END-THROW ( str -- ) {: a:^ u:n :}
   a u s" missing ;VARIANT"
      s" close each `VARIANT` with `;VARIANT`"
      E-TDECL-SYNTAX TDECL-THROW ;
: TDECL-PAY-ELEM ( str -- n ) {: a:^ u:n :}
   u 0= IF a u TDECL-NO-END-THROW THEN
   a u DELIM? IF a u s" bad payload token"
      TDECL-TYPE-FIX$ E-TDECL-SYNTAX TDECL-THROW THEN
   a u TDECL-KEYWORD? IF a u s" bad payload token"
      TDECL-TYPE-FIX$ E-TDECL-SYNTAX TDECL-THROW THEN
   a u TDECL-SELF-REF? IF
      a u s" invalid layout policy for recursive sum"
         TDECL-SELF-FIX$ E-TDECL-RECURSIVE TDECL-THROW
   THEN
   u 1 = IF a u TDECL-LETTER EXIT THEN
   a u CON-OF dup 0 <> IF SCHEMA-CON EXIT THEN drop
   a u TDECL-PAY-FAM? IF 0 0 SCHEMA-APP EXIT THEN drop
   a u TDECL-ABSENT-MAY? IF a u TDECL-SRC SIG-UNRES! 0 EXIT THEN
   a u s" unknown payload type"
      s" name a type declared before this declaration: `n`, `^u8`, a header parameter or a family; only a `SUMTYPE` variant may apply a family to arguments, as in `box<n>`"
      E-TDECL-PAYLOAD TDECL-THROW ;

\ --- a variant payload may also be a closed parametric application
\ `fam<arg,...>` (each argument a payload element) or a quotation type
\ `[ din -- dout | rin -- rout ]`. An application's schema is an SCH-APP of the
\ family id and argument roots; construct, MATCH and layout resolve each
\ argument against the declaring family's parameters (type-family.f TFC-*). A
\ quotation is one xt cell whose SCH-QUOT schema records all four rows, empty
\ sides included. The quotation parser is shared with STRUCTURE fields.

\ Reentrant scratch for ONE nesting level's argument nodes. Each TDECL-VPAY-ARGS
\ invocation saves a base, pushes its fully-parsed argument node ids above it,
\ then copies [base,top) into CONTIGUOUS schema roots (every deeper nested root
\ already sits below argbase) and rewinds. The nodes cannot accumulate directly on
\ the schema-root pool because a nested application interleaves its own arg roots
\ between this level's arguments.
\ A push may move this scratch, TDV-QUOT-SCR or TDV-PAY-SCR to a new
\ mapping, so a reader holds indices into each across a push, never an address.
PTR-VARIABLE TDV-ARG-P   SCRATCH-VARIABLE TDV-ARG-CAP
variable TDV-ARG-N
: TDV-ARG-SCR ( -- ^n ) TDV-ARG-P @ ;

: TDV-ARG+ ( n -- ) {: node:n :}
   TDV-ARG-P TDV-ARG-CAP TDV-ARG-N @ 1 + 1 ARENA-ROWS-ENSURE
   node TDV-ARG-N @ cells TDV-ARG-SCR + !
   TDV-ARG-N @ 1 + TDV-ARG-N ! ;

defer TDV-ELEM-XT ( str -- n nominal )   \ forward ref for the ELEM<->ARGS<->QUOT recursion

\ The layout or cell family a payload token names, arity left to TDV-ARGS-BUILD.
\ One with a scope or region parameter is rejected: a SUMTYPE header lists none.
: TDECL-VPAY-FAM? ( str -- n bool ) {: a:^ u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE {: id:n sr:n :}
   sr SR-FOUND <> IF sr a u TDECL-AMBIG-REJECT 0 RES-FALSE EXIT THEN
   id TFAM-NONVALUE-PARAM? IF
      a u s" takes a scope or region, which a `SUMTYPE` payload cannot hold"
      s" hold it in a field of an `ENUM` whose header lists its scope and region parameters, as in `ENUM box<p,q>` with `FIELD v read-view<p,q,u8>`"
      E-TDECL-PAYLOAD TDECL-THROW THEN
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TDV-ARGS-BUILD ( str n n -- n ) {: fa:^ fu:n fam:n base:n :}
   TDV-ARG-N @ base - {: cnt:n :}
   cnt fam TFAM-ARITY@ <> IF
      base TDV-ARG-N !
      fa fu s" type family applied to wrong number of arguments"
      s" give the family as many type arguments as its header declares"
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
\ recursion is rejected by id. The end is the address just past the closing `>`.
: TDECL-VPAY-ARGS ( str n -- n ^u8 ) {: fa:^ fu:n fam:n :}
   fam TDECL-CUR-FAM @ = IF
      fa fu s" invalid layout policy for recursive sum"
         TDECL-SELF-FIX$ E-TDECL-RECURSIVE TDECL-THROW
   THEN
   TDV-ARG-N @ {: base:n :}
   BEGIN
      TDECL-NEXT
      2dup s" >" CORE-STR= IF + fa fu fam base TDV-ARGS-BUILD swap EXIT THEN
      2dup DELIM? IF s" bad type application"
         TDECL-APP-FIX$ E-TDECL-SYNTAX TDECL-THROW THEN
      TDV-ELEM-XT TDV-ARG+
      TDECL-NEXT
      2dup s" ," CORE-STR= IF 2drop ELSE
      2dup s" >" CORE-STR= IF + fa fu fam base TDV-ARGS-BUILD swap EXIT ELSE
         s" bad type application" TDECL-APP-FIX$ E-TDECL-SYNTAX TDECL-THROW
      THEN THEN
   AGAIN ;

\ Each quotation side is an ordered row of zero or more payload elements.
\ Element ids collect in a reentrant scratch and land as one contiguous run of
\ schema roots per side, after every element's own argument roots, so each
\ SCH-ROW range stays contiguous. Without a `|` clause the rin and rout rows
\ are empty and no return clause is stored; an explicit clause pins both.
PTR-VARIABLE TDV-QUOT-P   SCRATCH-VARIABLE TDV-QUOT-CAP
: TDV-QUOT-SCR ( -- ^n ) TDV-QUOT-P @ ;
: TDV-QUOT+ ( n -- ) {: node:n :}
   TDV-QUOT-P TDV-QUOT-CAP TDV-QUOT-N @ 1 + 1 ARENA-ROWS-ENSURE
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
: TDV-QUOT-FINISH ( n n n n bool n n -- n )
   {: din:n dout:n rin:n rout:n hasr:bool facts:n outer:n :}
   TDV-QUOT-MISS @ {: missing:n :}
   outer TDV-QUOT-MISS !
   missing 0 <> IF 0 EXIT THEN
   din dout rin rout hasr facts SCHEMA-QUOT ;
: TDV-FACTS-HELP$ ( -- str )
   s" write the facts after the last outputs, as `[ n -- n renders ]`" ;
\ Rejects the token a u, a fact word where the side may not end in facts.
: TDV-FACT-PLACE ( str [ str str str n -- ] -- ) {: a:^ u:n fail :}
   a u s" a fact follows only the last outputs" TDV-FACTS-HELP$ E-TDECL-SYNTAX fail execute ;
\ One token a u of a side stating FACTS so far: a type joins the row, a fact word, when OK lets
\ the side end in them, joins the facts. A fact is stated once and only fact words follow it.
: TDV-QUOT-TOK ( n str [ str -- n ] [ str str str n -- ] bool -- n )
   {: facts:n a:^ u:n elem fail ok:bool :}
   a u STATED-FLAG {: flag:n :}
   flag 0 <> IF
      ok 0= IF a u fail TDV-FACT-PLACE facts EXIT THEN
      facts flag and 0 <> IF
         a u s" this fact is already stated" s" state each fact once"
         E-TDECL-SYNTAX fail execute facts EXIT
      THEN
      facts flag or EXIT
   THEN
   facts 0 <> IF
      a u s" a type follows the facts" TDV-FACTS-HELP$ E-TDECL-SYNTAX fail execute facts EXIT
   THEN
   a u elem execute TDV-QUOT+ facts ;
\ One side: its row, the facts it states, 0 when none, and the separator that ends it.
: TDV-QUOT-SIDE ( [ -- str ] [ str -- n ] [ str str str n -- ] bool -- n n str )
   {: next elem fail ok:bool :}
   TDV-QUOT-N @ {: base:n :}
   0 BEGIN next execute 2dup DELIM? 0= WHILE
      elem fail ok TDV-QUOT-TOK
   REPEAT
   {: facts:n sep:^ sepu:n :}
   base TDV-QUOT-BUILD-ROW facts sep sepu ;
: TDV-QUOT-EXPECT ( str str [ str str str n -- ] -- )
   {: a:^ u:n ea:^ eu:n fail :}
   a u ea eu CORE-STR= IF EXIT THEN
   u 0= IF a u s" the quotation type is not closed"
      s" close the quotation type with `]`"
      E-TDECL-SYNTAX fail execute EXIT THEN
   a u
   ea eu s" --" CORE-STR= IF s" the quotation type needs `--` here"
   ELSE s" the quotation type needs `]` here" THEN
   s" write the quotation type as `[ in -- out ]`"
   E-TDECL-SYNTAX fail execute ;

\ The levels open in the type being read, 0 at each whole type: a node reader
\ rejects its token at EFFECT-DEPTH-MAX, and a quotation's rows open one more.
variable TYPE-LEVEL
\ The reason and fix of a type nested past the limit.
: TDECL-DEPTH-TEXTS ( -- str str )
   LABEL-START
   s" type nested too deep: at most " LABEL+
   EFFECT-DEPTH-MAX REJECTION:DIGITS LABEL+
   s"  levels" LABEL+
   LABEL$
   s" declare an inner part as a type of its own (`NEWTYPE` or `STRUCTURE`) and use its name here" ;

\ Shared by variant payloads and STRUCTURE fields, each supplying its token
\ stream, element resolver and rejecter, which is given the token it stopped at.
\ The facts follow the last outputs: the data outputs' when there is no `|`.
: PARSE-QUOT ( [ -- str ] [ str -- n ] [ str str str n -- ] -- n )
   {: next elem fail :}
   TYPE-LEVEL @ EFFECT-DEPTH-MAX >= IF
      s" " TDECL-DEPTH-TEXTS E-TDECL-CAP fail execute THEN
   TYPE-LEVEL @ 1 + TYPE-LEVEL !
   TDV-QUOT-MISS @ {: outer:n :}
   0 TDV-QUOT-MISS !
   next elem fail RES-FALSE TDV-QUOT-SIDE rot drop {: din:n sep:^ sepu:n :}
   sep sepu s" --" fail TDV-QUOT-EXPECT
   next elem fail RES-TRUE TDV-QUOT-SIDE {: dout:n dfacts:n end:^ endu:n :}
   end endu s" |" CORE-STR= IF
      dfacts 0 <> IF end endu fail TDV-FACT-PLACE THEN
      next elem fail RES-FALSE TDV-QUOT-SIDE rot drop {: rin:n rsep:^ rsepu:n :}
      rsep rsepu s" --" fail TDV-QUOT-EXPECT
      next elem fail RES-TRUE TDV-QUOT-SIDE {: rout:n rfacts:n rend:^ rendu:n :}
      rend rendu s" ]" fail TDV-QUOT-EXPECT
      din dout rin rout RES-TRUE rfacts outer TDV-QUOT-FINISH
   ELSE
      end endu s" ]" fail TDV-QUOT-EXPECT
      din dout TDV-QUOT-EMPTY-ROW TDV-QUOT-EMPTY-ROW RES-FALSE dfacts outer TDV-QUOT-FINISH
   THEN
   TYPE-LEVEL @ 1 - TYPE-LEVEL ! ;

: TDECL-QUOT-FAIL ( str str str n str -- )
   {: ta:^ tu:n wa:^ wu:n fa:^ fu:n code:n qa:^ qu:n :}
   tu 0= IF qa qu ELSE ta tu THEN wa wu fa fu code TDECL-THROW ;
\ A quotation never reads past its variant: `;VARIANT` ends it as the text's end would.
: TDECL-VPAY-QUOT ( str -- n ) {: qa:^ qu:n :}
   [: TDECL-NEXT 2dup s" ;variant" CORE-STR=CI IF drop 0 THEN ;]
   [: TDV-ELEM-XT ;] [: qa qu TDECL-QUOT-FAIL ;] PARSE-QUOT
   SIG-UNRES @ IF drop 0 THEN ;

\ An application of a family no payload holds is rejected over its whole text,
\ head to `>`, as STRUCTURE marks `width<n>`.
: TDECL-VPAY-UNSTORED ( n ^u8 ^u8 -- ) {: fam:n a:^ end:^ :}
   a end NULL-PTR - a NULL-PTR - -  fam UNSTORED-REASON
   IF E-TDECL-PAYLOAD TDECL-THROW THEN 2drop 2drop 2drop ;

\ a resolved family head followed by `<` is a parametric application; any other
\ next token is pushed back and reported as not-an-application so the head falls
\ through to the bare arity-0 grammar (or its reject).
: TDECL-VPAY-APP? ( str -- n bool ) {: a:^ u:n :}
   a u TDECL-VPAY-FAM? 0= IF drop 0 RES-FALSE EXIT THEN
   {: fam:n :}
   TDECL-NEXT 2dup s" <" CORE-STR= IF
      2drop a u fam TDECL-VPAY-ARGS fam a rot TDECL-VPAY-UNSTORED RES-TRUE EXIT THEN
   PK! 0 RES-FALSE ;

\ An absent head has no arity or layout to build. Consume its complete
\ application, checking each independently known argument with the ordinary
\ variant payload parser before the declaration is rolled back.
: TDECL-VPAY-LOOSE-APP ( -- )
   BEGIN
      TDECL-NEXT
      2dup s" >" CORE-STR= IF 2drop EXIT THEN
      2dup DELIM? IF s" bad type application"
         TDECL-APP-FIX$ E-TDECL-SYNTAX TDECL-THROW THEN
      TDV-ELEM-XT drop
      TDECL-NEXT
      2dup s" ," CORE-STR= IF 2drop ELSE
      2dup s" >" CORE-STR= IF 2drop EXIT ELSE
         s" bad type application" TDECL-APP-FIX$ E-TDECL-SYNTAX TDECL-THROW
      THEN THEN
   AGAIN ;

: TDECL-VPAY-ONE ( str -- n ) {: a:^ u:n :}
   a u s" [" CORE-STR= IF a u TDECL-VPAY-QUOT EXIT THEN
   a u s" ptr" CORE-STR= IF a u s" unknown payload type"
      TDECL-PTR-FIX$ E-TDECL-PAYLOAD TDECL-THROW THEN
   a u SIG-CARET? IF
      a u SIG-CARET-BARE? IF a u s" pointer without a pointee"
         TDECL-PTR-FIX$ E-TDECL-PAYLOAD TDECL-THROW THEN
      a u SIG-CARET-POINTEE TDV-ELEM-XT
      SIG-UNRES @ IF drop 0 EXIT THEN
      SCHEMA-PTR EXIT THEN
   a u TDECL-VPAY-APP? IF EXIT THEN drop
   u 1 = IF a u TDECL-PAY-ELEM EXIT THEN
   a u CON-OF dup 0 <> IF drop a u TDECL-PAY-ELEM EXIT THEN drop
   a u TDECL-ABSENT-MAY? IF
      a u TDECL-SRC SIG-UNRES!
      TDECL-NEXT 2dup s" <" CORE-STR= IF 2drop TDECL-VPAY-LOOSE-APP
      ELSE PK! THEN
      0 EXIT
   THEN
   a u TDECL-PAY-ELEM ;
: TDECL-VPAY-ELEM ( str -- n ) {: a:^ u:n :}
   TYPE-LEVEL @ EFFECT-DEPTH-MAX >= IF
      a u TDECL-DEPTH-TEXTS E-TDECL-CAP TDECL-THROW THEN
   TYPE-LEVEL @ 1 + TYPE-LEVEL !
   a u TDECL-VPAY-ONE
   TYPE-LEVEL @ 1 - TYPE-LEVEL ! ;
: TDV-ELEM-INSTALL ( -- ) [: TDECL-VPAY-ELEM ;] is TDV-ELEM-XT ;
TDV-ELEM-INSTALL

\ --- READ-TYPE reads one type by the signature grammar (SIG-TYPE) and stores
\ the checker's term as a schema node. A definer's type has no letters: its
\ parameters come from EFFECT:NAMED. A type variable, a row variable and a
\ fresh name are letters, and a view's region slots take only those.

\ The token past its leading carets: `^a` spells a pointer to the letter `a`.
: READ-UNCARET ( str -- str ) {: a:^ u:n :}
   0 BEGIN dup u < IF a over + c@ 94 = ELSE RES-FALSE THEN WHILE 1+ REPEAT
   {: k:n :}  a k +  u k - ;

: READ-LETTER? ( str -- bool ) {: a:^ u:n :}
   u 0= IF RES-FALSE EXIT THEN
   a u FRESH-ATOM-TOK? IF RES-TRUE EXIT THEN
   a u ROW-LEAD? IF RES-TRUE EXIT THEN
   a u SIG-VAR-SLOT 0 < IF RES-FALSE EXIT THEN
   a u SIG-FAM? nip 0= ;

\ The first letter in the signature text, a 0 for none.
: READ-LETTER ( -- str )
   0 SI !  PKRESET
   BEGIN NEXT-SIG-TOK dup 0 <> WHILE
      READ-UNCARET 2dup READ-LETTER? IF EXIT THEN 2drop
   REPEAT ;

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

\ Stages term t's node; false when it has none.
: READ-STAGE-TERM ( n -- bool )
   TERM-SCHEMA-XT dup 0= IF drop RES-FALSE EXIT THEN TDV-ARG+ RES-TRUE ;

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
   din dout TDV-QUOT-EMPTY-ROW TDV-QUOT-EMPTY-ROW
   RES-FALSE q Q>FACTS CTL-STATABLE and SCHEMA-QUOT ;

\ The schema node of a closed type's term, 0 for one no node holds: an atom
\ other than `xt`, a binder or a variable.
: TERM-SCHEMA ( n -- n )
   T-RES {: t:n :}
   t TAG T-CON = IF t PAY SCHEMA-CON EXIT THEN
   t TAG T-PTR = IF
      t PTR>INNER TERM-SCHEMA-XT dup 0= IF EXIT THEN SCHEMA-PTR EXIT
   THEN
   t TAG T-PARAM = IF t READ-APP EXIT THEN
   t TAG T-QUOT = IF t READ-QUOT EXIT THEN
   t OPAQUE-XT? IF SCHEMA-XT EXIT THEN
   0 ;
: TERM-SCHEMA-INSTALL ( -- ) [: TERM-SCHEMA ;] is TERM-SCHEMA-XT ;
TERM-SCHEMA-INSTALL

\ The parse's reject: the token SGBAD latched and why.
: READ-BAD ( [ str str n n -- ] -- ) {: fail :}
   SGBAD-A @ SGBAD-U @
   SGBAD-AMBIG? IF
      s" ambiguous type family" E-USING-AMBIGUOUS 0 fail execute EXIT
   THEN
   SGBAD-UNKNOWN? IF s" unknown type" E-TDECL-PAYLOAD 0 fail execute EXIT THEN
   SGBAD-ARITY? IF
      s" type family applied to wrong number of arguments"
      E-TDECL-SYNTAX 0 fail execute EXIT
   THEN
   s" bad type" E-TDECL-SYNTAX 0 fail execute ;

\ The one type the text spells, read by the signature grammar where the
\ definer runs. A letter in it runs LETTER with the letter; any other reject,
\ a name that may arrive later and text past the type included, runs FAIL
\ with the token, the reason, the code and the STG- reason when no stored
\ form holds the type, else 0.
: READ-TYPE ( str [ str str n n -- ] [ str -- ] -- n )
   {: a:^ u:n fail letter :}
   a SB!  u SL !
   READ-LETTER {: la:^ lu:n :}
   lu 0 <> IF la lu letter execute 0 EXIT THEN
   SIG-UNRES-CLEAR  SGBAD-CLEAR  0 TDV-ARG-N !
   PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET  0 SQUOT-N !  0 BIND-DEPTH !
   0 SIG-LEVEL !  0 SGHASR !  0 RR-SHARED !  0 SI !
   NEXT-SIG-TOK {: ta:^ tu:n :}
   tu 0= IF ta 0 s" missing type" E-TDECL-SYNTAX 0 fail execute 0 EXIT THEN
   ta tu SIG-TYPE {: term:n :}
   NEXT-SIG-TOK {: ea:^ eu:n :}
   SGBAD @ IF fail READ-BAD 0 EXIT THEN
   CHECKER-SIG-UNRES-TAKE IF
      s" unknown type" E-TDECL-PAYLOAD 0 fail execute 0 EXIT
   THEN 2drop
   eu 0 <> IF
      ea eu s" one type expected" E-TDECL-SYNTAX 0 fail execute 0 EXIT
   THEN
   term TERM-SCHEMA dup 0 <> IF EXIT THEN drop
   a 0 STG-UNSTORABLE-TYPE STGR-REASON$ E-TDECL-SYNTAX
   STG-UNSTORABLE-TYPE fail execute 0 ;

variable TDV-TAG   variable TDV-N    variable TDV-MAX
variable TDV-SS    variable TDV-PC   variable TDV-PW
PTR-VARIABLE TDV-NA    variable TDV-NU

: TDECL-VARIANT-NAME ( str -- ) {: kw:^ ku:n :}
   TDECL-NEXT {: a:^ u:n :}
   u 0= IF kw ku s" missing name"
      s" write a name after `VARIANT`, as in `VARIANT none ;VARIANT`"
      E-TDECL-SYNTAX TDECL-THROW THEN
   a u TDECL-REQUIRE-VARIANT-NAME
   a u TFAM-DERIVED-TAIL? IF
      TDECL-CUR-FAM @ TFAM-DERIVE-ANY? IF a u TDECL-COLLIDE-THROW THEN
   THEN
   a TDV-NA !  u TDV-NU ! ;

: TDECL-VARIANT-CLOSE ( n -- ) {: fam:n :}
   TDV-NA @ TDV-NU @ TDECL-TOK!
   s" duplicate variant" TDECL-VARIANT-FIX$ TFAM:E-TFAM-DUP TDECL-ARM
   fam TDV-NA @ TDV-NU @ TDV-TAG @ TDV-SS @ TDV-PC @ TDV-PW @ SUMV-ADD drop
   TDV-PW @ TDV-MAX @ max TDV-MAX !
   TDV-TAG @ 1 + TDV-TAG !
   TDV-N @ 1 + TDV-N ! ;

\ Application arguments and quotation effect rows push their roots into the
\ schema-root pool while the element parses, so element nodes collect here and
\ land as one contiguous block after every argument root (TDECL-VARIANT-FINISH).
\ Variants never nest, so one scratch suffices.
PTR-VARIABLE TDV-PAY-P   SCRATCH-VARIABLE TDV-PAY-CAP
variable TDV-PAY-N
: TDV-PAY-SCR ( -- ^n ) TDV-PAY-P @ ;
: TDV-PAY+ ( n -- ) {: node:n :}
   TDV-PAY-P TDV-PAY-CAP TDV-PAY-N @ 1 + 1 ARENA-ROWS-ENSURE
   node TDV-PAY-N @ cells TDV-PAY-SCR + !
   TDV-PAY-N @ 1 + TDV-PAY-N ! ;
\ Every SCH-PARAM in a landed variant's payload subtree (plain element,
\ application argument, quotation side) must lie within the declaring family's
\ arity (TFAM-SCH-ARITY); an out-of-arity parameter rejects inside TDECL-RUN's
\ transaction. This is the authoritative check; the TDECL-LETTER parse gate
\ is not. The letter renders through the shared
\ declaration alphabet.
create TDV-BADLETTER 1 allot
: TDECL-VPAY-ARITY ( n n n -- ) {: fam:n base:n count:n :}
   0 BEGIN dup count < WHILE
      base over + SCHEMA-ROOT@ fam swap TFAM-SCH-ARITY
      dup 0 >= IF
         TFAM-DECL-PARAM>CHAR drop TDV-BADLETTER c!
         TDV-BADLETTER 1 s" payload parameter out of family arity"
         TDECL-PARAM-FIX$ E-TDECL-PAYLOAD TDECL-THROW
      THEN
      drop 1 +
   REPEAT drop ;
: TDECL-VARIANT-FINISH ( n -- ) {: fam:n :}
   SIG-UNRES @ IF
      TDV-NA @ TDV-NU @ TDECL-SEEN? IF
         TDV-NA @ TDV-NU @ s" duplicate variant"
            TDECL-VARIANT-FIX$ TFAM:E-TFAM-DUP TDECL-THROW THEN
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

\ The reason and fix, both empty when an integer compares by `=` or a family by
\ its own EQ and HASH; no checked pointer equality exists, and comparing a
\ linear value consumes it.
: TDECL-DERIVE-WHY ( n n -- str str )
   {: fam:n node:n :}
   node SCHEMA-CON? IF
      node SCHEMA-A@ CT-INT? IF s" " s" " EXIT THEN
      s" payload type has no derived equality"
      s" give the payload an integer type, or drop `eq` and `hash` from `DERIVE`"
      EXIT THEN
   node SCHEMA-PTR? IF
      s" pointer payloads have no derived equality"
      s" drop `eq` and `hash` from `DERIVE`, or give the payload a type that is not a pointer"
      EXIT THEN
   node SCHEMA-APP? 0= IF
      s" payload role has no derived equality"
      s" give each payload an integer type or a family that derives the same features, or drop `eq` and `hash` from `DERIVE`"
      EXIT THEN
   node SCHEMA-A@ {: ff:n :}
   ff TFAM-GEN-PUBLIC? 0= IF
      ff TFAM-PKG$ fam TFAM-PKG$ CORE-STR=CI 0= IF
         s" field family's derived words are private to its package"
         s" declare the field's family in a public section of its package, or drop `eq` and `hash` from `DERIVE`"
         EXIT THEN THEN
   fam TFAM-DERIVE-EQ? ff TFAM-DERIVE-EQ? 0= and IF
      s" field family must also derive eq"
      s" add `DERIVE eq` to the field's family, or drop `eq` from this `DERIVE`"
      EXIT THEN
   fam TFAM-DERIVE-HASH? ff TFAM-DERIVE-HASH? 0= and IF
      s" field family must also derive hash"
      s" add `DERIVE hash` to the field's family, or drop `hash` from this `DERIVE`"
      EXIT THEN
   s" " s" " ;

: TDECL-REQUIRE-DERIVABLE ( str n n -- )
   {: ta:^ tu:n fam:n node:n :}
   fam TFAM-DERIVE-EQ? fam TFAM-DERIVE-HASH? or 0= IF EXIT THEN
   fam node TDECL-DERIVE-WHY {: wa:^ wu:n fa:^ fu:n :}
   wu 0= IF EXIT THEN
   ta tu wa wu fa fu E-TDECL-DERIVE TDECL-THROW ;

: TDECL-VARIANT ( str n -- ) {: kw:^ ku:n fam:n :}
   kw ku TDECL-VARIANT-NAME
   0 TDV-PW !  0 TDV-ARG-N !  0 TDV-PAY-N !
   BEGIN
      TDECL-NEXT
      2dup s" ;variant" CORE-STR=CI IF 2drop fam TDECL-VARIANT-FINISH EXIT THEN
      0 TYPE-LEVEL !  2dup TDECL-VPAY-ELEM
      SIG-UNRES @ IF drop 2drop ELSE
         >r fam r@ TDECL-REQUIRE-DERIVABLE r>
         dup fam swap TFAM-SCH-WIDTH TDV-PW @ + TDV-PW ! TDV-PAY+
      THEN
   AGAIN ;

: TDECL-SUM-VARIANTS ( n -- ) {: fam:n :}
   BEGIN
      TDECL-NEXT
      dup 0= IF 2drop EXIT THEN
      2dup s" variant" CORE-STR=CI 0= IF
         s" unexpected token in sum declaration"
            s" write each variant as `VARIANT name type ... ;VARIANT`, after any `POLICY` and `DERIVE` clause"
            E-TDECL-SYNTAX TDECL-THROW
      THEN
      fam TDECL-VARIANT
   AGAIN ;

private
EXPORT BOOT:data-base
EXPORT CHECK:CHECKER-COLON-SCAN
EXPORT CHECKER-RESOLVE:HEAD-ROW
EXPORT CHECKER-RESOLVE:PART-ROW

0 constant TDECL-CTOR-FREE
1 constant TDECL-BY-PART
2 constant TDECL-BY-FAMILY
3 constant TDECL-BY-PART-FAMILY
4 constant TDECL-BY-PACKAGE

\ What takes the name: a source's part, else a part's family or a package no
\ family made (top-level family) or a family of the enclosing package (part's).
: TDECL-CTOR-HOLDER ( str n n -- n ) {: ca:^ cu:n cn:n ci:n :}
   cn 0 = IF
      ca cu PART-ROW {: top:^ :}
      top NULL-PTR = IF
         ca cu HEAD-ROW {: pkg:^ :}
         pkg NULL-PTR = IF TDECL-CTOR-FREE EXIT THEN
         ca cu TFAM-CTOR-PKG? IF TDECL-CTOR-FREE EXIT THEN
         TDECL-BY-PACKAGE EXIT
      THEN
      top CELL + @ 0 = IF TDECL-BY-PART-FAMILY EXIT THEN
      TDECL-BY-PART EXIT
   THEN
   ca ci 1 + +  cu ci 1 + -  PART-ROW {: row:^ :}
   row NULL-PTR = IF
      ca cu TFAM-CTOR-PKG? IF TDECL-BY-FAMILY EXIT THEN
      TDECL-CTOR-FREE EXIT
   THEN
   row CELL + @ 0 = IF TDECL-CTOR-FREE EXIT THEN
   TDECL-BY-PART ;

\ A part's family makes its constructor package a part with no private wordlist.
\ TF-INTERN may move the pool TFAM-PKG$ and TFAM-NAME$ read, so it runs last.
: TDECL-CTOR-PUBLISH ( n n n -- ) {: fam:n vstart:n count:n :}
   fam TFAM-GEN-PUBLIC? 0= IF EXIT THEN
   TDN-A @ TDN-U @ TDECL-TOK!
   fam TFAM-PKG$ fam TFAM-NAME$ TF-CTOR-PKG$ {: ca:^ cu:n :}
   ca cu CHECKER-COLON-SCAN {: cn:n ci:n :}
   ca cu cn ci TDECL-CTOR-HOLDER {: by:n :}
   by TDECL-BY-PART = IF
      fam TFAM-NAME$ s" a part of this package already bears its constructor package's name"
         s" rename the family or that part" E-TDECL-NAME TDECL-THROW-AT
   THEN
   by TDECL-BY-FAMILY = IF
      fam TFAM-NAME$
         s" a family of the enclosing package already makes a constructor package of this name"
         s" rename this family or the part" E-TDECL-NAME TDECL-THROW-AT
   THEN
   by TDECL-BY-PART-FAMILY = IF
      fam TFAM-NAME$
         s" a family of a part of this package already makes a constructor package of this name"
         s" rename this family or that part's family" E-TDECL-NAME TDECL-THROW-AT
   THEN
   by TDECL-BY-PACKAGE = IF
      ca cu s" a package already bears its constructor package's name"
         s" rename this family or that package" E-TDECL-NAME TDECL-THROW-AT
   THEN
   cn 0 <> IF ca cu RES-FALSE CHECKER-OVERLAY:NS-ENSURE drop THEN
   ca cu TF-INTERN {: coff:n :}
   count 0 DO  vstart i +  coff cu SUMV-CTOR-PKG!  LOOP ;

\ `POLICY <name>` follows the arity (sum/product) or the name (enum), before the
\ first VARIANT/FIELD; a missing clause leaves TL-STACK-CELL-TAG. stack-cell-tag
\ and packed-tag are accepted: packed keeps the stack representation and bakes a
\ LAY memory descriptor at close (TDECL-LAYOUT-DESC). niche-null is accepted for
\ one shape, checked once the variants are read; boxed rejects as not supported
\ until codegen lowers it. Any other token, `custom` included (the LAY registry
\ tolerates it), is an unknown policy.

: TDECL-POLICY-DEFERRED? ( str -- bool ) {: a:^ u:n :}
   a u s" boxed" CORE-STR=CI ;
: TDECL-POLICY-SET ( str n -- ) {: a:^ u:n fam:n :}
   a u s" stack-cell-tag" CORE-STR=CI IF fam TL-STACK-CELL-TAG TFAM-LAYOUT! EXIT THEN
   a u s" packed-tag" CORE-STR=CI IF fam TL-PACKED-TAG TFAM-LAYOUT! EXIT THEN
   a u s" niche-null" CORE-STR=CI IF fam TL-NICHE TFAM-LAYOUT! EXIT THEN
   a u TDECL-POLICY-DEFERRED? IF
      a u s" layout policy not yet supported"
         s" replace `boxed` with `stack-cell-tag`, `packed-tag` or `niche-null`, or drop the `POLICY` clause"
         E-TDECL-POLICY TDECL-THROW
   THEN
   a u s" unknown layout policy"
      s" name `stack-cell-tag`, `packed-tag` or `niche-null` after `POLICY`"
      E-TDECL-POLICY TDECL-THROW ;
\ Bake the packed memory descriptor at declaration close, when the variant /
\ field counts that PACKED-DESC reads are final. stack-cell-tag families bake
\ no LAY row (nothing consumes one). Runs inside TDECL-RUN's transaction, so a
\ later close-stage reject rolls LAY-N back with the family row.

: TDECL-LAYOUT-DESC ( n -- ) {: fam:n :}
   fam TFAM-LAYOUT-POLICY@ TL-PACKED-TAG = 0= IF EXIT THEN
   fam TL-PACKED-TAG fam PACKED-DESC LAY-ADD drop ;
: TDECL-POLICY ( n -- ) {: fam:n :}
   TDECL-NEXT {: a:^ u:n :}
   a u s" policy" CORE-STR=CI 0= IF a u PK! EXIT THEN     \ no clause: push the token back for the body loop
   TDECL-NEXT {: pa:^ pu:n :}
   pu 0= IF TDN-A @ TDN-U @ s" missing layout policy name"
      s" name a policy after `POLICY`: `stack-cell-tag`, `packed-tag` or `niche-null`"
      E-TDECL-POLICY TDECL-THROW THEN
   pa pu fam TDECL-POLICY-SET ;

\ A niche-null value is one cell whose zero is the empty first variant, so the
\ family is that variant and one holding a single `^` field.
: TDECL-NICHE-MESSAGE$ ( -- str )
   s" niche-null needs an empty variant, then one pointer field" ;

: TDECL-NICHE-THROW ( -- )
   TDN-A @ TDN-U @ TDECL-NICHE-MESSAGE$
      s" drop `POLICY niche-null`, or declare an empty variant, then one holding a single `^` payload"
      E-TDECL-POLICY TDECL-THROW ;
: TDECL-NICHE-SHAPE? ( n -- bool ) {: vstart:n :}
   TDV-N @ 2 <> IF RES-FALSE EXIT THEN
   vstart SUMV-SCH-COUNT@ 0 <> IF RES-FALSE EXIT THEN
   vstart 1 + SUMV-SCH-COUNT@ 1 <> IF RES-FALSE EXIT THEN
   vstart 1 + SUMV-SCH-START@ SCHEMA-ROOT@ SCHEMA-PTR? ;
: TDECL-NICHE-REQUIRE ( n n -- ) {: fam:n vstart:n :}
   fam TFAM-NICHE? 0= IF EXIT THEN
   vstart TDECL-NICHE-SHAPE? IF EXIT THEN
   TDECL-NICHE-THROW ;

\ `DERIVE <feature>...` follows POLICY and precedes every VARIANT and FIELD, so
\ each payload is judged against the family's final flags as it is read.

: TDECL-DERIVE-GUARD ( str n -- ) {: a:^ u:n fam:n :}
   fam TFAM-ARITY@ 0 <> IF
      a u s" derive requires a concrete (arity 0) family"
         s" drop `eq` and `hash` from `DERIVE`, or declare the family with no type parameters"
         E-TDECL-DERIVE TDECL-THROW THEN ;
: TDECL-DERIVE-SET ( str n -- ) {: a:^ u:n fam:n :}
   a u s" eq" CORE-STR=CI IF
      a u fam TDECL-DERIVE-GUARD  fam TFAM-DERIVE-EQ! EXIT THEN
   a u s" hash" CORE-STR=CI IF
      a u fam TDECL-DERIVE-GUARD  fam TFAM-DERIVE-HASH! EXIT THEN
   a u s" init" CORE-STR=CI IF
      a u s" derive init requires a STRUCTURE"
         s" drop `init` from `DERIVE`: only a `STRUCTURE` derives it"
         E-TDECL-DERIVE TDECL-THROW THEN
   a u s" tag" CORE-STR=CI IF
      a u s" derive tag requires an enum"
         s" drop `tag` from `DERIVE`: only an `ENUM` derives it"
         E-TDECL-DERIVE TDECL-THROW THEN
   a u s" order" CORE-STR=CI IF
      a u s" derive feature not yet supported"
         s" drop `order` from `DERIVE` and write the comparison as a word of your own"
         E-TDECL-DERIVE TDECL-THROW THEN
   a u s" unknown derive feature"
      s" name `eq`, `hash` or `init` after `DERIVE`"
      E-TDECL-DERIVE TDECL-THROW ;
: TDECL-DERIVE-MORE? ( n -- bool ) {: fam:n :}
   TDECL-NEXT {: a:^ u:n :}
   u 0= IF RES-FALSE EXIT THEN
   a u s" eq" CORE-STR=CI
   a u s" hash" CORE-STR=CI or
   a u s" init" CORE-STR=CI or
   a u s" tag" CORE-STR=CI or
   a u s" order" CORE-STR=CI or
   0= IF a u PK! RES-FALSE EXIT THEN
   a u fam TDECL-DERIVE-SET
   RES-TRUE ;
: TDECL-DERIVE ( n -- ) {: fam:n :}
   TDECL-NEXT {: a:^ u:n :}
   a u s" derive" CORE-STR=CI 0= IF a u PK! EXIT THEN
   TDECL-NEXT {: fa:^ fu:n :}
   fu 0= IF TDN-A @ TDN-U @ s" missing derive feature"
      s" name a feature after `DERIVE`: `eq`, `hash` or `init`"
      E-TDECL-DERIVE TDECL-THROW THEN
   fa fu fam TDECL-DERIVE-SET          \ the FIRST token must be a feature (unknown rejects);
   BEGIN fam TDECL-DERIVE-MORE? 0= UNTIL ;   \ later tokens end the order-free list by pushback

\ --- registration entry points (verify-source, sumtype-front.f and the definers below).

private

: CHECKER-DEFSUM-BODY ( -- n )
   s" write a name after `SUMTYPE`, as in `SUMTYPE shape`" TDECL-HEADER! {: ar:n :}
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
   TDV-N @ 0= IF TDN-A @ TDN-U @ s" empty sum"
      s" declare at least one variant, as in `VARIANT none ;VARIANT`"
      E-TDECL-SYNTAX TDECL-THROW THEN
   fam vstart TDECL-NICHE-REQUIRE
   fam vstart TDV-N @ TFAM-VAR-RANGE!
   fam TDV-MAX @ TFAM-SLOTS!
   fam TDECL-LAYOUT-DESC
   \ Generation arms its own reasons: the last payload's is cleared, so a code
   \ thrown while none is armed is rethrown, not reported against that payload.
   TDN-A @ TDN-U @ TDECL-TOK!  s" " s" " 0 TDECL-ARM
   fam ;


\ TYPE-DECL's parts install the transaction runner at load.
defer TDECL-TXN-XT ( [ -- renders parses nominal ] -- renders parses nominal )

: CHECKER-DEFFAMILY-BODY ( -- )
   s" write a name after `NEWTYPE`, as in `NEWTYPE meters`" TDECL-HEADER! {: ar:n :}
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   ar TK-CELL TDECL-FAMILY TDECL-FAM-REG ! ;


\ The family header and the source token that names it, the same bytes up to
\ case: a DEFTYPE name folds to its header, a NEWTYPE name is its own token.
\ With no name, the token is the keyword. KIND is the declaration's keyword,
\ "newtype" or "deftype", which its records name.
: TDECL-DEFFAMILY ( str str str -- )
   {: na:^ nu:n ta:^ tu:n ka:^ ku:n :}
   ka ku na nu s" " TDECL-CTX!
   ta tu TDECL-PLACE!
   [: CHECKER-DEFFAMILY-BODY ;] TDECL-RUN ;

defer TDECL-CAPACITY-PREFLIGHT-XT ( str n -- )

CAST: AS-STORAGE-RUN ( n -- [ str n n str -- n ] )

: TDECL-CHILD-RUN ( str n n str -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-STORAGE-RUN execute ;

CAST: AS-OPEN ( n -- [ str n -- ] )

: TDECL-DEFINE-RUN ( str n -- )
   data-base PROVIDED-XT:DEFINE-CELL + @ AS-OPEN execute ;

CAST: AS-ITEM ( n -- [ str n n -- ] )

\ Appends to the open definition the item of ITEM-ABI op OP, spelled A U,
\ with argument V.
: TDGEN-ITEM ( str n n -- )
   data-base PROVIDED-XT:ITEM-CELL + @ AS-ITEM execute ;

\ Makes the child word NAME through CHILD-RUN, of a CHILD-ABI SHAPE and PARAM.
: TDECL-CHILD ( str n n -- )
   {: na:^ nu:n shape:n param:n :}
   na nu shape param NULL-PTR 0 TDECL-CHILD-RUN drop ;

\ Makes the stored child NAME over TYPE, as TDECL-CHILD makes a child, and
\ answers its effect handle.
: TDECL-STORED-CHILD ( str str -- n )
   {: na:^ nu:n ta:^ tu:n :}
   na nu CHILD-ABI:STORED 0 ta tu TDECL-CHILD-RUN ;

\ Makes the stored child NAME over type node NODE, as TDECL-STORED-CHILD does.
: TDECL-NODE-CHILD ( str n -- n ) {: na:^ nu:n node:n :}
   na nu CHILD-ABI:STORED node NULL-PTR 0 TDECL-CHILD-RUN ;

\ The structures.f field words, OFF bytes into a record.
: TDECL-CELL-CHILD ( str n -- ) {: na:^ nu:n off:n :}
   na nu CHILD-ABI:CELL-OFF off TDECL-CHILD ;

: TDECL-PTR-CHILD ( str n -- ) {: na:^ nu:n off:n :}
   na nu CHILD-ABI:PTR-OFF off TDECL-CHILD ;

: TDECL-BYTE-CHILD ( str n -- ) {: na:^ nu:n off:n :}
   na nu CHILD-ABI:BYTE-OFF off TDECL-CHILD ;

: TDECL-NAME-PREFLIGHT-MISSING ( str -- )
   2drop s" sumtype: generated name preflight not installed" EXIT-STATUS:INTERNAL die ;

: TDECL-CAPACITY-PREFLIGHT-MISSING ( str n -- )
   2drop drop s" sumtype: generated capacity preflight not installed" EXIT-STATUS:INTERNAL die ;

: TDECL-PREFLIGHT-DEFAULTS ( -- )
   [: TDECL-NAME-PREFLIGHT-MISSING ;] is TDECL-NAME-PREFLIGHT-XT
   [: TDECL-CAPACITY-PREFLIGHT-MISSING ;] is TDECL-CAPACITY-PREFLIGHT-XT ;
TDECL-PREFLIGHT-DEFAULTS

PTR-VARIABLE TDGEN-P
SCRATCH-VARIABLE TDGEN-CAP        \ TDGEN-P's room, in cells
variable TDGEN-U
: TDGEN-BUF ( -- ^u8 ) TDGEN-P @ ;

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
7 constant TDPLAN-FROM-TAG
create TDPLAN-BOOT TDPLAN-CAP-INIT allot
create TDPLAN-ROW-BOOT TDPLAN-ROW-CAP-INIT TDPLAN-REC * allot
PERSISTED-PTR-VARIABLE TDPLAN-P       TDPLAN-BOOT TDPLAN-P !
PERSISTED-PTR-VARIABLE TDPLAN-ROW-P   TDPLAN-ROW-BOOT TDPLAN-ROW-P !
variable TDPLAN-CAP         TDPLAN-CAP-INIT TDPLAN-CAP !
variable TDPLAN-ROW-CAP     TDPLAN-ROW-CAP-INIT TDPLAN-ROW-CAP !
variable TDPLAN-U
variable TDPLAN-N

variable TDPLAN-SYM-HI        \ the largest symbol a row of this plan holds

: TDPLAN-BYTE-NEED ( n -- n ) TDPLAN-U @ + ;

: TDPLAN-GROW-CAP ( n n -- n ) {: need:n cap:n :}
   cap TDPLAN-BYTE-MAX 2 / <= IF cap 2 * need max EXIT THEN
   need ;

: TDPLAN-ENSURE ( n -- ) {: need:n :}
   need TDPLAN-CAP @ <= IF EXIT THEN
   need TDPLAN-CAP @ TDPLAN-GROW-CAP {: cap:n :}
   TDPLAN-P TDPLAN-CAP @ cap REG-GROW1
   cap TDPLAN-CAP ! ;

: TDPLAN-ROWS>BYTES ( n -- n ) TDPLAN-REC * ;

: TDPLAN-NEXT-ROW-CAP ( -- n )
   TDPLAN-N @ 1 + {: need:n :}
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


: TDPLAN-BEGIN ( -- )
   0 TDPLAN-U !
   0 TDPLAN-N !
   0 TDPLAN-SYM-HI ! ;


: TDPLAN-APP ( str -- ) {: a:^ u:n :}
   u TDPLAN-BYTE-NEED {: need:n :}
   need TDPLAN-ENSURE
   TDPLAN-U @ {: off:n :}
   u 0 ?do a i + c@ TDPLAN-P @ off + i + c! loop
   need TDPLAN-U ! ;

: TDPLAN-ROW ( n -- ^n )
   TDPLAN-REC * TDPLAN-ROW-P @ + ;

: TDPLAN.SYM ( ^a -- ^a ) TDPLAN.SYM-OFF + ;
: TDPLAN.NAME-OFF ( ^a -- ^a ) TDPLAN.NAME-OFF-OFF + ;
: TDPLAN.NAME-U ( ^a -- ^a ) TDPLAN.NAME-U-OFF + ;
: TDPLAN.BUILT ( ^a -- ^a ) TDPLAN.BUILT-OFF + ;
: TDPLAN.FAM ( ^a -- ^a ) TDPLAN.FAM-OFF + ;
: TDPLAN.VID ( ^a -- ^a ) TDPLAN.VID-OFF + ;

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
            TDGEN-BUF TDGEN-U @ s" generated declaration name already defined"
               s" rename the family or a variant so no two words this declaration generates share a name"
               E-TDECL-NAME TDECL-THROW
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
   TDGEN-BUF TDGEN-U @ {: na:^ nu:n :}
   na nu CHECKER-DEFINED-HERE? IF
      na nu s" generated declaration already defined"
         TDECL-GEN-FIX$ E-TDECL-NAME TDECL-THROW-AT
   THEN
   na nu CHECKER-RECORD-SYM dup TDPLAN-SYM+
   TDPLAN-N @ 1 - TDPLAN-ROW {: r:^ :}
   TDPLAN-U @ r TDPLAN.NAME-OFF !
   nu r TDPLAN.NAME-U !
   na nu TDPLAN-APP ;

: TDPLAN-CTOR-NAME+ ( n -- n ) {: vid:n :}
   TDPLAN-NAME+ dup vid swap SUMV-CTOR-SYM! ;

\ A built row records its name, family and kind; its kind's builder makes the
\ word at evaluation.
: TDPLAN-BUILT+ ( n n -- ) {: fam:n kind:n :}
   TDPLAN-NAME+ drop
   TDPLAN-N @ 1 - TDPLAN-ROW {: r:^ :}
   kind r TDPLAN.BUILT !  fam r TDPLAN.FAM !  0 r TDPLAN.VID ! ;

\ A built row whose name is variant VID's constructor symbol.
: TDPLAN-CTOR-BUILT+ ( n n n -- ) {: vid:n fam:n kind:n :}
   vid TDPLAN-CTOR-NAME+ drop
   TDPLAN-N @ 1 - TDPLAN-ROW {: r:^ :}
   kind r TDPLAN.BUILT !  fam r TDPLAN.FAM !  vid r TDPLAN.VID ! ;

: TDPLAN-SPAN$ ( n n -- str ) {: off:n u:n :}
   TDPLAN-P @ off + u ;


: TDPLAN-NAME$ ( n -- str )
   TDPLAN-ROW {: r:^ :}
   r TDPLAN.NAME-OFF @ r TDPLAN.NAME-U @ TDPLAN-SPAN$ ;


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
   row TDPLAN-NAME$ din dout RES-FALSE CHECKER-CHILD-ROWS drop ;

: TDPLAN-PREFLIGHT-NAMES ( -- )
   TDPLAN-N @ 0 ?do i TDPLAN-NAME$ TDECL-NAME-PREFLIGHT-XT loop ;

\ FAM's value type; derive requires arity 0.
: TDECL-SELF-NODE ( n -- n ) {: fam:n :}
   fam 0 0 SCHEMA-APP ;

\ TAG>'s effect, ( n -- fam ).
: TDECL-FROM-TAG-EFFECT ( n -- n ) {: fam:n :}
   EMPTY-EFFECT  CC-N SCHEMA-CON EFFECT-IN
   fam TDECL-SELF-NODE EFFECT-OUT ;

\ The effect a derived word of KIND over FAM takes.
: TDECL-KIND-EFFECT ( n n -- n ) {: fam:n kind:n :}
   kind TDPLAN-FROM-TAG = IF fam TDECL-FROM-TAG-EFFECT EXIT THEN
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
         i TDPLAN-NAME$  i TDECL-DRV-EFFECT EFFECT-ROWS 2drop
         i TDPLAN-SYM@ SUMV-FROM-CTOR-SYM nip  CHECKER-CHILD-ROWS drop
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
   TDPLAN-N @ 0= IF s" sumtype: empty generated declaration plan" EXIT-STATUS:INTERNAL die THEN
   0 TDPLAN-NAME$ TDPLAN-N @ TDECL-CAPACITY-PREFLIGHT-XT
   TDPLAN-PREFLIGHT-CHECKER ;

: TDGEN-CLEAR ( -- ) 0 TDGEN-U ! ;
: TDGEN-C, ( n -- )
   TDGEN-P TDGEN-CAP  TDGEN-U @ CELL / 1 +  1 ARENA-ROWS-ENSURE
   TDGEN-BUF TDGEN-U @ + c!
   TDGEN-U @ 1 + TDGEN-U ! ;
: TDGEN-APP ( str -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ TDGEN-C, loop ;
: TDGEN-UPPER ( str -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ TF-UPPER-C TDGEN-C, loop ;

\ --- derived typed equality. `DERIVE eq` builds checked words into the family's
\ constructor package from items, their engine words found where this file is
\ compiled: no pending window, no engine lowering, so equality is semantic and
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
\ A derived word is spelled as a constructor: "PKG-FAMILY:TAIL" for a public
\ family, "B-FAMILY:TAIL" for one in part A:B, "FAMILY-TAIL" for a private or
\ OPAQUE one, which stamps no constructor package (TDECL-CTOR-PUBLISH exits
\ early), so SUMV-CTOR-PKG$ would name nothing. A private or OPAQUE family's
\ words bind in the declaring package, so only a declaration in that package
\ derives through them (TDECL-DERIVE-WHY).
: TDGEN-DRV-REF ( n str -- ) {: fam:n ta:^ tu:n :}
   fam TFAM-GEN-PUBLIC? 0= IF
      fam TFAM-NAME$ ta tu TF-CTOR-PRIV$ TDGEN-APP EXIT THEN
   fam TFAM-VAR-START@ SUMV-CTOR-PKG$ {: ca:^ cu:n :}
   ca cu CHECKER-COLON-SCAN nip 1 + {: skip:n :}
   ca skip + cu skip - TDGEN-APP
   58 TDGEN-C,
   ta tu TDGEN-UPPER ;
: TDGEN-DRV-NAME ( n str -- ) {: fam:n ta:^ tu:n :}   \ the name alone
   TDGEN-CLEAR  fam ta tu TDGEN-DRV-REF ;
: TDGEN-KW ( str -- ) 0 ITEM-ABI:KEYWORD TDGEN-ITEM ;
: TDGEN-OF ( str -- )                \ a variant's `of`
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
\ A top-level public family's derived TAIL by its exact name, which no part
\ named as its constructor package captures; a part's or a private or OPAQUE
\ family's where it binds, the part's `B-F` to the part of A.
: TDGEN-DRV-CALL ( n str -- ) {: fam:n ta:^ tu:n :}
   TDGEN-CLEAR  fam ta tu TDGEN-DRV-REF
   fam TFAM-VAR-START@ SUMV-CTOR-PKG$ CHECKER-COLON-SCAN drop 0= {: top:bool :}
   fam TFAM-GEN-PUBLIC? top and IF ITEM-ABI:CALL-EXACT ELSE ITEM-ABI:CALL THEN {: op:n :}
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
\ TAG>: case <tag> of FAM:V endof .. E-ENUM-TAG throw endcase. Each arm calls
\ its variant's constructor, never a cast, so it holds under every layout policy.
: TDGEN-FROM-TAG-BODY ( n -- ) {: fam:n :}
   s" case" TDGEN-KW
   fam TFAM-VAR-START@ {: vstart:n :}
   vstart fam TFAM-VAR-COUNT@ + vstart ?do
      i SUMV-TAG@ literal  s" of" TDGEN-KW
      fam i SUMV-NAME$ TDGEN-DRV-CALL
      s" endof" TDGEN-KW
   loop
   E-ENUM-TAG literal  postpone throw
   s" endcase" TDGEN-KW ;
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
: TDGEN-HMIX ( -- )                       \ xor PRIME *
   postpone xor  FNV:PRIME literal  postpone * ;
: TDGEN-HASH-FOLD1 ( n n n n -- ) {: fam:n vid:n base:n i:n :}   \ slot i [FAM:HASH] mix
   fam vid base i TDGEN-SLOT@
   vid i SUMV-PAY-ROOT SCHEMA-ROOT@ {: node:n :}
   node SCHEMA-APP? IF node SCHEMA-A@ s" hash" TDGEN-DRV-CALL THEN
   TDGEN-HMIX ;
: TDGEN-HASH-FOLDS ( n n n -- ) {: fam:n vid:n base:n :}   \ fold every slot's local
   vid SUMV-PAY-N 0 ?do fam vid base i TDGEN-HASH-FOLD1 loop ;
: TDGEN-HASH-ARM ( n n -- ) {: fam:n vid:n :}   \ bind payloads, fold tag then payloads
   vid TDGEN-BINDS {: base:n :}
   FNV:BASIS literal
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
   FNV:BASIS literal
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
: TDECL-FROM-TAG-WORD ( n -- ) {: fam:n :}
   fam s" tag>" TDGEN-DRV-NAME
   fam TDPLAN-FROM-TAG TDPLAN-BUILT+ ;
: TDECL-DRV-WORDS ( n -- ) {: fam:n :}
   fam TFAM-DERIVE-ANY? 0= IF EXIT THEN
   fam TFAM-PRODUCT? 0= IF fam TDECL-TAG-WORD THEN   \ discriminant rides any derive
   fam TFAM-DERIVE-EQ? IF fam TDECL-EQ-WORD THEN
   fam TFAM-DERIVE-HASH? IF fam TDECL-HASH-WORD THEN
   fam TFAM-DERIVE-TAG? IF fam TDECL-FROM-TAG-WORD THEN ;

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
: TDECL-FROM-TAG-BUILD ( n n -- ) {: fam:n row:n :}
   row TDECL-BUILT-OPEN
   fam TDGEN-FROM-TAG-BODY
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
   kind TDPLAN-FROM-TAG = IF fam row TDECL-FROM-TAG-BUILD EXIT THEN
   kind TDPLAN-PROD? IF
      row TDPLAN-NAME$ kind TDPLAN-SHAPE fam TDECL-CHILD EXIT
   THEN
   s" sumtype: no builder for this plan row" EXIT-STATUS:INTERNAL die ;

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

EXPORT CHECKER-RESOLVE:PRIVATE-SECTION

\ A live declaration's switch runs the section keyword; a replay, whose
\ pre-pass opens the package in the checker alone, switches the checker only.
: TDECL-GEN-PRIVATE ( bool -- ) {: replay:bool :}
   replay IF CHECKER-PRIVATE EXIT THEN
   PRIVATE-SECTION ;

EXPORT CHECKER-RESOLVE:GLOBAL-SECTION
EXPORT CHECKER-RESOLVE:PUBLIC-SECTION

: TDECL-GEN-RESTORE ( n bool -- ) {: mode:n replay:bool :}
   mode CHECKER-PACKAGE-PRIVATE = IF replay TDECL-GEN-PRIVATE EXIT THEN
   mode CHECKER-PACKAGE-GLOBAL = IF
      replay IF CHECKER-GLOBAL EXIT THEN
      GLOBAL-SECTION EXIT
   THEN
   replay IF CHECKER-PUBLIC EXIT THEN
   PUBLIC-SECTION ;

\ Q plans, preflights and builds (or replays) a family's generated rows. A
\ private or OPAQUE family's rows (not TFAM-GEN-PUBLIC?) go into the declaring
\ package's private wordlist, so Q runs between a switch to private and a
\ restore of the section the declaration runs in, on either exit. Everything
\ that interns a name runs inside the switch. Q takes and returns the family,
\ which each caller's generator takes; catch takes only a
\ stack-preserving quotation.
: TDECL-GEN-PLACED ( n bool [ n -- n ] -- ) {: fam:n replay:bool q :}
   fam TFAM-GEN-PUBLIC? IF fam q execute drop EXIT THEN
   CHECKER-AUTH-PACKAGE-MODE@ {: mode:n :}
   replay TDECL-GEN-PRIVATE
   fam q catch {: rc:n :}
   mode replay TDECL-GEN-RESTORE
   rc 0 <> IF rc throw THEN
   drop ;

: TDECL-PROD-WORDS-BODY ( n -- ) {: fam:n :}
   fam RES-FALSE [: dup TDECL-PROD-PLAN dup TDECL-GEN-EVAL ;] TDECL-GEN-PLACED ;

\ The ( fam -- ) product generator structure-make.f calls: it declares its
\ variant rows through SUMV-ADD before generating, so the committed view is its
\ own view.

: TDECL-PROD-WORDS ( n -- ) {: fam:n :}
   fam TDECL-PROD-WORDS-BODY ;

\ --- generated ADDRESS accessors (`DERIVE addr`).
\ One child word per field plus AT / BYTES / CELLS, in the family's own
\ spelling, each made by the engine's CHILD-RUN from its CHILD-ABI shape and
\ parameter: no text, so no word of the declaring package can capture one. A
\ field's parameter is its committed field id, a BUFFER field's child a SPAN
\ and any other's a FIELD; AT's parameter is the family id; BYTES and
\ CELLS are numbers, and a family whose width reads a parameter has neither,
\ since its width is the instance's. Generation runs at commit: a field id is
\ an authority only once TYPE-FIELD has committed the row, which the runner's
\ commit order (DECL-EVENT before the address surface) guarantees.

: TDECL-ADDR-NAME ( n str -- str ) {: fam:n ta:^ tu:n :}
   TDGEN-CLEAR  fam ta tu TDGEN-DRV-REF  TDGEN-BUF TDGEN-U @ ;

\ Whether FAM's width is one number: it reads none of the family's parameters.
: TDECL-ADDR-SIZED? ( n -- bool ) {: fam:n :}
   fam TFAM-ARITY@ 0 ?do
      fam i TFAM-WIDTH-SLOT-XT IF RES-FALSE unloop EXIT THEN
   loop RES-TRUE ;

\ Runs XT on each child of FAM, with its name, shape and parameter.
: TDECL-ADDR-EACH ( n [ str n n -- ] -- ) {: fam:n xt :}
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fam  fs i + PF-NAME$  TDECL-ADDR-NAME
      fs i + PF-FLAGS@ PF-FLAG-BUFFER = IF CHILD-ABI:SPAN ELSE CHILD-ABI:FIELD THEN
      fs i +  xt execute
   loop
   fam s" at" TDECL-ADDR-NAME  CHILD-ABI:INDEX fam  xt execute
   fam TDECL-ADDR-SIZED? 0= IF EXIT THEN
   fam s" bytes" TDECL-ADDR-NAME
   CHILD-ABI:VALUE  fam TFAM-SLOTS@ CELL *  xt execute
   fam s" cells" TDECL-ADDR-NAME
   CHILD-ABI:VALUE  fam TFAM-SLOTS@  xt execute ;

\ A name the checker already holds in this scope, or one the name preflight
\ rejects, is the declaration's error, asked of every child before any is made.
: TDECL-ADDR-FREE ( str n n -- ) {: na:^ nu:n shape:n param:n :}
   na nu CHECKER-DEFINED-HERE? IF
      na nu  s" generated declaration already defined"
         TDECL-GEN-FIX$ E-TDECL-NAME TDECL-THROW-AT
   THEN
   na nu TDECL-NAME-PREFLIGHT-XT ;

: TDECL-ADDR-MAKE ( str n n -- ) {: na:^ nu:n shape:n param:n :}
   na nu shape param TDECL-CHILD ;

\ Replay makes no word: it registers each child's effect, as CHILD-RUN would.
: TDECL-ADDR-REG ( str n n -- ) {: na:^ nu:n shape:n param:n :}
   shape param TFAM-CHILD-ROWS {: din:n dout:n val:n :}
   na nu din dout RES-FALSE CHECKER-CHILD-ROWS drop
   shape param  CHECKER-REC-SYM @  MEMBER-SYM! ;

: TDECL-ADDR-ROOM ( n -- ) {: fam:n :}
   fam s" at" TDECL-ADDR-NAME
   fam TFAM-FLD-COUNT@ 1 +  fam TDECL-ADDR-SIZED? IF 2 + THEN
   TDECL-CAPACITY-PREFLIGHT-XT ;

: TDECL-ADDR-LIVE ( n -- n )
   dup [: TDECL-ADDR-FREE ;] TDECL-ADDR-EACH
   dup TDECL-ADDR-ROOM
   dup [: TDECL-ADDR-MAKE ;] TDECL-ADDR-EACH ;

: TDECL-ADDR-REPLAY-REG ( n -- n )
   dup [: TDECL-ADDR-FREE ;] TDECL-ADDR-EACH
   dup [: TDECL-ADDR-REG ;] TDECL-ADDR-EACH ;

: TDECL-ADDR-WORDS ( n -- )
   RES-FALSE [: TDECL-ADDR-LIVE ;] TDECL-GEN-PLACED ;

\ Replay (verify-source.f's declaration pre-pass) registers the checked
\ effects of a generated set without emitting code, so a file that uses its
\ own record accessors resolves on the pre-scan.
: TDECL-ADDR-REPLAY ( n -- )
   RES-TRUE [: TDECL-ADDR-REPLAY-REG ;] TDECL-GEN-PLACED ;

\ Makes DEFTYPE's converters over FAM, named from surface NA NU and base BA BU:
\ >NAME then NAME>BASE, the base in upper case. A family refused under a
\ multi-error load is -1 and has no converters.
: TDECL-CONVERTERS ( str str n -- )
   {: na:^ nu:n ba:^ bu:n fam:n :}
   fam 0 < IF EXIT THEN
   TDGEN-CLEAR  [char] > TDGEN-C,  na nu TDGEN-APP
   TDGEN-BUF TDGEN-U @  CHILD-ABI:NOMINAL-IN fam  TDECL-CHILD
   TDGEN-CLEAR  na nu TDGEN-APP  [char] > TDGEN-C,  ba bu TDGEN-UPPER
   TDGEN-BUF TDGEN-U @  CHILD-ABI:NOMINAL-OUT fam  TDECL-CHILD ;

\ Declares DEFTYPE's family, tail TA TU named by surface NA NU, and makes its
\ converters, >NAME then NAME>N.
: TDECL-DEFTYPE ( str str -- )
   {: na:^ nu:n ta:^ tu:n :}
   ta tu na nu s" deftype" TDECL-DEFFAMILY
   na nu s" n" TDECL-FAM-REG @ TDECL-CONVERTERS ;

\ Registers converter NA NU's effect, of SHAPE, over the last declared family.
: TDECL-NOMINAL-REPLAY ( str n -- ) {: na:^ nu:n shape:n :}
   na nu shape TDECL-FAM-REG @ TDECL-ADDR-REG ;

\ --- initialized-field accessors (`DERIVE init`).
\ A getter FIELD@ and a setter FIELD! per field, in the family's own spelling,
\ each made by the engine's CHILD-RUN from its CHILD-ABI shape and committed
\ field id: no text, so no word of the declaring package can capture one.

: TDINIT-TERM ( n -- n ) TFAM-FRESH-TERM ;

\ Names the family by its committed row: TDN is the SUMTYPE front end's, which
\ a STRUCTURE never sets.
: TDINIT-REJECT ( n -- ) {: fam:n :}
   fam TFAM-NAME$ s" derive init requires a canonical fixed-cell record"
   s" drop `init` from `DERIVE`, or give every field a fixed cell width"
   E-TDECL-DERIVE TDECL-THROW ;

\ INIT-RECORD? proves the whole fixed canonical layout; INIT-FIELD? then proves
\ every id and field extent against that same instantiation. These queries read
\ committed rows, so this runs only after PF-COMMIT.
: TDINIT-REQUIRE ( n -- ) {: fam:n :}
   fam TFAM-DERIVE-INIT? 0= fam TFAM-PRODUCT? 0= or IF fam TDINIT-REJECT THEN
   fam TFAM-ARITY@ TFAM-DECL-PARAM-COUNT 4 - > IF fam TDINIT-REJECT THEN
   fam TDINIT-TERM {: term:n :}
   term INIT-RECORD? 0= IF 2drop drop fam TDINIT-REJECT THEN
   2drop drop
   fam TFAM-FLD-COUNT@ 0 ?do
      fam TFAM-FLD-START@ i + term INIT-FIELD? 0= IF
         2drop 2drop unloop fam TDINIT-REJECT THEN
      2drop 2drop
   loop ;

: TDINIT-NAME ( n n bool -- str ) {: fam:n fid:n store:bool :}
   fam  fid store TFAM-INIT-MEMBER$  TDECL-ADDR-NAME ;

\ Runs XT on each accessor of FAM, with its name, shape and field id.
: TDINIT-EACH ( n [ str n n -- ] -- ) {: fam:n xt :}
   fam TFAM-FLD-START@ {: fs:n :}
   fam TFAM-FLD-COUNT@ 0 ?do
      fam fs i + RES-FALSE TDINIT-NAME  CHILD-ABI:INIT-GET fs i +  xt execute
      fam fs i + RES-TRUE TDINIT-NAME  CHILD-ABI:INIT-SET fs i +  xt execute
   loop ;

: TDINIT-ROOM ( n -- ) {: fam:n :}
   fam  fam TFAM-FLD-START@ RES-FALSE TDINIT-NAME
   fam TFAM-FLD-COUNT@ 2 * TDECL-CAPACITY-PREFLIGHT-XT ;

: TDINIT-CHECK ( n [ str n n -- ] -- n )
   over swap TDINIT-EACH
   dup TDINIT-ROOM ;

: TDINIT-LIVE ( n -- n )
   [: TDECL-ADDR-FREE ;] TDINIT-CHECK
   dup [: TDECL-ADDR-MAKE ;] TDINIT-EACH ;

: TDINIT-REPLAY-REG ( n -- n )
   [: TDECL-ADDR-FREE ;] TDINIT-CHECK
   dup [: TDECL-ADDR-REG ;] TDINIT-EACH ;

: TDECL-INIT-WORDS ( n -- )
   dup TDINIT-REQUIRE  RES-FALSE [: TDINIT-LIVE ;] TDECL-GEN-PLACED ;
: TDECL-INIT-REPLAY ( n -- )
   dup TDINIT-REQUIRE  RES-TRUE [: TDINIT-REPLAY-REG ;] TDECL-GEN-PLACED ;

: TDECL-GEN-PLAN ( n -- n )
   {: fam:n :}
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
   TDECL-GEN-PLAN TDPLAN-PREFLIGHT-NAMES TDPLAN-PREFLIGHT-DEFINITIONS ;


: TDECL-CTOR-REPLAY ( n -- )
   RES-TRUE [: TDECL-REPLAY-PLAN ;] TDECL-GEN-PLACED ;

: TDECL-CTOR-WORDS-BODY ( n -- ) {: fam:n :}
   fam TDECL-GEN-PLAN TDECL-GEN-EVAL ;


: TDECL-CLEAR ( -- ) 0 TDECL-U !  0 TDECL-TOK-N ! ;
: TDECL-BUF-ENSURE ( n -- ) {: need:n :}
   TDECL-BUF-P TDECL-BUF-CAP  need CELL + 1 - CELL /  1
   ARENA-ROWS-ENSURE ;
: TDECL-C, ( n -- )
   TDECL-U @ 1 + {: need:n :}
   need TDECL-BUF-ENSURE
   TDECL-BUF TDECL-U @ + c!
   need TDECL-U ! ;
: TDECL-APP ( str -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ TDECL-C, loop ;
: TDECL-ROW+ ( ^u8 n -- ) {: a:^ off:n :}
   TDECL-TOK-N @ 1 + {: need:n :}
   TDECL-TOK-OFF TDECL-TOK-OFF-CAP need 1 ARENA-ROWS-ENSURE
   TDECL-TOK-SRC TDECL-TOK-SRC-CAP need 1 ARENA-ROWS-ENSURE
   off TDECL-TOK-N @ TDECL-OFF !
   a TDECL-TOK-N @ TDECL-ORIGIN !
   need TDECL-TOK-N ! ;
: TDECL-TOKEN+ ( str -- ) {: a:^ u:n :}
   TDECL-U @ 0 > IF 32 TDECL-C, THEN
   a TDECL-U @ TDECL-ROW+
   a u TDECL-APP ;

: CHECKER-DEFFAMILY ( str str str -- ) TDECL-DEFFAMILY ;
NOMINAL-PROVIDER
: CHECKER-DEFTYPE ( str str -- ) TDECL-DEFTYPE ;
NOMINAL-PROVIDER
: CHECKER-NOMINAL-REPLAY ( str n -- ) TDECL-NOMINAL-REPLAY ;

;package
