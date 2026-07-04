\ sumtype.f — TYPEFAMILY/SUMTYPE declaration grammar (docs/type-families.md §9,
\ PLAN item 6). The public ADT authoring surface: `TYPEFAMILY name arity`
\ registers a TK-CELL family; `SUMTYPE name arity VARIANT v pay... ;VARIANT
\ ... ;SUMTYPE` registers a TK-SUM family, one SUMV row per variant (tag =
\ declaration order), payload schema nodes, the family's variant range, and its
\ max payload width. Declarations are package-aware: rows carry the active
\ checker package and visibility, so two packages may declare the same tail.
\ Registration is metadata only — constructors (item 8), hidden layout rows
\ (item 7), and MATCH (item 9) land later. v1 payload elements are positional
\ letter params (a.. within arity), concrete cell types, and `ptr T`; family
\ applications, quotations, and atoms as payloads reject (E-TDECL-PAYLOAD)
\ until schema instantiation work in items 7/8. Comments are not supported
\ inside a declaration block (VALUE-RECORD parity); stray tokens reject.
\ Every failure path is transactional: the registry high-water marks are
\ restored, the failure is reported through TDECL-DIAG (render.f), and the
\ declaration either counts as a multi-error reject or throws its named code.
\ Loaded unchecked in the checker prefix, right after render.f.

\ --- named reject codes (7101-7106 live in type-family.f, 7103 in type-schema.f).
7107 constant E-TDECL-SYNTAX    \ malformed declaration (name/arity/terminator/token)
7108 constant E-TDECL-ARITY     \ arity token is not a small decimal
7109 constant E-TDECL-PAYLOAD   \ unknown variant payload type
7110 constant E-TDECL-NAME      \ reserved or colliding family/variant name

26 constant TDECL-ARITY-CAP     \ positional params are letters a..z (docs §9.2)
$1000 constant TDECL-CAP        \ buffered declaration body bytes

\ --- declaration context (set before TDECL-RUN, read by bodies + diagnostics).
variable TDK-A   variable TDK-U      \ decl kind token ("typefamily"/"sumtype")
variable TDN-A   variable TDN-U      \ family name token
variable TDB-A   variable TDB-U      \ body (SUMTYPE token buffer / arity token)
variable TDT-A   variable TDT-U      \ offending token (diagnostics)
variable TDW-A   variable TDW-U      \ short reason (diagnostics)

: TDECL-TOK! ( ptr u8 n -- ) TDT-U ! TDT-A ! ;
: TDECL-WHY! ( ptr u8 n -- ) TDW-U ! TDW-A ! ;
: TDECL-THROW ( ptr u8 n ptr u8 n n -- ) {: ta:ptr tu:n wa:ptr wu:n code:n :}
   ta tu TDECL-TOK!
   wa wu TDECL-WHY!
   code throw ;

\ --- transactional mark/restore over every store a declaration can grow. The
\ registries retire by counter (linear scans, interned offsets), so restoring
\ the high-water marks fully removes a failed declaration's rows.
variable TDM-TFAM   variable TDM-STR   variable TDM-PK
variable TDM-SUMV   variable TDM-PF    variable TDM-LAY
variable TDM-SCH    variable TDM-ROOT

: TDECL-MARK ( -- )
   TFAM-N @ TDM-TFAM !   TF-STR-U @ TDM-STR !   TF-PK-N @ TDM-PK !
   SUMV-N @ TDM-SUMV !   PF-N @ TDM-PF !        LAY-N @ TDM-LAY !
   SCH-N @ TDM-SCH !     SCH-ROOT-N @ TDM-ROOT ! ;
: TDECL-RESTORE ( -- )
   TDM-TFAM @ TFAM-N !   TDM-STR @ TF-STR-U !   TDM-PK @ TF-PK-N !
   TDM-SUMV @ SUMV-N !   TDM-PF @ PF-N !        TDM-LAY @ LAY-N !
   TDM-SCH @ SCH-N !     TDM-ROOT @ SCH-ROOT-N ! ;

: TDECL-REPORT ( -- )
   TDK-A @ TDK-U @  TDN-A @ TDN-U @  TDT-A @ TDT-U @  TDW-A @ TDW-U @
   TDECL-DIAG ;

\ TDECL-RUN ( xt -- ) : run one declaration body transactionally. On any throw
\ the registries roll back and the failure is reported; a multi-error load
\ counts the reject and continues, otherwise the named code propagates.
: TDECL-RUN ( n -- )
   TDECL-MARK
   catch {: rc:n :}
   rc 0= IF EXIT THEN
   TDECL-RESTORE
   TDECL-REPORT
   MULTI-ERR? IF 1 MULTI-ERR-N +! EXIT THEN
   rc throw ;

: TDECL-CTX! ( ptr u8 n ptr u8 n ptr u8 n -- )   \ kind, name, body
   TDB-U ! TDB-A !
   TDN-U ! TDN-A !
   TDK-U ! TDK-A !
   0 TDT-U !  0 TDT-A !
   s" declaration failed" TDECL-WHY! ;

\ --- name gate: reserved signature/type tokens, control words, and grammar
\ keywords may not name a family or variant (docs §1, PLAN item 6).
: TDECL-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" variant" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;variant" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;sumtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" typefamily" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" sumtype" CORE-STR=CI ;

: TDECL-CONTROL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" then" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" else" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" begin" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" until" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" again" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" while" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" repeat" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" case" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" of" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" endof" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" endcase" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" do" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ?do" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" loop" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" +loop" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" leave" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" unloop" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" exit" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" recurse" CORE-STR=CI ;

: TDECL-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 = IF RES-TRUE EXIT THEN                    \ a..z incl n/f/r type letters
   a u VREC-FIND IF drop RES-TRUE EXIT THEN drop
   a u s" field" CORE-STR= IF RES-TRUE EXIT THEN
   a u CON-OF 0 <> IF RES-TRUE EXIT THEN          \ builtin + deftype CT names
   a u ATOM-TOK? IF RES-TRUE EXIT THEN
   a u FRESH-ATOM-TOK? IF RES-TRUE EXIT THEN
   a u TDECL-CONTROL? IF RES-TRUE EXIT THEN
   a u TDECL-KEYWORD? ;

\ TDECL-FAM-TAKEN? ( ptr u8 n -- bool ) : the name matches a family the
\ declaring scope can already resolve — the global scope always, plus the
\ active package's own rows when one is open. Scope-independent by design:
\ the top-level and in-package verdicts for the same token agree.
: TDECL-FAM-TAKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" " a u TFAM-FIND-IN nip IF RES-TRUE EXIT THEN
   CHECKER-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ a u TFAM-FIND-IN nip ;

: TDECL-REQUIRE-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF a u s" missing name" E-TDECL-SYNTAX TDECL-THROW THEN
   a u TF-CANON? 0= IF
      a u s" name must be a lowercase family tail" E-TFAM-CASE TDECL-THROW
   THEN
   a u TDECL-RESERVED? IF a u s" reserved name" E-TDECL-NAME TDECL-THROW THEN ;

\ A FAMILY name may not shadow a family from another scope; the same-scope
\ collision (incl. a global tail redeclared at top level, where the declaring
\ scope IS the global scope) falls through to TFAM-DECL's E-TFAM-DUP so the
\ diagnostic says "duplicate family". Both scopes consult the registry.
: TDECL-REQUIRE-FAMILY-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TDECL-REQUIRE-NAME
   CHECKER-PACKAGE-ACTIVE? 0= IF EXIT THEN
   s" " a u TFAM-FIND-IN nip 0= IF EXIT THEN
   a u s" shadows a global family" E-TDECL-NAME TDECL-THROW ;

\ A VARIANT name lives in no scope of its own: any collision with a family
\ the declaring scope resolves is a reserved name, in every scope.
: TDECL-REQUIRE-VARIANT-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TDECL-REQUIRE-NAME
   a u TDECL-FAM-TAKEN? 0= IF EXIT THEN
   a u s" collides with a type family" E-TDECL-NAME TDECL-THROW ;

\ --- arity token: small decimal, capped by the positional letter params.
variable TDA-I
: TDECL-DEC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF RES-FALSE EXIT THEN
   0 TDA-I !
   BEGIN TDA-I @ u < WHILE
      a TDA-I @ + c@ dup 48 < swap 57 > or IF RES-FALSE EXIT THEN
      TDA-I @ 1 + TDA-I !
   REPEAT RES-TRUE ;
: TDECL-DEC ( ptr u8 n -- n ) {: a:ptr u:n :}
   0  0 TDA-I !
   BEGIN TDA-I @ u < WHILE
      10 * a TDA-I @ + c@ 48 - +
      TDA-I @ 1 + TDA-I !
   REPEAT ;
: TDECL-ARITY ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= IF a u s" missing arity" E-TDECL-ARITY TDECL-THROW THEN
   a u TDECL-DEC? 0= u 2 > or IF
      a u s" arity must be a decimal, at most 26 (a..z)" E-TDECL-ARITY TDECL-THROW
   THEN
   a u TDECL-DEC
   dup TDECL-ARITY-CAP > IF
      drop a u s" arity must be a decimal, at most 26 (a..z)" E-TDECL-ARITY TDECL-THROW
   THEN ;

\ --- package scope: a declaration registers in the active checker package with
\ the active visibility; top level is the global scope, public.
: TDECL-PKG$ ( -- ptr u8 n )
   CHECKER-PACKAGE-ACTIVE? IF CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ EXIT THEN
   s" " ;
: TDECL-VIS ( -- n )
   CHECKER-PACKAGE-ACTIVE? 0= IF CHECKER-PACKAGE-PUBLIC EXIT THEN
   CHECKER-PACKAGE-MODE @ ;

: TDECL-FAMILY ( n n -- n ) {: ar:n kind:n :}   \ register the family row
   TDN-A @ TDN-U @ TDECL-TOK!
   s" duplicate family" TDECL-WHY!
   TDECL-PKG$ TDECL-VIS TDN-A @ TDN-U @ ar kind TFAM-DECL ;

\ --- body cursor over the buffered declaration tokens (checker sig lexer).
: TDECL-CURSOR! ( ptr u8 n -- ) {: a:ptr u:n :}
   a SB!  u SL !  0 SI !
   PKRESET ;
: TDECL-NEXT ( -- ptr u8 n ) NEXT-SIG-TOK ;

\ --- variant payload elements -> schema nodes (docs §8). v1: positional letter
\ params within arity, n/f/r + concrete CT cell types, and `ptr T`.
variable TDECL-FAM-ARITY

: TDECL-LETTER ( ptr u8 n -- n ) {: a:ptr u:n :}
   a c@ {: c:n :}
   c LOWER? 0= IF a u s" unknown payload type" E-TDECL-PAYLOAD TDECL-THROW THEN
   c 97 - TDECL-FAM-ARITY @ < IF c 97 - SCHEMA-PARAM EXIT THEN
   c 110 = IF CC-N SCHEMA-CON EXIT THEN
   c 102 = IF CC-BOOL SCHEMA-CON EXIT THEN
   c 114 = IF CC-R SCHEMA-CON EXIT THEN
   a u s" unknown payload type" E-TDECL-PAYLOAD TDECL-THROW ;

: TDECL-PAY-ELEM ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= IF a u s" missing ;VARIANT" E-TDECL-SYNTAX TDECL-THROW THEN
   a u DELIM? IF a u s" bad payload token" E-TDECL-SYNTAX TDECL-THROW THEN
   a u TDECL-KEYWORD? IF a u s" bad payload token" E-TDECL-SYNTAX TDECL-THROW THEN
   a u s" ptr" CORE-STR= IF TDECL-NEXT RECURSE SCHEMA-PTR EXIT THEN
   u 1 = IF a u TDECL-LETTER EXIT THEN
   a u CON-OF dup 0 <> IF SCHEMA-CON EXIT THEN drop
   a u s" unknown payload type" E-TDECL-PAYLOAD TDECL-THROW ;

\ --- variants: name gate, payload run in the schema-root pool, SUMV row.
variable TDV-TAG   variable TDV-N    variable TDV-MAX
variable TDV-SS    variable TDV-PC
variable TDV-NA    variable TDV-NU

: TDECL-VARIANT-NAME ( -- )
   TDECL-NEXT {: a:ptr u:n :}
   a u TDECL-REQUIRE-VARIANT-NAME
   a TDV-NA !  u TDV-NU ! ;

: TDECL-VARIANT-CLOSE ( n -- ) {: fam:n :}
   TDV-NA @ TDV-NU @ TDECL-TOK!
   s" duplicate variant" TDECL-WHY!
   fam TDV-NA @ TDV-NU @ TDV-TAG @ TDV-SS @ TDV-PC @ TDV-PC @ SUMV-ADD drop
   TDV-PC @ TDV-MAX @ max TDV-MAX !
   TDV-TAG @ 1 + TDV-TAG !
   TDV-N @ 1 + TDV-N ! ;

: TDECL-VARIANT ( n -- ) {: fam:n :}
   TDECL-VARIANT-NAME
   SCHEMA-ROOT-N@ TDV-SS !
   0 TDV-PC !
   BEGIN
      TDECL-NEXT
      2dup s" ;variant" CORE-STR=CI IF 2drop fam TDECL-VARIANT-CLOSE EXIT THEN
      TDECL-PAY-ELEM SCHEMA-ROOT+ drop
      TDV-PC @ 1 + TDV-PC !
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

\ --- registration entry points (verify-source and the definers below).
: CHECKER-DEFSUM-BODY ( -- )
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-CURSOR!
   TDECL-NEXT TDECL-ARITY {: ar:n :}
   ar TDECL-FAM-ARITY !
   ar TK-SUM TDECL-FAMILY {: fam:n :}
   SUMV-N @ {: vstart:n :}
   0 TDV-TAG !  0 TDV-N !  0 TDV-MAX !
   fam TDECL-SUM-VARIANTS
   TDV-N @ 0= IF TDN-A @ TDN-U @ s" empty sum" E-TDECL-SYNTAX TDECL-THROW THEN
   fam vstart TDV-N @ TFAM-VAR-RANGE!
   fam TDV-MAX @ TFAM-SLOTS! ;

: CHECKER-DEFSUM ( ptr u8 n ptr u8 n -- )   \ name, buffered body tokens
   {: na:ptr nu:n ba:ptr bu:n :}
   s" sumtype" na nu ba bu TDECL-CTX!
   ['] CHECKER-DEFSUM-BODY TDECL-RUN ;

: CHECKER-DEFFAMILY-BODY ( -- )
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-ARITY
   TK-CELL TDECL-FAMILY drop ;

: CHECKER-DEFFAMILY ( ptr u8 n ptr u8 n -- )   \ name, arity token
   {: na:ptr nu:n aa:ptr au:n :}
   s" typefamily" na nu aa au TDECL-CTX!
   ['] CHECKER-DEFFAMILY-BODY TDECL-RUN ;

\ --- public defining words. TYPEFAMILY consumes name + arity; SUMTYPE buffers
\ the block up to ;SUMTYPE (VALUE-RECORD's shape), then registers it whole.
: TYPEFAMILY ( -- )
   parse-name {: na:ptr nu:n :}
   parse-name {: aa:ptr au:n :}
   na nu aa au CHECKER-DEFFAMILY ;

create TDECL-BUF TDECL-CAP allot
variable TDECL-U
variable TDECL-I

: TDECL-CLEAR ( -- ) 0 TDECL-U ! ;
: TDECL-C, ( n -- )
   TDECL-U @ 1 + TDECL-CAP > IF s" sumtype: declaration too long" 70 die THEN
   TDECL-BUF TDECL-U @ + c!
   TDECL-U @ 1 + TDECL-U ! ;
: TDECL-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 TDECL-I !
   BEGIN TDECL-I @ u < WHILE
      a TDECL-I @ + c@ TDECL-C,
      TDECL-I @ 1 + TDECL-I !
   REPEAT ;
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

: TDECL-NOEND-BODY ( -- )
   TDN-A @ TDN-U @ s" missing ;SUMTYPE" E-TDECL-SYNTAX TDECL-THROW ;

: SUMTYPE ( -- )
   parse-name {: na:ptr nu:n :}
   TDECL-CLEAR
   SUMTYPE-COLLECT 0= IF
      s" sumtype" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
      ['] TDECL-NOEND-BODY TDECL-RUN EXIT
   THEN
   na nu TDECL-BUF TDECL-U @ CHECKER-DEFSUM ;
