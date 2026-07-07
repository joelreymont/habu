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
variable TDECL-FAM-REG   \ family id registered by the LAST successful sum (-1 = none)

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

\ TDECL-RUN ( [ -- ] -- ) : run one declaration body quotation transactionally. On
\ any throw the registries roll back and the failure is reported; a multi-error
\ load counts the reject and continues, otherwise the named code propagates.
: TDECL-RUN ( [ -- ] -- )
   TDECL-MARK
   -1 TDECL-FAM-REG !               \ set by a successful sum registration only
   catch {: rc:n :}
   rc 0= IF EXIT THEN
   CTOR-PEND-CLEAR                  \ no armed constructor window survives a reject
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

\ --- constructor metadata (item 8): a PUBLIC family derives its reserved
\ constructor package (Package Shape) once and records it in every variant's
\ SV.CTOR-PKG slot, keyed by family id so same-tail families in different
\ packages get disjoint constructor namespaces. Private families export nothing,
\ so the slot stays empty and construction waits on item 9's `construct` form.
\ The derived name is interned AFTER the escaped/hashed spelling is built, so the
\ transient family package/tail pointers are consumed before any pool grow.
: TDECL-CTOR-PUBLISH ( n n n -- ) {: fam:n vstart:n count:n :}
   fam TFAM-PUBLIC? 0= IF EXIT THEN
   fam TFAM-PKG$ fam TFAM-NAME$ TF-CTOR-PKG$ {: ca:ptr cu:n :}
   ca cu TF-INTERN {: coff:n :}
   count 0 DO  vstart i +  coff cu SUMV-CTOR-PKG!  LOOP ;

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
   fam TDV-MAX @ TFAM-SLOTS!
   fam vstart TDV-N @ TDECL-CTOR-PUBLISH
   fam TDECL-FAM-REG ! ;

: CHECKER-DEFSUM ( ptr u8 n ptr u8 n -- )   \ name, buffered body tokens
   {: na:ptr nu:n ba:ptr bu:n :}
   s" sumtype" na nu ba bu TDECL-CTX!
   [: CHECKER-DEFSUM-BODY ;] TDECL-RUN ;

: CHECKER-DEFFAMILY-BODY ( -- )
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-ARITY
   TK-CELL TDECL-FAMILY drop ;

: CHECKER-DEFFAMILY ( ptr u8 n ptr u8 n -- )   \ name, arity token
   {: na:ptr nu:n aa:ptr au:n :}
   s" typefamily" na nu aa au TDECL-CTX!
   [: CHECKER-DEFFAMILY-BODY ;] TDECL-RUN ;

\ --- runtime constructor generation (item 8, docs §12). Only the engine's
\ SUMTYPE word below generates dictionary words; the direct CHECKER-DEFSUM
\ entry stays metadata-only, so preverify/all-errors dispatch never mutates
\ the tool dictionary and checker rollback stays complete. Each PUBLIC variant
\ becomes one checked qualified definition
\    : <CTOR-PKG>:<VARIANT> ( payload.. -- family<a,..> ) 0 .. 0 <tag> ;
\ rendered from interned SUMV metadata only and evaluated with the checker's
\ single-shot pending-constructor window armed (checker.f CTOR-PEND!): the
\ body certifies against the SUMV-derived raw row and the declared
\ hidden-field sig publishes through the normal certify path. The generated
\ text never contains TRUST, TRUSTED:, or set-check.
\ the one evaluate crossing rides the audited INCLUDE-EVALUATE boundary
\ (include.f), reached through this friend xt installed by type-family-sha.f
\ once both sides exist (the TF-SHA16-XT pattern) — the generator itself
\ adds no unchecked code.
variable TDECL-EVAL-XT   0 TDECL-EVAL-XT !

$200 constant TDGEN-CAP
create TDGEN-BUF TDGEN-CAP allot
variable TDGEN-U
variable TDGEN-NA   variable TDGEN-NU     \ word-name span inside TDGEN-BUF
variable TDGEN-I    variable TDGEN-J      \ render loop indexes
variable TDGEN-K    variable TDGEN-M    variable TDGEN-B

: TDGEN-CLEAR ( -- ) 0 TDGEN-U ! ;
: TDGEN-C, ( n -- )
   TDGEN-U @ TDGEN-CAP >= IF s" sumtype: generated constructor too long" 76 die THEN
   TDGEN-BUF TDGEN-U @ + c!
   TDGEN-U @ 1 + TDGEN-U ! ;
: TDGEN-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 TDGEN-B !
   BEGIN TDGEN-B @ u < WHILE
      a TDGEN-B @ + c@ TDGEN-C,
      TDGEN-B @ 1 + TDGEN-B !
   REPEAT ;
: TDGEN-UPPER ( ptr u8 n -- ) {: a:ptr u:n :}   \ variant tail -> word tail
   0 TDGEN-B !
   BEGIN TDGEN-B @ u < WHILE
      a TDGEN-B @ + c@ TF-UPPER-C TDGEN-C,
      TDGEN-B @ 1 + TDGEN-B !
   REPEAT ;
: TDGEN-DEC ( n -- ) {: v:n :}           \ non-negative decimal (tag literal)
   v 10 >= IF v 10 / RECURSE THEN
   v 10 mod 48 + TDGEN-C, ;
: TDGEN-LETTER ( n -- ) 97 + TDGEN-C, ;  \ positional param -> a..z

: TDGEN-SCH ( n -- ) {: node:n :}        \ one payload schema node -> sig text
   node SCHEMA-PARAM? IF node SCHEMA-A@ TDGEN-LETTER EXIT THEN
   node SCHEMA-CON?   IF node SCHEMA-A@ CT-NAME$ TDGEN-APP EXIT THEN
   node SCHEMA-PTR?   IF s" ptr " TDGEN-APP node SCHEMA-A@ RECURSE EXIT THEN
   s" sumtype: unsupported constructor payload schema" 76 die ;

: TDGEN-PAYLOAD ( n -- ) {: vid:n :}     \ declared inputs, one per schema root
   vid SUMV-SCH-COUNT@ {: k:n :}
   0 TDGEN-J !
   BEGIN TDGEN-J @ k < WHILE
      vid SUMV-SCH-START@ TDGEN-J @ + SCHEMA-ROOT@ TDGEN-SCH
      32 TDGEN-C,
      TDGEN-J @ 1 + TDGEN-J !
   REPEAT ;

: TDGEN-OUT-TYPE ( n -- ) {: fam:n :}    \ family<a,b,..> (bare tail at arity 0)
   fam TFAM-NAME$ TDGEN-APP
   fam TFAM-ARITY@ {: ar:n :}
   ar 0= IF EXIT THEN
   60 TDGEN-C,
   0 TDGEN-I !
   BEGIN TDGEN-I @ ar < WHILE
      TDGEN-I @ 0 > IF 44 TDGEN-C, THEN
      TDGEN-I @ TDGEN-LETTER
      TDGEN-I @ 1 + TDGEN-I !
   REPEAT
   62 TDGEN-C, ;

: TDGEN-NAME ( n -- ) {: vid:n :}        \ ": PKG:VARIANT " with the span recorded
   s" : " TDGEN-APP
   TDGEN-U @ {: n0:n :}
   vid SUMV-CTOR-PKG$ TDGEN-APP
   58 TDGEN-C,
   vid SUMV-NAME$ TDGEN-UPPER
   TDGEN-BUF n0 + TDGEN-NA !
   TDGEN-U @ n0 - TDGEN-NU !
   32 TDGEN-C, ;

: TDGEN-BODY ( n n -- ) {: fam:n vid:n :}   \ "0 .. 0 tag ;" zero pads + tag
   fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - {: pads:n :}
   0 TDGEN-K !
   BEGIN TDGEN-K @ pads < WHILE
      48 TDGEN-C,  32 TDGEN-C,
      TDGEN-K @ 1 + TDGEN-K !
   REPEAT
   vid SUMV-TAG@ TDGEN-DEC
   s"  ;" TDGEN-APP ;

: TDECL-CTOR-WORD ( n n -- ) {: fam:n vid:n :}
   TDGEN-CLEAR
   vid TDGEN-NAME
   s" ( " TDGEN-APP
   vid TDGEN-PAYLOAD
   s" -- " TDGEN-APP
   fam TDGEN-OUT-TYPE
   s"  ) " TDGEN-APP
   fam vid TDGEN-BODY
   TDGEN-NA @ TDGEN-NU @  fam TFAM-SLOTS@ vid SUMV-PAYCELLS@ - 1 +  CTOR-PEND!
   TDECL-EVAL-XT @ 0= IF s" sumtype: constructor eval hook not installed" 76 die THEN
   TDGEN-BUF TDGEN-U @ TDECL-EVAL-XT @ execute
   CTOR-PEND-CLEAR
   vid TDGEN-NA @ TDGEN-NU @ CHECKER-RECORD-SYM SUMV-CTOR-SYM! ;

: TDECL-CTOR-WORDS ( -- )                \ engine-load generation for the last sum
   TDECL-FAM-REG @ {: fam:n :}
   fam 0 < IF EXIT THEN
   fam TFAM-PUBLIC? 0= IF EXIT THEN
   \ parametric (arity > 0) families publish too (item 11 slice 1): a
   \ constructor's parametric result stays one conservative logical cell while
   \ its args are unresolved, expands to hidden fields where instantiation
   \ proves the args non-linear (checker.f LOGHID coercion), and genuinely
   \ linear instantiations stay rejected at the sig/param-arg layers until
   \ whole-bundle linear counting lands.
   fam TFAM-VAR-START@ {: vstart:n :}
   fam TFAM-VAR-COUNT@ {: k:n :}
   0 TDGEN-M !
   BEGIN TDGEN-M @ k < WHILE
      fam vstart TDGEN-M @ + TDECL-CTOR-WORD
      TDGEN-M @ 1 + TDGEN-M !
   REPEAT ;

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
      [: TDECL-NOEND-BODY ;] TDECL-RUN EXIT
   THEN
   na nu TDECL-BUF TDECL-U @ CHECKER-DEFSUM
   TDECL-CTOR-WORDS ;
