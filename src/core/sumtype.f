\ sumtype.f — TYPEFAMILY/SUMTYPE/ENUM/PRODUCT declaration grammar
\ (docs/type-families.md §9, PLAN items 6 + 14 + 15). The public ADT authoring
\ surface: `TYPEFAMILY name
\ arity` registers a TK-CELL family; `ENUM name v0 v1 .. ;ENUM` registers a
\ TK-ENUM family (a zero-payload sum, docs §9.3); `PRODUCT name arity FIELD f t ..
\ ;PRODUCT` registers a TK-PRODUCT family (single-shape record, PF-* field rows,
\ no tag, docs §9.4); `SUMTYPE name arity VARIANT v
\ pay... ;VARIANT ... ;SUMTYPE` registers a TK-SUM family, one SUMV row per variant (tag =
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
7116 constant E-TDECL-POLICY    \ unknown or not-yet-supported layout policy (item 16; 7111-7115 = checker.f E-CTOR/E-EXPORT)
7117 constant E-TDECL-RECURSIVE \ direct self-family payload under a non-boxed policy (item 16 boxed sub-slice 1, docs §24)
7118 constant E-TDECL-CAP       \ declaration body exceeds TDECL-CAP (item 13 C2)
7119 constant E-TDECL-DERIVE    \ unknown, deferred, or kind-gated DERIVE clause (derive S1)

26 constant TDECL-ARITY-CAP     \ positional params are letters a..z (docs §9.2)
$1000 constant TDECL-CAP        \ buffered declaration body bytes

\ --- declaration context (set before TDECL-RUN, read by bodies + diagnostics).
variable TDK-A   variable TDK-U      \ decl kind token ("typefamily"/"sumtype")
variable TDN-A   variable TDN-U      \ family name token
variable TDB-A   variable TDB-U      \ body (SUMTYPE token buffer / arity token)
variable TDT-A   variable TDT-U      \ offending token (diagnostics)
variable TDW-A   variable TDW-U      \ short reason (diagnostics)
variable TDECL-OVERSIZE             \ a collection buffer capped an over-cap body (item 13 C2)

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

\ Reject an over-cap declaration body through the declaration packet (§24 C2):
\ either a collection buffer capped an over-long body (TDECL-OVERSIZE, native
\ TDECL-C,) or the collected body already exceeds TDECL-CAP (verify-source /
\ check-core, whose larger buffers hand the full body across). Runs at the head
\ of each body under TDECL-RUN, name already in the TDN context.
: TDECL-REQUIRE-FIT ( -- )
   TDECL-OVERSIZE @ {: over:n :}
   0 TDECL-OVERSIZE !
   over 0 <>  TDB-U @ TDECL-CAP >  or IF
      TDN-A @ TDN-U @ s" declaration too long" E-TDECL-CAP TDECL-THROW
   THEN ;

\ Shared unterminated-sum reject: report the declaration-shaped E-BAD-DECLARATION
\ packet (name + partial body) through TDECL-RUN so every path -- native SUMTYPE,
\ the verify-source recording pass, and the check.f collector -- agrees (§24).
\ Defined here (before CHECKER-DEFSUM) so every context that sees CHECKER-DEFSUM
\ also sees CHECKER-DEFSUM-NOEND.
: TDECL-NOEND-BODY ( -- )
   TDN-A @ TDN-U @ s" missing ;SUMTYPE" E-TDECL-SYNTAX TDECL-THROW ;
: CHECKER-DEFSUM-NOEND ( ptr u8 n ptr u8 n -- )   \ name, partial body -> declaration packet
   {: na:ptr nu:n ba:ptr bu:n :}
   s" sumtype" na nu ba bu TDECL-CTX!
   [: TDECL-NOEND-BODY ;] TDECL-RUN ;

\ --- name gate: reserved signature/type tokens, control words, and grammar
\ keywords may not name a family or variant (docs §1, PLAN item 6).
: TDECL-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" variant" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;variant" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" ;sumtype" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" enum" CORE-STR=CI IF RES-TRUE EXIT THEN       \ item 14 enum block definer
   a u s" ;enum" CORE-STR=CI IF RES-TRUE EXIT THEN      \ item 14 enum block close (;FOO)
   a u s" product" CORE-STR=CI IF RES-TRUE EXIT THEN    \ item 15 product block definer
   a u s" ;product" CORE-STR=CI IF RES-TRUE EXIT THEN   \ item 15 product block close (;FOO)
   a u s" field" CORE-STR=CI IF RES-TRUE EXIT THEN      \ item 15 product field keyword
   a u s" policy" CORE-STR=CI IF RES-TRUE EXIT THEN     \ item 16 layout-policy header keyword
   a u s" derive" CORE-STR=CI IF RES-TRUE EXIT THEN     \ derive S1 header keyword
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
   a u s" recurse" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" construct" CORE-STR=CI IF RES-TRUE EXIT THEN   \ item 9 reserved token protocol
   a u s" match" CORE-STR=CI IF RES-TRUE EXIT THEN       \ item 9 MATCH control form
   a u s" ;match" CORE-STR=CI ;                          \ item 9 MATCH block close

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

variable TDECL-NI
: TDECL-VAR-SCOPE? ( n -- bool )
   SUMV-FAM@ TFAM-PKG$ {: pa:ptr pu:n :}
   pu 0= IF RES-TRUE EXIT THEN
   CHECKER-PACKAGE-ACTIVE? 0= IF RES-FALSE EXIT THEN
   pa pu CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ CORE-STR= ;

: TDECL-VAR-TAKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 TDECL-NI !
   BEGIN TDECL-NI @ SUMV-N @ < WHILE
      TDECL-NI @ SUMV-NAME$ a u CORE-STR= IF
         TDECL-NI @ TDECL-VAR-SCOPE? IF RES-TRUE EXIT THEN
      THEN
      TDECL-NI @ 1 + TDECL-NI !
   REPEAT RES-FALSE ;

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
   a u TDECL-VAR-TAKEN? IF
      a u s" collides with a sum variant" E-TDECL-NAME TDECL-THROW
   THEN
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
\ the active visibility (TFAM-ACTIVE-PKG$, type-family.f); top level is the
\ global scope, public.
: TDECL-VIS ( -- n )
   CHECKER-PACKAGE-ACTIVE? 0= IF CHECKER-PACKAGE-PUBLIC EXIT THEN
   CHECKER-PACKAGE-MODE @ ;

: TDECL-FAMILY ( n n -- n ) {: ar:n kind:n :}   \ register the family row
   TDN-A @ TDN-U @ TDECL-TOK!
   s" duplicate family" TDECL-WHY!
   TFAM-ACTIVE-PKG$ TDECL-VIS TDN-A @ TDN-U @ ar kind TFAM-DECL ;

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

\ item 16 boxed sub-slice 1: a DIRECT self-family reference is a payload token
\ whose tail is the family being declared (TDN-A/TDN-U). An inline or ptr-wrapped
\ self-reference makes the family recursive, which only the boxed policy can lay
\ out (its pointer indirection breaks the width cycle); under any non-boxed policy
\ it is unrepresentable. Since packed/niche/boxed all still reject at the POLICY
\ clause, every family reaching payload parsing is stack-cell-tag, so a self-ref
\ always rejects here with the docs §24 recursive-sum diagnostic (was the generic
\ E-TDECL-PAYLOAD "unknown payload type"). The boxed accept sub-slice will route a
\ boxed family's self-ref to a pointer layout before this reject. Mutual recursion
\ (A -> B -> A) needs a schema cycle walk and is a later sub-slice; this is the
\ direct case only.
: TDECL-SELF-REF? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TDN-A @ TDN-U @ CORE-STR= ;
: TDECL-PAY-ELEM ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= IF a u s" missing ;VARIANT" E-TDECL-SYNTAX TDECL-THROW THEN
   a u DELIM? IF a u s" bad payload token" E-TDECL-SYNTAX TDECL-THROW THEN
   a u TDECL-KEYWORD? IF a u s" bad payload token" E-TDECL-SYNTAX TDECL-THROW THEN
   a u s" ptr" CORE-STR= IF TDECL-NEXT RECURSE SCHEMA-PTR EXIT THEN
   a u TDECL-SELF-REF? IF
      a u s" invalid layout policy for recursive sum" E-TDECL-RECURSIVE TDECL-THROW
   THEN
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

\ --- optional layout-policy header clause (item 16, docs §22): `POLICY <name>`
\ after the arity (sum/product) or the name (enum), before the first VARIANT /
\ FIELD. A missing clause leaves the TFAM-DECL default (TL-STACK-CELL-TAG, the
\ universal M-payload + tag stack representation, docs §22.1). `stack-cell-tag`
\ is the only policy v1 lowers; `packed-tag`/`niche-null`/`boxed` are recognised
\ but reject as not-yet-supported until their per-policy lowering ships (PLAN
\ item 16 risk: physical-layout policies must not be exposed before codegen
\ supports them), and each becomes its own follow-on slice. Any other token
\ (including the v1 non-goal `custom`, which the LAY-* registry tolerates but the
\ grammar must not) is an unknown policy. A DIRECT self-family payload reference
\ is recursive and rejects at TDECL-PAY-ELEM with the docs §24 recursive-sum
\ diagnostic (E-TDECL-RECURSIVE, boxed sub-slice 1 below); boxed — the only layout
\ that admits recursion — will later route its self-references to a pointer layout.
: TDECL-POLICY-DEFERRED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" packed-tag" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" niche-null" CORE-STR=CI IF RES-TRUE EXIT THEN
   a u s" boxed" CORE-STR=CI ;
: TDECL-POLICY-SET ( ptr u8 n n -- ) {: a:ptr u:n fam:n :}   \ map a policy-name token onto the fresh family
   a u s" stack-cell-tag" CORE-STR=CI IF fam TL-STACK-CELL-TAG TFAM-LAYOUT! EXIT THEN
   a u TDECL-POLICY-DEFERRED? IF
      a u s" layout policy not yet supported" E-TDECL-POLICY TDECL-THROW
   THEN
   a u s" unknown layout policy" E-TDECL-POLICY TDECL-THROW ;
: TDECL-POLICY ( n -- ) {: fam:n :}   \ consume an optional POLICY clause off the body cursor
   TDECL-NEXT {: a:ptr u:n :}
   a u s" policy" CORE-STR=CI 0= IF a u PK! EXIT THEN     \ no clause: push the token back for the body loop
   TDECL-NEXT {: pa:ptr pu:n :}
   pu 0= IF TDN-A @ TDN-U @ s" missing layout policy name" E-TDECL-POLICY TDECL-THROW THEN
   pa pu fam TDECL-POLICY-SET ;

\ --- optional derived-word header clause (derive S1+S2, dot habu-checker-
\ capability-derive): `DERIVE <feature>` after the POLICY position, before the
\ variants/fields. Accepts exactly `eq` on a PUBLIC arity-0 ENUM, SUMTYPE, or
\ PRODUCT; `order`/`hash` are recognised but deferred (their slices land
\ later), anything else is unknown. One feature per clause keeps the grammar
\ unambiguous against bare variant names; a later slice may repeat the clause
\ for more features. Payload roles are validated per variant/field after the
\ block parses (TDECL-DERIVE-REQUIRE below): integer scalars compare with `=`;
\ pointer payloads have NO derived equality (identity-eq needs a checked
\ pointer-equality surface that does not exist — fail closed, documented);
\ non-integer/linear scalars reject (comparing a linear value consumes it;
\ deferred to TFAM-11); a product's enum-typed field requires that family to
\ also derive eq (its PKG:TAG is the field comparator).
: TDECL-DERIVE-SET ( ptr u8 n n -- ) {: a:ptr u:n fam:n :}
   a u s" eq" CORE-STR=CI IF
      fam TFAM-PUBLIC? 0= IF
         a u s" derive requires a public family" E-TDECL-DERIVE TDECL-THROW THEN
      fam TFAM-ARITY@ 0 <> IF
         a u s" derive requires a concrete (arity 0) family" E-TDECL-DERIVE TDECL-THROW THEN
      fam TFAM-DERIVE-EQ! EXIT THEN
   a u s" order" CORE-STR=CI  a u s" hash" CORE-STR=CI  or IF
      a u s" derive feature not yet supported" E-TDECL-DERIVE TDECL-THROW THEN
   a u s" unknown derive feature" E-TDECL-DERIVE TDECL-THROW ;
: TDECL-DERIVE ( n -- ) {: fam:n :}   \ consume an optional DERIVE clause off the body cursor
   TDECL-NEXT {: a:ptr u:n :}
   a u s" derive" CORE-STR=CI 0= IF a u PK! EXIT THEN
   TDECL-NEXT {: fa:ptr fu:n :}
   fu 0= IF TDN-A @ TDN-U @ s" missing derive feature" E-TDECL-DERIVE TDECL-THROW THEN
   fa fu fam TDECL-DERIVE-SET ;

\ a DERIVE-marked family must not declare a variant spelled like a generated
\ derived tail: the ctor package would hold two words with one name.
variable TDD-I   variable TDD-J   variable TDD-K
: TDECL-DERIVE-COLLIDE ( n n n -- ) {: fam:n vstart:n count:n :}
   fam TFAM-DERIVE-EQ? 0= IF EXIT THEN
   0 TDD-I !
   BEGIN TDD-I @ count < WHILE
      vstart TDD-I @ + SUMV-NAME$ TFAM-DERIVED-TAIL? IF
         vstart TDD-I @ + SUMV-NAME$
         s" variant name collides with a derived word" E-TDECL-NAME TDECL-THROW THEN
      TDD-I @ 1 + TDD-I !
   REPEAT ;

\ payload-role gate (derive S2): every payload/field schema node must be
\ derivably comparable, else the DECLARATION rejects naming the exact role.
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
      node SCHEMA-A@ TFAM-DERIVE-EQ? 0= IF
         node SCHEMA-A@ TFAM-NAME$
         s" field family must also derive eq" E-TDECL-DERIVE TDECL-THROW THEN
      EXIT THEN
   TDN-A @ TDN-U @
   s" payload role has no derived equality" E-TDECL-DERIVE TDECL-THROW ;
: TDECL-DERIVE-REQUIRE ( n n n -- ) {: fam:n vstart:n count:n :}
   fam TFAM-DERIVE-EQ? 0= IF EXIT THEN
   0 TDD-I !
   BEGIN TDD-I @ count < WHILE
      0 TDD-J !
      BEGIN TDD-J @ vstart TDD-I @ + SUMV-SCH-COUNT@ < WHILE
         vstart TDD-I @ + SUMV-SCH-START@ TDD-J @ + SCHEMA-ROOT@ TDECL-DERIVE-NODE-OK
         TDD-J @ 1 + TDD-J !
      REPEAT
      TDD-I @ 1 + TDD-I !
   REPEAT ;

\ --- registration entry points (verify-source and the definers below).
: CHECKER-DEFSUM-BODY ( -- )
   TDECL-REQUIRE-FIT
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-CURSOR!
   TDECL-NEXT TDECL-ARITY {: ar:n :}
   ar TDECL-FAM-ARITY !
   ar TK-SUM TDECL-FAMILY {: fam:n :}
   fam TDECL-POLICY                       \ optional POLICY clause before the variants
   fam TDECL-DERIVE                       \ optional DERIVE clause (derive S2)
   SUMV-N @ {: vstart:n :}
   0 TDV-TAG !  0 TDV-N !  0 TDV-MAX !
   fam TDECL-SUM-VARIANTS
   TDV-N @ 0= IF TDN-A @ TDN-U @ s" empty sum" E-TDECL-SYNTAX TDECL-THROW THEN
   fam vstart TDV-N @ TDECL-DERIVE-COLLIDE
   fam vstart TDV-N @ TDECL-DERIVE-REQUIRE
   fam vstart TDV-N @ TFAM-VAR-RANGE!
   fam TDV-MAX @ TFAM-SLOTS!
   fam vstart TDV-N @ TDECL-CTOR-PUBLISH
   fam TDECL-FAM-REG ! ;

: CHECKER-DEFSUM ( ptr u8 n ptr u8 n -- )   \ name, buffered body tokens
   {: na:ptr nu:n ba:ptr bu:n :}
   s" sumtype" na nu ba bu TDECL-CTX!
   [: CHECKER-DEFSUM-BODY ;] TDECL-RUN ;

\ --- enum families (item 14, docs §9.3): an `ENUM name v0 v1 .. ;ENUM` block is
\ a zero-payload sum registered as TK-ENUM (arity 0, slots 0). One bare variant
\ name per token becomes a zero-payload SUMV row (tag = declaration order); the
\ shared close/ctor/rollback path is reused so construct + exhaustive MATCH lower
\ enum-kind families exactly as they already do for a 0-arity sum.
: TDECL-ENUM-VARIANT ( ptr u8 n n -- )   \ variant tail (a u) + family id
   {: a:ptr u:n fam:n :}
   a u TDECL-REQUIRE-VARIANT-NAME
   a TDV-NA !  u TDV-NU !
   SCHEMA-ROOT-N@ TDV-SS !
   0 TDV-PC !                            \ enum variants carry no payload
   fam TDECL-VARIANT-CLOSE ;

: TDECL-ENUM-VARIANTS ( n -- ) {: fam:n :}
   BEGIN
      TDECL-NEXT
      dup 0= IF 2drop EXIT THEN
      fam TDECL-ENUM-VARIANT
   AGAIN ;

: CHECKER-DEFENUM-BODY ( -- )
   TDECL-REQUIRE-FIT
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-CURSOR!
   0 TDECL-FAM-ARITY !                   \ enums are non-parametric
   0 TK-ENUM TDECL-FAMILY {: fam:n :}
   fam TDECL-POLICY                       \ optional POLICY clause before the variants
   fam TDECL-DERIVE                       \ optional DERIVE clause (derive S1+S2)
   SUMV-N @ {: vstart:n :}
   0 TDV-TAG !  0 TDV-N !  0 TDV-MAX !
   fam TDECL-ENUM-VARIANTS
   TDV-N @ 0= IF TDN-A @ TDN-U @ s" empty enum" E-TDECL-SYNTAX TDECL-THROW THEN
   fam vstart TDV-N @ TDECL-DERIVE-COLLIDE
   fam vstart TDV-N @ TFAM-VAR-RANGE!
   fam TDV-MAX @ TFAM-SLOTS!             \ TDV-MAX stays 0: enum = tag only
   fam vstart TDV-N @ TDECL-CTOR-PUBLISH
   fam TDECL-FAM-REG ! ;

: CHECKER-DEFENUM ( ptr u8 n ptr u8 n -- )   \ name, buffered variant tokens
   {: na:ptr nu:n ba:ptr bu:n :}
   s" enum" na nu ba bu TDECL-CTX!
   [: CHECKER-DEFENUM-BODY ;] TDECL-RUN ;

\ --- product families (item 15, docs §9.4): a `PRODUCT name arity FIELD f t ..
\ ;PRODUCT` block is a single-shape record registered as TK-PRODUCT (no tag, no
\ variants). Each `FIELD name type` becomes one PF-* row (fam, field tail, field
\ schema root, physical slot) plus one cell of width; TFAM-SLOTS = field count, so
\ TFAM-WIDTH(product) = field cells (no tag). Fields are the item-7/12 hidden
\ layout the generic LAYOUT-PUSH-FIELDS already expands off TFAM-WIDTH@*. Field
\ names are their own tail namespace: 1-char labels (x/y) are legal, so the family
\ RESERVED gate (which blocks a..z) does not apply; PF-ADD's TF-REQUIRE-CANON +
\ dup-reject enforce lowercase + no duplicate field within the product.
\ A product's generated surface is two words with FIXED generator-owned tails,
\ recorded as two SUMV rows so the whole item-8 publish/protection stack
\ (ctor-package derivation, closed-but-callable WID wall, CTOR-SYM records) is
\ shared verbatim with sums: `make` ( fields -- fam<..> ) and `unmake`
\ ( fam<..> -- fields ), both compiled with EMPTY bodies under the k=0
\ pending-constructor window — a product bundle IS its field cells in slot
\ order (no tag, docs §18), so construction and destructure are physical
\ no-ops and the declared sigs are checker-owned metadata truth. Parametric
\ products publish both words: MAKE's open result and UNMAKE's open input
\ expand/absorb at concrete sites through the LOGHID row coercion (U-ROW,
\ checker.f), and linear instantiations stay fail-closed at the sig/arg-bind
\ layers. The rows are registered here (preverify parity); dictionary words
\ are generated only by the engine PRODUCT definer below. MATCH and
\ `construct` stay kind-gated to sum/enum, so product rows are never matchable
\ variants and private products have no construction surface (fail-closed).
variable TDP-N   \ product field count (schema roots)
variable TDP-W   \ cumulative product cell width / next field's cell OFFSET

: TDECL-REQUIRE-FIELD-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF a u s" missing field name" E-TDECL-SYNTAX TDECL-THROW THEN
   a u DELIM? IF a u s" bad field name" E-TDECL-SYNTAX TDECL-THROW THEN
   a u TDECL-KEYWORD? IF a u s" reserved field name" E-TDECL-NAME TDECL-THROW THEN
   a u TF-CANON? 0= IF
      a u s" field name must be a lowercase tail" E-TFAM-CASE TDECL-THROW
   THEN ;

\ --- layout-kinded product fields S1 (dot habu-checker-capability-layout-4e7f1f03):
\ a PRODUCT field may be typed as an S1-tier layout family — sum/enum kind,
\ arity 0, registry width 1 (the enum tier; arity 0 also means no arg can ever
\ be linear). The field schema is a family application (SC-APP) holding the
\ RESOLVED family-id, so the field is born typed: MAKE consumes it as its
\ family, UNMAKE reproduces it, and a swapped same-width enum field is a
\ checker reject. Resolution is signature scope (TFAM-SIG-RESOLVE: own package
\ first, else the unique public; qualified PKG:tail accepted; ambiguity maps to
\ unresolved). Wider, parametric, product-kinded (incl. self-referential), and
\ linear layout fields fall through to the payload grammar's E-TDECL-PAYLOAD —
\ the S2 tier; SUM variant payloads keep the same reject until S3.
: TDECL-FIELD-FAM? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE 0= IF drop 0 RES-FALSE EXIT THEN
   {: id:n :}
   id TFAM-SUM? id TFAM-ENUM? or 0= IF 0 RES-FALSE EXIT THEN
   id TFAM-ARITY@ 0 <> IF 0 RES-FALSE EXIT THEN
   id TFAM-WIDTH@ 1 <> IF 0 RES-FALSE EXIT THEN
   id RES-TRUE ;

: TDECL-FIELD-ELEM ( ptr u8 n -- n ) {: a:ptr u:n :}   \ field type -> schema node
   a u TDECL-FIELD-FAM? IF 0 0 SCHEMA-APP EXIT THEN drop
   a u TDECL-PAY-ELEM ;

: TDECL-SCH-WIDTH ( n -- n ) {: node:n :}   \ field schema node -> cell width
   node SCHEMA-APP? IF node SCHEMA-A@ TFAM-WIDTH@ EXIT THEN
   1 ;                                       \ letter/con/ptr fields are one cell

\ TDECL-PRODUCT-FIELD ( fam -- ) : the `field` keyword is already consumed; read
\ the field tail + one field-shaped type into a schema root, add the PF row.
\ PF.SLOT is the field's cumulative CELL OFFSET and the product's SLOTS/width
\ is the field-width sum — identity with the old field-index/field-count values
\ while every admitted field is one cell, and the correct shape for wider tiers.
: TDECL-PRODUCT-FIELD ( n -- ) {: fam:n :}
   TDECL-NEXT {: fna:ptr fnu:n :}
   fna fnu TDECL-REQUIRE-FIELD-NAME
   SCHEMA-ROOT-N@ {: ss:n :}
   TDECL-NEXT TDECL-FIELD-ELEM SCHEMA-ROOT+ drop    \ one field type (family/letter/con/ptr T)
   fna fnu TDECL-TOK!
   s" duplicate field" TDECL-WHY!
   fam fna fnu ss TDP-W @ PF-ADD drop               \ PF-ADD: canon + dup-reject
   ss SCHEMA-ROOT@ TDECL-SCH-WIDTH TDP-W @ + TDP-W !
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

: CHECKER-DEFPRODUCT-BODY ( -- )
   TDECL-REQUIRE-FIT
   TDN-A @ TDN-U @ TDECL-REQUIRE-FAMILY-NAME
   TDB-A @ TDB-U @ TDECL-CURSOR!
   TDECL-NEXT TDECL-ARITY {: ar:n :}
   ar TDECL-FAM-ARITY !
   ar TK-PRODUCT TDECL-FAMILY {: fam:n :}
   fam TDECL-POLICY                       \ optional POLICY clause before the fields
   fam TDECL-DERIVE                       \ optional DERIVE clause (derive S2)
   PF-N @ {: fstart:n :}
   SCHEMA-ROOT-N@ {: rstart:n :}
   0 TDP-N !   0 TDP-W !
   fam TDECL-PRODUCT-FIELDS
   TDP-N @ 0= IF TDN-A @ TDN-U @ s" empty product" E-TDECL-SYNTAX TDECL-THROW THEN
   fam fstart TDP-N @ TFAM-FLD-RANGE!
   fam TDP-W @ TFAM-SLOTS!               \ product width = field cell width sum (no tag)
   fam rstart TDECL-PRODUCT-ROWS
   fam  fam TFAM-VAR-START@  1 TDECL-DERIVE-REQUIRE   \ field roles: the make row's schema
   fam TDECL-FAM-REG ! ;

: CHECKER-DEFPRODUCT ( ptr u8 n ptr u8 n -- )   \ name, buffered body tokens
   {: na:ptr nu:n ba:ptr bu:n :}
   s" product" na nu ba bu TDECL-CTX!
   [: CHECKER-DEFPRODUCT-BODY ;] TDECL-RUN ;

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
variable TDECL-PROT-WID-XT   0 TDECL-PROT-WID-XT !

$1000 constant TDGEN-CAP   \ derived-eq diagonal text is O(V^2); the C, guard still dies at the cap
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

\ a family-application field (SC-APP, arity 0 in the S1 tier) renders as its
\ signature reference: PKG:tail when the family lives in a package (the sig
\ parser resolves qualified refs; the generated eval runs while the declaring
\ package is still open, so own-private fields resolve too), bare tail at top
\ level.
: TDGEN-FAM-REF ( n -- ) {: fam:n :}
   fam TFAM-PKG$ {: pa:ptr pu:n :}
   pu 0 > IF pa pu TDGEN-APP 58 TDGEN-C, THEN
   fam TFAM-NAME$ TDGEN-APP ;

: TDGEN-SCH ( n -- ) {: node:n :}        \ one payload schema node -> sig text
   node SCHEMA-PARAM? IF node SCHEMA-A@ TDGEN-LETTER EXIT THEN
   node SCHEMA-CON?   IF node SCHEMA-A@ CT-NAME$ TDGEN-APP EXIT THEN
   node SCHEMA-PTR?   IF s" ptr " TDGEN-APP node SCHEMA-A@ RECURSE EXIT THEN
   node SCHEMA-APP?   IF node SCHEMA-A@ TDGEN-FAM-REF EXIT THEN
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

: TDECL-GEN-EVAL ( -- )   \ the one audited eval crossing for generated words
   TDECL-EVAL-XT @ 0= IF s" sumtype: constructor eval hook not installed" 76 die THEN
   TDGEN-BUF TDGEN-U @ TDECL-EVAL-XT @ execute ;

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
   TDECL-GEN-EVAL
   CTOR-PEND-CLEAR
   vid TDGEN-NA @ TDGEN-NU @ CHECKER-RECORD-SYM SUMV-CTOR-SYM! ;

: TDECL-CTOR-PROT-WID ( n -- ) {: vid:n :}
   TDECL-PROT-WID-XT @ 0= IF s" sumtype: protected-wid hook not installed" 76 die THEN
   TDGEN-CLEAR
   vid TDGEN-NAME
   TDGEN-NA @ TDGEN-NU @ TDECL-PROT-WID-XT @ execute ;

\ product generated words (item 15): `: PKG:MAKE ( fields -- fam<..> ) ;` and
\ `: PKG:UNMAKE ( fam<..> -- fields ) ;`. Both bodies are EMPTY and the pending
\ window is armed with k=0: a product bundle is its field cells in slot order
\ (no pads, no tag), so the body must contribute exactly nothing and the
\ declared sig is the checker-owned metadata truth rendered from the PF field
\ schemas. mk:n picks the direction (nonzero = make).
: TDECL-PROD-WORD ( n n n -- ) {: fam:n vid:n mk:n :}
   TDGEN-CLEAR
   vid TDGEN-NAME
   s" ( " TDGEN-APP
   mk 0 <> IF
      vid TDGEN-PAYLOAD
      s" -- " TDGEN-APP
      fam TDGEN-OUT-TYPE
      s"  ) ;" TDGEN-APP
   ELSE
      fam TDGEN-OUT-TYPE
      s"  -- " TDGEN-APP
      vid TDGEN-PAYLOAD
      s" ) ;" TDGEN-APP
   THEN
   TDGEN-NA @ TDGEN-NU @ 0 CTOR-PEND!
   TDECL-GEN-EVAL
   CTOR-PEND-CLEAR
   vid TDGEN-NA @ TDGEN-NU @ CHECKER-RECORD-SYM SUMV-CTOR-SYM! ;

\ --- derived typed equality (derive S1+S2, dot habu-checker-capability-derive):
\ `DERIVE eq` on a PUBLIC arity-0 ENUM/SUMTYPE/PRODUCT generates ORDINARY
\ CHECKED words into the family's reserved constructor package — no pending
\ window, no trust rows, no engine lowering: the bodies are plain checked
\ MATCH/UNMAKE/call text the checker certifies exactly like user code, so
\ equality is semantic and layout-policy agnostic. Derived eq CONSUMES both
\ operands (ordinary non-linear values; callers keep copies as usual).
\  - TAG ( fam -- n ): discriminant, declaration-order tag — public metadata
\    any checked MATCH could already observe. Enums + sums only (no product tag).
\  - EQ ( fam fam -- bool ): payload-free families compare tags (O(V) via TAG);
\    payload sums compare diagonally — the outer MATCH binds one value's
\    payloads to q-locals, the inner MATCH the other's to p-locals; the
\    same-variant arm compares payload scalars with `=` (integer widening at
\    the local bind makes every CT-INT payload an n), cross arms drop + false;
\    products UNMAKE both values, bind fields top-down (an enum-typed field
\    goes through ITS family's PKG:TAG first), and compare field-wise.
\ The scalar `=`/`0=` wall on layout values is untouched (TD12-ZEQ stays the
\ pinned negative); the extend/undefine protection recognizes the fixed tails
\ through TFAM-DERIVED-AT? (products: eq only), and the words ride the ctor
\ package's item-8 closed-but-callable WID protection and registry rollback.
: TDGEN-DRV-REF ( n ptr u8 n -- ) {: fam:n ta:ptr tu:n :}   \ "PKG:TAIL" (upper tail)
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
   0 TDD-J !
   BEGIN TDD-J @ k < WHILE  s" drop " TDGEN-APP  TDD-J @ 1 + TDD-J !  REPEAT ;
: TDGEN-LOCAL1 ( n n -- ) {: c:n i:n :}   \ "<c><i>:n "
   c TDGEN-C,  i TDGEN-DEC  s" :n " TDGEN-APP ;
: TDGEN-BINDS ( n n -- ) {: vid:n c:n :}  \ "{: c0:n .. :} " over vid's payload slots
   vid SUMV-SCH-COUNT@ {: m:n :}
   m 0= IF EXIT THEN
   s" {: " TDGEN-APP
   0 TDD-J !
   BEGIN TDD-J @ m < WHILE  c TDD-J @ TDGEN-LOCAL1  TDD-J @ 1 + TDD-J !  REPEAT
   s" :} " TDGEN-APP ;
: TDGEN-CMP ( n -- ) {: m:n :}            \ "p0 q0 = p1 q1 = and .. " (m=0 -> true)
   m 0= IF s" 0 0= " TDGEN-APP EXIT THEN
   0 TDD-J !
   BEGIN TDD-J @ m < WHILE
      112 TDGEN-C,  TDD-J @ TDGEN-DEC  32 TDGEN-C,
      113 TDGEN-C,  TDD-J @ TDGEN-DEC  32 TDGEN-C,
      s" = " TDGEN-APP
      TDD-J @ 0 > IF s" and " TDGEN-APP THEN
      TDD-J @ 1 + TDD-J !
   REPEAT ;
: TDGEN-MATCH-OPEN ( n -- ) {: fam:n :}   \ "match fam "
   s" match " TDGEN-APP  fam TFAM-NAME$ TDGEN-APP  32 TDGEN-C, ;
: TDGEN-TAG-BODY ( n -- ) {: fam:n :}     \ "match fam v of <drops> <tag> endof .. ;match ;"
   fam TDGEN-MATCH-OPEN
   fam TFAM-VAR-START@ {: vstart:n :}
   0 TDD-I !
   BEGIN TDD-I @ fam TFAM-VAR-COUNT@ < WHILE
      vstart TDD-I @ + SUMV-NAME$ TDGEN-APP
      s"  of " TDGEN-APP
      vstart TDD-I @ + SUMV-SCH-COUNT@ TDGEN-DROPS
      vstart TDD-I @ + SUMV-TAG@ TDGEN-DEC
      s"  endof " TDGEN-APP
      TDD-I @ 1 + TDD-I !
   REPEAT
   s" ;match ;" TDGEN-APP ;
: TDGEN-EQ-ARM ( n n -- ) {: vout:n vin:n :}   \ one inner arm: diagonal cmp / off-diag drop+false
   vin SUMV-NAME$ TDGEN-APP  s"  of " TDGEN-APP
   vin vout = IF
      vin 112 TDGEN-BINDS
      vin SUMV-SCH-COUNT@ TDGEN-CMP
   ELSE
      vin SUMV-SCH-COUNT@ TDGEN-DROPS
      s" 1 0= " TDGEN-APP
   THEN
   s" endof " TDGEN-APP ;
: TDGEN-EQ-DIAG ( n -- ) {: fam:n :}      \ diagonal double-match body for payload sums
   fam TDGEN-MATCH-OPEN
   fam TFAM-VAR-START@ {: vstart:n :}
   fam TFAM-VAR-COUNT@ {: k:n :}
   0 TDD-I !
   BEGIN TDD-I @ k < WHILE
      vstart TDD-I @ + SUMV-NAME$ TDGEN-APP  s"  of " TDGEN-APP
      vstart TDD-I @ + 113 TDGEN-BINDS
      fam TDGEN-MATCH-OPEN
      0 TDD-K !
      BEGIN TDD-K @ k < WHILE
         vstart TDD-I @ +  vstart TDD-K @ +  TDGEN-EQ-ARM
         TDD-K @ 1 + TDD-K !
      REPEAT
      s" ;match endof " TDGEN-APP
      TDD-I @ 1 + TDD-I !
   REPEAT
   s" ;match ;" TDGEN-APP ;
: TDGEN-UNBIND ( n n -- ) {: fam:n c:n :} \ "PKG:UNMAKE " + top-down field binds
   fam s" unmake" TDGEN-DRV-REF  32 TDGEN-C,
   fam TFAM-VAR-START@ {: mk:n :}
   mk SUMV-SCH-COUNT@ TDD-J !
   BEGIN TDD-J @ 0 > WHILE
      mk SUMV-SCH-START@ TDD-J @ + 1 - SCHEMA-ROOT@ dup SCHEMA-APP? IF
         dup SCHEMA-A@ s" tag" TDGEN-DRV-REF  32 TDGEN-C, THEN
      drop
      s" {: " TDGEN-APP  c TDD-J @ 1 - TDGEN-LOCAL1  s" :} " TDGEN-APP
      TDD-J @ 1 - TDD-J !
   REPEAT ;
: TDGEN-EQ-PROD ( n -- ) {: fam:n :}      \ UNMAKE both values, field-wise compare
   fam 113 TDGEN-UNBIND
   fam 112 TDGEN-UNBIND
   fam TFAM-VAR-START@ SUMV-SCH-COUNT@ TDGEN-CMP
   59 TDGEN-C, ;
: TDGEN-EQ-TAGS ( n -- ) {: fam:n :}      \ payload-free family: tag equality (O(V))
   fam s" tag" TDGEN-DRV-REF  s"  swap " TDGEN-APP
   fam s" tag" TDGEN-DRV-REF  s"  = ;" TDGEN-APP ;
: TDECL-TAG-WORD ( n -- ) {: fam:n :}
   TDGEN-CLEAR
   fam s" tag" TDGEN-DRV-NAME
   s" ( " TDGEN-APP  fam TDGEN-OUT-TYPE  s"  -- n ) " TDGEN-APP
   fam TDGEN-TAG-BODY
   TDECL-GEN-EVAL ;
: TDECL-EQ-WORD ( n -- ) {: fam:n :}
   TDGEN-CLEAR
   fam s" eq" TDGEN-DRV-NAME
   s" ( " TDGEN-APP
   fam TDGEN-OUT-TYPE  32 TDGEN-C,  fam TDGEN-OUT-TYPE
   s"  -- bool ) " TDGEN-APP
   fam TFAM-PRODUCT? IF fam TDGEN-EQ-PROD ELSE
   fam TFAM-SLOTS@ 0= IF fam TDGEN-EQ-TAGS ELSE fam TDGEN-EQ-DIAG THEN THEN
   TDECL-GEN-EVAL ;
: TDECL-DRV-WORDS ( n -- ) {: fam:n :}
   fam TFAM-DERIVE-EQ? 0= IF EXIT THEN
   fam TFAM-PRODUCT? 0= IF fam TDECL-TAG-WORD THEN
   fam TDECL-EQ-WORD ;

: TDECL-PROD-WORDS ( n -- ) {: fam:n :}   \ make (row 0) + unmake (row 1)
   fam TFAM-VAR-START@ {: vstart:n :}
   fam vstart 1 TDECL-PROD-WORD
   fam vstart 1 + 0 TDECL-PROD-WORD
   fam TDECL-DRV-WORDS                    \ derived words BEFORE the WID closes
   vstart TDECL-CTOR-PROT-WID ;

: TDECL-CTOR-WORDS ( -- )                \ engine-load generation for the last sum
   TDECL-FAM-REG @ {: fam:n :}
   fam 0 < IF EXIT THEN
   fam TFAM-PUBLIC? 0= IF EXIT THEN
   fam TFAM-PRODUCT? IF fam TDECL-PROD-WORDS EXIT THEN
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
   REPEAT
   fam TDECL-DRV-WORDS                   \ derived words BEFORE the WID closes
   vstart TDECL-CTOR-PROT-WID ;

\ --- public defining words. TYPEFAMILY consumes name + arity; SUMTYPE buffers
\ the block up to ;SUMTYPE (VALUE-RECORD's shape), then registers it whole.
: TYPEFAMILY ( -- )
   parse-name {: na:ptr nu:n :}
   parse-name {: aa:ptr au:n :}
   na nu aa au CHECKER-DEFFAMILY ;

create TDECL-BUF TDECL-CAP allot
variable TDECL-U
variable TDECL-I

: TDECL-CLEAR ( -- ) 0 TDECL-U !  0 TDECL-OVERSIZE ! ;
: TDECL-C, ( n -- )                     \ over-cap: cap + flag (never raw-die); the
   TDECL-U @ 1 + TDECL-CAP > IF drop -1 TDECL-OVERSIZE ! EXIT THEN   \ body reaches the checker (§24 C2)
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

: SUMTYPE ( -- )
   parse-name {: na:ptr nu:n :}
   TDECL-CLEAR
   SUMTYPE-COLLECT 0= IF
      na nu TDECL-BUF TDECL-U @ CHECKER-DEFSUM-NOEND EXIT
   THEN
   na nu TDECL-BUF TDECL-U @ CHECKER-DEFSUM
   TDECL-CTOR-WORDS ;

\ ENUM buffers the bare variant names up to ;ENUM (SUMTYPE's shape without an
\ arity token or VARIANT keywords), then registers the whole block.
: ENUM-COLLECT ( -- bool )   \ buffer tokens; false = input ended unterminated
   BEGIN
      parse-name
      dup 0= IF 2drop RES-FALSE EXIT THEN
      2dup s" ;enum" CORE-STR=CI IF 2drop RES-TRUE EXIT THEN
      TDECL-TOKEN+
   AGAIN ;

: TDECL-ENUM-NOEND-BODY ( -- )
   TDN-A @ TDN-U @ s" missing ;ENUM" E-TDECL-SYNTAX TDECL-THROW ;

: ENUM ( -- )
   parse-name {: na:ptr nu:n :}
   TDECL-CLEAR
   ENUM-COLLECT 0= IF
      s" enum" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
      [: TDECL-ENUM-NOEND-BODY ;] TDECL-RUN EXIT
   THEN
   na nu TDECL-BUF TDECL-U @ CHECKER-DEFENUM
   TDECL-CTOR-WORDS ;

\ PRODUCT buffers the `arity FIELD f t ..` body up to ;PRODUCT (SUMTYPE's shape),
\ then registers the whole block and generates the PKG:MAKE/PKG:UNMAKE words
\ for a public product (TDECL-CTOR-WORDS branches on the family kind).
: PRODUCT-COLLECT ( -- bool )   \ buffer tokens; false = input ended unterminated
   BEGIN
      parse-name
      dup 0= IF 2drop RES-FALSE EXIT THEN
      2dup s" ;product" CORE-STR=CI IF 2drop RES-TRUE EXIT THEN
      TDECL-TOKEN+
   AGAIN ;

: TDECL-PRODUCT-NOEND-BODY ( -- )
   TDN-A @ TDN-U @ s" missing ;PRODUCT" E-TDECL-SYNTAX TDECL-THROW ;

: PRODUCT ( -- )
   parse-name {: na:ptr nu:n :}
   TDECL-CLEAR
   PRODUCT-COLLECT 0= IF
      s" product" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
      [: TDECL-PRODUCT-NOEND-BODY ;] TDECL-RUN EXIT
   THEN
   na nu TDECL-BUF TDECL-U @ CHECKER-DEFPRODUCT
   TDECL-CTOR-WORDS ;
