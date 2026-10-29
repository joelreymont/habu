\ enum-decl.f: the ENUM front end (package ENUM-DECL), grammar loop and reject
\ dispatch only. DECL-FRONT (generated-declaration.f) owns the token source and
\ the guarded run; DECL-EVENT (decl-event.f) owns the events, field and variant
\ rows and the current-variant selector. The constructor participant
\ (generated-declaration.f) renders constructors at commit, so a reject publishes
\ none. Loads after decl-event.f and structure-decl.f.

using SCHEMA-REG
using TFAM
using TYPE-DECL
using DECL-FRONT

package ENUM-DECL

\ Transient parse state, not declaration state: ENUM is top-level
\ interpret-only, so one declaration runs at a time.
variable FAM        \ family id being declared
variable TOK        \ declaration-event token; ED-RESET zeroes it at body start
variable VBASE      \ variant high-water at open (variant range start)
variable NVAR       \ variant count in this declaration
variable FLDBASE    \ committed field high-water at open (field range start)
variable NFLD       \ total field count across all variants
variable VCELLS     \ running payload cell width WITHIN the open variant
variable MAXSLOTS   \ widest variant payload cell width (the sum's payload slots)
variable SEEN-VARIANT \ a VARIANT has appeared (header clauses must precede variants)

: ED-RESET ( -- )
   0 TOK !   0 SEEN-VARIANT ! ;
ED-RESET

\ The reserved list is TYPE-NAME:FAMILY-RESERVED?, the one every family definer
\ asks (the ENUM openers are grammar keywords there): a copy kept here drifted
\ twice. Case and duplicate are TFAM-DECL's.

: REQUIRE-NAME ( ptr u8 n -- )
   dup 0= IF 2drop s" missing name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail" E-CASE DECL-REJECT:REJECT throw THEN
   TYPE-NAME:FAMILY-RESERVED? IF s" reserved name" E-NAME DECL-REJECT:REJECT throw THEN ;

\ Payload types: n/f/r and con names, letter params within arity, ptr T, and
\ closed arity-0 layout or cell families; anything else is unresolved. Mirrors
\ structure-decl.f's resolver without type applications. A family owning a linear
\ value is accepted, owned by containment as in structure-decl.f; TK-EVIDENCE
\ falls through to "unknown payload type" because no source can declare it.
: FIELD-FAM? ( ptr u8 n -- n bool )
   TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE 0= IF drop 0 NO EXIT THEN
   {: id:n :}
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 NO EXIT THEN
   id TFAM-ARITY@ 0 <> IF
      s" payload type is parametric and needs type arguments" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   id YES ;

: LETTER-TYPE ( ptr u8 n -- n )
   drop c@
   dup ASCII-N = IF drop CON-N SCHEMA-CON EXIT THEN
   dup ASCII-F = IF drop CON-BOOL SCHEMA-CON EXIT THEN
   dup ASCII-R = IF drop CON-R SCHEMA-CON EXIT THEN
   TYPE-DECL:DECL-PARAM 0= IF
      drop s" unknown payload type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   SCHEMA-PARAM ;

\ `ptr <linear>` is refused for a payload by the rule structure-decl.f
\ REQUIRE-POINTEE applies to a field: TFCL-NODE? stops at a pointer node.
: REQUIRE-POINTEE ( n -- n )
   dup TFCL-NODE? IF
      s" payload type is a pointer to a linear value and cannot own it"
      E-PAYLOAD DECL-REJECT:REJECT throw THEN ;

: RESOLVE-TYPE ( ptr u8 n -- n )
   dup 0= IF 2drop s" missing payload type" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ptr" CORE-STR=CI IF
      2drop NEXT RECURSE
      FIELD-MISS @ IF drop 0 EXIT THEN
      REQUIRE-POINTEE SCHEMA-PTR EXIT THEN
   dup 1 = IF LETTER-TYPE EXIT THEN
   2dup CON-OF dup 0 <> IF nip nip SCHEMA-CON EXIT THEN drop
   2dup FIELD-FAM? IF nip nip 0 0 SCHEMA-APP EXIT THEN drop
   2dup TYPE-MAY-ARRIVE-XT IF MISSING! 0 EXIT THEN
   2drop s" unknown payload type" E-PAYLOAD DECL-REJECT:REJECT throw ;

: SCH-WIDTH ( n -- n )
   dup SCHEMA-APP? IF SCHEMA-A@ TFAM-WIDTH@ EXIT THEN drop 1 ;

\ Header clauses emit through DECL-EVENT, which owns duplicate and ordinal state;
\ each mutates only the fresh family record.
: HEADER-ORDER ( -- )
   SEEN-VARIANT @ IF
      s" header clause after the first variant" E-SYNTAX DECL-REJECT:REJECT throw THEN ;

: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   NEXT POLICY-CODE {: code:n :}
   FAM @ code TFAM-LAYOUT!
   TOK @ FAM @ code DECL-EVENT:POLICY TOK ! ;

: DERIVE-FEATURE? ( ptr u8 n -- bool )
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   s" order" CORE-STR=CI ;

\ The first token after the header clauses picks the mode: VARIANT starts full
\ blocks, a bare name a compact body; a parameter list forces full mode, and the
\ modes never mix. The engine and the replay reader both return spans in one
\ buffered source, so the prefix is kept as a span and re-read through
\ DECL-FRONT's token source (HEAD!): registration fixes the kind before any clause
\ applies, and diagnostics keep their bytes.
: ED-LOOK-END? ( ptr u8 n -- bool )
   dup 0= IF 2drop YES EXIT THEN
   s" ;enum" CORE-STR=CI ;
: ED-FIRST-VARIANT ( ptr u8 n -- ptr u8 n )
   BEGIN
      2dup s" policy" CORE-STR=CI IF
         2drop SOURCE-NEXT
         2dup ED-LOOK-END? IF EXIT THEN
         2drop SOURCE-NEXT
      ELSE
         2dup s" derive" CORE-STR=CI IF
            2drop SOURCE-NEXT
            BEGIN 2dup DERIVE-FEATURE? WHILE 2drop SOURCE-NEXT REPEAT
         ELSE
            \ OPAQUE may be a compact variant or a forbidden header before
            \ VARIANT blocks. Keep it in the lookahead and inspect the next token.
            2dup s" opaque" CORE-STR=CI IF 2drop SOURCE-NEXT ELSE EXIT THEN
         THEN
      THEN
   AGAIN ;
: ED-FULL? ( -- bool )
   SOURCE-NEXT {: first:ptr size:n :}
   first size ED-FIRST-VARIANT {: last:ptr lastu:n :}
   size 0 <> IF first  last lastu + first -  HEAD! THEN
   last lastu s" variant" CORE-STR=CI ;
: DERIVE-GUARD ( -- )
   FAM @ TFAM-PUBLIC? 0= IF
      s" derive requires a public family" E-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ TFAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family" E-DERIVE DECL-REJECT:REJECT throw THEN ;
: EMIT-DERIVE ( n n -- )
   TOK @ -rot DECL-EVENT:DERIVE TOK ! ;
: DERIVE-ONE ( ptr u8 n -- )
   DERIVE-GUARD
   2dup s" eq" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-EQ! FAM @ DRV-EQ EMIT-DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-HASH! FAM @ DRV-HASH EMIT-DERIVE EXIT THEN
   2dup s" order" CORE-STR=CI IF
      2drop s" derive feature not yet supported" E-DERIVE DECL-REJECT:REJECT throw THEN
   2drop s" unknown derive feature" E-DERIVE DECL-REJECT:REJECT throw ;
: DERIVE-CLAUSE ( -- )
   HEADER-ORDER
   NEXT dup 0= IF
      2drop s" missing derive feature" E-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
   BEGIN NEXT dup 0= IF 2drop EXIT THEN
      2dup DERIVE-FEATURE? IF DERIVE-ONE ELSE UNGET EXIT THEN
   AGAIN ;

\ Full mode: DECL-EVENT:VARIANT registers the SUMV row and pins the current-variant
\ selector the FIELD events carry; END-VARIANT clears it. Each variant's payload
\ restarts at slot 0 (VCELLS); the family width is the widest variant (MAXSLOTS).
: EMIT-FIELD ( ptr u8 n n -- )
   SCHEMA-ROOT+ {: sch:n :}
   sch SCHEMA-ROOT@ SCH-WIDTH {: fw:n :}
   2dup DECL-REJECT:TOKEN!
   s" duplicate field name" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ 2swap sch
   VCELLS @  fw  VCELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD TOK !
   fw VCELLS @ + VCELLS !
   NFLD @ 1 + NFLD ! ;
: FIELD-CLAUSE ( -- )
   NEXT dup 0= IF
      2drop s" missing field name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   {: na:ptr nu:n :}
   na nu NAME-LONG? IF TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   OUTCOME-NAMES @ 0 <> IF
      na nu s" enum field names exceed declaration window" NAME-CHECK THEN
   0 FIELD-MISS !
   NEXT RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF na nu node EMIT-FIELD THEN ;

: VARIANT-NAME ( -- ptr u8 n )
   NEXT dup 0= IF
      2drop s" missing variant name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN ;
: OPEN-VARIANT ( -- )
   SEEN-CLEAR
   VARIANT-NAME {: na:ptr nu:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate variant" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ na nu
   DECL-EVENT:VARIANT TOK !
   0 VCELLS ! ;
: CLOSE-VARIANT ( -- )
   VCELLS @ MAXSLOTS @ max MAXSLOTS !
   TOK @ FAM @ DECL-EVENT:END-VARIANT TOK !
   NVAR @ 1 + NVAR ! ;
: VARIANT-CLAUSE ( -- bool )            \ read one variant-body token; YES = ;VARIANT
   NEXT dup 0= IF 2drop s" missing ;VARIANT" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;variant" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2drop s" unexpected token in variant block" E-SYNTAX
   DECL-REJECT:REJECT throw ;
: VARIANT-BODY ( -- ) BEGIN VARIANT-CLAUSE UNTIL ;
: VARIANT-BLOCK ( -- )
   -1 SEEN-VARIANT !
   OPEN-VARIANT VARIANT-BODY CLOSE-VARIANT ;

\ Compact body: full-mode block keywords always reject; a header after a variant
\ rejects through HEADER-ORDER.
: COMPACT-KW? ( ptr u8 n -- bool )
   2dup s" variant" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;variant" CORE-STR=CI IF 2drop YES EXIT THEN
   s" field" CORE-STR=CI ;
: COMPACT-VARIANT ( ptr u8 n -- )
   {: na:ptr nu:n :}
   na nu NAME-LONG? IF TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   -1 SEEN-VARIANT !
   na nu DECL-REJECT:TOKEN!
   s" duplicate variant" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ na nu
   DECL-EVENT:VARIANT TOK !
   TOK @ FAM @ DECL-EVENT:END-VARIANT TOK !
   NVAR @ 1 + NVAR ! ;
: COMPACT-CLAUSE ( ptr u8 n -- bool )    \ dispatch one compact token; YES = ;ENUM
   2dup DECL-REJECT:TOKEN!               \ the mode-selecting token arrives pre-read
   2dup s" ;enum" CORE-STR=CI IF 2drop -1 SEEN-END ! YES EXIT THEN
   2dup COMPACT-KW? IF
      2drop s" block keyword in a compact enum" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   COMPACT-VARIANT NO ;

\ The kind is fixed at registration, before any variant parses, because TFAM-DECL
\ sets it once: compact registers TK-ENUM (arity 0), full registers TK-SUM with the
\ declared arity (PF-OWNER-OK? admits a named variant field on either). TFAM-WIDTH@
\ treats both alike at zero payload, so a payloadless full ENUM has the compact
\ width. A variant's fields are the TYPE-FIELD rows keyed (family, variant id);
\ DEV-VARIANT sets its SV.SCH-COUNT to 0.
: OPEN-TX ( -- )
   SUMV-N@ VBASE !
   TYPE-FIELD:COUNT FLDBASE !
   0 NVAR !   0 NFLD !   0 MAXSLOTS !
   DECL-EVENT:CURRENT TOK !
   TOK @ FAM @ DECL-EVENT:DECL TOK ! ;
: REGISTER-FULL ( ptr u8 n n -- )
   {: na:ptr nu:n ar:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate family" E-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   ar TK-SUM TFAM-DECL FAM !
   OPEN-TX
   TOK @ FAM @ ar DECL-EVENT:ARITY TOK ! ;
: REGISTER-COMPACT ( ptr u8 n -- )
   {: na:ptr nu:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate family" E-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   0 TK-ENUM TFAM-DECL FAM !
   OPEN-TX ;

\ ED-CLOSE renders nothing: it arms the constructor participant, which generates
\ at commit after DECL-EVENT has promoted this declaration's TYPE-FIELD rows to
\ the committed watermark the generator reads. GENERATED-DECL-CTOR:OWNS? is the
\ one gate for arming here and for the participant's re-proof; both modes pass it,
\ since a compact payloadless enum publishes constructors too.
\ The packed memory descriptor (TDECL-LAYOUT-DESC) needs the variant count and the
\ slot width bound first; a `POLICY stack-cell-tag` family bakes none. LAY-ADD
\ lands in the checker participant's savepoint (TF-SAVE carries LAY-N), so a later
\ reject retires the descriptor with the family.
: ED-CLOSE ( -- )
   DECL-REJECT:AT-FAMILY
   NVAR @ 0= IF s" empty enum" E-SYNTAX DECL-REJECT:REJECT throw THEN
   MISSING? IF E-UNRESOLVED throw THEN
   FAM @ VBASE @ NVAR @ TFAM-VAR-RANGE!
   FAM @ FLDBASE @ NFLD @ TFAM-FLD-RANGE!
   FAM @ MAXSLOTS @ TFAM-SLOTS!
   FAM @ TDECL-LAYOUT-DESC
   \ No reason is armed for the constructor participant's collide check: it runs at
   \ commit, where no token of this front end applies, so the packet answers a
   \ reserved-name reject from its code table.
   FAM @ GENERATED-DECL-CTOR:OWNS? IF FAM @ GENERATED-DECL-CTOR:ARM THEN ;

: FULL-CLAUSE ( -- bool )               \ read + dispatch one full-body token; YES = ;ENUM
   NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;ENUM" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;enum" CORE-STR=CI IF 2drop -1 SEEN-END ! YES EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2dup s" variant" CORE-STR=CI IF 2drop VARIANT-BLOCK NO EXIT THEN
   2drop s" unexpected token in enum declaration" E-SYNTAX
   DECL-REJECT:REJECT throw ;
: FULL-BODY ( -- ) BEGIN FULL-CLAUSE UNTIL ;

: COMPACT-BODY ( ptr u8 n -- )
   BEGIN
      dup 0= IF 2drop
         DECL-REJECT:AT-FAMILY
         s" missing ;ENUM" E-SYNTAX DECL-REJECT:REJECT throw THEN
      COMPACT-CLAUSE IF EXIT THEN
      NEXT
   AGAIN ;

: DRIVE ( -- )
   NEXT 2dup DECL-REJECT:FAMILY!
   dup 0= IF 2drop s" missing name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   DECL-HEADER 0= IF
      drop 2drop s" invalid type parameter header" E-ARITY DECL-REJECT:REJECT throw
   THEN
   {: ar:n :}
   2dup DECL-REJECT:FAMILY!
   2dup REQUIRE-NAME
   {: na:ptr nu:n :}
   ED-FULL? ar 0 <> or IF
      na nu ar REGISTER-FULL
      FULL-BODY
   ELSE
      na nu REGISTER-COMPACT
      NEXT COMPACT-BODY
   THEN
   ED-CLOSE ;

: ED-FRONT ( -- ptr u8 n ptr u8 n [ -- ] )
   s" enum" s" ;enum" [: ED-RESET DRIVE ;] ;

public

\ The last declaration's parse state back to zero at a file boundary or image
\ capture (generated-declaration-protection.f SNAPSHOT-RESET), so a cached load
\ and a fresh one capture the same bytes.
: SNAPSHOT-RESET ( -- )
   ED-RESET
   0 FAM !  0 VBASE !  0 NVAR !  0 FLDBASE !  0 NFLD !  0 VCELLS !  0 MAXSLOTS ! ;

: ED-RUN ( -- ) ED-FRONT DECL-FRONT:RUN ;

\ ED-REPLAY registers an ENUM from tokens a tool already lexed (tools/check-core.f,
\ verify-source.f) with ED-RUN's grammar, validation and reject packet, defining
\ no word: constructors are registered, not rendered. Both modes replay. The body
\ includes its ;ENUM terminator. ED-REPLAY-OUTCOME answers the first type the
\ body named that may still arrive instead of raising.
: ED-REPLAY-OUTCOME ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   ED-FRONT DECL-FRONT:REPLAY-OUTCOME ;
: ED-REPLAY ( ptr u8 n ptr u8 n -- )
   ED-FRONT DECL-FRONT:REPLAY ;

;package

\ ENUM is a documented global (package-first exception, like STRUCTURE) and the
\ only ENUM parser; it parses its body to ;ENUM at interpret time, so its checked
\ effect is ( -- ).
\   ENUM type-name[<params>] header-clause* (variant-name+ | variant-block+) ;ENUM
\   header-clause = POLICY policy-name | DERIVE derive-name+
\   variant-block = VARIANT variant-name (FIELD field-name type-expr)* ;VARIANT
: ENUM ( -- ) ENUM-DECL:ED-RUN ;

;using
;using
;using
;using
