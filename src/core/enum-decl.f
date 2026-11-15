\ enum-decl.f: the ENUM front end (package ENUM-DECL), grammar loop and reject
\ dispatch only. DECL-FRONT (generated-declaration.f) owns the token source and
\ the guarded run; DECL-EVENT (decl-event.f) owns the events, field and variant
\ rows and the current-variant selector. The constructor participant
\ (generated-declaration.f) builds constructors at commit, so a reject publishes
\ none. Loads after decl-event.f and structure-decl.f.

using SCHEMA-REG
using TFAM
using TYPE-DECL

package TYPE-DECL
private
package ENUM-DECL
using DECL-FRONT

\ Transient parse state, not declaration state: ENUM is top-level
\ interpret-only, so one declaration runs at a time.
variable FAM        \ family id being declared
variable VBASE      \ variant high-water at open (variant range start)
variable NVAR       \ variant count in this declaration
variable FLDBASE    \ committed field high-water at open (field range start)
variable NFLD       \ total field count across all variants
variable VCELLS     \ running payload cell width WITHIN the open variant
variable MAXSLOTS   \ widest variant payload cell width (the sum's payload slots)
variable SEEN-VARIANT \ a VARIANT has appeared (header clauses must precede variants)
variable POINTEE    \ resolving the pointee of `ptr`: the declared family may be named

: ED-RESET ( -- )
   0 SEEN-VARIANT !   0 POINTEE ! ;
ED-RESET

\ The reserved list is TYPE-NAME:FAMILY-RESERVED?, the one every family definer
\ asks (the ENUM openers are grammar keywords there): a copy kept here drifted
\ twice. Case and duplicate are TFAM-DECL's.

: REQUIRE-NAME ( ptr u8 n -- )
   dup 0= IF 2drop
      s" missing name"
      s" write the family name after `ENUM`, as in `ENUM color red green ;ENUM`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop
      TF-NAME-LONG$
      s" shorten the family name to 255 bytes or fewer"
      E-TDECL-CAP DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail"
      s" write the family name as a lowercase tail with no package qualifier, as in `color`"
      E-TFAM-CASE DECL-REJECT:REJECT throw THEN
   TYPE-NAME:FAMILY-RESERVED? IF
      s" reserved name"
      s" rename the family: keywords, control words, single letters, `ptr` and value records are reserved"
      E-TDECL-NAME DECL-REJECT:REJECT throw THEN ;

\ Payload types: n/f/r and con names, letter params within arity, ptr T, and
\ closed arity-0 layout or cell families but one UNSTORED-REASON names, a linear
\ owner included; an ambiguous family is rejected, and anything else,
\ TK-EVIDENCE among them, is unresolved.
: FIELD-FAM? ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE {: id:n sr:n :}
   sr SR-AMBIG = IF a u DECL-REJECT:TOKEN!  a u TDECL-AMBIG throw THEN
   sr SR-FOUND <> IF 0 NO EXIT THEN
   id UNSTORED-REASON IF E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN 2drop 2drop
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 NO EXIT THEN
   id FAM @ = POINTEE @ 0= and IF   \ its own family only beneath `ptr`
      s" payload type cannot recursively name its owner"
      s" name the enum being declared only beneath `ptr`, as in `FIELD next ptr list`"
      E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   id TFAM-ARITY@ 0 <> IF
      s" payload type is parametric and needs type arguments"
      s" an `ENUM` payload takes no type arguments: name a `STRUCTURE` whose field applies this family"
      E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   id YES ;

: LETTER-TYPE ( ptr u8 n -- n )
   drop c@
   dup ASCII-N = IF drop CC-N SCHEMA-CON EXIT THEN
   dup ASCII-F = IF drop CC-BOOL SCHEMA-CON EXIT THEN
   dup ASCII-R = IF drop CC-R SCHEMA-CON EXIT THEN
   TYPE-DECL:DECL-PARAM 0= IF
      drop s" unknown payload type"
      s" use `n`, `f` or `r`, or a type parameter the header declares, as in `ENUM box<a>`"
      E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   SCHEMA-PARAM ;

: RESOLVE-TYPE ( ptr u8 n -- n )
   dup 0= IF 2drop
      s" missing payload type"
      s" write a type after the field name, as in `FIELD x n`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ptr" CORE-STR=CI IF
      2drop POINTEE @ {: outer:n :}
      -1 POINTEE !
      NEXT RECURSE
      outer POINTEE !
      FIELD-MISS @ IF drop 0 EXIT THEN
      s" payload type is a pointer to a linear value and cannot own it"
      DECL-EVENT:REQUIRE-POINTEE SCHEMA-PTR EXIT THEN
   dup 1 = IF LETTER-TYPE EXIT THEN
   2dup CON-OF dup 0 <> IF nip nip SCHEMA-CON EXIT THEN drop
   2dup FIELD-FAM? IF nip nip 0 0 SCHEMA-APP EXIT THEN drop
   2dup TYPE-MAY-ARRIVE-XT IF MISSING! 0 EXIT THEN
   2drop s" unknown payload type"
   s" declare the type before this `ENUM`, or use a built-in one such as `n` or `ptr u8`"
   E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

: SCH-WIDTH ( n -- n )
   dup SCHEMA-APP? IF SCHEMA-A@ TFAM-WIDTH@ EXIT THEN drop 1 ;

\ Header clauses emit through DECL-EVENT, which owns duplicate and ordinal state;
\ each mutates only the fresh family record.
: HEADER-ORDER ( -- )
   SEEN-VARIANT @ IF
      s" header clause after the first variant"
      s" move the `POLICY` and `DERIVE` clauses before the first variant"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN ;

: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   NEXT POLICY-CODE {: code:n :}
   FAM @ code TFAM-LAYOUT!
   FAM @ code DECL-EVENT:POLICY ;

: DERIVE-FEATURE? ( ptr u8 n -- bool )
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" tag" CORE-STR=CI IF 2drop YES EXIT THEN
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
   FAM @ TFAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family"
      s" drop the type parameters or the `DERIVE` clause: only an arity-0 family derives"
      E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN ;
: DERIVE-ONE ( ptr u8 n -- )
   DERIVE-GUARD
   2dup s" eq" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-EQ! FAM @ DRV-EQ DECL-EVENT:DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-HASH! FAM @ DRV-HASH DECL-EVENT:DERIVE EXIT THEN
   2dup s" tag" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-TAG! FAM @ DRV-TAG DECL-EVENT:DERIVE EXIT THEN
   2dup s" order" CORE-STR=CI IF
      2drop s" derive feature not yet supported"
      s" derive `eq`, `hash` or `tag` instead: `order` is not implemented"
      E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   2drop s" unknown derive feature"
   s" name `eq`, `hash` or `tag` after `DERIVE`"
   E-TDECL-DERIVE DECL-REJECT:REJECT throw ;
: DERIVE-CLAUSE ( -- )
   HEADER-ORDER
   NEXT dup 0= IF
      2drop s" missing derive feature"
      s" name `eq`, `hash` or `tag` after `DERIVE`"
      E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
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
   s" duplicate field name"
   s" give each field of the variant its own name"
   E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ -rot sch
   VCELLS @  fw  VCELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD
   fw VCELLS @ + VCELLS !
   NFLD @ 1 + NFLD ! ;
: FIELD-CLAUSE ( -- )
   NEXT dup 0= IF
      2drop s" missing field name"
      s" write a name and a type after `FIELD`, as in `FIELD x n`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   {: na:ptr nu:n :}
   na nu NAME-LONG? IF
      TF-NAME-LONG$
      s" shorten the field name to 255 bytes or fewer"
      E-TDECL-CAP DECL-REJECT:REJECT throw THEN
   OUTCOME-NAMES @ 0 <> IF
      na nu NAME-CHECK THEN
   0 FIELD-MISS !
   NEXT {: ta:ptr tu:n :}
   ta tu RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF
      ta tu FAM @ node REQUIRE-DERIVABLE
      na nu node EMIT-FIELD THEN ;

: VARIANT-NAME ( -- ptr u8 n )
   NEXT dup 0= IF
      2drop s" missing variant name"
      s" write a name after `VARIANT`, as in `VARIANT quit ;VARIANT`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop
      TF-NAME-LONG$
      s" shorten the variant name to 255 bytes or fewer"
      E-TDECL-CAP DECL-REJECT:REJECT throw THEN ;
\ Records variant NAME, rejected at its token when a word DERIVE generates
\ takes its name or a variant already has it. DERIVE precedes every variant,
\ so the family's derive flags are set here.
: VARIANT-START ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu DECL-REJECT:TOKEN!
   FAM @ TFAM-DERIVE-ANY? na nu TFAM-DERIVED-TAIL? and IF
      TDECL-COLLIDE-WHY E-TDECL-NAME DECL-REJECT:REJECT throw THEN
   s" duplicate variant"
   s" give each variant of the enum its own name"
   E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ na nu DECL-EVENT:VARIANT ;
: OPEN-VARIANT ( -- )
   SEEN-CLEAR
   VARIANT-NAME VARIANT-START
   0 VCELLS ! ;
: CLOSE-VARIANT ( -- )
   VCELLS @ MAXSLOTS @ max MAXSLOTS !
   FAM @ DECL-EVENT:END-VARIANT
   NVAR @ 1 + NVAR ! ;
: VARIANT-CLAUSE ( -- bool )            \ read one variant-body token; YES = ;VARIANT
   NEXT dup 0= IF 2drop
      s" missing ;VARIANT"
      s" close each variant block with `;VARIANT`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;variant" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2drop s" unexpected token in variant block"
   s" write only `FIELD name type` clauses before `;VARIANT`, with no comments in the body"
   E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;
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
   na nu NAME-LONG? IF
      TF-NAME-LONG$
      s" shorten the variant name to 255 bytes or fewer"
      E-TDECL-CAP DECL-REJECT:REJECT throw THEN
   -1 SEEN-VARIANT !
   na nu VARIANT-START
   FAM @ DECL-EVENT:END-VARIANT
   NVAR @ 1 + NVAR ! ;
: COMPACT-CLAUSE ( ptr u8 n -- bool )    \ dispatch one compact token; YES = ;ENUM
   2dup DECL-REJECT:TOKEN!               \ the mode-selecting token arrives pre-read
   2dup s" ;enum" CORE-STR=CI IF 2drop -1 SEEN-END ! YES EXIT THEN
   2dup COMPACT-KW? IF
      2drop s" block keyword in a compact enum"
      s" write bare variant names only, or every variant as a `VARIANT name ;VARIANT` block"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   COMPACT-VARIANT NO ;

\ The kind is fixed at registration, before any variant parses, because TFAM-DECL
\ sets it once: compact registers TK-ENUM (arity 0), full registers TK-SUM with the
\ declared arity (PF-OWNER-OK? admits a named variant field on either). TFAM-WIDTH@
\ treats both alike at zero payload, so a payloadless full ENUM has the compact
\ width. A variant's fields are the TYPE-FIELD rows keyed (family, variant id);
\ DECL-EVENT:VARIANT sets its SV.SCH-COUNT to 0.
: OPEN-TX ( -- )
   SUMV-N@ VBASE !
   TYPE-FIELD:COUNT FLDBASE !
   0 NVAR !   0 NFLD !   0 MAXSLOTS ! ;
: REGISTER-FULL ( ptr u8 n n -- )
   {: na:ptr nu:n ar:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate family"
   s" rename the enum: this package already declares a family by that name"
   E-TFAM-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   ar TK-SUM TFAM-DECL FAM !
   OPEN-TX
   FAM @ ar DECL-EVENT:ARITY ;
: REGISTER-COMPACT ( ptr u8 n -- )
   {: na:ptr nu:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate family"
   s" rename the enum: this package already declares a family by that name"
   E-TFAM-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   0 TK-ENUM TFAM-DECL FAM !
   OPEN-TX ;

\ The variants' fields are still events until commit, so the niche-null shape
\ is read through DECL-EVENT.
: NICHE-SHAPE? ( -- bool )
   NVAR @ 2 <> IF NO EXIT THEN
   FAM @ VBASE @ DECL-EVENT:PAYLOAD-N 0 <> IF NO EXIT THEN
   FAM @ VBASE @ 1 + DECL-EVENT:PAYLOAD-N 1 <> IF NO EXIT THEN
   FAM @ VBASE @ 1 + 0 DECL-EVENT:PAYLOAD-SCHEMA@
   SCHEMA-ROOT@ SCHEMA-PTR? ;
: NICHE-REQUIRE ( -- )
   FAM @ TFAM-NICHE? 0= IF EXIT THEN
   NICHE-SHAPE? IF EXIT THEN
   TDECL-NICHE-MESSAGE$
   s" declare an empty variant, then one with a single `ptr` field, or drop `POLICY niche-null`"
   E-TDECL-POLICY DECL-REJECT:REJECT throw ;

\ TAG> builds each value from its tag alone, so no variant may carry a payload.
: TAG-REQUIRE ( -- )
   FAM @ TFAM-DERIVE-TAG? 0= IF EXIT THEN
   NFLD @ 0= IF EXIT THEN
   s" derive tag requires variants without payload"
   s" drop `tag` from `DERIVE`, or declare every variant without a field"
   E-TDECL-DERIVE DECL-REJECT:REJECT throw ;

\ ED-CLOSE builds nothing: it arms the constructor participant, which generates
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
   NVAR @ 0= IF
      s" empty enum"
      s" declare at least one variant before `;ENUM`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   MISSING? IF E-UNRESOLVED throw THEN
   NICHE-REQUIRE
   TAG-REQUIRE
   FAM @ VBASE @ NVAR @ TFAM-VAR-RANGE!
   FAM @ FLDBASE @ NFLD @ TFAM-FLD-RANGE!
   NFLD @ 0 ?do
      FAM @ FLDBASE @ i + DECL-EVENT:FIELD-SCHEMA@ SCHEMA-ROOT@
      s" payload type is a pointer to a linear value and cannot own it"
      DECL-EVENT:REQUIRE-POINTEES
   loop
   FAM @ MAXSLOTS @ TFAM-SLOTS!
   FAM @ TDECL-LAYOUT-DESC
   FAM @ GENERATED-DECL-CTOR:OWNS? IF FAM @ GENERATED-DECL-CTOR:ARM THEN ;

: FULL-CLAUSE ( -- bool )               \ read + dispatch one full-body token; YES = ;ENUM
   NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;ENUM"
      s" end the declaration with `;ENUM`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;enum" CORE-STR=CI IF 2drop -1 SEEN-END ! YES EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2dup s" variant" CORE-STR=CI IF 2drop VARIANT-BLOCK NO EXIT THEN
   2drop s" unexpected token in enum declaration"
   s" write only `POLICY`, `DERIVE` and `VARIANT` clauses before `;ENUM`, with no comments"
   E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;
: FULL-BODY ( -- ) BEGIN FULL-CLAUSE UNTIL ;

: COMPACT-BODY ( ptr u8 n -- )
   BEGIN
      dup 0= IF 2drop
         DECL-REJECT:AT-FAMILY
         s" missing ;ENUM"
         s" end the declaration with `;ENUM`"
         E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
      COMPACT-CLAUSE IF EXIT THEN
      NEXT
   AGAIN ;

: DRIVE ( -- )
   NEXT 2dup DECL-REJECT:FAMILY!
   dup 0= IF 2drop
      s" missing name"
      s" write the family name after `ENUM`, as in `ENUM color red green ;ENUM`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   DECL-HEADER 0= IF
      drop 2drop s" invalid type parameter header"
      s" list distinct parameter letters in angle brackets, as in `box<a>` or `result<a,b>`"
      E-TDECL-ARITY DECL-REJECT:REJECT throw
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

: DECLARE-ENUM ( -- ) ED-FRONT DECL-FRONT:RUN ;

\ Registers an ENUM from a body a tool lexed, by DECLARE-ENUM's grammar, defining
\ no word; a body without ;ENUM is rejected "missing ;ENUM" (enum-decl-suite 23e).
: REPLAY-ENUM ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   ED-FRONT DECL-FRONT:REPLAY-OUTCOME ;

;package

public

EXPORT ENUM-DECL:DECLARE-ENUM
EXPORT ENUM-DECL:REPLAY-ENUM

;package

\ ENUM is a documented global (package-first exception, like STRUCTURE) and the
\ only ENUM parser; it parses its body to ;ENUM at interpret time, so its checked
\ effect is ( -- ).
\   ENUM type-name[<params>] header-clause* (variant-name+ | variant-block+) ;ENUM
\   header-clause = POLICY policy-name | DERIVE derive-name+
\   variant-block = VARIANT variant-name (FIELD field-name type-expr)* ;VARIANT
: ENUM ( -- ) TYPE-DECL:DECLARE-ENUM ;

;using
;using
;using
