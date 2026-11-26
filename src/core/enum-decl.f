\ enum-decl.f: the ENUM front end (package ENUM-DECL), grammar loop and reject
\ dispatch only. DECL-FRONT (generated-declaration.f) owns the token source and
\ the guarded run; DECL-EVENT (decl-event.f) owns the events, field and variant
\ rows and the current-variant selector. The constructor participant
\ (generated-declaration.f) builds constructors at commit, so a reject publishes
\ none. Loads after decl-event.f and structure-decl.f.

using TYPE-DECL

package TYPE-DECL
private
package ENUM-DECL
using DECL-FRONT
EXPORT TFAM:TFAM-FLD-RANGE!

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

: ED-RESET ( -- )
   0 SEEN-VARIANT !  STRUCTURE-DECL:SELF-PTR-RESET ;
ED-RESET

\ The reserved list is TYPE-NAME:FAMILY-RESERVED?, the one every family definer
\ asks (the ENUM openers are grammar keywords there): a copy kept here drifted
\ twice. Case and duplicate are TFAM-DECL's.

: REQUIRE-NAME ( str -- )
   dup STR:LEN 0= IF drop
      s" missing name"
      s" write the family name after `ENUM`, as in `ENUM color red green ;ENUM`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   dup DEF-NAME-BAD? IF drop s" " s" " TFAM:E-TDECL-DEF-NAME DECL-REJECT:REJECT throw THEN
   dup TF-CANON? 0= IF
      drop s" name must be a lowercase family tail"
      s" write the family name as a lowercase tail with no package qualifier, as in `color`"
      TFAM:E-TFAM-CASE DECL-REJECT:REJECT throw THEN
   TYPE-NAME:FAMILY-RESERVED? IF
      s" reserved name"
      s" rename the family: single letters, `ptr` and value records are reserved"
      E-TDECL-NAME DECL-REJECT:REJECT throw THEN ;

: RESOLVE-TYPE ( str n -- n ) {: want:n :}
   dup STR:LEN 0= IF drop
      s" missing payload type"
      s" write a type after the field name, as in `FIELD x n`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   dup DECL-REJECT:TOKEN!
   s" ;variant" s" payload type" STRUCTURE-DECL:SD-SPAN
   FAM @ want s" payload type" STRUCTURE-DECL:SD-TYPE-NODE ;

: SCH-WIDTH ( n -- n ) FAM @ swap TFAM-SCH-WIDTH ;

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
   code DECL-EVENT:POLICY ;

: DERIVE-FEATURE? ( str -- bool )
   dup s" eq" STR:=CI IF drop YES EXIT THEN
   dup s" hash" STR:=CI IF drop YES EXIT THEN
   dup s" tag" STR:=CI IF drop YES EXIT THEN
   dup s" name" STR:=CI IF drop YES EXIT THEN
   s" order" STR:=CI ;

\ The first token after the header clauses picks the mode: VARIANT starts full
\ blocks, a bare name a compact body; a parameter list forces full mode, and the
\ modes never mix. The engine and the replay reader both return spans in one
\ buffered source, so the prefix is kept as a span and re-read through
\ DECL-FRONT's token source (HEAD!): registration fixes the kind before any clause
\ applies, and diagnostics keep their bytes.
: ED-LOOK-END? ( str -- bool )
   dup STR:LEN 0= IF drop YES EXIT THEN
   s" ;enum" STR:=CI ;
: ED-FIRST-VARIANT ( str -- str )
   BEGIN
      dup s" policy" STR:=CI  over s" derive" STR:=CI or IF
         drop SOURCE-NEXT
         dup ED-LOOK-END? IF EXIT THEN
         drop SOURCE-NEXT
      ELSE
         \ OPAQUE may be a compact variant or a forbidden header before
         \ VARIANT blocks. Keep it in the lookahead and inspect the next token.
         dup s" opaque" STR:=CI IF drop SOURCE-NEXT ELSE EXIT THEN
      THEN
   AGAIN ;
: ED-FULL? ( -- bool )
   SOURCE-NEXT {: first:str :}
   first ED-FIRST-VARIANT {: last:str :}
   first STR:DATA {: a:^ :}
   last STR:DATA last STR:LEN + a - {: u:n :}
   first STR:LEN 0 <> IF a u >str HEAD! THEN
   last s" variant" STR:=CI ;
: DERIVE-GUARD ( -- )
   FAM @ TFAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family"
      s" drop the type parameters or the `DERIVE` clause: only an arity-0 family derives"
      E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN ;
: DERIVE-ONE ( str -- )
   DERIVE-GUARD
   dup s" eq" STR:=CI IF drop FAM @ TFAM-DERIVE-EQ! DRV-EQ DECL-EVENT:DERIVE EXIT THEN
   dup s" hash" STR:=CI IF drop FAM @ TFAM-DERIVE-HASH! DRV-HASH DECL-EVENT:DERIVE EXIT THEN
   dup s" tag" STR:=CI IF drop FAM @ TFAM-DERIVE-TAG! DRV-TAG DECL-EVENT:DERIVE EXIT THEN
   dup s" name" STR:=CI IF drop FAM @ TFAM-DERIVE-NAME! DRV-NAME DECL-EVENT:DERIVE EXIT THEN
   dup s" order" STR:=CI IF
      drop s" derive feature not yet supported"
      s" derive `eq`, `hash`, `tag` or `name` instead: `order` is not implemented"
      E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   drop s" unknown derive feature"
   s" name `eq`, `hash`, `tag` or `name` after `DERIVE`"
   E-TDECL-DERIVE DECL-REJECT:REJECT throw ;
: DERIVE-CLAUSE ( -- )
   HEADER-ORDER
   NEXT dup STR:LEN 0= IF
      drop s" missing derive feature"
      s" name `eq`, `hash`, `tag` or `name` after `DERIVE`"
      E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
   NEXT dup STR:LEN 0= IF drop EXIT THEN
   dup DERIVE-FEATURE? IF
      DECL-REJECT:TOKEN!
      TDECL-DERIVE-ONE-WHY E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   UNGET ;

: ED-FIELD-FIX$ ( -- str ) s" give each field of the variant its own name" ;

\ Full mode: DECL-EVENT:VARIANT registers the SUMV row and pins the current-variant
\ selector the FIELD events carry; END-VARIANT clears it. Each variant's payload
\ restarts at slot 0 (VCELLS); the family width is the widest variant (MAXSLOTS).
: EMIT-FIELD ( str n -- )
   SCHEMA-ROOT+ {: sch:n :}
   sch SCHEMA-ROOT@ SCH-WIDTH {: fw:n :}
   dup DECL-REJECT:TOKEN!
   s" duplicate field name"
   ED-FIELD-FIX$ TFAM:E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ swap sch
   VCELLS @  fw  VCELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD
   fw VCELLS @ + VCELLS !
   NFLD @ 1 + NFLD ! ;
: FIELD-CLAUSE ( -- )
   NEXT dup STR:LEN 0= IF
      drop s" missing field name"
      s" write a name and a type after `FIELD`, as in `FIELD x n`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   {: ns:str :}
   OUTCOME-NAMES @ 0 <> IF
      ns ED-FIELD-FIX$ NAME-CHECK THEN
   ns DECL-FIELD!
   0 FIELD-MISS !
   NEXT {: ts:str :}
   ts PK-CELL RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF
      ts FAM @ node REQUIRE-DERIVABLE
      ns node EMIT-FIELD THEN ;

: VARIANT-NAME ( -- str )
   NEXT dup STR:LEN 0= IF
      drop s" missing variant name"
      s" write a name after `VARIANT`, as in `VARIANT quit ;VARIANT`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN ;
\ Records variant NAME, rejected at its token when a word DERIVE generates
\ takes its name or a variant already has it. DERIVE precedes every variant,
\ so the family's derive flags are set here.
: VARIANT-START ( str -- ) {: ns:str :}
   ns DECL-REJECT:TOKEN!
   FAM @ TFAM-DERIVE-ANY? ns TFAM-DERIVED-TAIL? and IF
      TDECL-COLLIDE-WHY E-TDECL-NAME DECL-REJECT:REJECT throw THEN
   s" duplicate variant"
   s" give each variant of the enum its own name"
   TFAM:E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ ns DECL-EVENT:VARIANT ;
: OPEN-VARIANT ( -- )
   SEEN-CLEAR
   VARIANT-NAME VARIANT-START
   0 VCELLS ! ;
: CLOSE-VARIANT ( -- )
   VCELLS @ MAXSLOTS @ max MAXSLOTS !
   DECL-EVENT:END-VARIANT
   NVAR @ 1 + NVAR ! ;
: VARIANT-CLAUSE ( -- bool )            \ read one variant-body token; YES = ;VARIANT
   NEXT dup STR:LEN 0= IF drop
      s" missing ;VARIANT"
      s" close each variant block with `;VARIANT`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   dup s" ;variant" STR:=CI IF drop YES EXIT THEN
   dup s" field" STR:=CI IF drop FIELD-CLAUSE NO EXIT THEN
   drop s" unexpected token in variant block"
   s" write only `FIELD name type` clauses before `;VARIANT`"
   E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;
: VARIANT-BODY ( -- ) BEGIN VARIANT-CLAUSE UNTIL ;
: VARIANT-BLOCK ( -- )
   -1 SEEN-VARIANT !
   OPEN-VARIANT VARIANT-BODY CLOSE-VARIANT ;

\ Compact body: full-mode block keywords always reject; a header after a variant
\ rejects through HEADER-ORDER.
: COMPACT-KW? ( str -- bool )
   dup s" variant" STR:=CI IF drop YES EXIT THEN
   dup s" ;variant" STR:=CI IF drop YES EXIT THEN
   s" field" STR:=CI ;
: COMPACT-VARIANT ( str -- )
   {: ns:str :}
   -1 SEEN-VARIANT !
   ns VARIANT-START
   DECL-EVENT:END-VARIANT
   NVAR @ 1 + NVAR ! ;
: COMPACT-CLAUSE ( str -- bool )    \ dispatch one compact token; YES = ;ENUM
   dup DECL-REJECT:TOKEN!               \ the mode-selecting token arrives pre-read
   dup s" ;enum" STR:=CI IF drop YES EXIT THEN
   dup COMPACT-KW? IF
      drop s" block keyword in a compact enum"
      s" write bare variant names only, or every variant as a `VARIANT name ;VARIANT` block"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   dup s" policy" STR:=CI IF drop POLICY-CLAUSE NO EXIT THEN
   dup s" derive" STR:=CI IF drop DERIVE-CLAUSE NO EXIT THEN
   COMPACT-VARIANT NO ;

\ The kind is fixed at registration, before any variant parses, as TFAM-DECL sets it once: compact
\ registers TK-ENUM (arity 0), full TK-SUM with the declared arity; both have one width at zero payload.
: OPEN-TX ( -- )
   SUMV-N@ VBASE !
   TYPE-FIELD:COUNT FLDBASE !
   0 NVAR !   0 NFLD !   0 MAXSLOTS ! ;
: REGISTER-FULL ( str n -- )
   {: ns:str ar:n :}
   ns DECL-REJECT:TOKEN!
   s" duplicate family"
   s" rename the enum: this package already declares a family by that name"
   TFAM:E-TFAM-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ TDECL-VIS ns
   ar TK-SUM TFAM-DECL FAM !
   OPEN-TX ;
: REGISTER-COMPACT ( str -- )
   {: ns:str :}
   ns DECL-REJECT:TOKEN!
   s" duplicate family"
   s" rename the enum: this package already declares a family by that name"
   TFAM:E-TFAM-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ TDECL-VIS ns
   0 TK-ENUM TFAM-DECL FAM !
   OPEN-TX ;

\ The variants' fields are still events until commit, so the niche-null shape
\ is read through DECL-EVENT.
: NICHE-SHAPE? ( -- bool )
   NVAR @ 2 <> IF NO EXIT THEN
   VBASE @ DECL-EVENT:PAYLOAD-N 0 <> IF NO EXIT THEN
   VBASE @ 1 + DECL-EVENT:PAYLOAD-N 1 <> IF NO EXIT THEN
   VBASE @ 1 + 0 DECL-EVENT:PAYLOAD-SCHEMA@
   SCHEMA-ROOT@ SCHEMA-PTR? ;
: NICHE-REQUIRE ( -- )
   FAM @ TFAM-NICHE? 0= IF EXIT THEN
   NICHE-SHAPE? IF EXIT THEN
   TDECL-NICHE-MESSAGE$
   s" declare an empty variant, then one with a single `^` field, or drop `POLICY niche-null`"
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
\ one gate for arming here; both modes pass it, since a compact payloadless enum
\ publishes constructors too.
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
      FLDBASE @ i + PF-SCH@ SCHEMA-ROOT@
      s" payload type is a pointer to a linear value and cannot own it"
      DECL-EVENT:REQUIRE-POINTEES
   loop
   FAM @ MAXSLOTS @ TFAM-SLOTS!
   FAM @ TDECL-LAYOUT-DESC
   FAM @ GENERATED-DECL-CTOR:OWNS? IF FAM @ GENERATED-DECL-CTOR:ARM THEN ;

: FULL-CLAUSE ( -- bool )               \ read + dispatch one full-body token; YES = ;ENUM
   NEXT dup STR:LEN 0= IF drop
      DECL-REJECT:AT-FAMILY
      s" missing ;ENUM"
      s" end the declaration with `;ENUM`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   dup s" ;enum" STR:=CI IF drop YES EXIT THEN
   dup s" policy" STR:=CI IF drop POLICY-CLAUSE NO EXIT THEN
   dup s" derive" STR:=CI IF drop DERIVE-CLAUSE NO EXIT THEN
   dup s" variant" STR:=CI IF drop VARIANT-BLOCK NO EXIT THEN
   drop s" unexpected token in enum declaration"
   s" write only `POLICY`, `DERIVE` and `VARIANT` clauses before `;ENUM`"
   E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;
: FULL-BODY ( -- ) BEGIN FULL-CLAUSE UNTIL ;

: COMPACT-BODY ( str -- )
   BEGIN
      dup STR:LEN 0= IF drop
         DECL-REJECT:AT-FAMILY
         s" missing ;ENUM"
         s" end the declaration with `;ENUM`"
         E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
      COMPACT-CLAUSE IF EXIT THEN
      NEXT
   AGAIN ;

: DRIVE ( -- )
   NAME-TOKEN dup DECL-REJECT:FAMILY!
   dup STR:LEN 0= IF drop
      s" missing name"
      s" write the family name after `ENUM`, as in `ENUM color red green ;ENUM`"
      E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   dup DECL-REJECT:SEAL-FAMILY
   DECL-HEADER 0= IF
      drop drop s" invalid type parameter header"
      s" list distinct parameter letters in angle brackets, as in `box<a>` or `result<a,b>`"
      E-TDECL-ARITY DECL-REJECT:REJECT throw
   THEN
   {: ar:n :}
   dup DECL-REJECT:FAMILY!
   dup REQUIRE-NAME
   {: ns:str :}
   ED-FULL? ar 0 <> or IF
      ns ar REGISTER-FULL
      FULL-BODY
   ELSE
      ns REGISTER-COMPACT
      NEXT COMPACT-BODY
   THEN
   ED-CLOSE ;

: ED-FRONT ( -- str str [ -- parses nominal ] )
   s" enum" s" ;enum" [: ED-RESET DRIVE ;] ;

: DECLARE-ENUM ( -- ) ED-FRONT DECL-FRONT:RUN ;

public

\ Registers an ENUM from a body a tool lexed, by DECLARE-ENUM's grammar, defining
\ no word; a body without ;ENUM is rejected "missing ;ENUM".
: REPLAY-ENUM ( str str -- str n ) {: ns:str bs:str :}
   ns bs ED-FRONT DECL-FRONT:REPLAY-OUTCOME
   dup 0 <> IF ns DECL-FAMILY-NAME NO STRUCTURE-DECL:LEFT-MARK THEN ;

;package

EXPORT ENUM-DECL:DECLARE-ENUM
EXPORT ENUM-DECL:REPLAY-ENUM

global
\ ENUM is a documented global (package-first exception, like STRUCTURE) and the
\ only ENUM parser; it parses its body to ;ENUM at interpret time, so its checked
\ effect is ( -- ).
\   ENUM type-name[<params>] header-clause* (variant-name+ | variant-block+) ;ENUM
\   header-clause = POLICY policy-name | DERIVE derive-name
\   variant-block = VARIANT variant-name (FIELD field-name type-expr)* ;VARIANT
: ENUM ( -- ) DECLARE-ENUM ;
INTRINSIC-DECLARATION INTRINSIC
;package

;using
