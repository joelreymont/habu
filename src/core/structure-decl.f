\ structure-decl.f: the STRUCTURE front end (package STRUCTURE-DECL), grammar loop
\ and reject dispatch only. DECL-FRONT (generated-declaration.f) owns the token
\ source and the guarded run; DECL-EVENT (decl-event.f) owns every event.
\   STRUCTURE type-name[<params>] (POLICY p | DERIVE f+ | OPAQUE)*
\      (FIELD name type | BUFFER name bytes)* ;STRUCTURE
\ A reject rolls the whole declaration back to a byte-identical registry. Loads
\ after decl-event.f and structure-make.f, whose GENERATE and ARM ;STRUCTURE calls.

using SCHEMA-REG
using TFAM

package TYPE-DECL
private
package STRUCTURE-DECL
using DECL-FRONT

\ Transient parse state, not declaration state: STRUCTURE is top-level
\ interpret-only, so one declaration runs at a time.
variable FAM        \ family id being declared
variable FLDBASE    \ committed field high-water at open (field range start)
variable NFLD       \ field count in this declaration
variable SD-CELLS      \ running field cell width (next field's slot / byte offset)
variable SD-ITEMS   \ MAKE's inputs so far: one per FIELD, one per buffer cell
variable SEEN-FIELD \ a FIELD has appeared (header clauses must precede fields)
variable POINTEE    \ resolving the pointee of `^`: the declared family may be named
PTR-VARIABLE BASE-A   variable BASE-U   \ a wrap's base token
PTR-VARIABLE NAME-A   variable NAME-U   \ a wrap's name as written

$100 constant SD-ARG-CAP
create SD-ARGS SD-ARG-CAP cells allot
variable SD-ARG-N

\ The first pointee naming the declared family while it takes no scope or
\ region, as written; a later scope or region parameter is rejected there.
PTR-VARIABLE SELF-A   variable SELF-U

public
: SELF-PTR-RESET ( -- ) 0 SELF-U ! ;
private

: SD-RESET ( -- )
   0 SEEN-FIELD !  SELF-PTR-RESET ;
SD-RESET
: SD-MISSING! ( -- ) TYPE@ MISSING! ;

\ The node parser's rejections name the type as its caller does ("type",
\ "payload type"); SD-TYPE-NODE sets the noun for one parse.
PTR-VARIABLE NOUN-A   variable NOUN-U
: NOUN@ ( -- ^u8 n ) NOUN-A @ NOUN-U @ ;

\ A rejection's label is composed here, grown to fit, and read when the record
\ prints, so the next rejection may reuse the buffer.
$80 constant SD-MSG-FIRST               \ bytes of the first mapping
PERSISTED-PTR-VARIABLE SD-MSG-P   NULL-PTR SD-MSG-P !
variable SD-MSG-CAP   0 SD-MSG-CAP !    \ bytes mapped at SD-MSG-P, 0 for none
variable SD-MSG-U
: SD-MSG+ ( ^u8 n -- ) {: a:^ u:n :}
   SD-MSG-P SD-MSG-CAP SD-MSG-FIRST SD-MSG-U @ u + ARENA-ROOM
   a SD-MSG-P @ SD-MSG-U @ + u ARENA-COPY
   SD-MSG-U @ u + SD-MSG-U ! ;
: SD-WORD+ ( ^u8 n -- ) {: a:^ u:n :}   \ a blank, then the word
   u 0= IF EXIT THEN
   s"  " SD-MSG+  a u SD-MSG+ ;

\ The text before the noun, the noun, the text after it, blank-separated.
: SD-NOUN-TEXT ( ^u8 n ^u8 n ^u8 n -- ^u8 n ) {: pa:^ pu:n na:^ nu:n sa:^ su:n :}
   0 SD-MSG-U !
   pa pu SD-MSG+
   pu 0= IF na nu SD-MSG+ ELSE na nu SD-WORD+ THEN
   sa su SD-WORD+
   SD-MSG-P @ SD-MSG-U @ ;
: SD-TEXT ( ^u8 n ^u8 n -- ^u8 n ) {: pa:^ pu:n sa:^ su:n :}   \ with the noun SD-TYPE-NODE set
   pa pu NOUN@ sa su SD-NOUN-TEXT ;
: SD-REJECT ( ^u8 n ^u8 n ^u8 n n -- n ) {: pa:^ pu:n sa:^ su:n fa:^ fu:n code:n :}
   pa pu sa su SD-TEXT fa fu code DECL-REJECT:REJECT ;

: SD-NAME-FIX$ ( -- ^u8 n ) s" write the family name after `STRUCTURE`, as in `STRUCTURE point`" ;
: SD-LONG-FIX$ ( -- ^u8 n ) s" shorten the name to 255 bytes or fewer" ;
: DEFTYPE? ( -- bool ) DECL-REJECT:KIND$ s" deftype" CORE-STR= ;
: ENUM? ( -- bool ) DECL-REJECT:KIND$ s" enum" CORE-STR= ;
\ A NEWTYPE or DEFTYPE: one base on one line, with no header and no clause.
: WRAP? ( -- bool ) DEFTYPE? DECL-REJECT:KIND$ s" newtype" CORE-STR= or ;
\ A DEFTYPE base is one name (WRAP-DRIVE), so its help offers no `^` or header.
: SD-TYPE-FIX$ ( -- ^u8 n )
   DEFTYPE? IF
      s" name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`" EXIT THEN
   WRAP? IF
      s" name a type declared before this declaration: `n`, `^u8`, a `STRUCTURE` or an `ENUM`" EXIT THEN
   s" name a type declared before this declaration: `n`, `^u8`, a header parameter, a `STRUCTURE` or an `ENUM`" ;
: SD-PARAM-FIX$ ( -- ^u8 n )
   WRAP? IF SD-TYPE-FIX$ EXIT THEN
   ENUM? IF
      s" use `n`, `f` or `r`, or a type parameter the header declares, as in `ENUM box<a>`" EXIT THEN
   s" list the parameter in the header, as in `STRUCTURE box<a>`, before a field names it" ;
\ NEWTYPE-HELP's text, mapped on first use and grown to fit: it names the case.
$100 constant HELP-FIRST                \ bytes of the first mapping
PERSISTED-PTR-VARIABLE HELP-P   NULL-PTR HELP-P !
variable HELP-CAP   0 HELP-CAP !        \ bytes mapped at HELP-P, 0 for none
variable HELP-U

: HELP-C+ ( n -- ) {: c:n :}
   HELP-P HELP-CAP HELP-FIRST HELP-U @ 1 + ARENA-ROOM
   c HELP-P @ HELP-U @ + c!
   HELP-U @ 1 + HELP-U ! ;
: HELP+ ( ^u8 n -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ HELP-C+ loop ;
\ The name at a u as its declaration folds it.
: HELP-FOLD+ ( ^u8 n -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ CORE-FOLD-C HELP-C+ loop ;

\ The NEWTYPE a DEFTYPE NAME over BASE becomes, NAME folded to its tail, its
\ closing "` instead" left to the caller.
: NEWTYPE-HELP ( ^u8 n ^u8 n -- ) {: na:^ nu:n ba:^ bu:n :}
   0 HELP-U !
   s" a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE " HELP+
   na nu HELP-FOLD+
   s"  " HELP+  ba bu HELP+ ;
\ The edit the loader accepts for `DEFTYPE NAME BASE` whose NAME and BASE the
\ wrap accepted: the NEWTYPE over BASE as written.
: BASE-HELP$ ( ^u8 n ^u8 n -- ^u8 n )
   NEWTYPE-HELP  s" ` instead" HELP+  HELP-P @ HELP-U @ ;
\ NEWTYPE-HELP over BASE up to its token at ta, which the caller then rewrites.
: HELP-HEAD ( ^u8 n ^u8 ^u8 -- ) {: na:^ nu:n ba:^ ta:^ :}
   na nu ba  ta NULL-PTR - ba NULL-PTR - -  NEWTYPE-HELP ;
\ BASE after its token ta tu, then the help's close.
: HELP-TAIL$ ( ^u8 n ^u8 n -- ^u8 n ) {: ba:^ bu:n ta:^ tu:n :}
   ta NULL-PTR - tu + ba NULL-PTR - - {: hu:n :}
   ba hu + bu hu - HELP+  s" ` instead" HELP+
   HELP-P @ HELP-U @ ;
\ The edit the loader accepts for `DEFTYPE NAME BASE` whose token TOK names bare a
\ family of arity ar taking `n` throughout: the NEWTYPE of BASE, `<n,...>` after TOK.
: ARGS-HELP$ ( ^u8 n ^u8 n ^u8 n n -- ^u8 n ) {: na:^ nu:n ba:^ bu:n ta:^ tu:n ar:n :}
   na nu ba ta HELP-HEAD  ta tu HELP+  s" <n" HELP+
   ar 1 ?do s" ,n" HELP+ loop
   s" >" HELP+  ba bu ta tu HELP-TAIL$ ;
\ The edit the loader accepts for a DEFTYPE whose base names `width` at ta inside
\ a longer base: the NEWTYPE of that base, `n` for the width.
: WIDTH-HELP$ ( ^u8 n -- ^u8 n ) {: ta:^ tu:n :}
   DECL-REJECT:FAMILY$ BASE-A @ ta HELP-HEAD  s" n" HELP+
   BASE-A @ BASE-U @ ta tu HELP-TAIL$ ;

\ A family whose parameters are all cells: `n` fills each (`option<n>` loads,
\ `width<n>` and `init<n,n>` do not).
: CELL-PARAMS? ( n -- bool ) {: fam:n :}
   YES fam TFAM-ARITY@ 0 ?do fam i TFAM-PK@ PK-CELL = and loop ;
\ Whether the token at a is a DEFTYPE's whole base, which a help naming one
\ type may replace.
: WHOLE-BASE? ( ^u8 n -- bool ) {: a:^ u:n :}
   DEFTYPE? a BASE-A @ = and u BASE-U @ = and ;
\ The help for family fam named bare at a: a DEFTYPE gets the NEWTYPE that
\ loads, or another base when no `n` arguments fit.
: SD-ARGS-FIX$ ( n ^u8 n -- ^u8 n ) {: fam:n a:^ u:n :}
   DEFTYPE? 0= IF s" give the family its type arguments, as in `box<n>`" EXIT THEN
   fam CELL-PARAMS? 0= IF SD-TYPE-FIX$ EXIT THEN
   DECL-REJECT:FAMILY$ BASE-A @ BASE-U @ a u fam TFAM-ARITY@ ARGS-HELP$ ;
: SD-APP-FIX$ ( -- ^u8 n )
   s" write a type application as `family<type,type>`, as in `SPAN:span<^u8>`" ;
\ The help for a malformed pointer token at a: a DEFTYPE's whole base gets the base
\ help, a longer base the NEWTYPE over `^T`, a wrap, payload or field the spelling.
: SD-PTR-FIX$ ( ^u8 n -- ^u8 n )
   WHOLE-BASE? IF SD-TYPE-FIX$ EXIT THEN
   DEFTYPE? IF
      DECL-REJECT:FAMILY$ {: na:^ nu:n :}
      na nu s" ^T" NEWTYPE-HELP
      s" ` instead, `^` joined to the type it points at, as in `NEWTYPE " HELP+
      na nu HELP-FOLD+
      s"  ^u8`" HELP+  HELP-P @ HELP-U @ EXIT THEN
   WRAP? IF
      s" spell a pointer `^T`, `^` joined to the type it points at, as in `NEWTYPE link ^u8`" EXIT THEN
   ENUM? IF
      s" spell a pointer `^T`, `^` joined to the type it points at, as in `FIELD next ^list`" EXIT THEN
   s" spell a pointer `^T`, `^` joined to the type it points at, as in `FIELD next ^node`" ;
: SD-OPEN-FIX$ ( -- ^u8 n )
   WRAP? IF s" close each `<` of the type with `>`" EXIT THEN
   s" close each `<` of the type with `>` before the next clause" ;

\ The reserved list is TYPE-NAME:FAMILY-RESERVED?, the one every family definer
\ asks: a copy kept here drifted twice (it admitted `if`, then value-record names).
\ Case and duplicate are TFAM-DECL's.
: NAME-RESERVED? ( ^u8 n -- bool )
   2dup s" structure" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop YES EXIT THEN
   TYPE-NAME:FAMILY-RESERVED? ;

: REQUIRE-NAME ( ^u8 n -- )
   dup 0= IF 2drop s" missing name" SD-NAME-FIX$ TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop TF-NAME-LONG$ SD-LONG-FIX$ TYPE-DECL:E-TDECL-CAP DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail"
      s" write the name as a lowercase tail with no package qualifier, as in `point`"
      E-TFAM-CASE DECL-REJECT:REJECT throw THEN
   NAME-RESERVED? IF s" reserved name"
      s" pick another name: a built-in type such as `n` or a keyword such as `if` cannot name a family"
      TYPE-DECL:E-TDECL-NAME DECL-REJECT:REJECT throw THEN ;

: NAME-GATE ( ^u8 n -- )         \ the packet names the family, then REQUIRE-NAME judges it
   2dup DECL-REJECT:FAMILY! REQUIRE-NAME ;

: SD-KIND$ ( -- ^u8 n ) s" structure" ;

\ A family that owns a linear value is an accepted field type: the structure owns
\ the obligation by containment (TFAM-CONCRETE-LINEAR? walks into it), the rule a
\ field naming a DEFLINEAR con already relies on.
\ The kind test falls through to "unknown <noun>" because the one kind it
\ excludes, TK-EVIDENCE, has no declarer source can write.
\ The declared family may be named only beneath a `^` (`^node`, `SPAN:span<^node>`);
\ by value, as in `option<node>`, it is 7109.
: FIELD-FAM? ( ^u8 n -- n bool ) {: a:^ u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE {: id:n sr:n :}
   sr SR-AMBIG = IF a u DECL-REJECT:TOKEN!  a u TYPE-DECL:TDECL-AMBIG throw THEN
   sr SR-FOUND <> IF 0 NO EXIT THEN
   id UNSTORED-REASON IF
      a u DECL-REJECT:TOKEN!
      id WIDTH-FAM @ = DEFTYPE? and a u WHOLE-BASE? 0= and IF 2drop a u WIDTH-HELP$ THEN
      TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN 2drop 2drop
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 NO EXIT THEN
   id FAM @ = POINTEE @ 0= and IF
      a u DECL-REJECT:TOKEN!
      s" " s" cannot recursively name its owner"
      DEFTYPE? IF SD-TYPE-FIX$ ELSE WRAP? IF
         s" name the type being declared only beneath `^`, as in `NEWTYPE link ^link`"
      ELSE ENUM? IF
         s" name the enum being declared only beneath `^`, as in `FIELD next ^list`"
      ELSE
         s" name the structure being declared only beneath `^`, as in `FIELD next ^node`"
      THEN THEN THEN
      TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   id YES ;

: USE-KIND$ ( n -- ^u8 n )
   dup PK-CELL = IF drop s" a value type" EXIT THEN
   dup PK-TYPE = IF drop s" an element type" EXIT THEN
   dup PK-SCOPE = IF drop s" a scope" EXIT THEN
   PK-REGION = IF s" a region" EXIT THEN
   s" a pointee or quotation element" ;
\ Rejects the parameter at a u used as want, which field fa fu used as kind.
: PARAM-KIND-REJECT ( n ^u8 n ^u8 n n -- ) {: kind:n fa:^ fu:n a:^ u:n want:n :}
   a u DECL-REJECT:TOKEN!
   0 SD-MSG-U !
   s" `" SD-MSG+  a u SD-MSG+  s" ` is " SD-MSG+  want USE-KIND$ SD-MSG+
   s"  here, but field `" SD-MSG+  fa fu SD-MSG+  s" ` uses it as " SD-MSG+
   kind USE-KIND$ SD-MSG+
   SD-MSG-P @ SD-MSG-U @
   ENUM? IF
      s" give each kind its own header parameter, as in `ENUM box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`"
   ELSE
      s" give each kind its own header parameter, as in `STRUCTURE box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`"
   THEN
   TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

\ The declared family cannot be held by value instead, so its help moves the view out.
: PTR-VIEW-FIX$ ( bool -- ^u8 n ) {: self:bool :}
   self ENUM? and IF
      s" keep the view out of the family a pointer reaches, as in `ENUM node<a>` with `FIELD next ^node<a>`, and `ENUM walk<a,p,q>` with `FIELD at ^node<a>` and `FIELD w read-view<p,q,n>`" EXIT THEN
   self IF
      s" keep the view out of the family a pointer reaches, as in `STRUCTURE node<a>` with `FIELD next ^node<a>`, and `STRUCTURE walk<a,p,q>` with `FIELD at ^node<a>` and `FIELD w read-view<p,q,n>`" EXIT THEN
   WRAP? IF
      s" declare a STRUCTURE whose field holds the view, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`" EXIT THEN
   ENUM? IF
      s" hold the view in the field itself, as in `ENUM box<p,q>` with `FIELD x read-view<p,q,n>`" EXIT THEN
   s" hold the view in the field itself, as in `STRUCTURE box<p,q>` with `FIELD x read-view<p,q,n>`" ;
\ Rejects a pointer reaching the family named at a u, which takes a scope or region;
\ self is whether that family is the declared one.
: PTR-VIEW-REJECT ( ^u8 n bool -- ) {: a:^ u:n self:bool :}
   a u DECL-REJECT:TOKEN!
   0 SD-MSG-U !
   s" a pointer cannot reach `" SD-MSG+  a u SD-MSG+
   s" `, which takes a scope or region" SD-MSG+
   SD-MSG-P @ SD-MSG-U @  self PTR-VIEW-FIX$
   TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

: LETTER-TYPE ( ^u8 n n -- n ) {: a:^ u:n want:n :}
   a c@
   dup ASCII-N = IF drop CC-N SCHEMA-CON EXIT THEN
   dup ASCII-F = IF drop CC-BOOL SCHEMA-CON EXIT THEN
   dup ASCII-R = IF drop CC-R SCHEMA-CON EXIT THEN
   TYPE-DECL:DECL-PARAM 0= IF
      drop a u DECL-REJECT:TOKEN!
      s" unknown" s" " SD-PARAM-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   {: idx:n :}
   want PK-SCOPE = want PK-REGION = or SELF-U @ 0 <> and IF
      SELF-A @ SELF-U @ YES PTR-VIEW-REJECT THEN
   idx want TYPE-DECL:DECL-PARAM-USE IF a u want PARAM-KIND-REJECT THEN
   2drop drop
   want PK-SCOPE = want PK-REGION = or
   want PK-TYPE = or IF
      FAM @ idx want TFAM-PK! THEN
   idx SCHEMA-PARAM ;

: SD-ARG+ ( n -- )
   SD-ARG-N @ SD-ARG-CAP >= IF
      s" " s" application is too deep"
      s" nest fewer than 256 type arguments in one type"
      TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   SD-ARG-N @ cells SD-ARGS + !
   SD-ARG-N @ 1 + SD-ARG-N ! ;

: SD-BLANK? ( n -- bool ) 33 < ;
: SD-DELIM? ( n -- bool )
   dup 60 = over 44 = or over 62 = or swap SD-BLANK? or ;

\ Each parser word takes the index it starts at and answers the index after what it
\ consumed; a type spelled over tokens (SD-SPAN) holds blanks.
: SD-ATOM ( ^u8 n n -- ^u8 n n ) {: a:^ u:n start:n :}
   start
   BEGIN
      dup u < IF a over + c@ SD-DELIM? 0= ELSE NO THEN
   WHILE
      1 +
   REPEAT {: j:n :}
   j start = IF
      s" malformed" s" application"
      SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a start + j start - j ;

\ The blanks an argument may start with.
: SD-SKIP ( ^u8 n n -- n ) {: a:^ u:n i:n :}
   i BEGIN
      dup u < IF a over + c@ SD-BLANK? ELSE NO THEN
   WHILE
      1 +
   REPEAT ;

\ Whether the atom ending at j opens a pointer, and the index its pointee is read
\ from: the byte after `^`. An atom `ptr` not followed by `<` names no type.
: SD-POINTEE-AT ( ^u8 n ^u8 n n -- n bool ) {: na:^ nu:n a:^ u:n j:n :}
   na c@ 94 = IF
      nu 1 = IF
         na nu DECL-REJECT:TOKEN!
         s" pointer without a pointee"
         na nu SD-PTR-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
      j nu - 1 + YES EXIT
   THEN
   na nu s" ptr" CORE-STR=CI 0= IF 0 NO EXIT THEN
   j u < IF a j + c@ 60 = ELSE NO THEN IF 0 NO EXIT THEN
   na nu DECL-REJECT:TOKEN!
   s" unknown" s" " na nu SD-PTR-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw ;

defer SD-PARSE-NODE ( ^u8 n n n -- n n )

\ The argument spelled at aa au fills slot k of fam; LETTER-TYPE judges a
\ parameter argument. A wrap reaches no region slot: each follows two scope
\ slots, the first of which rejects it.
: SD-ARG-KIND ( ^u8 n n n n -- ) {: aa:^ au:n fam:n k:n node:n :}
   FIELD-MISS @ IF EXIT THEN
   node SCHEMA-PARAM? IF EXIT THEN
   FAM @ node PF-NODE-KIND? 0= IF drop EXIT THEN
   fam k TFAM-PK@ {: want:n :}
   want PF-KIND-OK? IF EXIT THEN
   aa au DECL-REJECT:TOKEN!
   want PK-REGION = IF
      s" type argument must be a region parameter"
      ENUM? IF
         s" pass a parameter the header lists, as in `ENUM box<p,q,s>` with `FIELD v mut-view<p,q,s,u8>`"
      ELSE
         s" pass a parameter the header lists, as in `STRUCTURE box<p,q,s>` with `FIELD v mut-view<p,q,s,u8>`"
      THEN
   ELSE
      s" type argument must be a scope parameter"
      WRAP? IF
         s" declare a STRUCTURE whose header lists the parameter, as in `STRUCTURE box<p,q>` with `FIELD v read-view<p,q,u8>`"
      ELSE ENUM? IF
         s" pass a parameter the header lists, as in `ENUM box<p,q>` with `FIELD v read-view<p,q,u8>`"
      ELSE
         s" pass a parameter the header lists, as in `STRUCTURE box<p,q>` with `FIELD v read-view<p,q,u8>`"
      THEN THEN
   THEN
   TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

\ Argument k onward of fam's application, the first at i: each is judged against
\ its slot, then the count of arguments and the index after the closing `>`.
: SD-APP-ARGS ( ^u8 n n n n -- n n ) {: a:^ u:n fam:n k:n i:n :}
   k fam TFAM-ARITY@ >= IF
      s" " s" has too many arguments"
      s" drop the extra type arguments: give the family as many as its header declares"
      TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a u i  fam k TFAM-PK@  SD-PARSE-NODE {: node:n e:n :}
   a u i SD-SKIP {: s:n :}
   a s +  e s -  fam k node SD-ARG-KIND
   node SD-ARG+
   a u e SD-SKIP {: j:n :}
   j u >= IF
      s" unclosed" s" application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a j + c@ {: c:n :}
   c 44 = IF a u fam k 1 + j 1 + RECURSE EXIT THEN
   c 62 = IF k 1 + j 1 + EXIT THEN
   s" malformed" s" application" SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw ;

: SD-APP ( ^u8 n n n -- n n ) {: a:^ u:n fam:n i:n :}
   SD-ARG-N @ {: base:n :}
   a u fam 0 i SD-APP-ARGS {: k:n j:n :}
   k fam TFAM-ARITY@ <> IF
      s" " s" has wrong arity"
      s" give the family as many type arguments as its header declares"
      TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   FIELD-MISS @ IF base SD-ARG-N ! 0 j EXIT THEN
   SCHEMA-ROOT-N@ {: start:n :}
   base BEGIN dup SD-ARG-N @ < WHILE
      dup cells SD-ARGS + @ SCHEMA-ROOT+ drop 1 +
   REPEAT drop
   base SD-ARG-N !
   fam start k SCHEMA-APP j ;

defer SD-LOOSE-NODE ( ^u8 n n -- n )

: SD-LOOSE-ARGS ( ^u8 n n -- n ) {: a:^ u:n i:n :}
   a u  a u i SD-LOOSE-NODE  SD-SKIP {: j:n :}
   j u >= IF
      s" unclosed" s" application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a j + c@ {: c:n :}
   c 62 = IF j 1 + EXIT THEN
   c 44 <> IF
      s" malformed" s" application"
      SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a u j 1 + RECURSE ;

: SD-LOOSE-APP ( ^u8 n n -- n ) {: a:^ u:n i:n :}
   i u >= IF
      s" unclosed" s" application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a i + c@ 62 = IF i 1 + EXIT THEN
   a u i SD-LOOSE-ARGS ;

: SD-LOOSE-ATOM ( ^u8 n -- ) {: a:^ u:n :}
   u 1 = IF
      a c@ dup ASCII-N = over ASCII-F = or over ASCII-R = or
      IF drop EXIT THEN
      TFAM-DECL-CHAR>PARAM IF
         drop a c@ TYPE-DECL:DECL-PARAM IF drop EXIT THEN
         drop a u DECL-REJECT:TOKEN!
         s" type parameter is outside the declared arity"
         SD-PARAM-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw
      ELSE drop THEN
   THEN
   a u CON-OF dup 0 <> IF drop EXIT THEN drop
   a u FIELD-FAM? IF {: fam:n :}
      fam TFAM-ARITY@ 0 <> IF
         a u DECL-REJECT:TOKEN!
         s" " s" is parametric and needs type arguments"
         fam a u SD-ARGS-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw
      THEN EXIT
   THEN drop
   a u TYPE-MAY-ARRIVE-XT IF SD-MISSING! EXIT THEN
   a u DECL-REJECT:TOKEN!
   s" unknown" s" " SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw ;

\ The pointee of a pointer argument read from k (SD-POINTEE-AT), with POINTEE
\ set as SD-PTR-NODE sets it.
: SD-LOOSE-PTR ( ^u8 n n -- n ) {: a:^ u:n k:n :}
   POINTEE @ {: outer:n :}
   -1 POINTEE !
   a u k SD-LOOSE-NODE
   outer POINTEE ! ;

\ Beneath a `^`, judges the family fam named at a u before its arguments, which
\ a scope or region slot would reject first.
: SD-REACH ( ^u8 n n -- ) {: a:^ u:n fam:n :}
   POINTEE @ 0= IF EXIT THEN
   fam FAM @ = {: self:bool :}
   fam TFAM-NONVALUE-PARAM? IF a u self PTR-VIEW-REJECT THEN
   self SELF-U @ 0= and IF a SELF-A !  u SELF-U ! THEN ;

\ SD-PARSE-NODE-IMPL's grammar over the arguments of a family that may still arrive
\ (SD-LOOSE-APP), building no application; an empty `<>` passes here and SD-APP refuses it.
: SD-LOOSE-NODE-IMPL ( ^u8 n n -- n ) {: a:^ u:n i:n :}
   a u  a u i SD-SKIP  SD-ATOM {: na:^ nu:n j:n :}
   na nu a u j SD-POINTEE-AT IF {: k:n :} a u k SD-LOOSE-PTR EXIT THEN drop
   j u < IF a j + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? IF
         {: fam:n :}
         na nu fam SD-REACH
         a u fam j 1 + SD-APP nip EXIT
      THEN drop
      na nu TYPE-MAY-ARRIVE-XT 0= IF
         na nu DECL-REJECT:TOKEN!
         s" unknown" s" " SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
      SD-MISSING!
      a u j 1 + SD-LOOSE-APP EXIT
   THEN
   na nu SD-LOOSE-ATOM j ;
: SD-LOOSE-INSTALL ( -- ) [: SD-LOOSE-NODE-IMPL ;] is SD-LOOSE-NODE ;
SD-LOOSE-INSTALL

\ Every declared pointer is built here. A rejection in its pointee that sets no token
\ of its own is placed at the whole type, as one in a family's arguments is.
: SD-PTR-NODE ( ^u8 n n -- n n ) {: a:^ u:n k:n :}
   POINTEE @ {: outer:n :}
   -1 POINTEE !
   a u k TYPE-DECL:ANY-VALUE SD-PARSE-NODE {: node:n e:n :}
   outer POINTEE !
   FIELD-MISS @ IF 0 e EXIT THEN
   node TFCL-NODE? IF a k + e k - DECL-REJECT:TOKEN! THEN
   node s" " s" is a pointer to a linear value and cannot own it" SD-TEXT
   DECL-EVENT:REQUIRE-POINTEE SCHEMA-PTR e ;

\ The node at i of a field type's text: a type parameter, a con, a family
\ application or a pointer argument, with the index after it.
: SD-PARSE-NODE-IMPL ( ^u8 n n n -- n n ) {: a:^ u:n i:n want:n :}
   a u  a u i SD-SKIP  SD-ATOM {: na:^ nu:n j:n :}
   na nu a u j SD-POINTEE-AT IF {: k:n :} a u k SD-PTR-NODE EXIT THEN drop
   j u < IF a j + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? 0= IF
         drop
         na nu TYPE-MAY-ARRIVE-XT 0= IF
            na nu DECL-REJECT:TOKEN!
            s" unknown" s" " SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
         SD-MISSING!
         0 a u j 1 + SD-LOOSE-APP EXIT
      THEN
      {: fam:n :}
      na nu fam SD-REACH
      a u fam j 1 + SD-APP EXIT
   THEN
   nu 1 = IF na nu want LETTER-TYPE j EXIT THEN
   na nu CON-OF dup 0 <> IF SCHEMA-CON j EXIT THEN drop
   na nu FIELD-FAM? IF {: fam:n :}
      fam TFAM-ARITY@ 0 <> IF
         na nu DECL-REJECT:TOKEN!
         s" " s" is parametric and needs type arguments"
         fam na nu SD-ARGS-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
      fam 0 0 SCHEMA-APP j EXIT
   THEN drop
   na nu TYPE-MAY-ARRIVE-XT IF SD-MISSING! 0 j EXIT THEN
   na nu DECL-REJECT:TOKEN!
   s" unknown" s" " SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw ;

: SD-PARSER-INSTALL ( -- ) [: SD-PARSE-NODE-IMPL ;] is SD-PARSE-NODE ;
SD-PARSER-INSTALL

: SD-NEST ( n n -- n ) {: depth:n c:n :}   \ `<` depth after one byte
   c 60 = IF depth 1 + EXIT THEN
   c 62 = IF depth 1 - EXIT THEN
   depth ;
: SD-DEPTH ( n ^u8 n -- n ) {: depth:n a:^ u:n :}
   depth u 0 ?do a i + c@ SD-NEST loop ;

\ One declaration's tokens stand in order in one buffer (parse-name never refills, a replay
\ reads one body); its end or the closer goes back and the type is unclosed.
: SD-SPAN-REST ( ^u8 n n ^u8 n ^u8 n -- ^u8 n ) {: a:^ u:n depth:n ca:^ cu:n na:^ nu:n :}
   u depth
   BEGIN dup 0 > WHILE
      NEXT dup 0= IF YES ELSE 2dup ca cu CORE-STR=CI THEN IF
         UNGET  drop a swap DECL-REJECT:TOKEN!
         s" unclosed" na nu s" application" SD-NOUN-TEXT
         SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
      2swap nip  >r 2dup r> -rot SD-DEPTH  -rot
      swap NULL-PTR - a NULL-PTR - - +  swap
   REPEAT
   drop a swap ;

\ SD-SPAN ( first closer noun -- text ): a type whose first token leaves a `<`
\ open runs to the token closing it, as one span of the source or replay body,
\ which outlives the TYPE@ span SD-MISSING! keeps. The closer token reached
\ first leaves it unclosed, rejected naming the type by noun.
public
: SD-SPAN ( ^u8 n ^u8 n ^u8 n -- ^u8 n ) {: a:^ u:n ca:^ cu:n na:^ nu:n :}
   0 a u SD-DEPTH {: depth:n :}
   depth 1 < IF a u EXIT THEN
   a u depth ca cu na nu SD-SPAN-REST 2dup DECL-REJECT:TOKEN! ;
private

\ The node of a whole type text; bytes after the node are malformed.
: SD-WHOLE-NODE ( ^u8 n n -- n ) {: a:^ u:n want:n :}
   a u 0 want SD-PARSE-NODE {: node:n j:n :}
   j u <> IF
      s" malformed" s" application"
      SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   node ;

\ SD-TYPE-NODE ( text fam want noun -- node ): want is the use of a parameter that is
\ the whole type; FIELD-MISS stays set.
public
: SD-TYPE-NODE ( ^u8 n n n ^u8 n -- n )
   {: a:^ u:n fam:n want:n na:^ nu:n :}
   FAM @ {: ofam:n :}
   TYPE@ NOUN@ {: ota:^ otu:n oa:^ ou:n :}
   fam FAM !  0 POINTEE !  0 SD-ARG-N !
   a u TYPE!  na NOUN-A !  nu NOUN-U !
   [: a u want SD-WHOLE-NODE ;]
   [: ofam FAM !  ota otu TYPE!  oa NOUN-A !  ou NOUN-U ! ;] finally ;
private

defer SD-QUOT-ELEM ( ^u8 n -- n )
: SD-QUOT-FAIL ( ^u8 n n -- ) {: wa:^ wu:n code:n :}
   wa wu
   s" write the quotation type as `[ in -- out ]`, with at most 256 types on each side"
   code DECL-REJECT:REJECT throw ;

\ A field type's text, from its first token (SD-SPAN); a quotation `[` reads the
\ tokens after it.
: RESOLVE-TYPE ( ^u8 n n -- n ) {: want:n :}
   dup 0= IF 2drop s" missing type"
      s" write a type after the field name, as in `FIELD x n`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup DECL-REJECT:TOKEN!
   s" ;structure" s" type" SD-SPAN
   2dup TYPE!
   2dup s" [" CORE-STR= IF
      2drop [: NEXT ;] [: SD-QUOT-ELEM ;] [: SD-QUOT-FAIL ;] TYPE-DECL:PARSE-QUOT EXIT THEN
   FAM @ want s" type" SD-TYPE-NODE ;
: SD-QUOT-INSTALL ( -- ) [: TYPE-DECL:ANY-VALUE RESOLVE-TYPE ;] is SD-QUOT-ELEM ;
SD-QUOT-INSTALL

\ Clause drivers emit through DECL-EVENT, which owns duplicate, ordinal and
\ selector state; each mutates only the fresh family record.
: HEADER-ORDER ( -- )
   SEEN-FIELD @ IF
      s" header clause after the first field"
      s" move the `POLICY`, `DERIVE` and `OPAQUE` clauses before the first `FIELD`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN ;

: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   NEXT POLICY-CODE {: code:n :}
   code TL-NICHE = IF
      TYPE-DECL:TDECL-NICHE-MESSAGE$
      s" drop `POLICY niche-null`: only an `ENUM` with an empty variant, then one `^` field, can use it"
      TYPE-DECL:E-TDECL-POLICY DECL-REJECT:REJECT throw THEN
   FAM @ code TFAM-LAYOUT!
   code DECL-EVENT:POLICY ;

\ OPAQUE keeps the family's visibility and moves its generated words to the
\ private-family placement (TFAM-GEN-PUBLIC? answers placement, TFAM-PUBLIC?
\ visibility).
: OPAQUE-CLAUSE ( -- )
   HEADER-ORDER
   FAM @ DECL-EVENT:OPAQUE
   FAM @ TFAM-OPAQUE! ;

: DERIVE-FEATURE? ( ^u8 n -- bool )
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" addr" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" init" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" tag" CORE-STR=CI IF 2drop YES EXIT THEN
   s" order" CORE-STR=CI ;
\ Visibility is not a condition for a derived surface: it picks the spelling and
\ the wordlist (type-family.f TF-CTOR-PRIV$, rendered by sumtype.f TDGEN-DRV-REF).
: DERIVE-CONCRETE ( -- )                \ eq/hash compare whole values: arity 0 only
   FAM @ TFAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family"
      s" drop `eq` and `hash` from `DERIVE`, or declare the structure with no type parameters"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN ;
\ `addr` has no arity condition: an accessor projects the field schema at the
\ caller's instantiation, and the checker refuses the one instantiation a baked
\ offset would misdescribe (a type argument wider than one cell).
: DERIVE-ONE ( ^u8 n -- )
   2dup s" eq" CORE-STR=CI IF 2drop DERIVE-CONCRETE FAM @ TFAM-DERIVE-EQ! DRV-EQ DECL-EVENT:DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop DERIVE-CONCRETE FAM @ TFAM-DERIVE-HASH! DRV-HASH DECL-EVENT:DERIVE EXIT THEN
   2dup s" addr" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-ADDR! DRV-ADDR DECL-EVENT:DERIVE EXIT THEN
   2dup s" init" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-INIT! DRV-INIT DECL-EVENT:DERIVE EXIT THEN
   2dup s" tag" CORE-STR=CI IF
      2drop s" derive tag requires an enum"
      s" drop `tag` from `DERIVE`: only an `ENUM` derives it"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   2dup s" order" CORE-STR=CI IF
      2drop s" derive feature not yet supported"
      s" drop `order` from `DERIVE` and write the comparison as a word over the fields"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   2drop s" unknown derive feature"
   s" name `eq`, `hash`, `addr` or `init` after `DERIVE`"
   TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw ;
: DERIVE-CLAUSE ( -- )
   HEADER-ORDER
   NEXT dup 0= IF
      2drop s" missing derive feature"
      s" name a feature after `DERIVE`: `eq`, `hash`, `addr` or `init`"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
   BEGIN NEXT dup 0= IF 2drop EXIT THEN
      2dup DERIVE-FEATURE? IF DERIVE-ONE ELSE UNGET EXIT THEN
   AGAIN ;

: EMIT-FIELD ( ^u8 n n -- )
   SCHEMA-ROOT+ {: sch:n :}
   FAM @ sch SCHEMA-ROOT@ TFAM-SCH-WIDTH {: fw:n :}
   2dup DECL-REJECT:TOKEN!
   s" duplicate field name"
   s" give each field of this declaration its own name" E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ -rot sch
   SD-CELLS @  fw  SD-CELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
\ A field named like a generated member (MAKE, UNMAKE, F:AT, F:BYTES, F:CELLS, a
\ derived tail) would make the generator define a name twice, a die inside the
\ plan. Refused only under DERIVE addr, which header clauses have fixed by now.
: MEMBER-CLASH? ( ^u8 n -- bool )
   2dup s" make" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" unmake" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup TFAM-ADDR-FIXED-TAIL? IF 2drop YES EXIT THEN
   TFAM-DERIVED-TAIL? ;
: REQUIRE-FIELD-NAME ( ^u8 n -- )    \ consumes the copy; throws on a member clash
   FAM @ TFAM-DERIVE-ADDR? 0= IF 2drop EXIT THEN
   MEMBER-CLASH? IF
      s" field name is a generated member name"
      s" rename the field: under `DERIVE addr` the names `make`, `unmake`, `at`, `bytes`, `cells`, `eq`, `hash` and `tag` are taken"
      TYPE-DECL:E-TDECL-NAME DECL-REJECT:REJECT throw THEN ;
: FIELD-NAME ( -- ^u8 n )                 \ a FIELD or BUFFER name, judged
   NEXT dup 0= IF
      2drop s" missing field name"
      s" write a name and a type after `FIELD`, as in `FIELD x n`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   {: na:^ nu:n :}
   na nu NAME-LONG? IF TF-NAME-LONG$ SD-LONG-FIX$ TYPE-DECL:E-TDECL-CAP DECL-REJECT:REJECT throw THEN
   na nu REQUIRE-FIELD-NAME
   OUTCOME-NAMES @ 0 <> IF
      na nu NAME-CHECK THEN
   na nu ;
\ Each item is at least one level of MAKE's effect, so no record of more items
\ than EFFECT-DEPTH-MAX can be made; USIG-DEPTH-FITS? measures nested types.
: ITEMS-FULL ( -- )
   s" record takes more than 4096 items to make"
   s" split the record: MAKE takes one item per `FIELD` and one per buffer cell, at most 4096"
   TYPE-DECL:E-TDECL-CAP DECL-REJECT:REJECT throw ;
: ITEM-ROOM ( -- n ) EFFECT-DEPTH-MAX SD-ITEMS @ - ;
: FIELD-CLAUSE ( -- )
   FIELD-NAME {: na:^ nu:n :}
   na nu TYPE-DECL:DECL-FIELD!
   0 FIELD-MISS !
   NEXT {: ta:^ tu:n :}
   ta tu PK-CELL RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF
      ta tu FAM @ node REQUIRE-DERIVABLE
      ITEM-ROOM 1 < IF na nu DECL-REJECT:TOKEN! ITEMS-FULL THEN
      SD-ITEMS @ 1 + SD-ITEMS !
      na nu node EMIT-FIELD THEN
   -1 SEEN-FIELD ! ;

\ `BUFFER name N`: N bytes inline from the next slot, in whole cells. The count
\ is a number literal, never a name, so the layout is fixed by the text alone.
: BUFFER-COUNT ( -- n )
   NEXT dup 0= IF
      2drop s" missing buffer byte count"
      s" write a byte count after the buffer's name, as in `BUFFER bytes 16`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   num-parse {: v:n flt:bool ok:bool :}
   ok 0= flt or v 1 < or IF
      s" buffer byte count must be a number above 0"
      s" give the byte count as a number literal above 0, as in `BUFFER bytes 16`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   v ITEM-ROOM CELL * > IF ITEMS-FULL THEN     \ before its cells are counted
   v ;
: EMIT-BUFFER ( ^u8 n n -- ) {: na:^ nu:n bytes:n :}
   bytes 1 - CELL / 1 + {: fw:n :}
   CC-U8 SCHEMA-CON SCHEMA-ROOT+ {: sch:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate field name"
   s" give each field of this declaration its own name" E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ na nu sch
   SD-CELLS @  fw  SD-CELLS @ CELL *  bytes  CELL  PF-FLAG-BUFFER
   DECL-EVENT:FIELD
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
\ Derived eq and hash compare and hash whole cells, and a buffer's last cell
\ may hold bytes past its count, so neither may read one. DERIVE precedes the
\ fields (HEADER-ORDER): the family's derive flags are final here.
: BUFFER-DERIVABLE ( -- )
   FAM @ TFAM-DERIVE-EQ? IF
      s" derive eq cannot compare a buffer"
      s" drop `eq` from `DERIVE`, or hold the bytes in `FIELD`s"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ TFAM-DERIVE-HASH? IF
      s" derive hash cannot hash a buffer"
      s" drop `hash` from `DERIVE`, or hold the bytes in `FIELD`s"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN ;
: BUFFER-CLAUSE ( -- )
   FIELD-NAME {: na:^ nu:n :}
   BUFFER-DERIVABLE
   BUFFER-COUNT {: bytes:n :}
   bytes 1 - CELL / 1 + SD-ITEMS @ + SD-ITEMS !
   na nu bytes EMIT-BUFFER
   -1 SEEN-FIELD ! ;

\ Registers the family na nu names, its duplicate placed at the token ta tu.
: SD-REGISTER ( ^u8 n ^u8 n n -- )
   {: na:^ nu:n ta:^ tu:n ar:n :}
   ta tu DECL-REJECT:TOKEN!
   s" duplicate family"
   s" rename the family: this package already declares a family with this name"
   E-TFAM-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   ar TK-PRODUCT TFAM-DECL FAM !
   TYPE-FIELD:COUNT FLDBASE !
   0 NFLD !   0 SD-CELLS !   0 SD-ITEMS ! ;

: SD-MAKEABLE? ( -- bool )
   NFLD @ 0 > ;
\ Accessors are generated at commit because each is armed with a committed field
\ id (structure-make.f). A fieldless structure has nothing to address and no
\ MAKE / UNMAKE pair, whose constructor package the public accessor spelling uses.
: SD-ADDR-ARM ( -- )
   FAM @ TFAM-DERIVE-ADDR? FAM @ TFAM-DERIVE-INIT? or 0= IF EXIT THEN
   SD-MAKEABLE? 0= IF
      s" derived fields require a family with fields"
      s" add a `FIELD` to the structure, or drop `addr` and `init` from `DERIVE`"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ TFAM-DERIVE-INIT? IF
      s" derive init requires a canonical fixed-cell record"
      s" drop `init` from `DERIVE`, or give every field a fixed cell width"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:EXPECT THEN
   FAM @ STRUCTURE-MAKE:ARM ;
: SD-SEAL ( -- )
   DECL-REJECT:AT-FAMILY
   MISSING? IF E-UNRESOLVED throw THEN
   FAM @ FLDBASE @ NFLD @ TFAM-FLD-RANGE!
   NFLD @ 0 ?do
      FLDBASE @ i + PF-PENDING-SCH@ SCHEMA-ROOT@
      s" type is a pointer to a linear value and cannot own it"
      DECL-EVENT:REQUIRE-POINTEES
   loop
   FAM @ SD-CELLS @ TFAM-SLOTS! ;
: SD-CLOSE ( -- )
   SD-SEAL
   FAM @ TFAM-OPAQUE? SD-MAKEABLE? 0= and IF
      s" opaque requires a family with fields"
      s" add a `FIELD` to the structure, or drop `OPAQUE`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   \ Only a structure with fields gets MAKE / UNMAKE; a fieldless one is an opaque
   \ one-cell family, as a bare NEWTYPE declares.
   SD-MAKEABLE? IF FAM @ STRUCTURE-MAKE:GENERATE THEN
   SD-ADDR-ARM ;

: CLAUSE ( -- bool )                    \ read + dispatch one body token; YES = ;STRUCTURE
   NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;STRUCTURE"
      s" end the declaration with `;STRUCTURE` after its last field"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop -1 SEEN-END ! SD-CLOSE YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2dup s" buffer" CORE-STR=CI IF 2drop BUFFER-CLAUSE NO EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2dup s" opaque" CORE-STR=CI IF 2drop OPAQUE-CLAUSE NO EXIT THEN
   2drop s" unexpected token in structure declaration"
   s" start each clause with `FIELD`, `POLICY`, `DERIVE` or `OPAQUE`, and end with `;STRUCTURE`"
   TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;
: CLAUSES ( -- ) BEGIN CLAUSE UNTIL ;

: DRIVE ( -- )
   NEXT 2dup DECL-REJECT:FAMILY!
   dup 0= IF 2drop s" missing name" SD-NAME-FIX$ TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   TYPE-DECL:DECL-HEADER 0= IF
      drop 2drop s" invalid type parameter header"
      s" name each type parameter once in the header, as in `STRUCTURE box<a>` or `STRUCTURE pair<a,b>`"
      TYPE-DECL:E-TDECL-ARITY DECL-REJECT:REJECT throw
   THEN
   {: ar:n :}
   2dup NAME-GATE
   2dup ar
   SD-REGISTER
   CLAUSES ;

: SD-FRONT ( -- ^u8 n ^u8 n [ -- ] )
   SD-KIND$ s" ;structure" [: SD-RESET DRIVE ;] ;

\ A byte no one name holds: a package qualifier, an application or a pointer.
: NAME-STOP? ( n -- bool ) {: c:n :}
   c 58 =  c 60 = or  c 94 = or ;
\ Rejects a DEFTYPE base the wrap sealed that is not one name, since its
\ converter NAME>BASE is named after it, offering the NEWTYPE that loads.
: ONE-NAME-GATE ( ^u8 n ^u8 n -- ) {: na:^ nu:n ba:^ bu:n :}
   DEFTYPE? 0= IF EXIT THEN
   NO bu 0 ?do ba i + c@ NAME-STOP? or loop 0= IF EXIT THEN
   ba bu DECL-REJECT:TOKEN!
   s" a deftype base is one name"  na nu ba bu BASE-HELP$
   TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;

\ A newtype over a base: its name, then the base's tokens, which stand for one
\ field `base`; a token past the base is rejected. DEFTYPE reaches neither
\ rejection: its name is never empty and its base is one token. A record of
\ the name stands at the name as written: a DEFTYPE's name reaches the stream
\ folded, in a buffer no source holds.
: WRAP-DRIVE ( -- )
   DECL-PARAM-CLEAR
   NEXT {: na:^ nu:n :}
   na nu DECL-REJECT:FAMILY!
   nu 0= IF s" missing name"
      s" write the family name after `NEWTYPE`, as in `NEWTYPE path str`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   NAME-A @ NAME-U @ DECL-REJECT:TOKEN!
   na nu NAME-GATE
   na nu NAME-A @ NAME-U @ 0 SD-REGISTER
   0 FIELD-MISS !
   NEXT 2dup BASE-U ! BASE-A !
   PK-CELL RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF s" base" node EMIT-FIELD THEN
   NEXT nip 0 <> IF
      s" a newtype takes one base type"
      s" give one base type after the name, as in `NEWTYPE path str`, and end the line there"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   -1 SEEN-END !
   SD-SEAL
   na nu BASE-A @ BASE-U @ ONE-NAME-GATE ;

\ Takes the name as written and the kind, the declaration's keyword, "newtype"
\ or "deftype". The stream has no closer, so a rejected one resynchronizes to
\ its end.
: WRAP-FRONT ( ^u8 n ^u8 n -- ^u8 n ^u8 n [ -- ] )
   2swap NAME-U ! NAME-A !
   s" " [: SD-RESET WRAP-DRIVE ;] ;

public

: DECLARE-STRUCTURE ( -- ) SD-FRONT DECL-FRONT:RUN ;

\ Registers a STRUCTURE from a body a tool lexed, by DECLARE-STRUCTURE's grammar,
\ defining no word; a body without ;STRUCTURE is rejected "missing ;STRUCTURE".
: REPLAY-STRUCTURE ( ^u8 n ^u8 n -- ^u8 n bool )
   SD-FRONT DECL-FRONT:REPLAY-OUTCOME ;

private

\ TYPE-DECL:TDECL-FAM-REG holds the family only when the wrap left no rejection,
\ which a multi-error load tallies and any other load throws.
: WRAP-KEEP ( n -- ) {: tally:n :}
   REJECTION:TALLY @ tally <> IF EXIT THEN
   FAM @ TYPE-DECL:TDECL-FAM-REG ! ;

\ Rejects `DEFTYPE` NAME at BASE, armed with reason, help and code.
: ONE-NAME-REJECT ( ^u8 n ^u8 n ^u8 n ^u8 n n -- )
   {: na:^ nu:n ba:^ bu:n wa:^ wu:n fa:^ fu:n code:n :}
   s" deftype" DECL-REJECT:OPEN
   na nu DECL-REJECT:FAMILY!
   ba bu DECL-REJECT:TOKEN!
   wa wu fa fu code DECL-REJECT:REJECT
   DECL-REJECT:GUARD-CODE ;

public

\ Whether BASE holds no `[`: a quotation type spans tokens and a DEFTYPE base
\ is one; if it does, rejects NAME. The wrap judges any other base.
: SD-ONE-NAME ( ^u8 n ^u8 n -- bool ) {: na:^ nu:n ba:^ bu:n :}
   NO bu 0 ?do ba i + c@ 91 = or loop 0= IF YES EXIT THEN
   na nu ba bu s" a deftype base is one name"
   s" write the base as one bare name, with no `:`, `<`, `^` or `[`, as in `DEFTYPE REV str`: the converter `REV>STR` is named after it"
   TYPE-DECL:E-TDECL-SYNTAX ONE-NAME-REJECT
   NO ;

\ Registers the family HEADER names over BASE: one field base, no word made;
\ each record of the name stands at TOKEN, the name as written. KIND is the
\ declaration's keyword, "newtype" or "deftype", which its records name.
\ Answers as REPLAY-STRUCTURE; TDECL-FAM-REG is the family, or -1 if none.
: SD-WRAP-OUTCOME ( ^u8 n ^u8 n ^u8 n ^u8 n -- ^u8 n bool )
   {: ha:^ hu:n ta:^ tu:n ba:^ bu:n ka:^ ku:n :}
   -1 TYPE-DECL:TDECL-FAM-REG !  REJECTION:TALLY @ {: tally:n :}
   ha hu ba bu ta tu ka ku WRAP-FRONT DECL-FRONT:REPLAY-OUTCOME {: ma:^ mu:n miss:bool :}
   miss 0= IF tally WRAP-KEEP THEN
   ma mu miss ;
\ The same, raising E-UNRESOLVED instead of answering, and answering the
\ family, or -1 when a multi-error load rejected it.
: SD-WRAP ( ^u8 n ^u8 n ^u8 n ^u8 n -- n )
   SD-WRAP-OUTCOME IF 2drop E-UNRESOLVED throw THEN 2drop
   TYPE-DECL:TDECL-FAM-REG @ ;

;package

public

EXPORT STRUCTURE-DECL:DECLARE-STRUCTURE
EXPORT STRUCTURE-DECL:REPLAY-STRUCTURE
EXPORT STRUCTURE-DECL:SD-ONE-NAME
EXPORT STRUCTURE-DECL:SD-WRAP-OUTCOME
EXPORT STRUCTURE-DECL:SD-WRAP

;package

\ STRUCTURE is a documented global (package-first exception, like the SUMTYPE
\ opener); it parses its body to ;STRUCTURE at interpret time, so its
\ checked effect is ( -- ).
: STRUCTURE ( -- ) TYPE-DECL:DECLARE-STRUCTURE ;

\ NEWTYPE is a documented global. A base is the type on the name's line; a \
\ there, or a parametrised name, leaves the one-cell family. A missing name is
\ rejected at the keyword.
: NEWTYPE ( -- )
   SRCLOC:TOKEN$ {: kw:^ ku:n :}
   parse-name {: na:^ nu:n :}
   kw ku na nu REJECTION:NOT-A-NAME
   nu 0= IF na nu kw ku s" newtype" TYPE-DECL:TDECL-DEFFAMILY EXIT THEN
   0 0= 0= nu 0 ?do na i + c@ 60 = or loop
   IF na nu 2dup s" newtype" TYPE-DECL:TDECL-DEFFAMILY EXIT THEN
   STORAGE-LINE-TOKEN {: ta:^ tu:n :}
   tu 0= IF na nu 2dup s" newtype" TYPE-DECL:TDECL-DEFFAMILY EXIT THEN
   ta STORAGE-UNREAD
   tu 1 = IF ta c@ 92 = IF na nu 2dup s" newtype" TYPE-DECL:TDECL-DEFFAMILY EXIT THEN THEN
   na nu 2dup STORAGE-PARSE-TYPE s" newtype" TYPE-DECL:SD-WRAP drop ;

;using
;using
