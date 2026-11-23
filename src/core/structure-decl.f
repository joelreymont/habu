split-str
\ structure-decl.f: the STRUCTURE and MUTUAL front end (package STRUCTURE-DECL),
\ grammar loop and reject dispatch only. DECL-FRONT (generated-declaration.f) owns
\ the token source and the guarded run; DECL-EVENT (decl-event.f) owns every event.
\   STRUCTURE type-name[<params>] (POLICY p | DERIVE f+ | OPAQUE)*
\      (FIELD name type | BUFFER name bytes)* ;STRUCTURE
\   MUTUAL type-name[<params>]+ (STRUCTURE ... ;STRUCTURE)* ;MUTUAL
\ A reject rolls the whole declaration back to a byte-identical registry. Loads
\ after decl-event.f and structure-make.f, whose GENERATE and ARM ;STRUCTURE calls,
\ or ;MUTUAL for each of its members.

package TYPE-DECL
private
package STRUCTURE-DECL
using DECL-FRONT
EXPORT TFAM:PF-KIND-MEMO-END
EXPORT TFAM:PF-KIND-MEMO-START
EXPORT TFAM:TFAM-FLD-RANGE!
EXPORT TFAM:TFAM-PK!
EXPORT SCHEMA-REG:SCHEMA-PARAM?

\ Transient parse state, not declaration state: STRUCTURE is top-level
\ interpret-only, so one declaration runs at a time.
variable FAM        \ family id of the open member
variable FLDBASE    \ the open member's first field row (field range start)
variable NFLD       \ field count of the open member
variable SD-FLDS    \ field rows the declaration's closed members added
variable SD-CELLS      \ running field cell width (next field's slot / byte offset)
variable SD-ITEMS   \ MAKE's inputs so far: one per FIELD, one per buffer cell
variable SEEN-FIELD \ a FIELD has appeared (header clauses must precede fields)
variable POINTEE    \ resolving the pointee of `^`: the declared family may be named
PTR-VARIABLE BASE-A   variable BASE-U   \ a wrap's base token
PTR-VARIABLE NAME-A   variable NAME-U   \ a wrap's name as written

\ The members of the open declaration, one row each: a MUTUAL lists several, a
\ plain STRUCTURE is one member and no list. A member is listed, open or closed:
\   listed: its family row added, no fields; a row whose flag is set. It is named
\     only beneath `^` (FIELD-FAM?), a STRUCTURE for it opens it (MEMBER) and
\     ;MUTUAL rejects it (UNDECLARED).
\   open: the family FAM holds, its STRUCTURE body being read; named only beneath
\     `^` as its owner (FIELD-FAM?).
\   closed: neither; its ;STRUCTURE set its field range and width in its family
\     row, it is any family, and a second STRUCTURE for it is a duplicate (MEMBER).
\ The close judges and generates every member in the order their `;STRUCTURE`
\ was read: at ;MUTUAL, or at a plain STRUCTURE's ;STRUCTURE. A member may hold
\ another by value only once that one is closed, so close order generates a held
\ member before its holder. A row holds the name as listed (its span in
\ MEM-NAMES), the family, the listed flag, the first pointee naming it while
\ listed (its span in MEM-PTEES, M.PTEE-U 0 for none: SD-REACH) and its close
\ rank (M.CLOSE: the members closed before it, 0 until its ;STRUCTURE); arity
\ and field range are the family row's.
0 constant M.NAME-U
1 constant M.FAM
2 constant M.LISTED
3 constant M.PTEE-U
4 constant M.CLOSE
5 constant MEM-REC
PTR-U8-TABLE-VARIABLE MEM-NAMES
SCRATCH-VARIABLE MEM-NAMES-CAP
PTR-U8-TABLE-VARIABLE MEM-PTEES
SCRATCH-VARIABLE MEM-PTEES-CAP
PTR-VARIABLE MEM-P
SCRATCH-VARIABLE MEM-CAP              \ in rows
variable MEM-N                        \ members of the open declaration
0 MEM-N !
variable MEM-CLOSED                   \ its members closed so far: the next rank
0 MEM-CLOSED !

: MEM-CLEAR ( -- ) 0 MEM-N !  0 MEM-CLOSED ! ;
: MEM-NAME ( n -- ^^u8 ) MEM-NAMES @ swap ptr-field ;
: MEM-PTEE ( n -- ^^u8 ) MEM-PTEES @ swap ptr-field ;
: MEM-CELL ( n n -- ^n ) {: i:n k:n :} i MEM-REC * k + cells MEM-P @ + ;
: MEM-NAME$ ( n -- str ) {: i:n :} i MEM-NAME @  i M.NAME-U MEM-CELL @ ;
: MEM-LISTED@ ( n -- bool ) M.LISTED MEM-CELL @ 0 <> ;
\ Adds a member named na nu of family fam, listed or open.
: MEM-ADD ( str n bool -- ) {: na:^ nu:n fam:n listed:bool :}
   MEM-N @ {: i:n :}
   MEM-NAMES MEM-NAMES-CAP i 1 + 1 ARENA-ROWS-ENSURE
   MEM-PTEES MEM-PTEES-CAP i 1 + 1 ARENA-ROWS-ENSURE
   MEM-P MEM-CAP i 1 + MEM-REC ARENA-ROWS-ENSURE
   na i MEM-NAME !
   nu i M.NAME-U MEM-CELL !   fam i M.FAM MEM-CELL !   0 i M.PTEE-U MEM-CELL !
   0 i M.CLOSE MEM-CELL !
   listed IF -1 ELSE 0 THEN i M.LISTED MEM-CELL !
   i 1 + MEM-N ! ;
\ The member named na nu, -1 for none.
: MEM-FIND ( str -- n ) {: na:^ nu:n :}
   MEM-N @ 0 ?do
      i MEM-NAME$ na nu CORE-STR= IF i unloop EXIT THEN
   loop -1 ;
\ The member whose family is id, -1 for none.
: MEM-OF ( n -- n ) {: id:n :}
   MEM-N @ 0 ?do
      i M.FAM MEM-CELL @ id = IF i unloop EXIT THEN
   loop -1 ;
\ Whether family id is a listed member, whose ;STRUCTURE has not been read.
: MEM-LISTED? ( n -- bool )
   MEM-OF {: i:n :}  i 0 < IF NO EXIT THEN  i MEM-LISTED@ ;

PTR-VARIABLE SD-ARGS
SCRATCH-VARIABLE SD-ARGS-CAP          \ in rows
variable SD-ARG-N

\ The first pointee naming the open member while it takes no scope or region,
\ as written; a later scope or region parameter is rejected there. Each member
\ has its own: one named while the member is listed waits in its row (SD-REACH)
\ and MEM-BEGIN loads it here; one named while its family is open, in any
\ declaration, is set here as it is read.
PTR-VARIABLE SELF-A   variable SELF-U

public
: SELF-PTR-RESET ( -- ) 0 SELF-U ! ;
private

: SD-RESET ( -- )
   0 SEEN-FIELD !  0 SD-FLDS !  SELF-PTR-RESET ;
SD-RESET
: SD-MISSING! ( -- ) TYPE@ DW-TYPE MISSING! ;

\ The node parser's rejections name the type as its caller does ("type",
\ "payload type"); SD-TYPE-NODE sets the noun for one parse.
PTR-VARIABLE NOUN-A   variable NOUN-U
: NOUN@ ( -- str ) NOUN-A @ NOUN-U @ ;

: SD-WORD+ ( str -- ) {: a:^ u:n :}   \ a blank, then the word
   u 0= IF EXIT THEN
   s"  " LABEL+  a u LABEL+ ;

\ The text before the noun, the noun, the text after it, blank-separated.
: SD-NOUN-TEXT ( str str str -- str ) {: pa:^ pu:n na:^ nu:n sa:^ su:n :}
   LABEL-START
   pa pu LABEL+
   pu 0= IF na nu LABEL+ ELSE na nu SD-WORD+ THEN
   sa su SD-WORD+
   LABEL$ ;
: SD-TEXT ( str str -- str ) {: pa:^ pu:n sa:^ su:n :}   \ with the noun SD-TYPE-NODE set
   pa pu NOUN@ sa su SD-NOUN-TEXT ;
: SD-REJECT ( str str str n -- n ) {: pa:^ pu:n sa:^ su:n fa:^ fu:n code:n :}
   pa pu sa su SD-TEXT fa fu code DECL-REJECT:REJECT ;

: SD-NAME-FIX$ ( -- str ) s" write the family name after `STRUCTURE`, as in `STRUCTURE point`" ;
: LIST-NAME-FIX$ ( -- str ) s" list the families after `MUTUAL`, as in `MUTUAL node link`" ;
: SD-FIELD-FIX$ ( -- str ) s" give each field of the structure its own name" ;
: DEFTYPE? ( -- bool ) DECL-REJECT:KIND$ s" deftype" CORE-STR= ;
: ENUM? ( -- bool ) DECL-REJECT:KIND$ s" enum" CORE-STR= ;
\ A NEWTYPE or DEFTYPE: one base on one line, with no header and no clause.
: WRAP? ( -- bool ) DEFTYPE? DECL-REJECT:KIND$ s" newtype" CORE-STR= or ;
\ A DEFTYPE base is one name (WRAP-DRIVE), so its help offers no `^` or header.
: SD-TYPE-FIX$ ( -- str )
   DEFTYPE? IF
      s" name as the base one type declared before this declaration: `n`, `str`, a `STRUCTURE` or an `ENUM`, as in `DEFTYPE REV str`" EXIT THEN
   WRAP? IF
      s" name a type declared before this declaration: `n`, `^u8`, a `STRUCTURE` or an `ENUM`" EXIT THEN
   s" name a type declared before this declaration: `n`, `^u8`, a header parameter, a `STRUCTURE` or an `ENUM`" ;
: SD-PARAM-FIX$ ( -- str )
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
: HELP+ ( str -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ HELP-C+ loop ;
\ The name at a u as its declaration folds it.
: HELP-FOLD+ ( str -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ CORE-FOLD-C HELP-C+ loop ;

\ The NEWTYPE a DEFTYPE NAME over BASE becomes, NAME folded to its tail, its
\ closing "` instead" left to the caller.
: NEWTYPE-HELP ( str str -- ) {: na:^ nu:n ba:^ bu:n :}
   0 HELP-U !
   s" a `DEFTYPE` base is one name with no type arguments: declare `NEWTYPE " HELP+
   na nu HELP-FOLD+
   s"  " HELP+  ba bu HELP+ ;
\ The edit the loader accepts for `DEFTYPE NAME BASE` whose NAME and BASE the
\ wrap accepted: the NEWTYPE over BASE as written.
: BASE-HELP$ ( str str -- str )
   NEWTYPE-HELP  s" ` instead" HELP+  HELP-P @ HELP-U @ ;
\ NEWTYPE-HELP over BASE up to its token at ta, which the caller then rewrites.
: HELP-HEAD ( str ^u8 ^u8 -- ) {: na:^ nu:n ba:^ ta:^ :}
   na nu ba  ta NULL-PTR - ba NULL-PTR - -  NEWTYPE-HELP ;
\ BASE after its token ta tu, then the help's close.
: HELP-TAIL$ ( str str -- str ) {: ba:^ bu:n ta:^ tu:n :}
   ta NULL-PTR - tu + ba NULL-PTR - - {: hu:n :}
   ba hu + bu hu - HELP+  s" ` instead" HELP+
   HELP-P @ HELP-U @ ;
\ The edit the loader accepts for `DEFTYPE NAME BASE` whose token TOK names bare a
\ family of arity ar taking `n` throughout: the NEWTYPE of BASE, `<n,...>` after TOK.
: ARGS-HELP$ ( str str str n -- str ) {: na:^ nu:n ba:^ bu:n ta:^ tu:n ar:n :}
   na nu ba ta HELP-HEAD  ta tu HELP+  s" <n" HELP+
   ar 1 ?do s" ,n" HELP+ loop
   s" >" HELP+  ba bu ta tu HELP-TAIL$ ;
\ The edit the loader accepts for a DEFTYPE whose base names `width` at ta inside
\ a longer base: the NEWTYPE of that base, `n` for the width.
: WIDTH-HELP$ ( str -- str ) {: ta:^ tu:n :}
   DECL-REJECT:FAMILY$ BASE-A @ ta HELP-HEAD  s" n" HELP+
   BASE-A @ BASE-U @ ta tu HELP-TAIL$ ;

\ A family whose parameters are all cells: `n` fills each (`option<n>` loads,
\ `width<n>` and `init<n,n>` do not).
: CELL-PARAMS? ( n -- bool ) {: fam:n :}
   YES fam TFAM-ARITY@ 0 ?do fam i TFAM-PK@ PK-CELL = and loop ;
\ Whether the token at a is a DEFTYPE's whole base, which a help naming one
\ type may replace.
: WHOLE-BASE? ( str -- bool ) {: a:^ u:n :}
   DEFTYPE? a BASE-A @ = and u BASE-U @ = and ;
\ The help for family fam named bare at a: a DEFTYPE gets the NEWTYPE that
\ loads, or another base when no `n` arguments fit.
: SD-ARGS-FIX$ ( n str -- str ) {: fam:n a:^ u:n :}
   DEFTYPE? 0= IF s" give the family its type arguments, as in `box<n>`" EXIT THEN
   fam CELL-PARAMS? 0= IF SD-TYPE-FIX$ EXIT THEN
   DECL-REJECT:FAMILY$ BASE-A @ BASE-U @ a u fam TFAM-ARITY@ ARGS-HELP$ ;
: SD-APP-FIX$ ( -- str )
   s" write a type application as `family<type,type>`, as in `SPAN:span<^u8>`" ;
\ The help for a malformed pointer token at a: a DEFTYPE's whole base gets the base
\ help, a longer base the NEWTYPE over `^T`, a wrap, payload or field the spelling.
: SD-PTR-FIX$ ( str -- str )
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
: SD-OPEN-FIX$ ( -- str )
   WRAP? IF s" close each `<` of the type with `>`" EXIT THEN
   s" close each `<` of the type with `>` before the next clause" ;

\ The reserved list is TYPE-NAME:FAMILY-RESERVED?, the one every family definer
\ asks: a copy kept here drifted twice (it admitted `if`, then value-record names).
\ Case and duplicate are TFAM-DECL's.
: NAME-RESERVED? ( str -- bool )
   2dup s" structure" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop YES EXIT THEN
   TYPE-NAME:FAMILY-RESERVED? ;

: REQUIRE-NAME ( str -- )
   dup 0= IF 2drop s" missing name" SD-NAME-FIX$ TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup DEF-NAME-BAD? IF 2drop s" " s" " TFAM:E-TDECL-DEF-NAME DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail"
      s" write the name as a lowercase tail with no package qualifier, as in `point`"
      TFAM:E-TFAM-CASE DECL-REJECT:REJECT throw THEN
   NAME-RESERVED? IF s" reserved name"
      s" pick another name: a built-in type such as `n` cannot name a family"
      TYPE-DECL:E-TDECL-NAME DECL-REJECT:REJECT throw THEN ;

: NAME-GATE ( str -- )
   2dup DECL-REJECT:FAMILY! REQUIRE-NAME ;

: SD-KIND$ ( -- str ) s" structure" ;

EXPORT CHECK:SIG-DELIM-CHAR?
EXPORT CHECK:SIG-NEST
EXPORT CHECK:TYPE-MAY-ARRIVE-XT
EXPORT CHECK:WIDTH-FAM
\ A family that owns a linear value is an accepted field type: the structure owns
\ the obligation by containment (TFAM-CONCRETE-LINEAR? walks into it), the rule a
\ field naming a DEFLINEAR con already relies on.
\ The kind test falls through to "unknown <noun>" because the one kind it
\ excludes, TK-EVIDENCE, has no declarer source can write.
\ The declared family may be named only beneath a `^` (`^node`, `SPAN:span<^node>`);
\ by value, as in `option<node>`, it is 7109. So may a listed MUTUAL member,
\ whose ;STRUCTURE has not been read; a closed member is any family.
: FIELD-FAM? ( str -- n bool ) {: a:^ u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE {: id:n sr:n :}
   sr SR-AMBIG = IF a u DECL-REJECT:TOKEN!  a u TDECL-AMBIG throw THEN
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
   id MEM-LISTED? POINTEE @ 0= and IF
      a u DECL-REJECT:TOKEN!
      s" " s" names a MUTUAL member before its `;STRUCTURE`"
      s" name it only beneath `^` until its `;STRUCTURE`, as in `FIELD first ^link`"
      TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   id YES ;

: USE-KIND$ ( n -- str )
   dup PK-CELL = IF drop s" a value type" EXIT THEN
   dup PK-TYPE = IF drop s" an element type" EXIT THEN
   dup PK-SCOPE = IF drop s" a scope" EXIT THEN
   PK-REGION = IF s" a region" EXIT THEN
   s" a pointee or quotation element" ;
\ Rejects the parameter at a u used as want, which field fa fu used as kind.
: PARAM-KIND-REJECT ( n str str n -- ) {: kind:n fa:^ fu:n a:^ u:n want:n :}
   a u DECL-REJECT:TOKEN!
   LABEL-START
   s" `" LABEL+
   a u LABEL+
   s" ` is " LABEL+
   want USE-KIND$ LABEL+
   s"  here, but field `" LABEL+
   fa fu LABEL+
   s" ` uses it as " LABEL+
   kind USE-KIND$ LABEL+
   LABEL$
   ENUM? IF
      s" give each kind its own header parameter, as in `ENUM box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`"
   ELSE
      s" give each kind its own header parameter, as in `STRUCTURE box<a,p,q>` with `FIELD x a` and `FIELD v read-view<p,q,n>`"
   THEN
   TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

\ The declared family cannot be held by value instead, so its help moves the view out.
: PTR-VIEW-FIX$ ( bool -- str ) {: self:bool :}
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
: PTR-VIEW-REJECT ( str bool -- ) {: a:^ u:n self:bool :}
   a u DECL-REJECT:TOKEN!
   LABEL-START
   s" a pointer cannot reach `" LABEL+
   a u LABEL+
   s" `, which takes a scope or region" LABEL+
   LABEL$  self PTR-VIEW-FIX$
   TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

: LETTER-TYPE ( str n -- n ) {: a:^ u:n want:n :}
   a c@
   dup ASCII-N = IF drop CC-N SCHEMA-CON EXIT THEN
   dup ASCII-F = IF drop CC-BOOL SCHEMA-CON EXIT THEN
   dup ASCII-R = IF drop CC-R SCHEMA-CON EXIT THEN
   DECL-PARAM 0= IF
      drop a u DECL-REJECT:TOKEN!
      s" unknown" s" " SD-PARAM-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   {: idx:n :}
   want PK-SCOPE = want PK-REGION = or SELF-U @ 0 <> and IF
      SELF-A @ SELF-U @ YES PTR-VIEW-REJECT THEN
   idx want DECL-PARAM-USE IF a u want PARAM-KIND-REJECT THEN
   2drop drop
   want PK-SCOPE = want PK-REGION = or
   want PK-TYPE = or IF
      FAM @ idx want TFAM-PK! THEN
   idx SCHEMA-PARAM ;

: SD-ARG+ ( n -- )
   SD-ARG-N @ {: i:n :}
   SD-ARGS SD-ARGS-CAP i 1 + 1 ARENA-ROWS-ENSURE
   i cells SD-ARGS @ + !
   i 1 + SD-ARG-N ! ;

: SD-BLANK? ( n -- bool ) 33 < ;
: SD-DELIM? ( n -- bool ) dup SD-BLANK? swap SIG-DELIM-CHAR? or ;

\ Each parser word takes the index it starts at and answers the index after what it
\ consumed; a type's text holds blanks, and a `[` may touch the type after it.
: SD-ATOM ( str n -- str n ) {: a:^ u:n start:n :}
   start u < IF a start + c@ 91 = IF a start + 1 start 1 + EXIT THEN THEN
   start
   BEGIN
      dup u < IF a over + c@ SD-DELIM? 0= ELSE NO THEN
   WHILE
      1 +
   REPEAT {: j:n :}
   j start = IF
      a start +  start u < IF 1 ELSE 0 THEN  DECL-REJECT:TOKEN!
      s" malformed" s" application"
      SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a start + j start - j ;

\ The blanks an argument may start with.
: SD-SKIP ( str n -- n ) {: a:^ u:n i:n :}
   i BEGIN
      dup u < IF a over + c@ SD-BLANK? ELSE NO THEN
   WHILE
      1 +
   REPEAT ;

\ Whether the atom ending at j opens a pointer, and the index its pointee is read
\ from: the byte after `^`. An atom `ptr` not followed by `<` names no type.
: SD-POINTEE-AT ( str str n -- n bool ) {: na:^ nu:n a:^ u:n j:n :}
   na c@ 94 = IF
      nu 1 = j u < IF a j + c@ 91 = ELSE NO THEN and IF j YES EXIT THEN
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

defer SD-PARSE-NODE ( str n n -- n n nominal )

\ The argument spelled at aa au fills slot k of fam; LETTER-TYPE judges a
\ parameter argument. A wrap reaches no region slot: each follows two scope
\ slots, the first of which rejects it.
: SD-ARG-KIND ( str n n n -- ) {: aa:^ au:n fam:n k:n node:n :}
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

\ The index after the byte that brings the application whose head is at h back
\ to depth 0: its closing `>`, or a `]` where its brackets do not pair; else u.
: SD-APP-END ( str n -- n ) {: a:^ u:n h:n :}
   0 h BEGIN dup u < WHILE
      2dup a swap + c@ SIG-NEST
      rot 1 = over 0= and IF drop 1 + EXIT THEN
      swap 1 +
   REPEAT nip ;

: SD-APP-MARK ( str n -- ) {: a:^ u:n h:n :}
   a h +  a u h SD-APP-END h -  DECL-REJECT:TOKEN! ;

\ Argument k onward of fam's application, the first at i: each is judged against
\ its slot, then the count of arguments and the index after the closing `>`.
: SD-APP-ARGS ( str n n n n -- n n ) {: a:^ u:n fam:n h:n k:n i:n :}
   k fam TFAM-ARITY@ >= IF
      a u h SD-APP-MARK
      s" " s" has too many arguments"
      s" drop the extra type arguments: give the family as many as its header declares"
      TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a u i  fam k TFAM-PK@  SD-PARSE-NODE {: node:n e:n :}
   a u i SD-SKIP {: s:n :}
   a s +  e s -  fam k node SD-ARG-KIND
   node SD-ARG+
   a u e SD-SKIP {: j:n :}
   j u >= IF
      a u h SD-APP-MARK
      s" unclosed" s" application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a j + c@ {: c:n :}
   c 44 = IF a u fam h k 1 + j 1 + RECURSE EXIT THEN
   c 62 = IF k 1 + j 1 + EXIT THEN
   a u h SD-APP-MARK
   s" malformed" s" application" SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw ;

: SD-APP ( str n n n -- n n ) {: a:^ u:n fam:n h:n i:n :}
   SD-ARG-N @ {: base:n :}
   a u fam h 0 i SD-APP-ARGS {: k:n j:n :}
   k fam TFAM-ARITY@ <> IF
      a u h SD-APP-MARK
      s" " s" has wrong arity"
      s" give the family as many type arguments as its header declares"
      TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   FIELD-MISS @ IF base SD-ARG-N ! 0 j EXIT THEN
   SCHEMA-ROOT-N@ {: start:n :}
   base BEGIN dup SD-ARG-N @ < WHILE
      dup cells SD-ARGS @ + @ SCHEMA-ROOT+ drop 1 +
   REPEAT drop
   base SD-ARG-N !
   fam start k SCHEMA-APP j ;

defer SD-LOOSE-NODE ( str n -- n nominal )

: SD-LOOSE-ARGS ( str n n -- n ) {: a:^ u:n h:n i:n :}
   a u  a u i SD-LOOSE-NODE  SD-SKIP {: j:n :}
   j u >= IF
      a u h SD-APP-MARK
      s" unclosed" s" application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a j + c@ {: c:n :}
   c 62 = IF j 1 + EXIT THEN
   c 44 <> IF
      a u h SD-APP-MARK
      s" malformed" s" application"
      SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a u h j 1 + RECURSE ;

: SD-LOOSE-APP ( str n n -- n ) {: a:^ u:n h:n i:n :}
   i u >= IF
      a u h SD-APP-MARK
      s" unclosed" s" application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   a i + c@ 62 = IF i 1 + EXIT THEN
   a u h i SD-LOOSE-ARGS ;

: SD-LOOSE-ATOM ( str -- ) {: a:^ u:n :}
   u 1 = IF
      a c@ dup ASCII-N = over ASCII-F = or over ASCII-R = or
      IF drop EXIT THEN
      TFAM-DECL-CHAR>PARAM IF
         drop a c@ DECL-PARAM IF drop EXIT THEN
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
: SD-LOOSE-PTR ( str n -- n ) {: a:^ u:n k:n :}
   POINTEE @ {: outer:n :}
   -1 POINTEE !
   a u k SD-LOOSE-NODE
   outer POINTEE ! ;

\ PARSE-QUOT reads a quotation type over the tokens from SD-QUOT-AT, the index
\ after the last token or element read.
variable SD-QUOT-AT
: SD-QUOT-NEXT ( str -- str ) {: a:^ u:n :}
   a u SD-QUOT-AT @ SD-SKIP {: s:n :}
   s u < IF a s + c@ SIG-DELIM-CHAR? ELSE NO THEN IF s 1 + ELSE
      s BEGIN
         dup u < IF a over + c@ SD-DELIM? 0= ELSE NO THEN
      WHILE 1 + REPEAT
   THEN {: e:n :}
   e SD-QUOT-AT !
   a s +  e s - ;
\ The `<` and `[` depth after the bytes a u, from depth.
: SD-DEPTH ( n str -- n ) {: depth:n a:^ u:n :}
   depth u 0 ?do a i + c@ SIG-NEST loop ;
\ An element is read from its token on, so an application in it may span
\ blanks; an outer `^` does not reach it: the declared family needs its own.
: SD-QUOT-ELEM ( str str -- n ) {: ta:^ tu:n a:^ u:n :}
   ta NULL-PTR - a NULL-PTR - - {: i:n :}
   POINTEE @ {: outer:n :}
   0 POINTEE !
   a u i ANY-VALUE SD-PARSE-NODE
   SD-QUOT-AT !
   outer POINTEE ! ;
\ A malformed quotation is rejected at the token it stopped at, or at its `[`
\ when the text ends first.
: SD-QUOT-FAIL ( str str str n ^u8 -- )
   {: ta:^ tu:n wa:^ wu:n fa:^ fu:n code:n qa:^ :}
   tu 0= IF qa 1 ELSE ta tu THEN DECL-REJECT:TOKEN!
   wa wu fa fu code DECL-REJECT:REJECT throw ;
\ The quotation whose `[` is at k, and the index after its `]`.
: SD-QUOT-NODE ( str n -- n n ) {: a:^ u:n k:n :}
   k 1 + SD-QUOT-AT !
   [: a u SD-QUOT-NEXT ;]
   [: a u SD-QUOT-ELEM ;]
   [: a k + SD-QUOT-FAIL ;]
   PARSE-QUOT SD-QUOT-AT @ ;

\ Beneath a `^`, judges the family fam named at a u before its arguments, which
\ a scope or region slot would reject first. A pointee naming a member that
\ takes no scope or region is that member's first pointee when it has none: the
\ open member's in SELF-A, a listed member's in its row.
: SD-REACH ( str n -- ) {: a:^ u:n fam:n :}
   POINTEE @ 0= IF EXIT THEN
   fam FAM @ = {: self:bool :}
   fam TFAM-NONVALUE-PARAM? IF a u self PTR-VIEW-REJECT THEN
   self IF SELF-U @ 0= IF a SELF-A !  u SELF-U ! THEN EXIT THEN
   fam MEM-OF {: i:n :}
   i 0 < IF EXIT THEN
   i MEM-LISTED@ 0= IF EXIT THEN
   i M.PTEE-U MEM-CELL @ 0= IF a i MEM-PTEE !  u i M.PTEE-U MEM-CELL ! THEN ;

\ Opens a level for the type at i, rejecting its token at the limit.
: SD-LEVEL+ ( str n -- ) {: a:^ u:n i:n :}
   TYPE-LEVEL @ EFFECT-DEPTH-MAX >= IF
      a u  a u i SD-SKIP  SD-ATOM drop DECL-REJECT:TOKEN!
      TDECL-DEPTH-TEXTS TYPE-DECL:E-TDECL-CAP DECL-REJECT:REJECT throw THEN
   TYPE-LEVEL @ 1 + TYPE-LEVEL ! ;

\ SD-PARSE-NODE-ONE's grammar over the arguments of a family that may still arrive
\ (SD-LOOSE-APP), building no application; an empty `<>` passes here and SD-APP refuses it.
: SD-LOOSE-NODE-ONE ( str n -- n ) {: a:^ u:n i:n :}
   a u  a u i SD-SKIP  SD-ATOM {: na:^ nu:n j:n :}
   na nu a u j SD-POINTEE-AT IF {: k:n :} a u k SD-LOOSE-PTR EXIT THEN drop
   na nu s" [" CORE-STR= IF a u j 1 - SD-QUOT-NODE nip EXIT THEN
   j u < IF a j + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? IF
         {: fam:n :}
         na nu fam SD-REACH
         a u fam  na NULL-PTR - a NULL-PTR - -  j 1 + SD-APP nip EXIT
      THEN drop
      na nu TYPE-MAY-ARRIVE-XT 0= IF
         na nu DECL-REJECT:TOKEN!
         s" unknown" s" " SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
      SD-MISSING!
      a u  na NULL-PTR - a NULL-PTR - -  j 1 + SD-LOOSE-APP EXIT
   THEN
   na nu SD-LOOSE-ATOM j ;
: SD-LOOSE-NODE-IMPL ( str n -- n ) {: a:^ u:n i:n :}
   a u i SD-LEVEL+
   a u i SD-LOOSE-NODE-ONE
   TYPE-LEVEL @ 1 - TYPE-LEVEL ! ;
: SD-LOOSE-INSTALL ( -- ) [: SD-LOOSE-NODE-IMPL ;] is SD-LOOSE-NODE ;
SD-LOOSE-INSTALL

\ Every declared pointer is built here. A rejection in its pointee that sets no token
\ of its own is placed at the whole type, as one in a family's arguments is.
: SD-PTR-NODE ( str n -- n n ) {: a:^ u:n k:n :}
   POINTEE @ {: outer:n :}
   -1 POINTEE !
   a u k ANY-VALUE SD-PARSE-NODE {: node:n e:n :}
   outer POINTEE !
   FIELD-MISS @ IF 0 e EXIT THEN
   node TFCL-NODE? IF a k + e k - DECL-REJECT:TOKEN! THEN
   node s" " s" is a pointer to a linear value and cannot own it" SD-TEXT
   DECL-EVENT:REQUIRE-POINTEE SCHEMA-PTR e ;

\ The node at i of a field type's text: a type parameter, a con, a family
\ application, a pointer argument or a quotation, with the index after it.
: SD-PARSE-NODE-ONE ( str n n -- n n ) {: a:^ u:n i:n want:n :}
   a u  a u i SD-SKIP  SD-ATOM {: na:^ nu:n j:n :}
   na nu a u j SD-POINTEE-AT IF {: k:n :} a u k SD-PTR-NODE EXIT THEN drop
   na nu s" [" CORE-STR= IF a u j 1 - SD-QUOT-NODE EXIT THEN
   j u < IF a j + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? 0= IF
         drop
         na nu TYPE-MAY-ARRIVE-XT 0= IF
            na nu DECL-REJECT:TOKEN!
            s" unknown" s" " SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
         SD-MISSING!
         0 a u  na NULL-PTR - a NULL-PTR - -  j 1 + SD-LOOSE-APP EXIT
      THEN
      {: fam:n :}
      na nu fam SD-REACH
      a u fam  na NULL-PTR - a NULL-PTR - -  j 1 + SD-APP EXIT
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
: SD-PARSE-NODE-IMPL ( str n n -- n n ) {: a:^ u:n i:n want:n :}
   a u i SD-LEVEL+
   a u i want SD-PARSE-NODE-ONE
   TYPE-LEVEL @ 1 - TYPE-LEVEL ! ;

: SD-PARSER-INSTALL ( -- ) [: SD-PARSE-NODE-IMPL ;] is SD-PARSE-NODE ;
SD-PARSER-INSTALL

: SD-SQUARE ( n n -- n ) {: depth:n c:n :}   \ `[` depth after one byte
   c 91 = IF depth 1 + EXIT THEN
   c 93 = IF depth 1 - EXIT THEN
   depth ;
: SD-QUOT-OPEN? ( str -- bool ) {: a:^ u:n :}
   0 u 0 ?do a i + c@ SD-SQUARE loop 0 > ;

\ A declaration's tokens share one buffer: parse-name never refills, a replay
\ reads one body. A type with a `[` open is left to PARSE-QUOT to reject.
: SD-SPAN-REST ( str n str str -- ^u8 n ) {: a:^ u:n depth:n ca:^ cu:n na:^ nu:n :}
   u depth
   BEGIN dup 0 > WHILE
      NEXT dup 0= IF YES ELSE
         2dup ca cu CORE-STR=CI IF YES ELSE 2dup TERMINATOR? THEN THEN IF
         UNGET  drop a swap
         2dup SD-QUOT-OPEN? IF EXIT THEN
         DECL-REJECT:TOKEN!
         s" unclosed" na nu s" application" SD-NOUN-TEXT
         SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
      2swap nip  >r 2dup r> -rot SD-DEPTH  -rot
      swap NULL-PTR - a NULL-PTR - - +  swap
   REPEAT
   drop a swap ;

\ The type to the token closing each `<` and `[` it opens, a span of the source
\ or replay body that SD-MISSING! may keep; SD-SPAN-REST ends it early.
public
: SD-SPAN ( str str str -- str ) {: a:^ u:n ca:^ cu:n na:^ nu:n :}
   0 a u SD-DEPTH {: depth:n :}
   depth 1 < IF a u EXIT THEN
   a u depth ca cu na nu SD-SPAN-REST 2dup DECL-REJECT:TOKEN! ;
private

\ The node of a whole type text.
: SD-WHOLE-NODE ( str n -- n ) {: a:^ u:n want:n :}
   a u 0 want SD-PARSE-NODE {: node:n j:n :}
   j u <> IF
      a j +  u j -  {: ba:^ bu:n :}
      ba bu DECL-REJECT:TOKEN!
      s" unexpected bytes after a whole" s" "
      s" remove the bytes after the type, or separate them from it with a blank"
      TYPE-DECL:E-TDECL-PAYLOAD SD-REJECT throw THEN
   node ;

\ SD-TYPE-NODE ( text fam want noun -- node ): want is the use of a parameter that is
\ the whole type; FIELD-MISS stays set.
public
: SD-TYPE-NODE ( str n n str -- n )
   {: a:^ u:n fam:n want:n na:^ nu:n :}
   FAM @ TYPE-LEVEL @ {: ofam:n olevel:n :}
   TYPE@ NOUN@ {: ota:^ otu:n oa:^ ou:n :}
   fam FAM !  0 TYPE-LEVEL !  0 POINTEE !  0 SD-ARG-N !
   a u TYPE!  na NOUN-A !  nu NOUN-U !
   PF-KIND-MEMO-START
   [: a u want SD-WHOLE-NODE ;]
   [: PF-KIND-MEMO-END  ofam FAM !  olevel TYPE-LEVEL !
      ota otu TYPE!  oa NOUN-A !  ou NOUN-U ! ;] finally ;
private

\ A field type's text, from its first token (SD-SPAN).
: RESOLVE-TYPE ( str n -- n ) {: want:n :}
   dup 0= IF 2drop s" missing type"
      s" write a type after the field name, as in `FIELD x n`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup DECL-REJECT:TOKEN!
   s" ;structure" s" type" SD-SPAN
   2dup TYPE!
   FAM @ want s" type" SD-TYPE-NODE ;

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
      TDECL-NICHE-MESSAGE$
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

: DERIVE-FEATURE? ( str -- bool )
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
: DERIVE-ONE ( str -- )
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

: EMIT-FIELD ( str n -- )
   SCHEMA-ROOT+ {: sch:n :}
   FAM @ sch SCHEMA-ROOT@ TFAM-SCH-WIDTH {: fw:n :}
   2dup DECL-REJECT:TOKEN!
   s" duplicate field name"
   SD-FIELD-FIX$ TFAM:E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ -rot sch
   SD-CELLS @  fw  SD-CELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
\ A field named like a generated member (MAKE, UNMAKE, F:AT, F:BYTES, F:CELLS, a
\ derived tail) would make the generator define a name twice, a die inside the
\ plan. Refused only under DERIVE addr, which header clauses have fixed by now.
: MEMBER-CLASH? ( str -- bool )
   2dup s" make" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" unmake" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup TFAM-ADDR-FIXED-TAIL? IF 2drop YES EXIT THEN
   TFAM-DERIVED-TAIL? ;
\ A public family's accessor is a word named by the field itself (`POINT:X`),
\ so its field name takes the definer name rule; a private family's is `POINT-X`.
: REQUIRE-FIELD-NAME ( str -- )    \ consumes the copy; throws on a member clash or a keyword accessor
   FAM @ TFAM-DERIVE-ADDR? 0= IF 2drop EXIT THEN
   FAM @ TFAM-GEN-PUBLIC? IF 2dup DEF-NAME-BAD? IF 2drop s" " s" " TFAM:E-TDECL-DEF-NAME DECL-REJECT:REJECT throw THEN THEN
   MEMBER-CLASH? IF
      s" field name is a generated member name"
      s" rename the field: under `DERIVE addr` the names `make`, `unmake`, `at`, `bytes`, `cells`, `eq`, `hash` and `tag` are taken"
      TYPE-DECL:E-TDECL-NAME DECL-REJECT:REJECT throw THEN ;
: FIELD-NAME ( -- str )                 \ a FIELD or BUFFER name, judged
   NEXT dup 0= IF
      2drop s" missing field name"
      s" write a name and a type after `FIELD`, as in `FIELD x n`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   {: na:^ nu:n :}
   na nu REQUIRE-FIELD-NAME
   OUTCOME-NAMES @ 0 <> IF
      na nu SD-FIELD-FIX$ NAME-CHECK THEN
   na nu ;
\ ITEMS-FULL's reason and fix, composed in one label.
: ITEMS-TEXTS ( -- str str )
   LABEL-START
   s" record takes more than " LABEL+
   EFFECT-DEPTH-MAX REJECTION:DIGITS LABEL+
   s"  items to make" LABEL+
   LABEL$ nip {: r:n :}
   s" split the record: MAKE takes one item per `FIELD` and one per buffer cell, at most " LABEL+
   EFFECT-DEPTH-MAX REJECTION:DIGITS LABEL+
   LABEL$ {: a:^ u:n :}  a r  a r +  u r - ;
\ Each item is at least one level of MAKE's effect, so no record of more items
\ than EFFECT-DEPTH-MAX can be made; USIG-DEPTH-FITS? measures nested types.
: ITEMS-FULL ( -- )
   ITEMS-TEXTS TYPE-DECL:E-TDECL-CAP DECL-REJECT:REJECT throw ;
: ITEM-ROOM ( -- n ) EFFECT-DEPTH-MAX SD-ITEMS @ - ;
: FIELD-CLAUSE ( -- )
   FIELD-NAME {: na:^ nu:n :}
   na nu DECL-FIELD!
   0 FIELD-MISS !
   NEXT {: ta:^ tu:n :}
   ta tu PK-CELL RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF
      ta tu FAM @ node REQUIRE-DERIVABLE
      ITEM-ROOM 1 < IF na nu DECL-REJECT:TOKEN! ITEMS-FULL THEN
      SD-ITEMS @ 1 + SD-ITEMS !
      na nu node EMIT-FIELD THEN
   -1 SEEN-FIELD ! ;

EXPORT CHECKER-RESOLVE:CK-REC-VALUE
: COUNT-BAD ( -- )
   s" buffer byte count must be a number or a constant above 0"
   s" give the byte count as a number literal or a constant's name, as in `BUFFER bytes 16`; bind a computed size with `constant` first"
   TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;
EXPORT CHECK:CK-REC-WID
EXPORT CHECK:CHECKER-BIND
\ Whether scope-find's answers are a using refusal: two used publics export the
\ tail and nothing earlier binds it, or a used public exports a global's tail.
: USING-SHAPE? ( ^n ^n n -- bool ) {: rec:^ used:^ flags:n :}
   rec NULL-PTR = IF flags SCOPE-FIND-AMBIGUOUS and 0 <> EXIT THEN
   rec CK-REC-WID WL:GLOBAL?  used NULL-PTR <>  and ;
\ The record a count name binds; a using refusal is raised as a bare call's.
: COUNT-REC ( str -- ^n ) {: a:^ u:n :}
   a u scope-find {: rec:^ used:^ used2:^ flags:n :}
   rec used flags USING-SHAPE? IF a u a u CHECKER-BIND drop THEN
   rec ;
\ A count token's value and true: an integer literal, or the value of the
\ constant it binds; false for anything else.
: COUNT-VALUE ( str -- n bool ) {: a:^ u:n :}
   a u num-parse {: v:n flt:bool ok:bool :}
   ok IF v flt 0= EXIT THEN
   a u COUNT-REC CK-REC-VALUE ;
\ A count name no record here holds the value of: in a replay, a program
\ constant only the run defines, so the declaration is left to the run.
: COUNT-UNREAD ( str -- ) {: a:^ u:n :}
   a u num-parse nip nip 0=  OUTCOME-NAMES @ 0 <>  and IF
      a u DW-COUNT MISSING!  E-UNRESOLVED throw THEN
   COUNT-BAD ;
\ `BUFFER name N`: N bytes inline from the next slot, in whole cells. N is a
\ number literal or a constant's name, read from its record: no program code runs.
: BUFFER-COUNT ( -- n )
   NEXT dup 0= IF
      2drop s" missing buffer byte count"
      s" write a byte count after the buffer's name, as in `BUFFER bytes 16`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   {: ca:^ cu:n :}
   ca cu COUNT-VALUE {: v:n ok:bool :}
   ok 0= IF ca cu COUNT-UNREAD THEN
   v 1 < IF COUNT-BAD THEN
   v ITEM-ROOM CELL * > IF ITEMS-FULL THEN     \ before its cells are counted
   v ;
: EMIT-BUFFER ( str n -- ) {: na:^ nu:n bytes:n :}
   bytes 1 - CELL / 1 + {: fw:n :}
   CC-U8 SCHEMA-CON SCHEMA-ROOT+ {: sch:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate field name"
   SD-FIELD-FIX$ TFAM:E-TFAM-DUP DECL-REJECT:EXPECT
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

: SD-DUP-FIX$ ( -- str ) s" rename the family: this package already declares a family with this name" ;
\ Registers the family na nu names, its duplicate placed at the token ta tu,
\ and answers its id.
: SD-DECL ( str str n -- n )
   {: na:^ nu:n ta:^ tu:n ar:n :}
   ta tu DECL-REJECT:TOKEN!
   s" duplicate family" SD-DUP-FIX$ TFAM:E-TFAM-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ TDECL-VIS na nu
   ar TK-PRODUCT TFAM-DECL ;
\ Opens family fam's fields: they follow the committed rows and the rows of the
\ members closed before it.
: SD-OPEN ( n -- )
   FAM !
   TYPE-FIELD:COUNT SD-FLDS @ + FLDBASE !
   0 NFLD !   0 SD-CELLS !   0 SD-ITEMS ! ;
: SD-REGISTER ( str str n -- ) SD-DECL SD-OPEN ;

: SD-MAKEABLE? ( -- bool )
   FAM @ TFAM-FLD-COUNT@ 0 > ;
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
\ The open family's field range and width, set at its end so a later member
\ may hold it by value.
: SD-SHAPE ( -- )
   FAM @ FLDBASE @ NFLD @ TFAM-FLD-RANGE!
   FAM @ SD-CELLS @ TFAM-SLOTS! ;
\ Judges the family's fields, its range set (SD-SHAPE), again once every member
\ has its fields: a pointee was read while it had none. The fault is placed
\ first: by CLOSE-ALL (through SD-CLOSE) at the member and the token its caller
\ gives, a plain STRUCTURE's name or `;MUTUAL`; by WRAP-DRIVE at the family.
: SD-SEAL ( -- )
   FAM @ TFAM-FLD-START@ {: base:n :}  FAM @ TFAM-FLD-COUNT@ {: count:n :}
   MISSING? IF E-UNRESOLVED throw THEN
   count 0 ?do
      base i + PF-SCH@ SCHEMA-ROOT@
      s" type is a pointer to a linear value and cannot own it"
      DECL-EVENT:REQUIRE-POINTEES
   loop ;
: SD-CLOSE ( -- )
   SD-SEAL
   FAM @ TFAM-OPAQUE? SD-MAKEABLE? 0= and IF
      s" opaque requires a family with fields"
      s" add a `FIELD` to the structure, or drop `OPAQUE`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   \ Only a structure with fields gets MAKE / UNMAKE; a fieldless one is an opaque
   \ one-cell family, as a bare NEWTYPE declares.
   SD-MAKEABLE? IF FAM @ STRUCTURE-MAKE:GENERATE ELSE FAM @ TK-CELL TFAM-KIND! THEN
   SD-ADDR-ARM ;

\ Opens member i: its fields follow, its header clauses and a replay's field
\ names repeat only within it, and SELF-A holds the first pointee its row
\ recorded while it was listed, none if it has none.
: MEM-BEGIN ( n -- ) {: i:n :}
   i M.FAM MEM-CELL @ SD-OPEN
   0 SEEN-FIELD !
   SEEN-CLEAR
   i M.PTEE-U MEM-CELL @ {: pu:n :}
   pu 0 <> IF i MEM-PTEE @ SELF-A ! THEN
   pu SELF-U !
   0 i M.LISTED MEM-CELL !
   DECL-EVENT:MEMBER ;
\ ;STRUCTURE: the open member is closed, its field range and width set, its
\ row given the next close rank.
: MEM-END ( -- )
   SD-SHAPE
   SD-FLDS @ NFLD @ + SD-FLDS !
   MEM-CLOSED @ FAM @ MEM-OF M.CLOSE MEM-CELL !
   MEM-CLOSED @ 1 + MEM-CLOSED ! ;
\ The member of close rank r, -1 for none.
: MEM-RANKED ( n -- n ) {: r:n :}
   MEM-N @ 0 ?do
      i M.CLOSE MEM-CELL @ r = IF i unloop EXIT THEN
   loop -1 ;
\ The close: every member, in the order its `;STRUCTURE` was read, judged
\ again, then generated; each fault names its member and stands at the token
\ ta tu.
: CLOSE-ALL ( str -- ) {: ta:^ tu:n :}
   MEM-N @ 0 ?do
      i MEM-RANKED dup MEM-NAME$ DECL-REJECT:FAMILY!
      ta tu DECL-REJECT:TOKEN!
      M.FAM MEM-CELL @ FAM !  SD-CLOSE
   loop ;

: CLAUSE ( -- bool )                    \ read + dispatch one body token; YES = ;STRUCTURE
   NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;STRUCTURE"
      s" end the declaration with `;STRUCTURE` after its last field"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop MEM-END YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2dup s" buffer" CORE-STR=CI IF 2drop BUFFER-CLAUSE NO EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2dup s" opaque" CORE-STR=CI IF 2drop OPAQUE-CLAUSE NO EXIT THEN
   2drop s" unexpected token in structure declaration"
   s" start each clause with `FIELD`, `POLICY`, `DERIVE` or `OPAQUE`, and end with `;STRUCTURE`"
   TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;
: CLAUSES ( -- ) BEGIN CLAUSE UNTIL ;

\ The name and arity of the header token a u, the header's parameters bound.
: HEADER ( ^u8 n -- ^u8 n n )
   2dup DECL-REJECT:SEAL-FAMILY
   DECL-HEADER 0= IF
      drop 2drop s" invalid type parameter header"
      s" name each type parameter once in the header, as in `STRUCTURE box<a>` or `STRUCTURE pair<a,b>`"
      TYPE-DECL:E-TDECL-ARITY DECL-REJECT:REJECT throw
   THEN ;
\ The header token a u after `STRUCTURE`, recorded as the family.
: STRUCTURE-HEADER ( str -- str n )
   2dup DECL-REJECT:FAMILY!
   dup 0= IF 2drop s" missing name" SD-NAME-FIX$ TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   HEADER ;

\ A plain STRUCTURE: one member, registered and opened at once, closed at its
\ ;STRUCTURE. Its name never counts as the terminator.
: DRIVE ( -- )
   NAME-TOKEN STRUCTURE-HEADER {: na:^ nu:n ar:n :}
   na nu NAME-GATE
   na nu na nu ar SD-DECL {: fam:n :}
   na nu fam NO MEM-ADD
   0 MEM-BEGIN
   CLAUSES
   na nu CLOSE-ALL ;

\ A header in the MUTUAL list: the member is listed, its family row added.
: LIST-ENTRY ( str -- )
   2dup DECL-REJECT:FAMILY!
   2dup DECL-FAMILY-NAME nip 0= IF
      2drop s" missing name"
      s" write the family name before its parameters, as in `box<a>`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   HEADER {: na:^ nu:n ar:n :}
   na nu NAME-GATE
   na nu na nu ar SD-DECL {: fam:n :}
   na nu fam YES MEM-ADD ;
\ The token after the list: end of input, a member or the terminator.
: LIST-END? ( str -- bool ) {: a:^ u:n :}
   u 0=  a u s" structure" CORE-STR=CI or  a u s" ;mutual" CORE-STR=CI or ;
: LIST ( -- )
   NEXT 2dup LIST-END? IF
      2drop s" missing name" LIST-NAME-FIX$
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   BEGIN LIST-ENTRY NEXT 2dup LIST-END? UNTIL
   UNGET ;

\ `STRUCTURE h` in a MUTUAL: h names a listed member with its listed arity,
\ which opens; its body is a plain STRUCTURE's. The name is judged as a plain
\ STRUCTURE's before it is looked up. A name `;MUTUAL` counts as the terminator,
\ as the verifier's scan ends the declaration there: once the member is
\ rejected, the load resumes after it.
: MEMBER ( -- )
   NEXT STRUCTURE-HEADER {: na:^ nu:n ar:n :}
   na nu NAME-GATE
   na nu MEM-FIND {: i:n :}
   i 0 < IF
      s" structure is not a member of this MUTUAL"
      s" list it after `MUTUAL`, or declare it outside"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   i MEM-LISTED@ 0= IF
      na nu DECL-REJECT:TOKEN!
      s" duplicate family" SD-DUP-FIX$ TFAM:E-TFAM-DUP DECL-REJECT:REJECT throw THEN
   i M.FAM MEM-CELL @ TFAM-ARITY@ ar <> IF
      s" header differs from its MUTUAL entry"
      s" give the header the type parameters its `MUTUAL` entry lists"
      TYPE-DECL:E-TDECL-ARITY DECL-REJECT:REJECT throw THEN
   i MEM-BEGIN
   CLAUSES ;

\ ;MUTUAL: a member still listed is rejected at its list entry.
: UNDECLARED ( -- )
   MEM-N @ 0 ?do
      i MEM-LISTED@ IF
         i MEM-NAME$ 2dup DECL-REJECT:FAMILY! DECL-REJECT:TOKEN!
         s" member has no STRUCTURE"
         s" declare `STRUCTURE name … ;STRUCTURE` before `;MUTUAL`, or drop the name from the list"
         TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   loop ;

: MUTUAL-STEP ( -- bool )               \ read + dispatch one member; YES = ;MUTUAL
   NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;MUTUAL"
      s" end the declaration with `;MUTUAL` after its last `;STRUCTURE`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" structure" CORE-STR=CI IF 2drop MEMBER NO EXIT THEN
   2dup s" ;mutual" CORE-STR=CI IF
      UNDECLARED  CLOSE-ALL YES EXIT THEN
   2drop s" unexpected token in mutual declaration"
   s" write each member as `STRUCTURE name … ;STRUCTURE` and end with `;MUTUAL`, with no comments"
   TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;

: MUTUAL-DRIVE ( -- ) LIST BEGIN MUTUAL-STEP UNTIL ;

\ The member table is cleared on both exits: a later declaration's family may
\ reuse a rejected member's id.
: SD-FRONT ( -- str str [ -- renders parses nominal ] )
   SD-KIND$ s" ;structure"
   [: SD-RESET [: DRIVE ;] [: MEM-CLEAR ;] finally ;] ;
: MUTUAL-FRONT ( -- str str [ -- renders parses nominal ] )
   s" mutual" s" ;mutual"
   [: SD-RESET [: MUTUAL-DRIVE ;] [: MEM-CLEAR ;] finally ;] ;

\ A byte no one name holds: a package qualifier, an application or a pointer.
: NAME-STOP? ( n -- bool ) {: c:n :}
   c 58 =  c 60 = or  c 94 = or ;
\ Rejects a DEFTYPE base the wrap sealed that is not one name, since its
\ converter NAME>BASE is named after it, offering the NEWTYPE that loads.
: ONE-NAME-GATE ( str str -- ) {: na:^ nu:n ba:^ bu:n :}
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
   NAME-A @ NAME-U @ DECL-REJECT:FAMILY!
   nu 0= IF s" missing name"
      s" write the family name after `NEWTYPE`, as in `NEWTYPE path str`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   NAME-A @ NAME-U @ DECL-REJECT:TOKEN!
   na nu 2dup TDECL-NAME-PREFLIGHT-XT REQUIRE-NAME
   na nu NAME-A @ NAME-U @ 0 SD-REGISTER
   0 FIELD-MISS !
   NEXT 2dup BASE-U ! BASE-A !
   PK-CELL RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF s" base" node EMIT-FIELD THEN
   NEXT nip 0 <> IF
      s" a newtype takes one base type"
      s" give one base type after the name, as in `NEWTYPE path str`, and end the line there"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   DECL-REJECT:AT-FAMILY SD-SHAPE SD-SEAL
   na nu BASE-A @ BASE-U @ ONE-NAME-GATE ;

\ Takes the name as written and the kind, the declaration's keyword, "newtype"
\ or "deftype". The stream has no closer, so a rejected one resynchronizes to
\ its end.
: WRAP-FRONT ( str str -- str str [ -- parses nominal ] )
   2swap NAME-U ! NAME-A !
   s" " [: SD-RESET WRAP-DRIVE ;] ;

: DECLARE-STRUCTURE ( -- ) SD-FRONT DECL-FRONT:RUN ;

\ Declares the structures a MUTUAL lists, which may point at each other.
: DECLARE-MUTUAL ( -- ) MUTUAL-FRONT DECL-FRONT:RUN ;

EXPORT CHECK:CHECKER-UNSEEN-MARK
EXPORT CHECK:CHECKER-TYPE-MARK
\ A family whose buffer count only the run knows has no records here, so later
\ uses of it and its generated words defer rather than reject.
: COUNT-LEFT ( n -- n )
   dup DW-COUNT = IF CHECKER-UNSEEN-MARK NO CHECKER-TYPE-MARK THEN ;

public

\ Registers a STRUCTURE from a body a tool lexed, by DECLARE-STRUCTURE's grammar,
\ defining no word; a body without ;STRUCTURE is rejected "missing ;STRUCTURE".
\ Answers the first span left to the run and its deferral cause, 0 for none.
: REPLAY-STRUCTURE ( str str -- str n )
   SD-FRONT DECL-FRONT:REPLAY-OUTCOME COUNT-LEFT ;

\ Registers a MUTUAL from a body a tool lexed, by DECLARE-MUTUAL's grammar,
\ defining no word: first list token, then the rest of the body, which holds
\ the other list tokens, the members and ;MUTUAL. A body without ;MUTUAL is
\ rejected "missing ;MUTUAL". Answers as REPLAY-STRUCTURE.
: REPLAY-MUTUAL ( str str -- str n )
   MUTUAL-FRONT DECL-FRONT:REPLAY-OUTCOME COUNT-LEFT ;

private

\ TDECL-FAM-REG holds the family only when the wrap left no rejection,
\ which a multi-error load tallies and any other load throws.
: WRAP-KEEP ( n -- ) {: tally:n :}
   REJECTION:TALLY tally <> IF EXIT THEN
   FAM @ TDECL-FAM-REG ! ;

\ Rejects `DEFTYPE` NAME at BASE, armed with reason, help and code.
: ONE-NAME-REJECT ( str str str str n -- )
   {: na:^ nu:n ba:^ bu:n wa:^ wu:n fa:^ fu:n code:n :}
   s" deftype" DECL-REJECT:OPEN
   na nu DECL-REJECT:FAMILY!
   ba bu DECL-REJECT:TOKEN!
   wa wu fa fu code DECL-REJECT:REJECT
   DECL-REJECT:GUARD-CODE ;

public

\ Whether BASE holds no `[`: a quotation type spans tokens and a DEFTYPE base
\ is one; if it does, rejects NAME. The wrap judges any other base.
: SD-ONE-NAME ( str str -- bool ) {: na:^ nu:n ba:^ bu:n :}
   NO bu 0 ?do ba i + c@ 91 = or loop 0= IF YES EXIT THEN
   na nu ba bu s" a deftype base is one name"
   s" write the base as one bare name, with no `:`, `<`, `^` or `[`, as in `DEFTYPE REV str`: the converter `REV>STR` is named after it"
   TYPE-DECL:E-TDECL-SYNTAX ONE-NAME-REJECT
   NO ;

\ Registers the family HEADER names over BASE: one field base, no word made;
\ each record of the name stands at TOKEN, the name as written. KIND is the
\ declaration's keyword, "newtype" or "deftype", which its records name.
\ Answers as REPLAY-STRUCTURE; TDECL-FAM-REG is the family, or -1 if none.
: SD-WRAP-OUTCOME ( str str str str -- str n )
   {: ha:^ hu:n ta:^ tu:n ba:^ bu:n ka:^ ku:n :}
   -1 TDECL-FAM-REG !  REJECTION:TALLY {: tally:n :}
   ha hu ba bu ta tu ka ku WRAP-FRONT DECL-FRONT:REPLAY-OUTCOME {: ma:^ mu:n why:n :}
   why 0= IF tally WRAP-KEEP THEN
   ma mu why ;

private

\ The same, raising E-UNRESOLVED instead of answering, and answering the
\ family, or -1 when a multi-error load rejected it.
: SD-WRAP ( ^u8 n ^u8 n ^u8 n ^u8 n -- n )
   SD-WRAP-OUTCOME 0 <> IF 2drop E-UNRESOLVED throw THEN 2drop
   TDECL-FAM-REG @ ;

;package

EXPORT STRUCTURE-DECL:DECLARE-STRUCTURE
EXPORT STRUCTURE-DECL:DECLARE-MUTUAL
EXPORT STRUCTURE-DECL:SD-WRAP
EXPORT STRUCTURE-DECL:REPLAY-STRUCTURE
EXPORT STRUCTURE-DECL:REPLAY-MUTUAL
EXPORT STRUCTURE-DECL:SD-ONE-NAME
EXPORT STRUCTURE-DECL:SD-WRAP-OUTCOME

\ The one-cell family NAME.
: NEWTYPE-CELL ( str -- ) {: na:^ nu:n :}
   na nu 2dup s" newtype" TDECL-DEFFAMILY ;

global

\ STRUCTURE is a documented global (package-first exception, like the SUMTYPE
\ opener); it parses its body to ;STRUCTURE at interpret time, so its
\ checked effect is ( -- ).
: STRUCTURE ( -- ) DECLARE-STRUCTURE ;
INTRINSIC-DECLARATION INTRINSIC

\ MUTUAL is a documented global, as STRUCTURE is: it lists the families of a
\ cycle of structures, then holds one STRUCTURE ... ;STRUCTURE per listed
\ family, in any order, to ;MUTUAL, as one declaration. A member whose
\ ;STRUCTURE has not been read is named only beneath `^`. It parses its body at
\ interpret time, so its checked effect is ( -- ).
: MUTUAL ( -- ) DECLARE-MUTUAL ;
INTRINSIC-DECLARATION INTRINSIC

\ NEWTYPE is a documented global: a base is the type on the name's line, and a \ there or a
\ parametrised name leaves the one-cell family.
: NEWTYPE ( -- )
   SRCLOC:TOKEN$ {: kw:^ ku:n :}
   parse-name {: na:^ nu:n :}
   kw ku na nu REJECTION:NOT-A-NAME
   nu 0= IF na nu kw ku s" newtype" TDECL-DEFFAMILY EXIT THEN
   0 0= 0= nu 0 ?do na i + c@ 60 = or loop
   IF na nu NEWTYPE-CELL EXIT THEN
   LBUF:STORAGE-LINE-TOKEN {: ta:^ tu:n :}
   tu 0= IF na nu NEWTYPE-CELL EXIT THEN
   ta LBUF:STORAGE-UNREAD
   tu 1 = IF ta c@ 92 = IF na nu NEWTYPE-CELL EXIT THEN THEN
   na nu 2dup LBUF:STORAGE-PARSE-TYPE
   s" newtype" SD-WRAP drop ;
INTRINSIC-DECLARATION INTRINSIC

;package
