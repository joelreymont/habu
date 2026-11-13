\ name-length-test.f - every checker word source can call with a name or a text
\ refuses a length that describes no memory, -1 and the maximum cell, and a
\ stored type one past its buffer. A query answers no and moves no pool mark; a
\ declaring word ends with its own refusal. A name has no ceiling: a long one is
\ taken, while a package name past its row is refused by name.

require src/lib/error.f
require src/lib/memory.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/habu/layout.f

package NAME-LENGTH-TEST

$400 constant OUT-CAP
$8000 constant ERR-CAP             \ a diagnostic that names a whole name
10000 constant TIMEOUT-MS
-1 1 rshift constant MAX-CELL
\ A name has no ceiling: a definer captures a word's name and one separator
\ into the definition's body text, which grows with the definition, before it
\ writes the record. LONG-LEN is a name's length past the old 8000-byte cap.
20000 constant LONG-LEN
\ The longest stored-type text: src/core/checker.f LBUF-SIG-CAP, the buffer
\ every stored type is spelled into.
$1000 constant TEXT-CAP
create OUT OUT-CAP allot
create ERR ERR-CAP allot

\ Room for a name or text one past the longest a case hands, in whole pages
\ between two guard pages. Each name and text ends at a guard page, so a word
\ that reads past a short one faults at once instead of reading on through
\ what is mapped after it. Every byte of the names is q.
LONG-LEN TEXT-CAP max 1 + STACK-ABI:PAGE-BYTES / 1 + STACK-ABI:PAGE-BYTES *
   constant ROOM
PTR-VARIABLE NAMES
: NAMES! ( -- )
   ROOM MEM:ALLOC-GUARDED drop {: p:ptr :}
   ROOM 0 ?do 113 p i + c! loop
   p NAMES ! ;
NAMES!

: TAIL ( n -- ptr u8 n )
   {: u:n :}
   NAMES @ ROOM + u - u ;

\ The name the next case hands its word: LEN bytes of q ending at the guard
\ page, or for a length no span has, its two last bytes.
variable LEN
: BAD$ ( -- ptr u8 n )
   LEN @ {: u:n :}
   u 0 < u ROOM > or IF 2 TAIL drop u EXIT THEN
   u TAIL ;

\ The text the next case hands its word: LEN bytes ending at the guard page,
\ `head` and then spaces, or for a length no span has, two spaces.
PTR-VARIABLE TEXTS
: TEXTS! ( -- ) ROOM MEM:ALLOC-GUARDED drop TEXTS ! ;
TEXTS!
: TEXT ( ptr u8 n -- ptr u8 n )
   {: h:ptr hu:n :}
   TEXTS @ {: p:ptr :}
   ROOM 0 ?do 32 p i + c! loop
   LEN @ {: u:n :}
   u 0 < u ROOM > or IF p ROOM + 2 - u EXIT THEN
   p ROOM + u - {: t:ptr :}
   hu 0 ?do h i + c@ t i + c! loop
   t u ;

\ A definition CHECK certifies, and a type every storage gate admits: the
\ product this file declares below.
: BODY$ ( -- ptr u8 n ) s" NLT-W ( -- )" TEXT ;
: STORED$ ( -- ptr u8 n ) s" nlt-pair" TEXT ;

: SIG$ ( -- ptr u8 n ) s" --" ;
: CAST$ ( -- ptr u8 n ) s" len -- n" ;
: CAST-NAME$ ( -- ptr u8 n ) s" nltcast" ;
: CLAUSE$ ( -- ptr u8 n ) s" drop 1 +" ;
: CLAUSE-SIG$ ( -- ptr u8 n ) s" ( n -- n )" ;

\ The checker's pool marks, which a refused name leaves where it found them.
13 constant MARK-N
create BEFORE MARK-N cells allot
: MARK ( n -- n )
   case
      0 of SYM-N @ endof
      1 of SYM-STR-U @ endof
      2 of DFER-END @ endof
      3 of NORET-END @ endof
      4 of UEND @ endof
      5 of VREC-N @ endof
      6 of VREC-STR-U @ endof
      7 of CTN @ endof
      8 of CT-STR-U @ endof
      9 of CHECKER-PACKAGE-U @ endof
      10 of EXT-FREE-N @ endof
      11 of ASIG-ROW-U @ endof
      12 of ASIG-STR-U @ endof
      0 swap
   endcase ;
: MARKS! ( -- ) MARK-N 0 ?do i MARK BEFORE i cells + ! loop ;
: MARKS-SAME? ( -- bool )
   0 0= MARK-N 0 ?do i MARK BEFORE i cells + @ = and loop ;

\ A query or a no-op ends a returning case: its answer is a refusal and no
\ pool mark moved.
: VERDICT ( bool -- )
   MARKS-SAME? and IF s" refused" ELSE s" taken" THEN type cr ;

\ ---- the cases a child runs -------------------------------------------------
: RESOLVES ( -- ) MARKS! BAD$ CHECKER-RESOLVES? 0= VERDICT ;
: CERT-DUP ( -- ) MARKS! BAD$ CHECKER-CERT-DUP? 0= VERDICT ;
: DEAD ( -- ) MARKS! BAD$ CTL-DEAD? 0= VERDICT ;
: QUERY ( -- ) MARKS! BAD$ EFFECT-QUERY 0= VERDICT ;
: LBUF-NAME ( -- ) MARKS! BAD$ CHECKER-LBUF-NAME-OK? 0= VERDICT ;
: ASIG-NAME ( -- ) MARKS! s" " 0 0= BAD$ CHECKER-ASIG-KNOWN? 0= VERDICT ;
: ASIG-PKG ( -- ) MARKS! BAD$ 0 0= s" x" CHECKER-ASIG-KNOWN? 0= VERDICT ;
: ASIG-MISSING ( -- ) MARKS! s" " 0 0= BAD$ CHECKER-ASIG-MISSING? 0= VERDICT ;
: ASIG-ROW ( -- ) MARKS! s" " 0 0= BAD$ CHECKER-ASIG-ROW-FOR 0= VERDICT ;
: RESERVED ( -- ) MARKS! BAD$ TYPE-RESERVED? VERDICT ;
: TRY-NAME ( -- ) MARKS! BAD$ s" f n" CHECKER-TRY-RECORD nip nip 0 <> VERDICT ;
: TRY-FIELDS ( -- )
   MARKS! s" zzrec" BAD$ CHECKER-TRY-RECORD nip nip 0 <> VERDICT ;
: CTOR-WORD ( -- ) MARKS! BAD$ TFAM-CTOR-WORD? 0= VERDICT ;
: UNDEFINE-GUARD ( -- ) MARKS! BAD$ CHECKER-UNDEFINE-GUARD 0 0= VERDICT ;
: FREE-TAIL ( -- ) MARKS! BAD$ EXT-MARK-FREE-TAIL 0 0= VERDICT ;
: HEAD-ROW ( -- ) MARKS! BAD$ CHECKER-RESOLVE:HEAD-ROW NULL-PTR = VERDICT ;
: PRIM ( -- ) MARKS! BAD$ PRIM-SPEC:FIND 0 < VERDICT ;
: LAYOUT ( -- ) MARKS! BAD$ CHECKER-LAYOUT-INFO nip nip 0= VERDICT ;
: STORAGE ( -- ) MARKS! BAD$ CHECKER-STORAGE-INFO nip 0= VERDICT ;
: DYNAMIC ( -- ) MARKS! BAD$ CHECKER-DYNAMIC-INFO nip 0= VERDICT ;
: MATCH-FAM ( -- ) MARKS! BAD$ TFL-MATCH-FAM? nip 0= VERDICT ;
: CON-FAM ( -- ) MARKS! BAD$ TFL-CON-FAM? nip 0= VERDICT ;
: LAYOUT-TEXT ( -- ) MARKS! STORED$ CHECKER-LAYOUT-INFO nip nip 0= VERDICT ;
: STORAGE-TEXT ( -- ) MARKS! STORED$ CHECKER-STORAGE-INFO nip 0= VERDICT ;
: DYNAMIC-TEXT ( -- ) MARKS! STORED$ CHECKER-DYNAMIC-INFO nip 0= VERDICT ;

: DEFER-IT ( -- ) BAD$ CHECKER-DEFER ;
: UNDEFINE-IT ( -- ) BAD$ CHECKER-UNDEFINE ;
: HERE-IT ( -- ) BAD$ CHECKER-DEFINED-HERE? drop ;
: LINEAR-IT ( -- ) BAD$ CHECKER-DEFLINEAR ;
: RECORD-IT ( -- ) BAD$ s" f n" CHECKER-DEFRECORD ;
: FIELDS-IT ( -- ) s" zzrec" BAD$ CHECKER-DEFRECORD ;
: TRUNCATE-RAW-IT ( -- ) BAD$ CHECKER-USIGS-TRUNCATE-FROM-RAW ;
: TRUNCATE-IT ( -- ) BAD$ CHECKER-USIGS-TRUNCATE-FROM ;
: PACKAGE-IT ( -- ) BAD$ CHECKER-PACKAGE ;
: USING-IT ( -- ) BAD$ CHECKER-USING ;
: USING-PUSH-IT ( -- ) BAD$ CHECKER-USING-PUSH ;
: PART-PATH-IT ( -- ) BAD$ CHECKER-RESOLVE:PART-PATH 2drop ;
: EXPORT-IT ( -- ) BAD$ CHECKER-EXPORT ;
: FAMILY-IT ( -- ) BAD$ BAD$ CHECKER-DEFFAMILY ;
: SUM-IT ( -- ) BAD$ s" VARIANT vacant ;VARIANT" CHECKER-DEFSUM ;
: SUM-NOEND-IT ( -- ) BAD$ s" 0" CHECKER-DEFSUM-NOEND ;
: PRODUCT-IT ( -- ) BAD$ s" FIELD a n" CHECKER-DEFPRODUCT ;
: CHECK-IT ( -- ) BODY$ CHECK drop ;
: CHECK!-IT ( -- ) BODY$ CHECK! drop ;
: CANDIDATE-IT ( -- ) BODY$ CHECK-CANDIDATE! drop ;
: HOOK-IT ( -- ) BODY$ CHECK:DEFINITION drop ;

\ A product of this file's own, so a field transaction has a family to add to:
\ the family of the field row it just committed.
PRODUCT nlt-pair
   FIELD a n
;PRODUCT
TYPE-FIELD:COUNT 1 - TYPE-FIELD:FAMILY@ constant PAIR
: FIELD-ADD-IT ( -- )
   TYPE-FIELD-OWNER:OPEN PAIR TYPE-FIELD:NO-VARIANT BAD$ 0 0 0 0 0 0 0
   TYPE-FIELD-OWNER:ADD drop ;

\ ---- the parent's side -------------------------------------------------------
create LBL 128 allot
variable LBL-U
: LBL+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ LBL LBL-U @ + i + c! loop
   LBL-U @ u + LBL-U ! ;
: RELABEL ( -- ) LBL LBL-U @ T-LABEL ;

\ A declaring word's refusal ends the process, so every case runs `src` in a
\ forked child, with LEN at `u`.
: RUN-AT ( n ptr u8 n ptr u8 n -- len len PROCESS:ending )
   {: u:n at:ptr atu:n src:ptr srcu:n :}
   0 LBL-U !  src srcu LBL+  s"  at " LBL+  at atu LBL+
   u LEN !
   src srcu OUT OUT-CAP >LEN ERR ERR-CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN ;

\ The child prints that its word refused, and nothing else, and exits 0.
: RETURNS-AT ( n ptr u8 n ptr u8 n -- )
   {: u:n at:ptr atu:n src:ptr srcu:n :}
   u at atu src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED=
   RELABEL ERR erru LEN>N s" " T$=
   RELABEL OUT outu LEN>N S\" refused\n" T$= ;

\ The child ends with the word's own refusal: `rc` and its one-line message.
: DIES-AT ( n ptr u8 n ptr u8 n n ptr u8 n -- )
   {: u:n at:ptr atu:n src:ptr srcu:n rc:n want:ptr wantu:n :}
   u at atu src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc rc T-ENDING-EXITED=
   RELABEL outu LEN>N 0 T=
   RELABEL ERR erru LEN>N want wantu T$= ;

\ -1, the cap plus one and the maximum cell: no text the cap bounds has any of
\ them.
: RETURNS-PAST ( n ptr u8 n -- )
   {: cap:n src:ptr srcu:n :}
   -1 s" -1" src srcu RETURNS-AT
   cap 1 + s" cap+1" src srcu RETURNS-AT
   MAX-CELL s" max" src srcu RETURNS-AT ;

\ -1 and the maximum cell, for a name or a text with no bound of its own.
: RETURNS ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   -1 s" -1" src srcu RETURNS-AT
   MAX-CELL s" max" src srcu RETURNS-AT ;
: DIES ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n rc:n want:ptr wantu:n :}
   -1 s" -1" src srcu rc want wantu DIES-AT
   MAX-CELL s" max" src srcu rc want wantu DIES-AT ;

: TEST-QUERIES ( -- )
   s" RESOLVES" RETURNS
   s" CERT-DUP" RETURNS
   s" DEAD" RETURNS
   s" QUERY" RETURNS
   s" LBUF-NAME" RETURNS
   s" ASIG-NAME" RETURNS
   s" ASIG-PKG" RETURNS
   s" ASIG-MISSING" RETURNS
   s" ASIG-ROW" RETURNS
   s" RESERVED" RETURNS
   s" TRY-NAME" RETURNS
   s" CTOR-WORD" RETURNS
   s" UNDEFINE-GUARD" RETURNS
   s" FREE-TAIL" RETURNS
   s" HEAD-ROW" RETURNS
   s" PRIM" RETURNS
   s" LAYOUT" RETURNS
   s" STORAGE" RETURNS
   s" DYNAMIC" RETURNS ;

\ The family lookups copy the name into the checker's token buffer, which
\ refuses a maximum-cell length by ending the process.
: FAM-DIES-AT-MAX ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   -1 s" -1" src srcu RETURNS-AT
   MAX-CELL s" max" src srcu 76 S\" checker: token buffer too large\n" DIES-AT ;

: TEST-FAMILIES ( -- )
   s" MATCH-FAM" FAM-DIES-AT-MAX
   s" CON-FAM" FAM-DIES-AT-MAX ;

\ An uncaught code above 255 exits 70 when the checker rendered it, else 67.
\ In this file's package PART-PATH refuses a long path (7136).
: TEST-DECLARERS ( -- )
   s" DEFER-IT" 76 S\" checker: symbol string capacity overflow\n" DIES
   s" UNDEFINE-IT" 76 S\" checker: symbol string capacity overflow\n" DIES
   s" HERE-IT" 76 S\" checker: symbol string capacity overflow\n" DIES
   s" LINEAR-IT" 70 S\" checker: bad or duplicate signature type\n" DIES
   s" RECORD-IT" 70 S\" checker: bad or duplicate value-record type\n" DIES
   s" TRUNCATE-RAW-IT" 76 S\" checker: missing signature truncation mark\n" DIES
   s" TRUNCATE-IT" 83 S\" seal: cannot truncate sealed checker signatures\n" DIES
   s" PACKAGE-IT" 76 S\" checker: package name too long\n" DIES
   LONG-LEN s" long" s" PACKAGE-IT" 67 S\" hb: uncaught throw code 7154\n" DIES-AT
   s" USING-IT" 76 S\" checker: using name length describes no memory\n" DIES
   LONG-LEN s" long" s" USING-IT" 67 S\" hb: uncaught throw code 7154\n" DIES-AT
   s" USING-PUSH-IT" 67 S\" hb: uncaught throw code 7136\n" DIES
   -1 s" -1" s" PART-PATH-IT" 67 S\" hb: uncaught throw code 7154\n" DIES-AT
   LONG-LEN s" long" s" PART-PATH-IT" 67 S\" hb: uncaught throw code 7136\n" DIES-AT
   MAX-CELL s" max" s" PART-PATH-IT" 67 S\" hb: uncaught throw code 7136\n" DIES-AT
   s" EXPORT-IT" 67 S\" hb: uncaught throw code 7113\n" DIES
   s" FAMILY-IT" 70 S\" habu: bad newtype declaration '': missing name\nhb: uncaught throw code 7107\n" DIES
   s" SUM-IT" 70 S\" habu: bad sumtype declaration '': missing name\nhb: uncaught throw code 7107\n" DIES
   s" SUM-NOEND-IT" 70 S\" habu: bad sumtype declaration '': missing ;SUMTYPE\nhb: uncaught throw code 7107\n" DIES
   s" PRODUCT-IT" 70 S\" habu: bad product declaration '': missing name\nhb: uncaught throw code 7107\n" DIES
   s" FIELD-ADD-IT" 67 S\" hb: uncaught throw code 7101\n" DIES ;

\ TRUST, CHECKER-DEFCAST and CHECKER-NAMED-USE have no effect a
\ checked body may call, so the child names them at its top level or in a
\ TRUSTED: body, as verify-source.f's NAMED-USE calls CHECKER-NAMED-USE: a
\ refused name publishes no use and reads none of its bytes. TRUST-DECL and
\ TRUST-RAW are sealed (src/core/checker.f: the interpreter refuses them), so a
\ TRUSTED: body calls them, as the engine's own callers do. A cast reaches
\ its name once its signature certifies, and stores it.
$400 constant WANT-CAP
create WANT WANT-CAP allot
variable WANT-U
: WANT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a WANT WANT-U @ + u BYTE-COPY
   WANT-U @ u + WANT-U ! ;

: STALE$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   0 WANT-U !
   s" E-TRUST-UNRESOLVED habu: trust row for '" WANT+
   a u WANT+
   S\" ' names no word where its record lands: nothing in the open section's wordlist, the global wordlist outside a package, or PKG's public wordlist for PKG:TAIL is spelled that way, so the effect would be recorded against a symbol the engine never defined. Delete the row, correct the name to the word it was meant to describe, or write it in the section that defines that word\nhb: uncaught throw code 7143\n" WANT+
   WANT WANT-U @ ;

: TEST-TOP-LEVEL ( -- )
   s" TRUSTED: NL-NU ( -- ) MARKS! BAD$ CHECKER-NAMED-USE 0 0= VERDICT ; NL-NU"
      RETURNS
   s" BAD$ CAST$ CHECKER-DEFCAST" 76 S\" checker: symbol string capacity overflow\n" DIES ;

\ ---- a name no record is keyed by --------------------------------------------
\ A name with a second inner ':' has a length a name has and keys no record:
\ the engine defines no such word (rc 75), and CHECKER-RECORD-SYM answers
\ symbol 0 for one. No row may carry symbol 0: a defer row keyed 0 is the
\ defer store's terminator and hides every row after it. A trust row refuses
\ the name as naming no word, and echoes it; every other word that records a
\ symbol for it names it as malformed and throws. Each refusal is rendered
\ before its throw, so the child exits 70.
: MAL$ ( -- ptr u8 n ) s" q:q:q" ;
: MALFORMED$ ( -- ptr u8 n )
   S\" E-BAD-QUALIFIED-RECORD habu: record for 'q:q:q' refused: malformed qualified name, where one non-edge ':' selects a package and a second ':' names no word. Use one ':' qualifier, e.g. PKG:WORD\nhb: uncaught throw code 7152\n" ;
: MAL-DIES ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n rc:n want:ptr wantu:n :}
   0 s" two inner colons" src srcu rc want wantu DIES-AT ;
: TEST-MALFORMED ( -- )
   s" MAL$ CHECKER-DEFER" 70 MALFORMED$ MAL-DIES
   s" MAL$ CHECKER-UNDEFINE" 70 MALFORMED$ MAL-DIES
   s" MAL$ CAST$ CHECKER-DEFCAST" 70 MALFORMED$ MAL-DIES
   s" MAL$ SIG$ TRUST" 70 MAL$ STALE$ MAL-DIES
   s" TRUSTED: NL-TD ( -- ) MAL$ SIG$ TRUST-DECL ; NL-TD" 70 MALFORMED$ MAL-DIES
   s" TRUSTED: NL-TR ( -- ) MAL$ SIG$ TRUST-RAW ; NL-TR" 70 MALFORMED$ MAL-DIES ;

\ CHECKER-DEFRECORD's and CHECKER-TRY-RECORD's second string is the record's
\ field text, which is no name, so only -1 and the maximum cell describe no
\ text.
: TEST-FIELD-TEXT ( -- )
   s" TRY-FIELDS" RETURNS
   s" FIELDS-IT" 70 S\" checker: empty value-record\n" DIES ;

\ A definition's source text, which CHECK and its entries judge, and a
\ signature, which CHECK-DOES! and CHECKER-DEFCAST parse, are no names either:
\ only -1 and the maximum cell describe none of them. The storage gates answer
\ no and move no pool mark. CHECK and every entry that reaches it end with the
\ token buffer's refusal, which the maximum cell already met. A signature is
\ refused as one that does not parse: a does> clause is rejected, a cast is
\ E-CAST-ARITY. CHECK-DOES! is trusted-only and CHECKER-DEFCAST has no effect a
\ checked body may call, so the child names them at its top level; the cast's
\ refusal names this file, which the child loads, and exits 70. hb's checker
\ holds only what its own files' top levels interned (the builder's checker
\ checked hb's definitions), so the first top-level CHECK-DOES! interns its row:
\ the child runs one with a signature that parses before it takes the marks.
: TOO-LARGE$ ( -- ptr u8 n ) S\" checker: token buffer too large\n" ;
SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL LOADING
: CAST-WANT$ ( -- ptr u8 n )
   SB-RESET
   s" E-CAST-ARITY habu: " SB-APPEND
   LOADING @ data-base SRCLOC:PATHLEN-CELL + @ SB-APPEND
   s" : cast: 'nltcast': the signature's length describes no memory" SB-APPEND
   S\" \nhb: uncaught throw code 7129\n" SB-APPEND
   SB$ ;
: TEST-TEXTS ( -- )
   TEXT-CAP s" LAYOUT-TEXT" RETURNS-PAST
   TEXT-CAP s" STORAGE-TEXT" RETURNS-PAST
   TEXT-CAP s" DYNAMIC-TEXT" RETURNS-PAST
   s" CHECK-IT" 76 TOO-LARGE$ DIES
   s" CHECK!-IT" 76 TOO-LARGE$ DIES
   s" CANDIDATE-IT" 76 TOO-LARGE$ DIES
   s" HOOK-IT" 76 TOO-LARGE$ DIES
   s" CLAUSE$ CLAUSE-SIG$ CHECK-DOES! drop MARKS! CLAUSE$ BAD$ CHECK-DOES! 0= VERDICT"
   RETURNS
   s" CAST-NAME$ BAD$ CHECKER-DEFCAST" 70 CAST-WANT$ DIES ;

\ ---- a long name -------------------------------------------------------------
\ `: <u bytes of c> ;`, in a fresh mapping.
: DEF-SRC ( n n -- ptr u8 n )
   {: u:n c:n :}
   u 4 + MEM-ALLOC-BYTES drop {: p:ptr :}
   58 p c!  32 p 1 + c!
   u 0 ?do c p 2 + i + c! loop
   32 p u 2 + + c!  59 p u 3 + + c!
   p u 4 + ;

\ `: <u bytes of c> ( -- ) 82 emit ; <u bytes of c>`, in a fresh mapping: a
\ definition and its one call, which prints R.
: RAN$ ( -- ptr u8 n ) s"  ( -- ) 82 emit ; " ;
: RUN-SRC ( n n -- ptr u8 n )
   {: u:n c:n :}
   RAN$ {: b:ptr bu:n :}
   u 2 * bu + 2 + {: su:n :}
   su MEM-ALLOC-BYTES drop {: p:ptr :}
   58 p c!  32 p 1 + c!
   u 0 ?do c p 2 + i + c! loop
   bu 0 ?do b i + c@ p u 2 + + i + c! loop
   u 0 ?do c p u 2 + bu + + i + c! loop
   p su ;

: LETTERS ( n n -- ptr u8 n )
   {: u:n c:n :}
   u MEM-ALLOC-BYTES drop {: p:ptr :}
   u 0 ?do c p i + c! loop
   p u ;

: LONG$ ( -- ptr u8 n ) LONG-LEN TAIL ;

\ The body text grows with the definition: on the load path a child's `:`
\ defines a word with a long name, whose body text passes 20000 bytes, and
\ the word runs. The name is w, which no word of this file is named.
: TEST-ENGINE ( -- )
   LONG-LEN 119 RUN-SRC {: da:ptr du:n :}
   0 s" a long name" da du RUN-AT {: outu:len erru:len oc :}
   s" `:` defines a word with a long name" T-LABEL
   da du OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED=
   s" ... and says nothing" T-LABEL
   ERR erru LEN>N s" " T$=
   s" ... and the word runs" T-LABEL
   OUT outu LEN>N s" R" T$= ;

\ The word this file defines below, with a long name.
: TEST-LONG ( -- )
   s" a word with a long name resolves" T-LABEL
   LONG$ CHECKER-RESOLVES? TTRUE
   s" ... is defined in this scope" T-LABEL
   LONG$ CHECKER-DEFINED-HERE? TTRUE
   s" ... has an effect to query" T-LABEL
   LONG$ EFFECT-QUERY TTRUE
   s" ... may be published by a storage definer" T-LABEL
   LONG$ CHECKER-LBUF-NAME-OK? TTRUE
   s" ... names no type" T-LABEL
   LONG$ TYPE-RESERVED? TFALSE
   s" a value-record with a long name is declared" T-LABEL
   LONG-LEN 114 LETTERS {: ra:ptr ru:n :}
   ra ru s" f n" CHECKER-DEFRECORD
   ra ru TYPE-RESERVED? TTRUE
   s" a linear type with a long name is declared" T-LABEL
   LONG-LEN 115 LETTERS {: la:ptr lu:n :}
   la lu CHECKER-DEFLINEAR
   la lu TYPE-RESERVED? TTRUE
   s" a record tried with a long name is registered" T-LABEL
   LONG-LEN 116 LETTERS s" f n" CHECKER-TRY-RECORD nip nip 0 T=
   s" a deferred long name is stored whole" T-LABEL
   SYM-STR-U @ {: used:n :}
   LONG-LEN 112 LETTERS CHECKER-DEFER
   SYM-STR-U @ used LONG-LEN + T=
   s" the word with a long name is undefined" T-LABEL
   LONG$ CHECKER-UNDEFINE
   LONG$ CHECKER-RESOLVES? TFALSE ;

\ A stored type's text, which the storage gates parse, is bounded by the
\ buffer a storage definer spells it into (src/core/checker.f LBUF-SIG-CAP),
\ so a text of exactly the cap is still taken.
: TEST-TEXT-CAP ( -- )
   TEXT-CAP LEN !
   s" a stored type at the cap is a layout" T-LABEL
   STORED$ CHECKER-LAYOUT-INFO nip nip TTRUE
   s" ... is storage" T-LABEL
   STORED$ CHECKER-STORAGE-INFO nip TTRUE
   s" ... is a dynamic element" T-LABEL
   STORED$ CHECKER-DYNAMIC-INFO nip TTRUE ;

: MAIN ( -- )
   T-RESET
   TEST-QUERIES
   TEST-FAMILIES
   TEST-DECLARERS
   TEST-TOP-LEVEL
   TEST-MALFORMED
   TEST-FIELD-TEXT
   TEST-TEXTS
   TEST-ENGINE
   TEST-LONG
   TEST-TEXT-CAP
   T-REPORT
   s" name-length-test: ok" type cr ;

\ The word with a long name, and the top-level-only word taking its name.
LONG-LEN 113 DEF-SRC evaluate
LONG$ SIG$ TRUST

MAIN

;package
