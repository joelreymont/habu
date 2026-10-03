\ name-length-test.f - every checker word source can call with a name or a text
\ refuses a length that describes no name or no text.
\
\ A definer captures a word's name and one separator into the definition's body
\ text before it writes the record (src/habu/layout.f BODYBUF-CAP), so the
\ longest name the engine defines is BODYBUF-CAP - 1 bytes: `:` takes one of
\ that length and refuses one byte more, rc 71. src/core/checker.f bounds every
\ name it is handed by the same ceiling. Each checker word a source file can
\ call with a name is driven with -1, the ceiling plus one and the maximum cell,
\ and refuses each without a signal: a query answers no and moves no pool mark,
\ a declaring word ends with its own refusal. A name of exactly the ceiling is
\ still taken.
\
\ A declaring word's refusal ends the process, and before the fix a trusted
\ length faulted it, so every refused case runs in a forked child. The name ends
\ at a guard page: a word that reads past a short name faults at once instead of
\ reading on through whatever is mapped after it.
\
\ CHECKER-DEFRECORD's and CHECKER-TRY-RECORD's second string is the record's
\ field text, which is no name, and so are a definition's source text, which
\ CHECK and its entries judge, and a signature, which CHECK-DOES! and
\ CHECKER-DEFCAST parse: each length is refused only when it describes no
\ memory, -1 or the maximum cell. CHECK judges raw source, and the engine
\ captures a definition without its comments and runs of spaces, so a source
\ text one byte past BODYBUF-CAP still certifies.
\
\ A stored type's text, which the storage gates parse, is bounded by the
\ definition body text: a storage definer spells its stored type in the effect
\ of the accessor it generates, which the engine captures into BODYBUF-CAP
\ bytes. Each storage gate is driven with -1, BODYBUF-CAP plus one and the
\ maximum cell, a text of exactly BODYBUF-CAP is still taken, and the past-cap
\ text is one the gate would otherwise take. CHECKER-DEFFAMILY's arity token is
\ bounded by the two digits of the largest arity.
\
\ A name with a second inner ':' has a length a name has and still keys no
\ record; every word that records a symbol refuses it too (TEST-MALFORMED).
\
\ Run: bin/hb --load test/name-length-test.f

require lib/errors.f
require lib/memory.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require src/habu/layout.f

package NAME-LENGTH-TEST

$400 constant OUT-CAP
$8000 constant ERR-CAP             \ a diagnostic that names a whole name
10000 constant TIMEOUT-MS
-1 1 rshift constant MAX-CELL
BODYBUF-CAP 1 - constant CEIL      \ the longest name `:` defines
BODYBUF-CAP constant TEXT-CAP      \ the longest stored-type text
2 constant ARITY-CAP               \ digits of the largest arity, 23
create OUT OUT-CAP allot
create ERR ERR-CAP allot

\ Room for a text one past its cap, in whole pages between two guard pages.
\ Every byte of the names is q.
TEXT-CAP 1 + STACK-ABI:PAGE-BYTES / 1 + STACK-ABI:PAGE-BYTES * constant ROOM
PTR-VARIABLE NAMES
: NAMES! ( -- )
   ROOM MEM-ALLOC-GUARDED drop {: p:ptr :}
   ROOM 0 ?do 113 p i + c! loop
   p NAMES ! ;
NAMES!

\ The `u` bytes of q that end at the guard page.
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
: TEXTS! ( -- ) ROOM MEM-ALLOC-GUARDED drop TEXTS ! ;
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

\ The checker's pool marks, which a refused name leaves where it found them.
13 constant MARK-N
create BEFORE MARK-N cells allot
: MARK ( n -- n )
   case
      0 of SYM-N @ endof
      1 of SYM-STR-U @ endof
      2 of DFER-END @ endof
      3 of NORET-END @ endof
      4 of FMEND @ endof
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
: SEALED ( -- ) MARKS! BAD$ CHECKER-SEALED-PKG? 0= VERDICT ;
: FIELD-FIND ( -- ) MARKS! 0 0 BAD$ TYPE-FIELD:FIND nip 0= VERDICT ;
: PRIM ( -- ) MARKS! BAD$ PRIM-SPEC:FIND 0 < VERDICT ;
: LAYOUT ( -- ) MARKS! BAD$ CHECKER-LAYOUT-INFO nip nip 0= VERDICT ;
: STORAGE ( -- ) MARKS! BAD$ CHECKER-STORAGE-INFO nip 0= VERDICT ;
: DYNAMIC ( -- ) MARKS! BAD$ CHECKER-DYNAMIC-INFO nip 0= VERDICT ;
: MATCH-FAM ( -- ) MARKS! BAD$ TFL-MATCH-FAM? nip 0= VERDICT ;
: CON-FAM ( -- ) MARKS! BAD$ TFL-CON-FAM? nip 0= VERDICT ;
: VAR ( -- ) MARKS! BAD$ 0 TFAM:TFL-VAR? nip 0= VERDICT ;
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
: EXPORT-IT ( -- ) BAD$ CHECKER-EXPORT ;
: FAMILY-IT ( -- ) BAD$ s" 0" CHECKER-DEFFAMILY ;
: SUM-IT ( -- ) BAD$ s" 0 VARIANT vacant ;VARIANT" CHECKER-DEFSUM ;
: SUM-NOEND-IT ( -- ) BAD$ s" 0" CHECKER-DEFSUM-NOEND ;
: PRODUCT-IT ( -- ) BAD$ s" 0 FIELD a n" CHECKER-DEFPRODUCT ;
: ARITY-IT ( -- ) s" nltfam" BAD$ CHECKER-DEFFAMILY ;
: CHECK-IT ( -- ) BODY$ CHECK drop ;
: CHECK!-IT ( -- ) BODY$ CHECK! drop ;
: CANDIDATE-IT ( -- ) BODY$ CHECK-CANDIDATE! drop ;
: HOOK-IT ( -- ) BODY$ LOWER-CERT-HOOK:HOOK drop ;

\ A product of this file's own, so a field transaction has a family to add to:
\ the family of the field row it just committed.
PRODUCT nlt-pair 0
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

\ Run `src` in a child with LEN at `u`, labelled with the source and `at`.
: RUN-AT ( n ptr u8 n ptr u8 n -- len len outcome )
   {: u:n at:ptr atu:n src:ptr srcu:n :}
   0 LBL-U !  src srcu LBL+  s"  at " LBL+  at atu LBL+
   u LEN !
   src srcu OUT OUT-CAP >LEN ERR ERR-CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN ;

\ The child prints that its word refused, and nothing else, and exits 0.
: RETURNS-AT ( n ptr u8 n ptr u8 n -- )
   {: u:n at:ptr atu:n src:ptr srcu:n :}
   u at atu src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   RELABEL ERR erru LEN>N s" " T$=
   RELABEL OUT outu LEN>N S\" refused\n" T$= ;

\ The child ends with the word's own refusal: `rc` and its one-line message.
: DIES-AT ( n ptr u8 n ptr u8 n n ptr u8 n -- )
   {: u:n at:ptr atu:n src:ptr srcu:n rc:n want:ptr wantu:n :}
   u at atu src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc rc T-OUTCOME-EXITED=
   RELABEL outu LEN>N 0 T=
   RELABEL ERR erru LEN>N want wantu T$= ;

\ -1, the cap plus one and the maximum cell: no name or text the cap bounds has
\ any of them.
: RETURNS-PAST ( n ptr u8 n -- )
   {: cap:n src:ptr srcu:n :}
   -1 s" -1" src srcu RETURNS-AT
   cap 1 + s" cap+1" src srcu RETURNS-AT
   MAX-CELL s" max" src srcu RETURNS-AT ;
: RETURNS ( ptr u8 n -- ) {: src:ptr srcu:n :} CEIL src srcu RETURNS-PAST ;

: DIES ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n rc:n want:ptr wantu:n :}
   -1 s" -1" src srcu rc want wantu DIES-AT
   CEIL 1 + s" ceiling+1" src srcu rc want wantu DIES-AT
   MAX-CELL s" max" src srcu rc want wantu DIES-AT ;

\ -1 and the maximum cell alone, for a text with no bound of its own.
: RETURNS-NO-SPAN ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   -1 s" -1" src srcu RETURNS-AT
   MAX-CELL s" max" src srcu RETURNS-AT ;
: DIES-NO-SPAN ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n rc:n want:ptr wantu:n :}
   -1 s" -1" src srcu rc want wantu DIES-AT
   MAX-CELL s" max" src srcu rc want wantu DIES-AT ;

: TEST-QUERIES ( -- )
   s" RESOLVES" RETURNS
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
   s" SEALED" RETURNS
   s" FIELD-FIND" RETURNS
   s" PRIM" RETURNS
   s" LAYOUT" RETURNS
   s" STORAGE" RETURNS
   s" DYNAMIC" RETURNS ;

\ The family lookups copy the name into the checker's token buffer, which
\ refuses a maximum-cell length by ending the process.
: FAM-DIES-AT-MAX ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   -1 s" -1" src srcu RETURNS-AT
   CEIL 1 + s" ceiling+1" src srcu RETURNS-AT
   MAX-CELL s" max" src srcu 76 S\" checker: token buffer too large\n" DIES-AT ;

: TEST-FAMILIES ( -- )
   s" MATCH-FAM" FAM-DIES-AT-MAX
   s" CON-FAM" FAM-DIES-AT-MAX
   s" VAR" FAM-DIES-AT-MAX ;

: TEST-DECLARERS ( -- )
   s" DEFER-IT" 76 S\" checker: symbol string capacity overflow\n" DIES
   s" UNDEFINE-IT" 76 S\" checker: symbol string capacity overflow\n" DIES
   s" HERE-IT" 76 S\" checker: symbol string capacity overflow\n" DIES
   s" LINEAR-IT" 70 S\" checker: bad or duplicate signature type\n" DIES
   s" RECORD-IT" 70 S\" checker: bad or duplicate value-record type\n" DIES
   s" TRUNCATE-RAW-IT" 76 S\" checker: missing signature truncation mark\n" DIES
   s" TRUNCATE-IT" 83 S\" seal: cannot truncate sealed checker signatures\n" DIES
   s" PACKAGE-IT" 76 S\" checker: package name too long\n" DIES
   s" USING-IT" 76 S\" checker: using name too long\n" DIES
   s" USING-PUSH-IT" 67 S\" hb: uncaught throw code 7136\n" DIES
   s" EXPORT-IT" 67 S\" hb: uncaught throw code 7113\n" DIES
   s" FAMILY-IT" 67 S\" habu: bad newtype declaration '': missing name\nhb: uncaught throw code 7107\n" DIES
   s" SUM-IT" 67 S\" habu: bad sumtype declaration '': missing name\nhb: uncaught throw code 7107\n" DIES
   s" SUM-NOEND-IT" 67 S\" habu: bad sumtype declaration '': missing ;SUMTYPE\nhb: uncaught throw code 7107\n" DIES
   s" PRODUCT-IT" 67 S\" habu: bad product declaration '': missing name\nhb: uncaught throw code 7107\n" DIES
   s" FIELD-ADD-IT" 67 S\" hb: uncaught throw code 7101\n" DIES ;

\ TRUST, PTX-BARRIER! and CHECKER-DEFCAST have no effect a checked body may call,
\ so the child names them at its top level. TRUST-DECL and TRUST-RAW are sealed
\ (src/core/checker.f: the interpreter refuses them), so a TRUSTED: body calls
\ them, as the engine's own callers do. A trust row refuses a false name as it
\ refuses one that names no word, without echoing it; a cast reaches its name
\ once its signature certifies, and stores it.
$400 constant WANT-CAP
create WANT WANT-CAP allot
variable WANT-U
: WANT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a WANT WANT-U @ + u BYTE-COPY
   WANT-U @ u + WANT-U ! ;

\ The trust-row refusal, naming the row's name `a u`.
: STALE$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   0 WANT-U !
   s" E-TRUST-UNRESOLVED habu: trust row for '" WANT+
   a u WANT+
   S\" ' names no word where its record lands: nothing in the open section's wordlist, or the global wordlist outside a package, is spelled that way, so the effect would be recorded against a symbol the engine never defined. Delete the row, correct the name to the word it was meant to describe, or write it in the section that defines that word\nhb: uncaught throw code 7143\n" WANT+
   WANT WANT-U @ ;

: TEST-TOP-LEVEL ( -- )
   s" BAD$ PTX-BARRIER!" 76 S\" PTX-BARRIER!: unknown word\n" DIES
   s" BAD$ SIG$ TRUST" 67 s" " STALE$ DIES
   s" TRUSTED: NL-TD ( -- ) BAD$ SIG$ TRUST-DECL ; NL-TD" 67 s" " STALE$ DIES
   s" TRUSTED: NL-TR ( -- ) BAD$ SIG$ TRUST-RAW ; NL-TR" 67 s" " STALE$ DIES
   s" BAD$ CAST$ CHECKER-DEFCAST" 76 S\" checker: symbol string capacity overflow\n" DIES ;

\ ---- a name no record is keyed by --------------------------------------------
\ The engine defines no word named with a second inner ':' (rc 75), and
\ CHECKER-RECORD-SYM answers symbol 0 for one. No row may carry symbol 0: a
\ defer row keyed 0 is the defer store's terminator and hides every row after
\ it. A trust row refuses the name as naming no word, and echoes it; every other
\ word that records a symbol for it names it as malformed and throws.
: MAL$ ( -- ptr u8 n ) s" q:q:q" ;
: MALFORMED$ ( -- ptr u8 n )
   S\" E-BAD-QUALIFIED-RECORD habu: record for 'q:q:q' refused: malformed qualified name, where one non-edge ':' selects a package and a second ':' names no word. Use one ':' qualifier, e.g. PKG:WORD\nhb: uncaught throw code 7147\n" ;
: MAL-DIES ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n rc:n want:ptr wantu:n :}
   0 s" two inner colons" src srcu rc want wantu DIES-AT ;
: TEST-MALFORMED ( -- )
   s" MAL$ CHECKER-DEFER" 67 MALFORMED$ MAL-DIES
   s" MAL$ CHECKER-UNDEFINE" 67 MALFORMED$ MAL-DIES
   s" MAL$ CAST$ CHECKER-DEFCAST" 67 MALFORMED$ MAL-DIES
   s" MAL$ SIG$ TRUST" 67 MAL$ STALE$ MAL-DIES
   s" TRUSTED: NL-TD ( -- ) MAL$ SIG$ TRUST-DECL ; NL-TD" 67 MALFORMED$ MAL-DIES
   s" TRUSTED: NL-TR ( -- ) MAL$ SIG$ TRUST-RAW ; NL-TR" 67 MALFORMED$ MAL-DIES ;

\ A field text is no name, so only -1 and the maximum cell describe no text.
: TEST-FIELD-TEXT ( -- )
   s" TRY-FIELDS" RETURNS-NO-SPAN
   s" FIELDS-IT" 70 S\" checker: empty value-record\n" DIES-NO-SPAN ;

\ The storage gates answer no and move no pool mark. CHECK and every entry that
\ reaches it end with the token buffer's refusal, which the maximum cell already
\ met. A signature is refused as one that does not parse: a does> clause is
\ rejected, a cast throws E-CAST-ARITY. CHECK-DOES! is trusted-only and
\ CHECKER-DEFCAST has no effect a checked body may call, so the child names them
\ at its top level. An arity token past two digits is refused as a bad arity,
\ and its text is echoed only when its length is one a name can have.
: TOO-LARGE$ ( -- ptr u8 n ) S\" checker: token buffer too large\n" ;
: ARITY$ ( -- ptr u8 n )
   S\" habu: bad newtype declaration 'nltfam': arity must be a decimal, at most 23 parameters\nhb: uncaught throw code 7108\n" ;
: TEST-TEXTS ( -- )
   TEXT-CAP s" LAYOUT-TEXT" RETURNS-PAST
   TEXT-CAP s" STORAGE-TEXT" RETURNS-PAST
   TEXT-CAP s" DYNAMIC-TEXT" RETURNS-PAST
   s" CHECK-IT" 76 TOO-LARGE$ DIES-NO-SPAN
   s" CHECK!-IT" 76 TOO-LARGE$ DIES-NO-SPAN
   s" CANDIDATE-IT" 76 TOO-LARGE$ DIES-NO-SPAN
   s" HOOK-IT" 76 TOO-LARGE$ DIES-NO-SPAN
   s" MARKS! CLAUSE$ BAD$ CHECK-DOES! 0= VERDICT" RETURNS-NO-SPAN
   s" CAST-NAME$ BAD$ CHECKER-DEFCAST" 67 S\" hb: uncaught throw code 7129\n"
   DIES-NO-SPAN
   -1 s" -1" s" ARITY-IT" 67 ARITY$ DIES-AT
   ARITY-CAP 1 + s" cap+1" s" ARITY-IT" 67
   S\" habu: bad newtype declaration 'nltfam': arity must be a decimal, at most 23 parameters at 'qqq'\nhb: uncaught throw code 7108\n"
   DIES-AT
   MAX-CELL s" max" s" ARITY-IT" 67 ARITY$ DIES-AT ;

\ ---- a name of exactly the ceiling ------------------------------------------
\ `: <u bytes of c> ;`, in a fresh mapping.
: DEF-SRC ( n n -- ptr u8 n )
   {: u:n c:n :}
   u 4 + MEM-ALLOC-BYTES drop {: p:ptr :}
   58 p c!  32 p 1 + c!
   u 0 ?do c p 2 + i + c! loop
   32 p u 2 + + c!  59 p u 3 + + c!
   p u 4 + ;

\ A name of `u` bytes of byte `c`, in a fresh mapping.
: LETTERS ( n n -- ptr u8 n )
   {: u:n c:n :}
   u MEM-ALLOC-BYTES drop {: p:ptr :}
   u 0 ?do c p i + c! loop
   p u ;

: CEIL$ ( -- ptr u8 n ) CEIL TAIL ;

\ The engine's own bound: `:` defines a name of the ceiling, and one byte more
\ runs the definition's body text past BODYBUF-CAP. The names are w, which no
\ word of this file is named.
: FULL$ ( -- ptr u8 n ) s" hb: definition body text full at 8000 bytes:" ;
: TEST-ENGINE ( -- )
   CEIL 119 DEF-SRC {: da:ptr du:n :}
   0 s" ceiling" da du RUN-AT {: outu:len erru:len oc :}
   s" `:` defines a name of the ceiling" T-LABEL
   da du OUT outu LEN>N ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   s" ... and says nothing" T-LABEL
   ERR erru LEN>N s" " T$=
   CEIL 1 + 119 DEF-SRC {: ea:ptr eu:n :}
   0 s" ceiling+1" ea eu RUN-AT {: outv:len errv:len od :}
   s" `:` refuses a name one byte past it" T-LABEL
   ea eu OUT outv LEN>N ERR errv LEN>N od 71 T-OUTCOME-EXITED=
   s" ... for the body text it would need" T-LABEL
   ERR FULL$ nip FULL$ T$= ;

\ The word this file defines below, named by exactly the ceiling.
: TEST-CEILING ( -- )
   s" a word named at the ceiling resolves" T-LABEL
   CEIL$ CHECKER-RESOLVES? TTRUE
   s" ... is defined in this scope" T-LABEL
   CEIL$ CHECKER-DEFINED-HERE? TTRUE
   s" ... has an effect to query" T-LABEL
   CEIL$ EFFECT-QUERY TTRUE
   s" ... may be published by a storage definer" T-LABEL
   CEIL$ CHECKER-LBUF-NAME-OK? TTRUE
   s" ... names no type" T-LABEL
   CEIL$ TYPE-RESERVED? TFALSE
   s" a value-record named at the ceiling is declared" T-LABEL
   CEIL 114 LETTERS {: ra:ptr ru:n :}
   ra ru s" f n" CHECKER-DEFRECORD
   ra ru TYPE-RESERVED? TTRUE
   s" a linear type named at the ceiling is declared" T-LABEL
   CEIL 115 LETTERS {: la:ptr lu:n :}
   la lu CHECKER-DEFLINEAR
   la lu TYPE-RESERVED? TTRUE
   s" a record tried at the ceiling is registered" T-LABEL
   CEIL 116 LETTERS s" f n" CHECKER-TRY-RECORD nip nip 0 T=
   s" a deferred name of the ceiling is stored whole" T-LABEL
   SYM-STR-U @ {: used:n :}
   CEIL 112 LETTERS CHECKER-DEFER
   SYM-STR-U @ used CEIL + T=
   s" the word named at the ceiling is undefined" T-LABEL
   CEIL$ CHECKER-UNDEFINE
   CEIL$ CHECKER-RESOLVES? TFALSE ;

\ A stored-type text of exactly the cap is still taken, and a source text one
\ past it certifies.
: TEST-TEXT-CAP ( -- )
   TEXT-CAP LEN !
   s" a stored type at the cap is a layout" T-LABEL
   STORED$ CHECKER-LAYOUT-INFO nip nip TTRUE
   s" ... is storage" T-LABEL
   STORED$ CHECKER-STORAGE-INFO nip TTRUE
   s" ... is a dynamic element" T-LABEL
   STORED$ CHECKER-DYNAMIC-INFO nip TTRUE
   TEXT-CAP 1 + LEN !
   s" a definition text one past the capture certifies" T-LABEL
   BODY$ CHECK-CANDIDATE! -1 T= ;

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
   TEST-CEILING
   TEST-TEXT-CAP
   T-REPORT
   s" name-length-test: ok" type cr ;

\ The word named at the ceiling, and the top-level-only words taking its name.
CEIL 113 DEF-SRC evaluate
CEIL$ SIG$ TRUST
CEIL$ PTX-BARRIER!

MAIN

;package
