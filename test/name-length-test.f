\ name-length-test.f - every checker word source can call with a name or a text
\ rejects a length that describes no memory, -1 and the maximum cell. A query
\ answers no; a declaring word ends with its own rejection. A name has no
\ ceiling: a long one is taken, while a package name past its row and a field
\ name past 255 bytes are rejected by name.

require src/lib/error.f
require src/lib/memory.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/test/rejection-text.f
require src/lib/process.f
require src/habu/layout.f

package NAME-LENGTH-TEST

$400 constant OUT-CAP
$10000 constant ERR-CAP            \ a record names a whole name up to three times
-1 1 rshift constant MAX-CELL
$4E constant DUPLICATE             \ E-DUPLICATE-DEFINITION's throw code
\ A name has no ceiling: a definer captures a word's name and one separator
\ into the definition's body text, which grows with the definition, before it
\ writes the record. LONG-LEN is a name's length past the old 8000-byte cap.
20000 constant LONG-LEN
create OUT OUT-CAP allot
create ERR ERR-CAP allot

\ Room for a name or text one past the longest a case hands, in whole pages
\ between two guard pages. Each name and text ends at a guard page, so a word
\ that reads past a short one faults at once instead of reading on through
\ what is mapped after it. Every byte of the names is q.
LONG-LEN 1 + STACK-ABI:PAGE-BYTES / 1 + STACK-ABI:PAGE-BYTES *
   constant ROOM
PTR-VARIABLE NAMES
: NAMES! ( -- )
   ROOM MEM:ALLOC-GUARDED drop {: p:^ :}
   ROOM 0 ?do 113 p i + c! loop
   p NAMES ! ;
NAMES!

: TAIL ( n -- ^u8 n )
   {: u:n :}
   NAMES @ ROOM + u - u ;

\ The name the next case hands its word: LEN bytes of q ending at the guard
\ page, or for a length no span has, its two last bytes.
variable LEN
: BAD$ ( -- ^u8 n )
   LEN @ {: u:n :}
   u 0 < u ROOM > or IF 2 TAIL drop u EXIT THEN
   u TAIL ;

\ The text the next case hands its word: LEN bytes ending at the guard page,
\ `head` and then spaces, or for a length no span has, two spaces.
PTR-VARIABLE TEXTS
: TEXTS! ( -- ) ROOM MEM:ALLOC-GUARDED drop TEXTS ! ;
TEXTS!
: TEXT ( ^u8 n -- ^u8 n )
   {: h:^ hu:n :}
   TEXTS @ {: p:^ :}
   ROOM 0 ?do 32 p i + c! loop
   LEN @ {: u:n :}
   u 0 < u ROOM > or IF p ROOM + 2 - u EXIT THEN
   p ROOM + u - {: t:^ :}
   hu 0 ?do h i + c@ t i + c! loop
   t u ;

\ A definition CHECK! certifies.
: BODY$ ( -- ^u8 n ) s" NLT-W ( -- )" TEXT ;

\ A query ends a returning case: its answer is a refusal.
: VERDICT ( bool -- )
   IF s" refused" ELSE s" taken" THEN type cr ;

\ ---- the cases a child runs -------------------------------------------------
: CTOR-WORD ( -- ) BAD$ TFAM-CTOR-WORD? 0= VERDICT ;
: FIELD-FIND ( -- ) 0 0 BAD$ TFAM:PF-FIND nip 0= VERDICT ;
: PRIM ( -- ) BAD$ PRIM-SPEC:FIND 0 < VERDICT ;
: MATCH-FAM ( -- ) BAD$ TFL-MATCH-FAM? nip 0= VERDICT ;
: CON-FAM ( -- ) BAD$ TFL-CON-FAM? nip 0= VERDICT ;

: LINEAR-IT ( -- ) BAD$ CHECKER-DEFLINEAR ;
: RECORD-IT ( -- ) BAD$ s" f n" CHECKER-DEFRECORD ;
: FIELDS-IT ( -- ) s" zzrec" BAD$ CHECKER-DEFRECORD ;
: PACKAGE-IT ( -- ) BAD$ BAD$ CHECKER-PACKAGE ;
\ `;package`, then NLTP and its part named BAD$: the part opens in NLTP's
\ buffer, so `package` reaches the part's path.
: PART$ ( -- ^u8 n )
   s" ;package package NLTP package " {: h:^ hu:n :}
   BAD$ {: a:^ u:n :}
   hu u + MEM-ALLOC-BYTES drop {: p:^ :}
   h p hu BYTE-COPY
   a p hu + u BYTE-COPY
   p hu u + ;
: FAMILY-IT ( -- ) BAD$ BAD$ s" newtype" CHECKER-DEFFAMILY ;
: CHECK!-IT ( -- ) BODY$ CHECK! drop ;
: CANDIDATE-IT ( -- ) BODY$ CHECK-CANDIDATE! drop ;
: HOOK-IT ( -- ) BODY$ CHECK:DEFINITION drop ;

\ A structure whose one field is named BAD$, declared as a program declares it.
: FIELD-NAME$ ( -- ^u8 n )
   s" STRUCTURE nlt-long FIELD " {: h:^ hu:n :}
   BAD$ {: a:^ u:n :}
   s"  n ;STRUCTURE" {: t:^ tu:n :}
   hu u + tu + MEM-ALLOC-BYTES drop {: p:^ :}
   h p hu BYTE-COPY
   a p hu + u BYTE-COPY
   t p hu + u + tu BYTE-COPY
   p hu u + tu + ;
: FIELD-NAME-IT ( -- )
   FIELD-NAME$ evaluate-closed ;

\ ---- the parent's side -------------------------------------------------------
\ A label keeps what fits in LBL.
128 constant LBL-CAP
create LBL LBL-CAP allot
variable LBL-U
: LBL+ ( ^u8 n -- )
   {: a:^ u:n :}
   u LBL-CAP LBL-U @ - min {: k:n :}
   k 0 ?do a i + c@ LBL LBL-U @ + i + c! loop
   LBL-U @ k + LBL-U ! ;
: RELABEL ( -- ) LBL LBL-U @ T-LABEL ;
: LABEL-AT ( ^u8 n ^u8 n -- )
   {: at:^ atu:n src:^ srcu:n :}
   0 LBL-U !  src srcu LBL+  s"  at " LBL+  at atu LBL+ ;

\ A declaring word's refusal ends the process, so every case runs `src` in a
\ forked child, with LEN at `u`.
: RUN-AT ( n ^u8 n -- len len PROCESS:ending )
   {: u:n src:^ srcu:n :}
   u LEN !
   src srcu OUT OUT-CAP >LEN ERR ERR-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

\ The child prints that its word refused, and nothing else, and exits 0.
: RETURNS-AT ( n ^u8 n ^u8 n -- )
   {: u:n at:^ atu:n src:^ srcu:n :}
   at atu src srcu LABEL-AT
   u src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED=
   RELABEL ERR erru LEN>N s" " T$=
   RELABEL OUT outu LEN>N S\" refused\n" T$= ;

\ The child ends with the word's own refusal: `rc` and its one-line message.
: DIES-AT ( n ^u8 n ^u8 n n ^u8 n -- )
   {: u:n at:^ atu:n src:^ srcu:n rc:n want:^ wantu:n :}
   at atu src srcu LABEL-AT
   u src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc rc T-ENDING-EXITED=
   RELABEL outu LEN>N 0 T=
   RELABEL ERR erru LEN>N want wantu T$= ;

\ The child ends with one rejection record: `rc`, a record that, its gutter
\ dropped, starts with `head`, holds `note` and is followed by no other.
: REJECTS-AT ( n ^u8 n ^u8 n n ^u8 n ^u8 n -- )
   {: u:n at:^ atu:n src:^ srcu:n rc:n head:^ headu:n note:^ noteu:n :}
   at atu src srcu LABEL-AT
   u src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc rc T-ENDING-EXITED=
   RELABEL outu LEN>N 0 T=
   RELABEL ERR erru LEN>N REJECTION-TEXT:FLAT head headu STARTS-WITH? TTRUE
   RELABEL ERR erru LEN>N note noteu REJECTION-TEXT:HAS? TTRUE
   RELABEL ERR erru LEN>N S\" \nerror[" CONTAINS? TFALSE ;

\ -1 and the maximum cell, for a name or a text with no bound of its own.
: RETURNS ( ^u8 n -- )
   {: src:^ srcu:n :}
   -1 s" -1" src srcu RETURNS-AT
   MAX-CELL s" max" src srcu RETURNS-AT ;
: DIES ( ^u8 n n ^u8 n -- )
   {: src:^ srcu:n rc:n want:^ wantu:n :}
   -1 s" -1" src srcu rc want wantu DIES-AT
   MAX-CELL s" max" src srcu rc want wantu DIES-AT ;
: REJECTS ( ^u8 n n ^u8 n ^u8 n -- )
   {: src:^ srcu:n rc:n head:^ headu:n note:^ noteu:n :}
   -1 s" -1" src srcu rc head headu note noteu REJECTS-AT
   MAX-CELL s" max" src srcu rc head headu note noteu REJECTS-AT ;

: TEST-QUERIES ( -- )
   s" CTOR-WORD" RETURNS
   s" FIELD-FIND" RETURNS
   s" PRIM" RETURNS ;

\ The family lookups copy the name into the checker's token buffer, which
\ refuses a maximum-cell length by ending the process.
: FAM-DIES-AT-MAX ( ^u8 n -- )
   {: src:^ srcu:n :}
   -1 s" -1" src srcu RETURNS-AT
   MAX-CELL s" max" src srcu 76 S\" checker: token buffer too large\n" DIES-AT ;

: TEST-FAMILIES ( -- )
   s" MATCH-FAM" FAM-DIES-AT-MAX
   s" CON-FAM" FAM-DIES-AT-MAX ;

SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL LOADING
\ A package name past its row is rejected naming its length; no source holds
\ the name, so the record is placed at the word this file ran, names this file
\ and shows the name in its `word` note. The head is copied out of the string
\ builder the note reuses.
: CAP-HEAD$ ( -- ^u8 n )
   SB-RESET
   s" error[E-PACKAGE-NAME-CAP]: the package name is " SB-APPEND
   LONG-LEN FMT:SB-INT  s"  bytes long, more than the " SB-APPEND
   PACKAGE-PATH:CAP 1 - FMT:SB-INT
   S\"  a package name may have\n--> " SB-APPEND
   LOADING @ data-base SRCLOC:PATHLEN-CELL + @ SB-APPEND
   s" :" SB-APPEND
   SB$ {: h:^ hu:n :}
   hu MEM-ALLOC-BYTES drop {: p:^ :}
   h p hu BYTE-COPY
   p hu ;
: CAP-NOTE$ ( -- ^u8 n )
   SB-RESET
   S\" \n= help: shorten the package name to " SB-APPEND
   PACKAGE-PATH:CAP 1 - FMT:SB-INT
   S\"  bytes or fewer\n" SB-APPEND
   SB$ {: t:^ tu:n :}
   s" = word: " {: h:^ hu:n :}
   hu LONG-LEN + tu + {: wu:n :}
   wu MEM-ALLOC-BYTES drop {: p:^ :}
   h p hu BYTE-COPY
   LONG-LEN TAIL drop p hu + LONG-LEN BYTE-COPY
   t p hu + LONG-LEN + tu BYTE-COPY
   p wu ;

\ A rejection exits 70 and an engine defect dies 76.
: TEST-DECLARERS ( -- )
   s" LINEAR-IT" 76 S\" checker: a linear type's name describes no memory\n" DIES
   s" RECORD-IT" 76 S\" checker: a value record's name describes no memory\n" DIES
   LONG-LEN s" long" s" PACKAGE-IT" 70 CAP-HEAD$ CAP-NOTE$ REJECTS-AT
   \ Each child's record is placed at MAIN, the token its parent was loading.
   s" FAMILY-IT" 70 S\" error[E-BAD-DECLARATION]: bad newtype declaration\n" S\" | ^^^^ missing name\n" REJECTS
   256 s" 256" s" FIELD-NAME-IT" 70
      S\" error[E-BAD-DECLARATION]: bad structure declaration `nlt-long`\n"
      s" ^ name longer than 255 bytes" REJECTS-AT ;

\ CHECKER-DEFRECORD's second string is the record's field text, which is no
\ name, so only -1 and the maximum cell describe no text.
: TEST-FIELD-TEXT ( -- )
   s" FIELDS-IT" 70 S\" error[E-BAD-DECLARATION]: value record `zzrec` has no fields\n"
      s" ^ no fields" REJECTS ;

\ A definition's source text, which CHECK!, CHECK-CANDIDATE! and
\ CHECK:DEFINITION judge, is no name either: only -1 and the maximum cell
\ describe none of it. Each ends with the token buffer's refusal, which the
\ maximum cell already met.
: TOO-LARGE$ ( -- ^u8 n ) S\" checker: token buffer too large\n" ;
: TEST-TEXTS ( -- )
   s" CHECK!-IT" 76 TOO-LARGE$ DIES
   s" CANDIDATE-IT" 76 TOO-LARGE$ DIES
   s" HOOK-IT" 76 TOO-LARGE$ DIES ;

\ No token is -1 or the maximum cell long, so `package` meets only a long part
\ name, whose path is past its row: rejected, naming it.
: TEST-PART ( -- )
   LONG-LEN s" long" s" PART$ evaluate" 70 s" error[E-PACKAGE-NAME-CAP]: `"
      LONG-LEN TAIL REJECTS-AT ;

\ ---- a long name -------------------------------------------------------------
\ `: <u bytes of c> ;`, in a fresh mapping.
: DEF-SRC ( n n -- ^u8 n )
   {: u:n c:n :}
   u 4 + MEM-ALLOC-BYTES drop {: p:^ :}
   58 p c!  32 p 1 + c!
   u 0 ?do c p 2 + i + c! loop
   32 p u 2 + + c!  59 p u 3 + + c!
   p u 4 + ;

\ `: <u bytes of c> ( -- ) 82 emit ; <u bytes of c>`, in a fresh mapping: a
\ definition and its one call, which prints R.
: RAN$ ( -- ^u8 n ) s"  ( -- ) 82 emit ; " ;
: RUN-SRC ( n n -- ^u8 n )
   {: u:n c:n :}
   RAN$ {: b:^ bu:n :}
   u 2 * bu + 2 + {: su:n :}
   su MEM-ALLOC-BYTES drop {: p:^ :}
   58 p c!  32 p 1 + c!
   u 0 ?do c p 2 + i + c! loop
   bu 0 ?do b i + c@ p u 2 + + i + c! loop
   u 0 ?do c p u 2 + bu + + i + c! loop
   p su ;

: LETTERS ( n n -- ^u8 n )
   {: u:n c:n :}
   u MEM-ALLOC-BYTES drop {: p:^ :}
   u 0 ?do c p i + c! loop
   p u ;

: LONG$ ( -- ^u8 n ) LONG-LEN TAIL ;

\ `a` and then `b`, in a fresh mapping.
: JOIN ( ^u8 n ^u8 n -- ^u8 n )
   {: a:^ au:n b:^ bu:n :}
   au bu + MEM-ALLOC-BYTES drop {: p:^ :}
   a p au BYTE-COPY
   b p au + bu BYTE-COPY
   p au bu + ;

\ A candidate that calls the word with a long name.
: CALL-LONG$ ( -- ^u8 n ) s" NLT-CALL ( -- ) " LONG$ JOIN ;

\ The body text grows with the definition: on the load path a child's `:`
\ defines a word with a long name, whose body text passes 20000 bytes, and
\ the word runs. The name is w, which no word of this file is named.
: TEST-ENGINE ( -- )
   LONG-LEN 119 RUN-SRC {: da:^ du:n :}
   0 da du RUN-AT {: outu:len erru:len oc :}
   s" `:` defines a word with a long name" T-LABEL
   da du OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED=
   s" ... and says nothing" T-LABEL
   ERR erru LEN>N s" " T$=
   s" ... and the word runs" T-LABEL
   OUT outu LEN>N s" R" T$= ;

\ The word this file defines below, with a long name, and long-named types and
\ a defer, each declared through source. A second declaration of a type ends
\ the process, so a child makes it.
: TEST-LONG ( -- )
   s" a word with a long name resolves" T-LABEL
   CALL-LONG$ CHECK-QUIET-CANDIDATE! -1 T=
   s" ... is defined in this scope" T-LABEL
   LONG-LEN 113 DEF-SRC TEST-EVAL:RC DUPLICATE T=
   s" a value-record with a long name is declared" T-LABEL
   s" VALUE-RECORD " LONG-LEN 114 LETTERS JOIN s"  f n END-VALUE-RECORD" JOIN
   {: ra:^ ru:n :}
   ra ru TEST-EVAL:RC 0 T=
   0 s" again" ra ru 70 S\" error[E-BAD-DECLARATION]: value record `"
   s" already a type or reserved" REJECTS-AT
   s" a linear type with a long name is declared" T-LABEL
   s" DEFLINEAR " LONG-LEN 115 LETTERS JOIN {: la:^ lu:n :}
   la lu TEST-EVAL:RC 0 T=
   0 s" again" la lu 70 S\" error[E-BAD-DECLARATION]: `DEFLINEAR "
   s" already a type or reserved" REJECTS-AT
   s" a deferred long name is stored whole" T-LABEL
   LONG-LEN 112 LETTERS {: da:^ du:n :}
   s" defer " da du JOIN s"  ( -- n )" JOIN TEST-EVAL:RC 0 T=
   s" NLT-DEFERRED ( -- n ) " da du JOIN CHECK-QUIET-CANDIDATE! -1 T=
   s" the word with a long name is undefined" T-LABEL
   s" undefine " LONG$ JOIN TEST-EVAL:RC 0 T=
   CALL-LONG$ CHECK-QUIET-CANDIDATE! 0 T= ;

: MAIN ( -- )
   T-RESET
   TEST-QUERIES
   TEST-FAMILIES
   TEST-DECLARERS
   TEST-PART
   TEST-FIELD-TEXT
   TEST-TEXTS
   TEST-ENGINE
   TEST-LONG
   T-REPORT
   s" name-length-test: ok" type cr ;

\ The word with a long name.
LONG-LEN 113 DEF-SRC evaluate

MAIN

;package
