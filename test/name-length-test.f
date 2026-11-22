split-str
\ name-length-test.f - every checker word source can call with a name or a text
\ rejects a length that describes no memory, -1 and the maximum cell. A query
\ answers no; a declaring word ends with its own rejection. A name has no
\ ceiling: a long one is taken, a package name, a part path and a field name
\ included.

require src/lib/error.f
require src/lib/memory.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/test/rejection-text.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/lib/fs-mutate.f
require test/e2e-tree.f

package NAME-LENGTH-TEST

$400 constant OUT-CAP
$10000 constant ERR-CAP            \ a record names a whole name up to three times
-1 1 rshift constant MAX-CELL
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
: BAD$ ( -- str )
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
: PRIM ( -- ) BAD$ PRIM-SPEC:FIND 0 < VERDICT ;

: CHECK!-IT ( -- ) BODY$ CHECK! drop ;
: CANDIDATE-IT ( -- ) BODY$ CHECK-CANDIDATE! drop ;
: HOOK-IT ( -- ) BODY$ CHECK:DEFINITION drop ;


\ ---- the parent's side -------------------------------------------------------
\ A label keeps what fits in LBL.
128 constant LBL-CAP
create LBL LBL-CAP allot
variable LBL-U
: LBL+ ( str -- )
   {: a:^ u:n :}
   u LBL-CAP LBL-U @ - min {: k:n :}
   k 0 ?do a i + c@ LBL LBL-U @ + i + c! loop
   LBL-U @ k + LBL-U ! ;
: RELABEL ( -- ) LBL LBL-U @ T-LABEL ;
: LABEL-AT ( str str -- )
   {: at:^ atu:n src:^ srcu:n :}
   0 LBL-U !  src srcu LBL+  s"  at " LBL+  at atu LBL+ ;

\ A declaring word's refusal ends the process, so every case runs `src` in a
\ forked child, with LEN at `u`.
: RUN-AT ( n ^u8 n -- len len PROCESS:ending )
   {: u:n src:^ srcu:n :}
   u LEN !
   src srcu OUT OUT-CAP >LEN ERR ERR-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

\ The child prints that its word refused, and nothing else, and exits 0.
: RETURNS-AT ( n str str -- )
   {: u:n at:^ atu:n src:^ srcu:n :}
   at atu src srcu LABEL-AT
   u src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED=
   RELABEL ERR erru LEN>N s" " T$=
   RELABEL OUT outu LEN>N S\" refused\n" T$= ;

\ The child ends with the word's own refusal: `rc` and its one-line message.
: DIES-AT ( n str str n str -- )
   {: u:n at:^ atu:n src:^ srcu:n rc:n want:^ wantu:n :}
   at atu src srcu LABEL-AT
   u src srcu RUN-AT {: outu:len erru:len oc :}
   RELABEL src srcu OUT outu LEN>N ERR erru LEN>N oc rc T-ENDING-EXITED=
   RELABEL outu LEN>N 0 T=
   RELABEL ERR erru LEN>N want wantu T$= ;

\ The child ends with one rejection record: `rc`, a record that, its gutter
\ dropped, starts with `head`, holds `note` and is followed by no other.
: REJECTS-AT ( n str str n str str -- )
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
   s" PRIM" RETURNS ;

\ A definition's source text is no name either: only -1 and the maximum cell
\ describe none of it, and each dies at the token buffer's limit.
: TOO-LARGE$ ( -- str ) S\" checker: token buffer too large\n" ;
: TEST-TEXTS ( -- )
   s" CHECK!-IT" 76 TOO-LARGE$ DIES
   s" CANDIDATE-IT" 76 TOO-LARGE$ DIES
   s" HOOK-IT" 76 TOO-LARGE$ DIES ;

\ ---- a long name -------------------------------------------------------------
\ `: <u bytes of c> ;`, in a fresh mapping.
: DEF-SRC ( n n -- str )
   {: u:n c:n :}
   u 4 + MEM-ALLOC-BYTES drop {: p:^ :}
   58 p c!  32 p 1 + c!
   u 0 ?do c p 2 + i + c! loop
   32 p u 2 + + c!  59 p u 3 + + c!
   p u 4 + ;

\ `: <u bytes of c> ( -- ) 82 emit ; <u bytes of c>`, in a fresh mapping: a
\ definition and its one call, which prints R.
: RAN$ ( -- str ) s"  ( -- ) 82 emit ; " ;
: RUN-SRC ( n n -- str )
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

: LONG$ ( -- str ) LONG-LEN TAIL ;

\ `a` and then `b`, in a fresh mapping.
: JOIN ( ^u8 n ^u8 n -- ^u8 n )
   {: a:^ au:n b:^ bu:n :}
   au bu + MEM-ALLOC-BYTES drop {: p:^ :}
   a p au BYTE-COPY
   b p au + bu BYTE-COPY
   p au bu + ;

\ A LONG-LEN package and part, a word in each and a SUMTYPE whose constructor
\ package is longer still: each runs, qualified and under `using`; prints PWWS.
: PKG-SRC$ ( -- str )
   LONG-LEN 81 LETTERS {: l:^ lu:n :}
   LONG-LEN 82 LETTERS {: r:^ ru:n :}
   s" package " l lu JOIN
   S\" \npublic\n: NLT-W ( -- ) 87 emit ;\nSUMTYPE nltshape VARIANT dot ;VARIANT ;SUMTYPE\nprivate\npackage " JOIN
   r ru JOIN S\" \npublic\n: NLT-P ( -- ) 80 emit ;\n;package\n" JOIN
   r ru JOIN S\" :NLT-P\n;package\n" JOIN
   l lu JOIN S\" :NLT-W\nusing " JOIN
   l lu JOIN S\" \nNLT-W\n;using\n: NLT-S ( -- ) " JOIN
   l lu JOIN S\" -NLTSHAPE:DOT drop 83 emit ;\nNLT-S\n" JOIN ;

\ `--load` and tools/check.f each take the package from a file, so each runs
\ in a child over a scratch tree's copy of it.
: TEST-PACKAGE ( -- )
   s" nlt-package" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+
   s" test/nlt-package.f" E2E-TREE:AT-A PKG-SRC$ FS:WRITE-ALL
   s" a long package name and part path load" T-LABEL
   s" test/nlt-package.f" E2E-TREE:ARGS
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:NEED-OK
   s" ... and their words and the constructor run" T-LABEL
   E2E-TREE:OUT$ s" PWWS" T$=
   s" tools/check.f takes them" T-LABEL
   s" tools/check.f" E2E-TREE:ARGS
   s" --" PROC-ARGV+
   s" test/nlt-package.f" PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:NEED-OK ;

\ A candidate that calls the word with a long name.
: CALL-LONG$ ( -- str ) s" NLT-CALL ( -- ) " LONG$ JOIN ;

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

\ The word this file defines below, with a long name, and long-named types, a
\ field and a defer, each declared through source. A second declaration of a type ends
\ the process, so a child makes it.
: TEST-LONG ( -- )
   s" a word with a long name resolves" T-LABEL
   CALL-LONG$ CHECK-QUIET-CANDIDATE! -1 T=
   s" ... is defined in this scope" T-LABEL
   LONG-LEN 113 DEF-SRC TEST-EVAL:RC 70 T=
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
   s" a structure with a long field name is declared" T-LABEL
   s" STRUCTURE nlt-long FIELD " LONG-LEN 116 LETTERS JOIN s"  n ;STRUCTURE" JOIN
   TEST-EVAL:RC 0 T=
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
   TEST-PACKAGE
   TEST-TEXTS
   TEST-ENGINE
   TEST-LONG
   T-REPORT
   s" name-length-test: ok" type cr ;

\ The word with a long name.
LONG-LEN 113 DEF-SRC evaluate

MAIN

;package
