\ name-length-test.f - a name has no ceiling: a long one is taken, a
\ package name, a part path and a field name included.

require src/lib/error.f
require src/lib/memory.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/eval.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/test/rejection-text.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/lib/fs-mutate.f
require test/e2e-tree.f
require src/lib/fs.f

package NAME-LENGTH-TEST

$400 constant OUT-CAP
$10000 constant ERR-CAP            \ a record names a whole name up to three times
\ A name has no ceiling: a definer captures a word's name and one separator
\ into the definition's body text, which grows with the definition, before it
\ writes the record.
20000 constant LONG-LEN
create OUT OUT-CAP allot
create ERR ERR-CAP allot

\ Room for a name one past the longest a case hands, in whole pages between
\ two guard pages. Each name ends at a guard page, so a word that reads past
\ one faults at once instead of reading on through what is mapped after it.
\ Every byte of the names is q.
LONG-LEN 1 + STACK-ABI:PAGE-BYTES / 1 + STACK-ABI:PAGE-BYTES *
   constant ROOM
PTR-VARIABLE NAMES
: NAMES! ( -- )
   ROOM MEM:ALLOC-GUARDED drop {: p:^ :}
   ROOM 0 ?do 113 p i + c! loop
   p NAMES ! ;
NAMES!

: TAIL ( n -- str )
   {: u:n :}
   NAMES @ ROOM + u - u >str ;

\ A label keeps what fits in LBL.
128 constant LBL-CAP
create LBL LBL-CAP allot
variable LBL-U
: LBL+ ( str -- )
   {: s:str :}
   s STR:LEN LBL-CAP LBL-U @ - min {: k:n :}
   k 0 ?do s STR:DATA i + c@ LBL LBL-U @ + i + c! loop
   LBL-U @ k + LBL-U ! ;
: RELABEL ( -- ) LBL LBL-U @ >str T-LABEL ;
: LABEL-AT ( str str -- )
   {: at:str src:str :}
   0 LBL-U !  src LBL+  s"  at " LBL+  at LBL+ ;

\ A declaring word's rejection ends the process, so `src` runs in a forked
\ child.
: RUN-AT ( str -- len len PROCESS:ending )
   {: src:str :}
   src OUT OUT-CAP >LEN ERR ERR-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

\ The child ends with one rejection record: `rc`, a record that, its gutter
\ dropped, starts with `head`, holds `note` and is followed by no other.
: REJECTS-AT ( str str n str str -- )
   {: at:str src:str rc:n head:str note:str :}
   at src LABEL-AT
   src RUN-AT {: outu:len erru:len oc :}
   RELABEL src OUT outu LEN>N >str ERR erru LEN>N >str oc rc T-ENDING-EXITED=
   RELABEL outu LEN>N 0 T=
   RELABEL ERR erru LEN>N >str REJECTION-TEXT:FLAT head STR:STARTS-WITH? TTRUE
   RELABEL ERR erru LEN>N >str note REJECTION-TEXT:HAS? TTRUE
   RELABEL ERR erru LEN>N >str S\" \nerror[" STR:CONTAINS? TFALSE ;

\ ---- a long name -------------------------------------------------------------
\ `: <u bytes of c> ;`, in a fresh mapping.
: DEF-SRC ( n n -- str )
   {: u:n c:n :}
   u 4 + MEM-ALLOC-BYTES drop {: p:^ :}
   58 p c!  32 p 1 + c!
   u 0 ?do c p 2 + i + c! loop
   32 p u 2 + + c!  59 p u 3 + + c!
   p u 4 + >str ;

\ `: <u bytes of c> ( -- ) 82 emit ; <u bytes of c>`, in a fresh mapping: a
\ definition and its one call, which prints R.
: RAN$ ( -- str ) s"  ( -- ) 82 emit ; " ;
: RUN-SRC ( n n -- str )
   {: u:n c:n :}
   RAN$ {: b:str :}
   u 2 * b STR:LEN + 2 + {: su:n :}
   su MEM-ALLOC-BYTES drop {: p:^ :}
   58 p c!  32 p 1 + c!
   u 0 ?do c p 2 + i + c! loop
   b STR:LEN 0 ?do b STR:DATA i + c@ p u 2 + + i + c! loop
   u 0 ?do c p u 2 + b STR:LEN + + i + c! loop
   p su >str ;

: LETTERS ( n n -- str )
   {: u:n c:n :}
   u MEM-ALLOC-BYTES drop {: p:^ :}
   u 0 ?do c p i + c! loop
   p u >str ;

: LONG$ ( -- str ) LONG-LEN TAIL ;

\ `a` and then `b`, in a fresh mapping.
: JOIN ( str str -- str )
   {: a:str b:str :}
   a STR:LEN b STR:LEN + MEM-ALLOC-BYTES drop {: p:^ :}
   a STR:DATA p a STR:LEN BYTE-COPY
   b STR:DATA p a STR:LEN + b STR:LEN BYTE-COPY
   p a STR:LEN b STR:LEN + >str ;

\ A LONG-LEN package and part, a word in each and a SUMTYPE whose constructor
\ package is longer still: each runs; prints PWS.
: PKG-SRC$ ( -- str )
   LONG-LEN 81 LETTERS {: l:str :}
   LONG-LEN 82 LETTERS {: r:str :}
   s" package " l JOIN
   S\" \npublic\n: NLT-W ( -- ) 87 emit ;\nSUMTYPE nltshape VARIANT dot ;VARIANT ;SUMTYPE\nprivate\npackage " JOIN
   r JOIN S\" \npublic\n: NLT-P ( -- ) 80 emit ;\n;package\n" JOIN
   r JOIN S\" :NLT-P\n;package\n" JOIN
   l JOIN S\" :NLT-W\n: NLT-S ( -- ) " JOIN
   l JOIN S\" -NLTSHAPE:DOT drop 83 emit ;\nNLT-S\n" JOIN ;

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
   E2E-TREE:OUT$ s" PWS" T$=
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
   LONG-LEN 119 RUN-SRC {: src:str :}
   src RUN-AT {: outu:len erru:len oc :}
   s" `:` defines a word with a long name" T-LABEL
   src OUT outu LEN>N >str ERR erru LEN>N >str oc 0 T-ENDING-EXITED=
   s" ... and says nothing" T-LABEL
   ERR erru LEN>N >str s" " T$=
   s" ... and the word runs" T-LABEL
   OUT outu LEN>N >str s" R" T$= ;

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
   {: rs:str :}
   rs TEST-EVAL:RC 0 T=
   s" again" rs 70 S\" error[E-BAD-DECLARATION]: value record `"
   s" already a type or reserved" REJECTS-AT
   s" a linear type with a long name is declared" T-LABEL
   s" DEFLINEAR " LONG-LEN 115 LETTERS JOIN {: ls:str :}
   ls TEST-EVAL:RC 0 T=
   s" again" ls 70 S\" error[E-BAD-DECLARATION]: `DEFLINEAR "
   s" already a type or reserved" REJECTS-AT
   s" a structure with a long field name is declared" T-LABEL
   s" STRUCTURE nlt-long FIELD " LONG-LEN 116 LETTERS JOIN s"  n ;STRUCTURE" JOIN
   TEST-EVAL:RC 0 T=
   s" a deferred long name is stored whole" T-LABEL
   LONG-LEN 112 LETTERS {: ds:str :}
   s" defer " ds JOIN s"  ( -- n )" JOIN TEST-EVAL:RC 0 T=
   s" NLT-DEFERRED ( -- n ) " ds JOIN CHECK-QUIET-CANDIDATE! -1 T=
   s" the word with a long name is undefined" T-LABEL
   s" undefine " LONG$ JOIN TEST-EVAL:RC 0 T=
   CALL-LONG$ CHECK-QUIET-CANDIDATE! 0 T= ;

: MAIN ( -- )
   T-RESET
   TEST-PACKAGE
   TEST-ENGINE
   TEST-LONG
   T-REPORT
   s" name-length-test: ok" type cr ;

\ The word with a long name.
LONG-LEN 113 DEF-SRC str> evaluate

MAIN

;package
