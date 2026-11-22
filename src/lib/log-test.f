split-str
\ log-test.f - LOG's lines as fd 2 receives them. A case writes under
\ STDERR:CAPTURE into a file in a private temp directory and reads it back, or
\ points fd 2 at a pipe a reader task drains.
\ Run: bin/hb --load src/lib/log-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/date.f
require src/lib/fmt.f
require src/lib/task.f
require src/lib/fd.f
require src/lib/adt/option.f
require src/lib/adt/result.f
require src/lib/log.f
require src/lib/test/stderr.f

\ LOG's line row, read to show a task's release disposed it.
package LOG
: LOG-TEST:ROW ( -- ^VEC:vector<u8> ) ROW @ ;
;package

package LOG-TEST

private
CAST: N>PTR ( n -- ^u8 )
public

-9672 s" the log test's code" ERROR:CODE E-LOG-CODE
-9678 constant UNDECLARED
$A constant LF
$100000 constant BIG-U              \ 1 MiB of text in one line
$10000 constant PAD-U               \ a pipe line's padding, far past PIPE_BUF
$10000 constant CHUNK-U
64 constant TASK-LINES
86400 constant DAY-SECONDS
0 constant F-DUPFD
$A constant SAVE-FD-MIN             \ the saved stderr lands clear of 0..2
$4000000000000 constant HUGE-U      \ 2^50: a vector admits it, a 47- or 48-bit address space cannot map it
$800000000000 constant SPACE-U      \ 2^47, the first span FILL tries
$1000 constant PAGE-MIN             \ the smallest page any target maps
$400 constant BLOCK-MIN             \ the smallest heap block FILL takes
256 constant HOLDS-CAP

create PATH-BUF PATH-CAP allot      \ the capture file's path
variable PATH-U
TYPED-VARIABLE FILE-B ^VEC:vector<u8>  \ the capture file's bytes, read back
TYPED-VARIABLE BIG-B ^VEC:vector<u8>
TYPED-VARIABLE PIPE-B ^VEC:vector<u8>  \ what the reader drained from the pipe
create CHUNK CHUNK-U allot
variable PIPE-R
variable PIPE-W
variable SAVED-ERR
variable ROW-LIVE                   \ whether a task's row was made as it ended
variable T0                         \ epoch seconds before a case's first line
variable T1                         \ and after its last
variable CODE-A
variable CODE-B
variable START
variable LINES
variable BAD
2 TYPED-BUFFER NEXTS n               \ each writer's next line number, a then b
variable READY
HOLDS-CAP TYPED-BUFFER HOLD-ATS n    \ each reserved span's address
HOLDS-CAP TYPED-BUFFER HOLD-US n     \ and its bytes
variable HOLDS
HOLDS-CAP TYPED-BUFFER BLOCKS ^u8    \ each heap block FILL took
variable BLOCKS-N

TASK:MIN-STACK TASK:TASK WRITER-A
TASK:MIN-STACK TASK:TASK WRITER-B
TASK:MIN-STACK TASK:TASK READER
\ RELEASE-CASE's own task: LOG's release must register after ROW-PROBE, and a
\ registration serves later activations.
TASK:MIN-STACK TASK:TASK RELEASER

: NEW-BYTES ( -- ^VEC:vector<u8> )  HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

: PATH$ ( -- str )  PATH-BUF PATH-U @ ;


: PATH-MAKE ( -- )
   s" log-test" FS:MKTEMP-DIR {: dir:^ dirlen:n :}
   dir dirlen FS:CLEANUP-TREE+
   dir dirlen s" log.txt" PATH-BUF FS:JOIN-PATH PATH-U ! ;


\ The whole file, valid until the next read.
: FILE$ ( -- str )
   PATH$ FS:FILE-SIZE {: size:n :}
   FILE-B @ size VEC:RESIZE
   PATH$ FILE-B @ VEC:SPAN SPAN:$ FS:READ-ALL size T=
   FILE-B @ VEC:SPAN SPAN:$ ;


: CAPTURED ( [ -- ] -- )
   epoch-seconds T0 !
   PATH$ STDERR:CAPTURE
   [: STDERR:;CAPTURE ;] finally
   epoch-seconds T1 ! ;


\ '0' in the shape stands for any digit.
: SHAPE-AT? ( ^u8 n -- bool )
   {: at:^ i:n :}
   s" 0000-00-00T00:00:00Z" drop i + c@ {: want:n :}
   at i + c@ {: got:n :}
   want STR-ZERO <> if got want = exit then
   got STR-ZERO >= got STR-ZERO 9 + <= and ;


: SHAPE? ( ^u8 -- bool )
   {: at:^ :}
   true DATE:TIME-LEN 0 ?do at i SHAPE-AT? and loop ;


: DIGITS ( str -- n )
   {: at:^ u:n :}
   0 u 0 ?do 10 * at i + c@ STR-ZERO - + loop ;


\ The stamp's epoch seconds, -1 for a date PARSE-YMD refuses.
: STAMP-SECONDS ( ^u8 -- n )
   {: at:^ :}
   at DATE:LEN DATE:PARSE-YMD MATCH option
      none OF -1 exit ENDOF
      some OF DAY-SECONDS * ENDOF
   ;MATCH
   at 11 + 2 DIGITS 3600 * +
   at 14 + 2 DIGITS 60 * +
   at 17 + 2 DIGITS + ;


: STAMP-OK ( ^u8 -- )
   {: at:^ :}
   at SHAPE? TTRUE
   at STAMP-SECONDS {: s:n :}
   s T0 @ >= s T1 @ <= and TTRUE ;


\ The file holds exactly a stamp followed by the rest, for each rest in turn.
: LINE-OK ( str str -- str )
   {: at:^ u:n rest:^ ru:n :}
   at STAMP-OK
   at DATE:TIME-LEN + ru rest ru T$=
   at DATE:TIME-LEN + ru + u DATE:TIME-LEN - ru - ;


: LEVEL-LINES ( -- )
   LOG:ERROR s" model" LOG:LINE s" refused " LOG:TEXT+ -42 LOG:INT+ LOG:;LINE
   LOG:WARN s" crew" LOG:LINE s" min " LOG:TEXT+ STR-MIN-I64 LOG:INT+ LOG:;LINE
   LOG:INFO s" mcp" LOG:LINE s" ping" LOG:TEXT+ LOG:;LINE ;


: MODEL-REST$ ( -- str )  S\"  error model: refused -42\n" ;
: MIN-REST$ ( -- str )  S\"  warn crew: min -9223372036854775808\n" ;
: PING-REST$ ( -- str )  S\"  info mcp: ping\n" ;


: LEVELS-CASE ( -- )
   s" error, warn and info lines, text and numbers" T-LABEL
   [: LEVEL-LINES ;] CAPTURED
   FILE$ {: at:^ u:n :}
   u DATE:TIME-LEN 3 * MODEL-REST$ nip + MIN-REST$ nip + PING-REST$ nip + T=
   at u MODEL-REST$ LINE-OK
   MIN-REST$ LINE-OK PING-REST$ LINE-OK nip 0 T= ;


: BIG-MAKE ( -- )
   HEAP:ALLOCATOR BIG-U [type] u8 VEC:NEW BIG-B !
   BIG-B @ VEC:SPAN SPAN:$ {: at:^ u:n :}
   u 0 ?do i 26 mod [char] a + at i + c! loop ;


: BIG$ ( -- str )  BIG-B @ VEC:SPAN SPAN:$ ;
: BIG-HEAD$ ( -- str )  s"  error big: " ;


: BIG-LINE ( -- )
   LOG:ERROR s" big" LOG:LINE BIG$ LOG:TEXT+ LOG:;LINE ;


: BIG-OK ( str -- )
   {: at:^ u:n :}
   u DATE:TIME-LEN BIG-HEAD$ nip + BIG-U + 1 + T=
   at STAMP-OK
   at DATE:TIME-LEN + BIG-HEAD$ nip BIG-HEAD$ T$=
   at DATE:TIME-LEN + BIG-HEAD$ nip + BIG-U BIG$ T$=
   at u + 1- c@ LF T= ;


: BIG-CASE ( -- )
   s" a 1 MiB text arrives whole in one line" T-LABEL
   [: BIG-LINE ;] CAPTURED
   FILE$ BIG-OK ;


: SHED-CASE ( -- )
   s" a line past START-CAP leaves its row released" T-LABEL
   [: BIG-LINE ;] CAPTURED
   LOG-TEST:ROW NULL-PTR = TTRUE ;


\ The stray words find no line open: the big line's ;LINE closed it.
: STRAY-LINES ( -- )
   BIG-LINE
   [: s" stray" LOG:TEXT+ ;] catch CODE-A !
   [: 7 LOG:INT+ ;] catch CODE-B ! ;


: STRAY-CASE ( -- )
   s" a stray TEXT+ or INT+ after a 1 MiB line neither writes nor throws" T-LABEL
   [: STRAY-LINES ;] CAPTURED
   FILE$ BIG-OK
   CODE-A @ 0 T=
   CODE-B @ 0 T= ;


: ABANDON ( -- )
   LOG:ERROR s" t" LOG:LINE s" lost" LOG:TEXT+ E-FS-IO throw ;


: KEPT-LINE ( -- )
   LOG:WARN s" t" LOG:LINE s" kept" LOG:TEXT+ LOG:;LINE ;


: ABANDON-THEN-LINE ( -- )
   [: ABANDON ;] catch CODE-A !
   [: KEPT-LINE ;] catch CODE-B ! ;


: KEPT-REST$ ( -- str )  S\"  warn t: kept\n" ;


: ABANDON-CASE ( -- )
   s" a line a throw abandoned is never written" T-LABEL
   [: ABANDON-THEN-LINE ;] CAPTURED
   FILE$ {: at:^ u:n :}
   u DATE:TIME-LEN KEPT-REST$ nip + T=
   at u KEPT-REST$ LINE-OK nip 0 T=
   CODE-A @ E-FS-IO T=
   CODE-B @ 0 T= ;


: UNPAIRED-LINES ( -- )  KEPT-LINE LOG:;LINE ;


: UNPAIRED-CASE ( -- )
   s" a ;LINE with no line open writes nothing" T-LABEL
   [: UNPAIRED-LINES ;] CAPTURED
   FILE$ {: at:^ u:n :}
   u DATE:TIME-LEN KEPT-REST$ nip + T=
   at u KEPT-REST$ LINE-OK nip 0 T= ;


\ The callee's line opens while this one is open, so this one is abandoned and
\ the words after the call find no line open.
: OUTER-LINE ( [ -- ] -- )
   LOG:ERROR s" outer" LOG:LINE s" head" LOG:TEXT+
   execute
   s" tail" LOG:TEXT+ 7 LOG:INT+ E-LOG-CODE LOG:CODE+ LOG:;LINE ;


: SHORT-IN-LINE ( -- )  [: [: KEPT-LINE ;] OUTER-LINE ;] catch CODE-A ! ;
: BIG-IN-LINE ( -- )  [: [: BIG-LINE ;] OUTER-LINE ;] catch CODE-A ! ;


: LINE-IN-LINE-CASE ( -- )
   s" a line inside an open line is written once, the open one never" T-LABEL
   [: SHORT-IN-LINE ;] CAPTURED
   FILE$ {: at:^ u:n :}
   u DATE:TIME-LEN KEPT-REST$ nip + T=
   at u KEPT-REST$ LINE-OK nip 0 T=
   CODE-A @ 0 T=
   [: BIG-IN-LINE ;] CAPTURED
   FILE$ BIG-OK
   CODE-A @ 0 T= ;


: CONTROL-LINE ( -- )
   LOG:ERROR s" t" LOG:LINE S\" a\nb\x1b\x7f\\x0a" LOG:TEXT+ LOG:;LINE ;


: CONTROL-REST$ ( -- str )  S\"  error t: a\\x0ab\\x1b\\x7f\\x5cx0a\n" ;


: CONTROL-CASE ( -- )
   s" control bytes and a backslash in a text are escaped" T-LABEL
   [: CONTROL-LINE ;] CAPTURED
   FILE$ {: at:^ u:n :}
   u DATE:TIME-LEN CONTROL-REST$ nip + T=
   at u CONTROL-REST$ LINE-OK nip 0 T= ;


\ The text's span is HUGE-U bytes from a one-byte string, so this case relies on
\ TEXT+ growing the row before it reads a byte of the text.
: LOST-LINES ( -- )
   [: LOG:ERROR s" t" LOG:LINE s" x" drop HUGE-U LOG:TEXT+ LOG:;LINE ;] catch CODE-A !
   [: KEPT-LINE ;] catch CODE-B ! ;


: NOTICE-REST$ ( -- str )  S\"  error log: out of memory, a line was lost\n" ;


: NOTICE-THEN-KEPT ( -- )
   FILE$ {: at:^ u:n :}
   u DATE:TIME-LEN 2 * NOTICE-REST$ nip + KEPT-REST$ nip + T=
   at u NOTICE-REST$ LINE-OK
   KEPT-REST$ LINE-OK nip 0 T= ;


: LOST-CASE ( -- )
   s" a line no mapping can hold is written as the notice" T-LABEL
   [: LOST-LINES ;] CAPTURED
   NOTICE-THEN-KEPT
   CODE-A @ 0 T=
   CODE-B @ 0 T= ;


: JOIN-CODE ( result<n,n> -- n )
   MATCH result ok OF drop 0 ENDOF err OF ENDOF ;MATCH ;


\ Runs after LOG's release, which the task registers later: the exit chain runs
\ newest first. The released row's cell is null, and the rest of the line the
\ release closed writes nothing.
: ROW-PROBE ( -- )
   LOG-TEST:ROW NULL-PTR <> bool>n ROW-LIVE !
   [: s" late" LOG:TEXT+ LOG:;LINE ;] catch CODE-A ! ;


: ABANDON-TASK ( -- )
   [: ROW-PROBE ;] TASK:SELF TASK:AT-EXIT
   ABANDON ;


: RELEASE-TASK ( -- )
   [: ABANDON-TASK ;] RELEASER TASK:ACTIVATE
   RELEASER TASK:JOIN JOIN-CODE E-FS-IO T= ;


: RELEASE-CASE ( -- )
   s" a task that abandons a line releases its row and closes the line" T-LABEL
   1 ROW-LIVE !  -1 CODE-A !
   [: RELEASE-TASK ;] CAPTURED
   ROW-LIVE @ 0 T=
   CODE-A @ 0 T=
   PATH$ FS:FILE-SIZE 0 T= ;


\ Reserves an inaccessible span of n bytes for EMPTY; false when none is left.
: HOLD ( n -- bool )
   {: u:n :}
   MEM-ADDR-ANY u MEM-PROT-NONE MEM-MAP-PRIVATE-ANON MEM-ANON-FD MEM-OFF-ZERO mmap
   {: at:n :}
   at 0 < if false exit then
   at HOLDS @ HOLD-ATS !
   u HOLDS @ HOLD-US !
   1 HOLDS +!
   true ;


\ Takes a heap block of n bytes for EMPTY; false when the heap refuses it.
: TAKE-BLOCK ( n -- bool )
   {: u:n :}
   [: HEAP:ALLOCATOR u ALLOC:BYTES drop BLOCKS-N @ BLOCKS ! ;] catch {: code:n :}
   code 0= if 1 BLOCKS-N +! true exit then
   code E-MEM-MAP <> if code throw then
   false ;


\ Reserves spans, largest first, until the kernel has no page left, so every
\ mapping the process asks for is refused until EMPTY; then takes the heap's
\ free blocks the same way, so a heap request of BLOCK-MIN or more is refused.
: FILL ( -- )
   0 HOLDS !
   SPACE-U
   begin dup PAGE-MIN >= HOLDS @ HOLDS-CAP < and while
      dup HOLD 0= if 2 / then
   repeat drop
   HOLDS @ HOLDS-CAP < TTRUE
   0 BLOCKS-N !
   SPACE-U
   begin dup BLOCK-MIN >= BLOCKS-N @ HOLDS-CAP < and while
      dup TAKE-BLOCK 0= if 2 / then
   repeat drop
   BLOCKS-N @ HOLDS-CAP < TTRUE ;


: EMPTY ( -- )
   BLOCKS-N @ 0 ?do HEAP:ALLOCATOR i BLOCKS @ ALLOC:FREE loop
   0 BLOCKS-N !
   HOLDS @ 0 ?do
      i HOLD-ATS @ N>PTR i HOLD-US @ munmap 0 T=
   loop
   0 HOLDS ! ;


: FULL ( [ -- ] -- )
   FILL
   [: EMPTY ;] finally ;


\ Appends n bytes of BIG$; more than BIG-U fails here.
: BIG-TEXT+ ( n -- ) {: u:n :}
   u BIG-U <= TTRUE
   BIG$ drop u BIG-U min LOG:TEXT+ ;


: ROW-LEN ( -- n )  LOG-TEST:ROW VEC:COUNT ;


\ The row fills its block, so the LF ;LINE appends asks the heap for twice the
\ block, more than the row and any free block below BLOCK-MIN next to it hold.
: ROW-FILL ( -- )
   LOG-TEST:ROW VEC:CAP ROW-LEN - BIG-TEXT+ ;


: FINISH-LOST ( -- )
   LOG:ERROR s" t" LOG:LINE ROW-FILL
   [: LOG:;LINE ;] FULL ;


: FINISH-LINES ( -- )
   [: FINISH-LOST ;] catch CODE-A !
   [: KEPT-LINE ;] catch CODE-B ! ;


: FINISH-CASE ( -- )
   s" a line whose LF ;LINE cannot map is written as the notice" T-LABEL
   [: FINISH-LINES ;] CAPTURED
   NOTICE-THEN-KEPT
   CODE-A @ 0 T=
   CODE-B @ 0 T= ;


: NEXT ( n -- ^n )  [char] a - NEXTS ;


: PAD$ ( -- str )  BIG$ drop PAD-U ;


\ Each writer's lines arrive whole and in its own order: `<stamp> warn a: line
\ <k> <pad>` with k counting from 0.
: TASK-LINE-OK ( str -- )
   {: at:^ u:n :}
   1 LINES +!
   u DATE:TIME-LEN 8 + < if 1 BAD +! exit then
   at SHAPE? 0= if 1 BAD +! exit then
   at DATE:TIME-LEN + 6 + c@ {: tag:n :}
   tag [char] a <> tag [char] b <> and if 1 BAD +! exit then
   SB-RESET s"  warn " SB-APPEND tag SB-APPEND-C s" : line " SB-APPEND
   tag NEXT @ FMT:SB-INT s"  " SB-APPEND
   SB$ {: head:^ hu:n :}
   at DATE:TIME-LEN + {: rest:^ :}
   u DATE:TIME-LEN - hu PAD-U + <> if 1 BAD +! exit then
   rest hu head hu STR= 0= if 1 BAD +! exit then
   rest hu + PAD-U PAD$ STR= 0= if 1 BAD +! exit then
   1 tag NEXT +! ;


: TASK-LINES-OK ( str -- )
   {: at:^ u:n :}
   0 START !  0 LINES !  0 BAD !  0 0 NEXTS !  0 1 NEXTS !
   u 0 ?do
      at i + c@ LF = if
         at START @ + i START @ - TASK-LINE-OK
         i 1+ START !
      then
   loop
   START @ u <> if 1 BAD +! then ;


: WRITE-LINES ( str -- )
   {: tag:^ tu:n :}
   1 READY atomic-add drop
   begin READY atomic@ 2 < while TASK:PAUSE repeat
   TASK-LINES 0 ?do
      LOG:WARN tag tu LOG:LINE s" line " LOG:TEXT+ i LOG:INT+
      s"  " LOG:TEXT+ PAD$ LOG:TEXT+ LOG:;LINE
   loop
   0 TASK:RETURN ;


: TWO-WRITERS ( -- )
   0 READY !
   [: s" a" WRITE-LINES ;] WRITER-A TASK:ACTIVATE
   [: s" b" WRITE-LINES ;] WRITER-B TASK:ACTIVATE
   WRITER-A TASK:JOIN JOIN-CODE CODE-A !
   WRITER-B TASK:JOIN JOIN-CODE CODE-B ! ;


\ fd 2 becomes the descriptor; the process's own stderr waits in SAVED-ERR.
: ERR-TO ( n -- )
   {: f:n :}
   FD:STDERR FD>N F-DUPFD SAVE-FD-MIN fcntl {: saved:n :}
   saved 0 >= TTRUE
   saved SAVED-ERR !
   f FD:STDERR FD>N dup2 FD:STDERR FD>N T= ;


: ERR-BACK ( -- )
   SAVED-ERR @ FD:STDERR FD>N dup2 FD:STDERR FD>N T=
   SAVED-ERR @ close-rc 0 T= ;


: PIPE-OPEN ( -- )
   pipe {: r:n w:n rc:n :}
   rc 0 T=
   r PIPE-R !
   w PIPE-W !
   w ERR-TO ;


\ With fd 2 and the write end closed, the reader meets the end of the pipe.
: PIPE-BACK ( -- )
   ERR-BACK
   PIPE-W @ close-rc 0 T= ;


: DRAIN-ONE ( -- bool )
   PIPE-R @ CHUNK CHUNK-U read {: got:n :}
   got 0 < if E-FS-IO throw then
   CHUNK got PIPE-B @ VEC:APPEND
   got 0 > ;


: DRAIN ( -- )
   begin DRAIN-ONE 0= until
   0 TASK:RETURN ;


\ A line past PIPE_BUF can reach the pipe in pieces, so only LOG's lock keeps
\ the two writers' lines whole.
: PIPE-CASE ( -- )
   s" two tasks' 64 KiB lines stay whole on a pipe" T-LABEL
   NEW-BYTES PIPE-B !
   PIPE-OPEN
   [: DRAIN ;] READER TASK:ACTIVATE
   TWO-WRITERS
   PIPE-BACK
   READER TASK:JOIN JOIN-CODE {: drained:n :}
   PIPE-R @ close-rc 0 T=
   PIPE-B @ VEC:SPAN SPAN:$ TASK-LINES-OK
   LINES @ TASK-LINES 2 * T=
   BAD @ 0 T=
   0 NEXTS @ TASK-LINES T=
   1 NEXTS @ TASK-LINES T=
   drained 0 T=
   CODE-A @ 0 T=
   CODE-B @ 0 T=
   PIPE-B @ VEC:DELETE ;


\ A pipe's read end refuses writes. A refused line leaves fd 2's last line
\ whole, so no mending LF comes before the next one.
: REFUSED-CASE ( -- )
   s" a line fd 2 refuses is lost without a throw" T-LABEL
   pipe {: r:n w:n rc:n :}
   rc 0 T=
   r ERR-TO
   [: KEPT-LINE ;] catch CODE-A !
   ERR-BACK
   [: KEPT-LINE ;] CAPTURED
   FILE$ {: at:^ u:n :}
   u DATE:TIME-LEN KEPT-REST$ nip + T=
   at u KEPT-REST$ LINE-OK nip 0 T=
   r close-rc 0 T=
   w close-rc 0 T=
   CODE-A @ 0 T= ;


\ A write to a pipe whose reader has gone raises SIGPIPE, which ends the process
\ with status 141 unless LOG holds it off.
: BROKEN-CASE ( -- )
   s" a line to a pipe with no reader is lost without a throw" T-LABEL
   pipe {: r:n w:n rc:n :}
   rc 0 T=
   r close-rc 0 T=
   w ERR-TO
   [: KEPT-LINE ;] catch CODE-A !
   ERR-BACK
   w close-rc 0 T=
   CODE-A @ 0 T= ;


: TAKE-ONE ( -- bool )
   PIPE-R @ CHUNK CHUNK-U read {: got:n :}
   got 0 > if CHUNK got PIPE-B @ VEC:APPEND then
   got 0 > ;


\ Reads what the non-blocking pipe holds into PIPE-B, until a read finds none.
: TAKE ( -- )
   PIPE-B @ 0 VEC:RESIZE
   begin TAKE-ONE 0= until ;


: CUT-LINES ( -- )
   [: BIG-LINE ;] catch CODE-A !
   TAKE
   [: KEPT-LINE ;] catch CODE-B ! ;


\ A non-blocking pipe nobody drains takes part of a line longer than it holds,
\ then refuses the rest and the LF that ends the cut line at once.
: CUT-CASE ( -- )
   s" a line fd 2 takes in part is ended before the next line" T-LABEL
   NEW-BYTES PIPE-B !
   PIPE-OPEN
   PIPE-R @ >FD FD:NONBLOCK!
   PIPE-W @ >FD FD:NONBLOCK!
   epoch-seconds T0 !
   CUT-LINES
   TAKE
   epoch-seconds T1 !
   PIPE-BACK
   PIPE-R @ close-rc 0 T=
   PIPE-B @ VEC:SPAN SPAN:$ {: at:^ u:n :}
   u 1 DATE:TIME-LEN + KEPT-REST$ nip + T=
   at c@ LF T=
   at 1+ u 1- KEPT-REST$ LINE-OK nip 0 T=
   CODE-A @ 0 T=
   CODE-B @ 0 T=
   PIPE-B @ VEC:DELETE ;


: CODE-LINES ( -- )
   LOG:ERROR s" t" LOG:LINE E-LOG-CODE LOG:CODE+ LOG:;LINE
   LOG:ERROR s" t" LOG:LINE UNDECLARED LOG:CODE+ LOG:;LINE ;


: CODE-REST$ ( -- str )  S\"  error t: -9672 LOG-TEST:E-LOG-CODE: the log test's code\n" ;
: BARE-REST$ ( -- str )  S\"  error t: -9678\n" ;


: CODE-CASE ( -- )
   s" a declared code is written with its name and text, another alone" T-LABEL
   [: CODE-LINES ;] CAPTURED
   FILE$ {: at:^ u:n :}
   u DATE:TIME-LEN 2 * CODE-REST$ nip + BARE-REST$ nip + T=
   at u CODE-REST$ LINE-OK
   BARE-REST$ LINE-OK nip 0 T= ;


: LOG-RUN ( -- )
   PATH-MAKE
   NEW-BYTES FILE-B !
   BIG-MAKE
   LEVELS-CASE
   BIG-CASE
   SHED-CASE
   STRAY-CASE
   CONTROL-CASE
   ABANDON-CASE
   UNPAIRED-CASE
   LINE-IN-LINE-CASE
   LOST-CASE
   FINISH-CASE
   RELEASE-CASE
   PIPE-CASE
   REFUSED-CASE
   BROKEN-CASE
   CUT-CASE
   CODE-CASE
   BIG-B @ VEC:DELETE
   FILE-B @ VEC:DELETE ;

LOG-RUN
T-REPORT

;package
