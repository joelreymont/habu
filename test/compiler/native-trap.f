\ native-trap.f - checked tests for the terminator that does not return.

require src/lib/test.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/compiler/native/compiler.f

\ Tier 1 below: the routines each case compiles are the optimizing compiler's.
1 set-tier

package NTRAP-TEST
private

\ ---- running the forge in a child --------------------------------------------
\ The forge ends its process, so it cannot be run in this one. The child is this
\ same file under the argument the tail dispatches on, run through the engine
\ under test rather than through a named binary, so a candidate validation
\ measures the candidate.
$4000 constant CAP-CAP
30000 constant CHILD-MS

create OUT-BUF CAP-CAP allot
create ERR-BUF CAP-CAP allot

variable CHILD-OUT-N
variable CHILD-ERR-N
variable CHILD-RC

: CHILD-ARGV ( ptr u8 n -- )
   {: file:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/aot-mode.f" >LEN PROC-ARGV+
   file u >LEN PROC-ARGV+ ;

: CHILD-RUN ( ptr u8 n -- )
   CHILD-ARGV
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT-BUF CAP-CAP >LEN
   ERR-BUF CAP-CAP >LEN
   CHILD-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME       \ ( out-len err-len outcome )
   PROC-OUTCOME>RC RC>N CHILD-RC !
   LEN>N CHILD-ERR-N !
   LEN>N CHILD-OUT-N ! ;

: CHILD-ERR$ ( -- ptr u8 n )
   ERR-BUF CHILD-ERR-N @ ;

\ ---- the other message, and the other exit -----------------------------------
\ A callee's name is as long as the definition capture lets it be, past every
\ fixed buffer the message was once built in.
1000 constant LONG-N
LONG-N BUFFER: LONG-BUF

: LONG-NAME$ ( -- ptr u8 n )
   LONG-N 0 ?do 121 LONG-BUF i + c! loop
   LONG-BUF LONG-N ;

\ A trap site the ELABORATOR builds after a call that does not come back says,
\ if it is ever reached, that the certificate the caller was compiled against
\ was false. It says so in its own words and with its own status - the
\ diagnostic of a scrutinee whose tag matched no arm would be a lie about a word
\ that returned - so the two are proved to differ in BOTH, in a process that
\ dies of it.
: NORET-CASE ( -- )
   s" a long callee name's message holds it whole" T-LABEL
   LONG-NAME$ NTRAP:RETURNED {: a:ptr u:n rc:n :}
   rc ENGINE-ERROR:CODE-CERT T=
   u LONG-N 13 + T=
   a 4 s" hb: " T$=
   a 4 + LONG-N LONG-NAME$ T$=
   a u + 9 - 9 s"  returned" T$=

   s" an empty callee name is refused too" T-LABEL
   [: s" " NTRAP:RETURNED drop drop drop ;] E-NTRAP-NAME TTHROWSQ

   s" the no-return exit is ENGINE-ERROR:CODE-CERT, not the bad-tag one" T-LABEL
   s" test/compiler/native-trap-noret.f" CHILD-RUN
   CHILD-RC @ ENGINE-ERROR:CODE-CERT T=

   s" and the diagnostic names the word that came back" T-LABEL
   CHILD-ERR$ s" hb: ntrapy returned" CONTAINS? TTRUE

   s" and says nothing about a tag" T-LABEL
   CHILD-ERR$ s" tag" CONTAINS? TFALSE ;

: REBIND-CASE ( -- )
   s" a global source replacement of throw or die still returns" T-LABEL
   s" test/compiler/native-terminal-rebind.f" CHILD-RUN
   CHILD-RC @ 0 T= ;

\ ---- callers of many and long-named words that never return ------------------
\ Behind each call to a word that never returns, a tier-1 caller compiles a trap
\ whose message names the callee whole. No table holds those names, so tier 1
\ compiles what tier 0 runs and each caller answers what tier 0 answers; the
\ messages share the literal store with source strings, and it grows by segments
\ (src/compiler/native/string.f). The sizes pass what a fixed store would hold:
\ eighty messages naming callees in 6995 bytes each are 560,640 bytes, past one
\ segment's 512 KB arena, and 1100 callees named in a few bytes pass 1024 names.
$2000 constant SRC-CAP               \ one definition around a 6995-byte name
SRC-CAP BUFFER: SRC
variable SRC-U
variable ANSWERS                     \ callers that answered what tier 0 answers

: SRC+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   SRC-U @ u + SRC-CAP > if E-STR-CAPACITY throw then
   a  SRC SRC-U @ +  u BYTE-COPY
   u SRC-U +! ;

: SRC$ ( -- ptr u8 n )
   SRC SRC-U @ ;

: NUM+ ( n -- )
   SB-RESET FMT:SB-U SB$ SRC+ ;

\ Callee k's name: a stem and k, then `x` up to `len` bytes.
: CALLEE+ ( n n -- )
   {: k:n len:n :}
   SRC-U @ {: at:n :}
   s" NTN" SRC+  k NUM+
   len  SRC-U @ at -  -  0 ?do s" x" SRC+ loop ;

\ Callee k never returns, and its caller calls it only on 0.
: NORET-PAIR ( n n -- )
   {: k:n len:n :}
   0 SRC-U !
   s" : " SRC+  k len CALLEE+  S\"  ( -- ) s\q died\q 3 die ;" SRC+
   SRC$ evaluate-closed
   0 SRC-U !
   s" : NTK" SRC+  k NUM+  s"  ( n -- n ) dup 0= if " SRC+  k len CALLEE+
   s"  then 1 + ;" SRC+
   SRC$ evaluate-closed
   0 SRC-U !
   s" 5 NTK" SRC+  k NUM+
   SRC$ TEST-EVAL:N  6 = if 1 ANSWERS +! then ;

: NORET-PAIRS ( n n n -- )
   {: first:n count:n len:n :}
   count 0 ?do  first i + len NORET-PAIR  loop ;

: NORET-ANY-CASE ( -- )
   s" eighty callers of callees named in 6995 bytes each compile" T-LABEL
   0 ANSWERS !
   [: 0 80 6995 NORET-PAIRS ;] 0 TTHROWSQ
   s" and each answers what tier 0 answers" T-LABEL
   ANSWERS @ 80 T=

   s" 1100 callers of as many short-named callees compile" T-LABEL
   0 ANSWERS !
   [: 80 1100 0 NORET-PAIRS ;] 0 TTHROWSQ
   s" and each answers what tier 0 answers" T-LABEL
   ANSWERS @ 1100 T= ;

variable ZERO-CONTINUED
defer ZERO-THROW ( n -- )

: ZERO-BODY ( -- ) 0 ZERO-THROW 1 ZERO-CONTINUED ! ;

: PRIMITIVE-THROW-CASE ( -- )
   s" even zero throw unwinds instead of continuing at its caller" T-LABEL
   0 ZERO-CONTINUED !
   [: throw ;] is ZERO-THROW
   [: ZERO-BODY ;] 0 TTHROWSQ
   ZERO-CONTINUED @ 0 T=
   s" a compiled guard keeps its successful and throwing paths" T-LABEL
   s" : NT-GUARD ( n -- n ) dup 0< if E-A-EMPTY throw then 1+ ;" evaluate-closed
   s" 41 NT-GUARD 42 T=" evaluate-closed
   s" : NT-REFUSE ( -- ) -1 NT-GUARD drop ;" evaluate-closed
   s" ' NT-REFUSE E-A-EMPTY TTHROWS" evaluate-closed ;

\ An earlier exit block must not interrupt the guard's successful trace.
public
: NT-GUARD ( n -- n )
   dup 0= if 1+ exit then
   dup 1 = if E-A-EMPTY throw then
   2 * ;

: NT-LT ( n n -- n ) < if E-A-EMPTY throw then 42 ;
: NT-FLT ( r r -- n ) f< if E-A-EMPTY throw then 42 ;
: NT-FZERO ( r -- n ) f0< if E-A-EMPTY throw then 42 ;
: NT-COLD-SELECT ( n -- n )
   dup 0= if drop 42 exit then
   dup 1 = if drop E-A-EMPTY throw then
   drop E-A-BOUNDS throw ;
private

: COMPILED-GUARD-CASE ( -- )
   s" a guard continues into its normal successor before an earlier exit" T-LABEL
   0 NT-GUARD 1 T= 2 NT-GUARD 4 T=
   [: 1 NT-GUARD drop ;] E-A-EMPTY TTHROWSQ

   s" inverted register comparisons keep both integer edges" T-LABEL
   2 1 NT-LT 42 T= 1 1 NT-LT 42 T=
   [: 1 2 NT-LT drop ;] E-A-EMPTY TTHROWSQ

   s" inverted float guards preserve the unordered edge" T-LABEL
   2.0 1.0 NT-FLT 42 T=
   [: 1.0 2.0 NT-FLT drop ;] E-A-EMPTY TTHROWSQ
   0.0 0.0 f/ 1.0 NT-FLT 42 T=
   1.0 0.0 0.0 f/ NT-FLT 42 T=
   0.0 NT-FZERO 42 T=
   [: -1.0 NT-FZERO drop ;] E-A-EMPTY TTHROWSQ
   0.0 0.0 f/ NT-FZERO 42 T= ;

: COLD-DISPATCH-CASE ( -- )
   s" a select returns on its success arm and throws each error" T-LABEL
   0 NT-COLD-SELECT 42 T=
   [: 1 NT-COLD-SELECT drop ;] E-A-EMPTY TTHROWSQ
   [: 2 NT-COLD-SELECT drop ;] E-A-BOUNDS TTHROWSQ ;

public

: RUN ( -- )
   T-RESET

   \ ---- the message a mismatch carries ----
   s" a family's message says its tag matched no arm and exits BAD-TAG" T-LABEL
   s" alpha" NTRAP:BAD-TAG {: a:ptr u:n rc:n :}
   a u s" hb: bad alpha tag" T$=
   rc ENGINE-ERROR:BAD-TAG T=

   s" an empty name is refused, since no message could name it" T-LABEL
   [: s" " NTRAP:BAD-TAG drop drop drop ;] E-NTRAP-NAME TTHROWSQ

   \ ---- what a routine compiled from source does ----
   COMPILED-GUARD-CASE
   COLD-DISPATCH-CASE
   PRIMITIVE-THROW-CASE

   \ ---- callers of many and long-named words that never return ----
   NORET-ANY-CASE

   \ ---- and the whole of it, in a process that dies ----
   NORET-CASE
   REBIND-CASE

   T-REPORT ;

;package

NTRAP-TEST:RUN
