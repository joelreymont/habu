\ codegen-compare-gap.f - the words a corpus holds that the native chain cannot
\ compile yet, and the check that every corpus word is accounted for.
\ One concern: the gap account of a measured pass.
\
\ EVERY CORPUS WORD IS ACCOUNTED FOR. A word is either compiled - the real chain
\ runs on it and it gets a row of its own - or declared a gap that names the
\ capability the chain still lacks. Nothing is skipped: COVERAGE-CK refuses a
\ pass in which some corpus word is neither, so "the new column has fewer rows"
\ can only ever mean "these named capabilities are missing", never "the harness
\ quietly stopped looking".
\
\ THERE IS NO WAY TO DECLARE A GAP WITHOUT A CAPABILITY. GAP takes one, by type,
\ and GAP-ALSO adds the next; a row that named none would have to be written past
\ this package's own declarers, and the decoder refuses a code outside the
\ vocabulary at first touch. That is what keeps a shorter new column from ever
\ reading as an excuse.
\
\ A GAP ROW NAMES EVERY CAPABILITY IT NEEDS rather than the first that stops it.
\ A word that needs a branch and a comparison is not unblocked by branches alone,
\ and a reader planning the next capability should see that.
\
\ THIS FILE IS SHARED BY BOTH CORPORA AND HOLDS ONE STORE. A measurement pass
\ holds one corpus at a time - tools/codegen-compare-cases.f measures the first
\ and tools/codegen-compare-cases2.f the second, each opening with a RESET - so
\ one store serves both and the report renders whichever pass just ran without
\ knowing which corpus it was.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f

package CODEGEN-GAP

public

\ What a corpus word needs that the chain has not got. A gap row stores these
\ rather than a sentence, so a row that names no capability at all is unwritable
\ and the report renders every one of them the same way. In order: a branch, a
\ loop, or an exit from the middle of one; a typed locals frame; calling another
\ word, recursion included; a load or a store; an ordering or equality
\ operation; integer division.
ENUM cap DERIVE eq
   control-flow
   locals
   calls
   memory
   comparison
   division
;ENUM

private

16 constant GAP-MAX
6 constant CAP-MAX

GAP-MAX CODEGEN-COMPARE:NAME-MAX * BUFFER: GAP-NAMES
create GAP-LENS GAP-MAX cells allot
create GAP-CAP-N GAP-MAX cells allot
create GAP-CAPS GAP-MAX CAP-MAX * cells allot

variable GAP-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

: GAP-OK ( n -- n )
   dup 0 < over GAP-N @ >= or if E-CODEGEN-COMPARE-ROW throw then ;

: GAP-NAME-AT ( n -- ptr u8 )
   CODEGEN-COMPARE:NAME-MAX * GAP-NAMES + ;

\ A stored row is cells, so a capability crosses to a number here and back
\ there. The decoder is exhaustive and refuses a code outside the vocabulary at
\ first touch, so a corrupted row cannot decode as some other capability.
: CAP-CODE ( CODEGEN-GAP:cap -- n )
   MATCH cap
      control-flow OF 0 ENDOF
      locals       OF 1 ENDOF
      calls        OF 2 ENDOF
      memory       OF 3 ENDOF
      comparison   OF 4 ENDOF
      division     OF 5 ENDOF
   ;MATCH ;

: N>CAP ( n -- CODEGEN-GAP:cap )
   case
      0 of CODEGEN--GAP-CAP:CONTROL-FLOW endof
      1 of CODEGEN--GAP-CAP:LOCALS endof
      2 of CODEGEN--GAP-CAP:CALLS endof
      3 of CODEGEN--GAP-CAP:MEMORY endof
      4 of CODEGEN--GAP-CAP:COMPARISON endof
      5 of CODEGEN--GAP-CAP:DIVISION endof
      E-CODEGEN-COMPARE-ROW throw
   endcase ;

\ Every name a gap writes down has to be a corpus word the old column really
\ measured. A misspelling would otherwise become a gap for a word that does not
\ exist, or leave a real word accounted for by nothing.
: CORPUS-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   CODEGEN-COMPARE:PATH-OLD a u CODEGEN-COMPARE:FIND-ROW 0 < if
      E-CODEGEN-COMPARE-CORPUS throw
   then ;

public

\ Declare a corpus word the chain cannot express yet, and the first capability
\ it is waiting for. There is no way to declare one without a capability.
: GAP ( ptr u8 n CODEGEN-GAP:cap -- ) {: a:ptr u:n c:CODEGEN-GAP:cap :}
   GAP-N @ GAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   u CODEGEN-COMPARE:NAME-MAX > if E-CODEGEN-COMPARE-CAP throw then
   a u CORPUS-CK
   a  GAP-N @ GAP-NAME-AT  u STR-LEN BYTE-COPY-LEN
   u GAP-LENS GAP-N @ SLOT !
   c CAP-CODE GAP-CAPS GAP-N @ CAP-MAX * SLOT !
   1 GAP-CAP-N GAP-N @ SLOT !
   GAP-N @ 1+ GAP-N ! ;

\ Another capability the gap just declared is also waiting for.
: GAP-ALSO ( CODEGEN-GAP:cap -- ) {: c:CODEGEN-GAP:cap :}
   GAP-N @ 1- GAP-OK {: k:n :}
   GAP-CAP-N k SLOT @ {: j:n :}
   j CAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   c CAP-CODE GAP-CAPS k CAP-MAX * j + SLOT !
   j 1+ GAP-CAP-N k SLOT ! ;

: GAPS ( -- n )
   GAP-N @ ;

: GAP-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k GAP-OK GAP-NAME-AT
   GAP-LENS k SLOT @ ;

: GAP-CAPS@ ( n -- n ) {: k:n :}
   GAP-CAP-N k GAP-OK SLOT @ ;

: GAP-CAP@ ( n n -- CODEGEN-GAP:cap ) {: k:n j:n :}
   k GAP-OK drop
   j 0 < j k GAP-CAPS@ >= or if E-CODEGEN-COMPARE-ROW throw then
   GAP-CAPS k CAP-MAX * j + SLOT @ N>CAP ;

\ How a capability reads in the report. The one place a capability becomes text.
: CAP$ ( CODEGEN-GAP:cap -- ptr u8 n ) {: c:CODEGEN-GAP:cap :}
   c MATCH cap
      control-flow OF s" control flow" ENDOF
      locals       OF s" locals" ENDOF
      calls        OF s" calls" ENDOF
      memory       OF s" memory access" ENDOF
      comparison   OF s" comparison" ENDOF
      division     OF s" division" ENDOF
   ;MATCH ;

: RESET ( -- )
   0 GAP-N ! ;

private

: GAP-FOR? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   false
   GAP-N @ 0 ?do
      i GAP-NAME$ a u STR= if drop true leave then
   loop ;

public

\ Every corpus word is compiled or declared a gap. A word that is neither would
\ leave the new column quietly shorter than the old one, which is the one
\ failure a comparison harness must never have. Called at the end of a new
\ column's pass, over the rows that pass left behind.
: COVERAGE-CK ( -- )
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if
         CODEGEN-COMPARE:PATH-NEW i CODEGEN-COMPARE:NAME$
         CODEGEN-COMPARE:FIND-ROW 0 < if
            i CODEGEN-COMPARE:NAME$ GAP-FOR? 0= if
               E-CODEGEN-COMPARE-CORPUS throw
            then
         then
      then
   loop ;

;package
