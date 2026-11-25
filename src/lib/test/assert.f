\ assert.f - checked test assertions. An assertion counts
\ one case and never throws on a mismatch: it prints `F<case>`, the label, a TFAIL
\ record (src/lib/test/record.f) and an `assert:` line, and the run continues; a
\ string mismatch prints both strings whole. T-REPORT prints `test: ok` or exits
\ EXIT-STATUS:FAILURE. T-LABEL (at most T-LABEL-CAP bytes, else that exit)
\ names every assertion until the next T-LABEL, T-LABEL-CLEAR or T-RESET.
\ STORAGE CLASS. PROCESS-WIDE: static counters, label and report; see below.

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/test/record.f
require src/lib/fmt.f                          \ FMT:.INT - one-line number text, shared builder untouched
require src/core/exit-status.f

variable T-CASE#
variable T-FAIL#

\ Only the counters accept concurrent increments. Reset, labels, failure
\ output, and report stay process-owned.

256 constant T-LABEL-CAP
create T-LABEL-BUF T-LABEL-CAP allot
variable T-LABEL-U

: T-RESET ( -- )
   0 T-CASE# !
   0 T-FAIL# !
   0 T-LABEL-U ! ;

: T-CASES ( -- n )
   T-CASE# @ ;

: T-FAILURES ( -- n )
   T-FAIL# @ ;

: T-NEXT ( -- )
   1 T-CASE# atomic-add drop ;

: T-LABEL-CLEAR ( -- )
   0 T-LABEL-U ! ;

: T-LABEL$ ( -- str )
   T-LABEL-BUF T-LABEL-U @ >str ;

: T-LABEL ( str -- ) {: s:str :}
   s STR:LEN T-LABEL-CAP > if s" test: label too long" EXIT-STATUS:FAILURE die then
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@  over T-LABEL-BUF + c!
      1+
   repeat drop
   s STR:LEN T-LABEL-U ! ;

: T-LABEL. ( -- )
   T-LABEL-U @ 0 > if s" case: " type T-LABEL$ type cr then ;

: T-FAIL+ ( -- )
   1 T-FAIL# atomic-add drop ;

: T-FAIL-AS ( str -- ) {: ls:str :}
   [char] F emit T-CASE# @ .
   T-LABEL.
   ls T-CASE# @ T-LABEL$ TREC-FAIL
   T-FAIL+ ;

: T-FAIL ( -- )
   s" assert" T-FAIL-AS ;

: T-ASSERT-DETAIL ( str -- ) {: msg:str :}
   T-FAIL
   s" assert: " type msg type cr ;

: T-ASSERT ( bool -- )
   T-NEXT
   0= if s" expected true got false" T-ASSERT-DETAIL then ;

: T= ( n n -- ) {: got:n want:n :}
   T-NEXT
   got want <> if
      T-FAIL
      s" assert: expected " type want FMT:.INT
      s"  got " type got FMT:.INT
      cr
   then ;

: T<> ( n n -- ) {: got:n want:n :}
   T-NEXT
   got want = if
      T-FAIL
      s" assert: expected not " type want FMT:.INT
      s"  got " type got FMT:.INT
      cr
   then ;

: TTRUE ( bool -- )
   T-ASSERT ;

: TFALSE ( bool -- )
   T-NEXT
   if s" expected false got true" T-ASSERT-DETAIL then ;

: T-STR= ( str str -- bool ) {: a:str b:str :}
   a STR:LEN b STR:LEN <> if 0 0= 0= exit then
   0 begin dup a STR:LEN < while
      dup a STR:DATA + c@ over b STR:DATA + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: T$= ( str str -- )
   {: a:str b:str :}
   T-NEXT
   a b T-STR= 0= if
      T-FAIL
      s" assert: expected string:" type cr
      b type cr
      s" got string:" type cr
      a type cr
   then ;

: T$<> ( str str -- )
   {: a:str b:str :}
   T-NEXT
   a b T-STR= if
      T-FAIL
      s" assert: expected different string:" type cr
      a type cr
   then ;

: T-DIGITS-END ( str n -- n ) {: s:str i:n :}
   i begin dup s STR:LEN < while
      dup s STR:DATA + c@ STR:DIGIT? 0= if exit then
      1+
   repeat ;

: T-LINE-END ( str n -- n ) {: s:str i:n :}
   i begin dup s STR:LEN < while
      dup s STR:DATA + c@ 10 = if exit then
      1+
   repeat ;

: T-FROM ( str str n -- n )
   STR:FIND-SUB-FROM MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ The offset of the line number after `f:` in the ` --> ` line of r, else -1.
: T-PLACE-LINE ( str str -- n ) {: r:str f:str :}
   r s"  --> " 0 T-FROM {: a:n :}
   a 0< if -1 exit then
   r STR:DATA  r a T-LINE-END >str  f a T-FROM {: t:n :}
   t 0< if -1 exit then
   t f STR:LEN + {: s:n :}
   s r STR:LEN < if s r STR:DATA + c@ [char] : = if s 1+ exit then then
   -1 ;

\ Whether r holds, at h, a line `d | ` whose rest contains c.
: T-GUTTER-AT? ( str str str n -- bool )
   {: r:str d:str c:str h:n :}
   h d STR:LEN + 3 + {: s:n :}
   s r STR:LEN > if STR:FALSE exit then
   h 1- r STR:DATA + c@ 10 <> if STR:FALSE exit then
   h d STR:LEN + r STR:DATA + 3 >str s"  | " STR:= 0= if STR:FALSE exit then
   s r STR:DATA +  r s T-LINE-END s - >str  c STR:CONTAINS? ;

: T-GUTTER? ( str str str -- bool )
   {: r:str d:str c:str :}
   d STR:LEN 0= if STR:FALSE exit then
   1 begin
      >r r d r> T-FROM
      dup 0< 0= while
      dup >r r d c r> T-GUTTER-AT? if drop STR:TRUE exit then
      1+
   repeat drop STR:FALSE ;

\ A record's place in the file whose name ends f, asserted without pinning a
\ line: its ` --> ` line names `f:N:col`, and its gutter line `N | ` shows a
\ source line that contains c. Two assertions, as T= and TTRUE count them.
: T-PLACED ( str str n str -- )
   {: r:str f:str col:n c:str :}
   r f T-PLACE-LINE {: ls:n :}
   ls 0< if -1 col T=  STR:FALSE TTRUE exit then
   r ls T-DIGITS-END {: le:n :}
   le r STR:LEN < if le r STR:DATA + c@ [char] : = else STR:FALSE then
   if
      le 1+ r STR:DATA +  r le 1+ T-DIGITS-END le 1+ - >str  STR:PARSE-POS MATCH option
        none OF -1 ENDOF
        some OF ENDOF
      ;MATCH
   else -1 then
   col T=
   r  ls r STR:DATA +  le ls - >str  c T-GUTTER? TTRUE ;

: T-REPORT ( -- )
   T-FAIL# @ 0= if s" test: ok" type cr exit then
   s" test: " type T-FAIL# @ FMT:.INT s"  failure(s)" type cr
   s" test: failures" EXIT-STATUS:FAILURE die ;

\ A deadline is no verdict on the code under test (src/lib/test/ending.f): a
\ check that wants another code lets E-PROC-TIMEOUT go uncaught and counts
\ nothing. The expected code is a local, so a throw check inside the xt keeps
\ its own.
: TTHROWSQ ( [ -- ] n -- ) {: want:n :}
   catch {: code:n :}
   code E-PROC-TIMEOUT = want E-PROC-TIMEOUT <> and if code throw then
   code want = T-ASSERT ;

\ Named execution tokens and literal quotations share the same checked effect.
: TTHROWS ( [ -- ] n -- )
   TTHROWSQ ;
