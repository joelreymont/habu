\ assert.f - checked test assertions, loaded by src/lib/test.f. An assertion counts
\ one case and never throws on a mismatch: it prints `F<case>`, the label, a TFAIL
\ record (src/lib/test/record.f) and an `assert:` line, and the run continues; a
\ string mismatch prints both strings whole. T-REPORT prints `test: ok` or exits
\ T-EX-FAIL. T-LABEL (at most T-LABEL-CAP bytes, else T-EX-FAIL) names every
\ assertion until the next T-LABEL, T-LABEL-CLEAR or T-RESET.
\ STORAGE CLASS. PROCESS-WIDE: static counters, label and report; see below.

require src/lib/error.f
require src/lib/string.f
require src/lib/test/record.f
require src/lib/fmt.f                          \ FMT:.INT - one-line number text, shared builder untouched

1 constant T-EX-FAIL

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

: T-LABEL$ ( -- ptr u8 n )
   T-LABEL-BUF T-LABEL-U @ ;

: T-LABEL ( ptr u8 n -- ) {: a:ptr u:n :}
   u T-LABEL-CAP > if s" test: label too long" T-EX-FAIL die then
   0 begin dup u < while
      dup a + c@  over T-LABEL-BUF + c!
      1+
   repeat drop
   u T-LABEL-U ! ;

: T-LABEL. ( -- )
   T-LABEL-U @ 0 > if s" case: " type T-LABEL$ type cr then ;

: T-FAIL+ ( -- )
   1 T-FAIL# atomic-add drop ;

: T-FAIL-AS ( ptr u8 n -- ) {: la:ptr lu:n :}
   [char] F emit T-CASE# @ .
   T-LABEL.
   la lu T-CASE# @ T-LABEL$ TREC-FAIL
   T-FAIL+ ;

: T-FAIL ( -- )
   s" assert" T-FAIL-AS ;

: T-ASSERT-DETAIL ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   T-FAIL
   s" assert: " type msg msgu type cr ;

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

: T-STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: T$= ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr v:n :}
   T-NEXT
   a u b v T-STR= 0= if
      T-FAIL
      s" assert: expected string:" type cr
      b v type cr
      s" got string:" type cr
      a u type cr
   then ;

: T$<> ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr v:n :}
   T-NEXT
   a u b v T-STR= if
      T-FAIL
      s" assert: expected different string:" type cr
      a u type cr
   then ;

: T-DIGITS-END ( ptr u8 n n -- n ) {: a:ptr u:n i:n :}
   i begin dup u < while
      dup a + c@ STR-DIGIT? 0= if exit then
      1+
   repeat ;

: T-LINE-END ( ptr u8 n n -- n ) {: a:ptr u:n i:n :}
   i begin dup u < while
      dup a + c@ 10 = if exit then
      1+
   repeat ;

: T-FROM ( ptr u8 n ptr u8 n n -- n )   \ FIND-SUB-FROM's offset, else -1
   FIND-SUB-FROM MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

\ The offset of the line number after `f:` in the ` --> ` line of r, else -1.
: T-PLACE-LINE ( ptr u8 n ptr u8 n -- n ) {: r:ptr ru:n f:ptr fu:n :}
   r ru s"  --> " 0 T-FROM {: a:n :}
   a 0< if -1 exit then
   r  r ru a T-LINE-END  f fu a T-FROM {: t:n :}
   t 0< if -1 exit then
   t fu + {: s:n :}
   s ru < if s r + c@ [char] : = if s 1+ exit then then
   -1 ;

\ Whether r holds, at h, a line `d | ` whose rest contains c.
: T-GUTTER-AT? ( ptr u8 n ptr u8 n ptr u8 n n -- bool )
   {: r:ptr ru:n d:ptr du:n c:ptr cu:n h:n :}
   h du + 3 + {: s:n :}
   s ru > if STR-FALSE exit then
   h 1- r + c@ 10 <> if STR-FALSE exit then
   h du + r + 3 s"  | " STR= 0= if STR-FALSE exit then
   s r +  r ru s T-LINE-END s -  c cu CONTAINS? ;

: T-GUTTER? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: r:ptr ru:n d:ptr du:n c:ptr cu:n :}
   du 0= if STR-FALSE exit then
   1 begin
      >r r ru d du r> T-FROM
      dup 0< 0= while
      dup >r r ru d du c cu r> T-GUTTER-AT? if drop STR-TRUE exit then
      1+
   repeat drop STR-FALSE ;

\ A record's place in the file whose name ends f, asserted without pinning a
\ line: its ` --> ` line names `f:N:col`, and its gutter line `N | ` shows a
\ source line that contains c. Two assertions, as T= and TTRUE count them.
: T-PLACED ( ptr u8 n ptr u8 n n ptr u8 n -- )
   {: r:ptr ru:n f:ptr fu:n col:n c:ptr cu:n :}
   r ru f fu T-PLACE-LINE {: ls:n :}
   ls 0< if -1 col T=  STR-FALSE TTRUE exit then
   r ru ls T-DIGITS-END {: le:n :}
   le ru < if le r + c@ [char] : = else STR-FALSE then
   if
      le 1+ r +  r ru le 1+ T-DIGITS-END le 1+ -  STR-PARSE-POS MATCH option
        none OF -1 ENDOF
        some OF ENDOF
      ;MATCH
   else -1 then
   col T=
   r ru  ls r +  le ls -  c cu T-GUTTER? TTRUE ;

: T-REPORT ( -- )
   T-FAIL# @ 0= if s" test: ok" type cr exit then
   s" test: " type T-FAIL# @ FMT:.INT s"  failure(s)" type cr
   s" test: failures" T-EX-FAIL die ;

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
