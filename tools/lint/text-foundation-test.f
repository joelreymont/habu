\ text-foundation-test.f - focused tests for the tools/lint/text.f text helpers
\ and the tools/lint/token.f token table.
\ Run: bin/hb --load tools/lint/text-foundation-test.f

require lib/errors.f
require lib/string.f
require lib/string-roles.f               \ package STR: the typed string surface
require lib/memory.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require lib/fmt.f                        \ FMT:.INT - one-line number text

package LINT-TEXT-TEST
using LINT-SPLIT
private

\ STR:BUF-LEN@ reads a fixture buffer's length as a NUM:byte-len role, and
\ every ( -- ptr u8 n ) accessor below returns a raw span, so the length is read
\ out here. A checked cast at this test's own scope: projection out of a cell
\ family needs no ownership (checker.f CAST-OWNER?), so NUM is not reopened.
CAST: TFT-BL>RAW ( NUM:byte-len -- n )

variable TEST-N
: ASSERT  ( bool -- )
   IF
      TEST-N @ 1+ TEST-N !
      exit
   THEN
   s" text-foundation-test failed at assertion " type TEST-N @ FMT:.INT cr
   s" text-foundation-test failed" 1 die ;
: ASSERT=  ( n n -- )  = ASSERT ;
: ASSERT$  ( ptr u8 n ptr u8 n -- )  LINT-STR= ASSERT ;

$100 constant FIX-CAP
create BAD-FIX FIX-CAP allot     variable BAD-LEN
create TOK-FIX FIX-CAP allot     variable TOK-LEN
create MOVE-FIX 16 allot

\ `: BAD s" nope` - the literal opened at byte 6 never closes.
: UNTERM-FIX$  ( -- ptr u8 n )
   BAD-LEN STR:BUF-RESET
   s" : BAD s" STR:LENGTH BAD-FIX FIX-CAP STR:LENGTH BAD-LEN STR:BUF-APPEND
   DQUOTE BAD-FIX FIX-CAP STR:LENGTH BAD-LEN STR:BUF-APPEND-C
   s"  nope" STR:LENGTH BAD-FIX FIX-CAP STR:LENGTH BAD-LEN STR:BUF-APPEND
   BAD-FIX BAD-LEN STR:BUF-LEN@ TFT-BL>RAW ;

: INIT-TOK-FIX  ( -- )
   TOK-LEN STR:BUF-RESET
   s" : X ( n -- n ) dup " STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND
   92 TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND-C
   s"  skip" STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND
   10 TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND-C
   s" : Y ;" STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND ;
: TOK-FIX$  ( -- ptr u8 n )  TOK-FIX TOK-LEN STR:BUF-LEN@ TFT-BL>RAW ;

\ ---- string-literal fixtures ---------------------------------------------
\ Built byte-wise: writing a quote or a backslash as a literal here would end
\ this file's own strings. ROW-Q appends a double quote, ROW-BS a backslash and
\ ROW-NL a newline.
$400 constant ROW-CAP
create ROW-FIX ROW-CAP allot     variable ROW-LEN

: ROW-RESET  ( -- )  ROW-LEN STR:BUF-RESET ;
: ROW+  ( ptr u8 n -- )  STR:LENGTH ROW-FIX ROW-CAP STR:LENGTH ROW-LEN STR:BUF-APPEND ;
: ROW-C+  ( n -- )  ROW-FIX ROW-CAP STR:LENGTH ROW-LEN STR:BUF-APPEND-C ;
: ROW-Q  ( -- )  DQUOTE ROW-C+ ;
: ROW-BS  ( -- )  92 ROW-C+ ;
: ROW-NL  ( -- )  10 ROW-C+ ;
: ROW$  ( -- ptr u8 n )  ROW-FIX ROW-LEN STR:BUF-LEN@ TFT-BL>RAW ;

\ Scanner callers normally supply positive spans; negative/zero lengths
\ must preserve the destination rather than reach BYTE-COPY as a huge count.
: TEST-EMPTY-MOVE ( -- )
   s" unchanged" drop MOVE-FIX 9 LINT-BMOVE
   s" x" drop MOVE-FIX -1 LINT-BMOVE
   MOVE-FIX 9 s" unchanged" ASSERT$
   s" x" drop MOVE-FIX 0 LINT-BMOVE
   MOVE-FIX 9 s" unchanged" ASSERT$ ;

: TEST-TOKENIZER  ( -- )
   LINT-TRUE PARENS? !
   TOK-FIX$ TOKENIZE
   TN# @ 6 ASSERT=
   0 TOK s" :" ASSERT$
   0 TOK0? ASSERT
   1 TOK s" X" ASSERT$
   1 TOK0? 0= ASSERT
   2 TOK s" dup" ASSERT$
   2 TEOL? ASSERT
   3 TOK s" :" ASSERT$
   3 TOK0? ASSERT
   4 TOK s" Y" ASSERT$
   5 TOK s" ;" ASSERT$
   5 TEOL? ASSERT ;

\ A backslash-leading payload must not eat the definition's closing semicolon.
\ Keep the payload opaque even when it spells emitter instructions or comments.
: TOKEN-LITERAL ( n bool -- ) {: first:n escaped:bool :}
   ROW-RESET s" : X " ROW+ first ROW-C+
   escaped if ROW-BS then ROW-Q
   s"  " ROW+ ROW-BS s" n : LFAKE LABEL@ LBL, ; ( " ROW+
   escaped if ROW-BS ROW-Q then ROW-Q
   s"  ; : Y ;" ROW+
   ROW$ TOKENIZE
   TN# @ 8 ASSERT=
   3 TOK drop c@ 92 ASSERT=
   3 TOK s" LFAKE LABEL@ LBL," LINT-CONTAINS? ASSERT
   4 TOK s" ;" ASSERT$
   5 TOK s" :" ASSERT$
   6 TOK s" Y" ASSERT$
   7 TOK s" ;" ASSERT$ ;

: TEST-TOKEN-LITERALS ( -- )
   LINT-TRUE PARENS? !
   115 LINT-FALSE TOKEN-LITERAL
   99 LINT-FALSE TOKEN-LITERAL
   46 LINT-FALSE TOKEN-LITERAL
   115 LINT-TRUE TOKEN-LITERAL
   99 LINT-TRUE TOKEN-LITERAL
   46 LINT-TRUE TOKEN-LITERAL
   ROW-RESET s" : X .( : FAKE ; ) ; " ROW+
   ROW-BS s" : ALSO-FAKE ;" ROW+ ROW-NL s" : Y ;" ROW+
   ROW$ TOKENIZE
   TN# @ 6 ASSERT=
   2 TOK s" ;" ASSERT$
   3 TOK s" :" ASSERT$
   3 TOK0? ASSERT
   [: UNTERM-FIX$ TOKENIZE ;] catch E-LINT-TOKEN-SOURCE ASSERT=
   TN# @ 0 ASSERT= ;

\ CMP-CI is an ORDER, and a caller only gets to replace a scan with a binary
\ search if it is a total one whose 0 answer is exactly LINT-STR=CI's true. The
\ three laws are checked directly: sign, antisymmetry, and agreement with the
\ equality the scan used - including the case where one name is a prefix of the
\ other, which is where a compare that only walked the shared bytes would call
\ two different names equal.
: CMP-SIGN ( n -- n )
   dup 0 < if drop -1 exit then
   0 > if 1 exit then
   0 ;

: BOOL>N ( bool -- n )
   IF 1 ELSE 0 THEN ;

: ASSERT-CMP ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n b:ptr v:n want:n :}
   a u b v LINT-ORDER:CMP-CI CMP-SIGN want ASSERT=
   b v a u LINT-ORDER:CMP-CI CMP-SIGN 0 want - ASSERT=          \ antisymmetric
   a u b v LINT-STR=CI BOOL>N  want 0= BOOL>N  ASSERT= ;        \ 0 iff LINT-STR=CI

: TEST-CMP-CI ( -- )
   s" abc" s" abd" -1 ASSERT-CMP
   s" abd" s" abc" 1 ASSERT-CMP
   s" abc" s" abc" 0 ASSERT-CMP
   s" ABC" s" abc" 0 ASSERT-CMP                                 \ folded, like the dictionary
   s" aBc" s" AbC" 0 ASSERT-CMP
   s" ab" s" abc" -1 ASSERT-CMP                                 \ prefix sorts first
   s" abc" s" ab" 1 ASSERT-CMP
   s" " s" " 0 ASSERT-CMP
   s" " s" a" -1 ASSERT-CMP
   s" RAW>NODE" s" RAW>SLOT" -1 ASSERT-CMP                      \ real mint names
   s" MINT-ROW" s" MINT-PATH" 1 ASSERT-CMP
   s" raw>node" s" RAW>NODE" 0 ASSERT-CMP ;

: TEST-CMP-CI-TRANSITIVE ( -- )                                 \ a<b and b<c imply a<c
   s" MINT-BYTE-LEN" s" MINT-CELL-OFF" -1 ASSERT-CMP
   s" MINT-CELL-OFF" s" MINT-INDEX" -1 ASSERT-CMP
   s" MINT-BYTE-LEN" s" MINT-INDEX" -1 ASSERT-CMP ;

\ The split words read no byte outside the caller's span. Each span here lies
\ against an inaccessible page (MEM:ALLOC-GUARDED keeps one on either side), so
\ a read past its end or before its start faults instead of answering.
: TEST-SPLIT-EDGES ( -- )
   STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED {: a:ptr u:n :}
   s" x yz" {: t:ptr tu:n :}
   t  a u tu - +  tu BYTE-COPY
   a u tu - +  tu SPLIT-WHITESPACE                      \ the last word ends at the edge
   SN# @ 2 ASSERT=
   1 S@ s" yz" ASSERT$
   32 a u 1- + c!
   a u 1- +  1 SPLIT-WHITESPACE                         \ a blank ends at the edge
   SN# @ 0 ASSERT=
   10 a c!
   a 1 SPLIT-LINES                                      \ an empty first line starts at it
   SN# @ 1 ASSERT=
   0 S@ nip 0 ASSERT=
   a u MEM:RELEASE-GUARDED ;

: RUN  ( -- )
   1 TEST-N !
   TEST-EMPTY-MOVE
   TEST-CMP-CI
   TEST-CMP-CI-TRANSITIVE
   INIT-TOK-FIX
   TEST-TOKENIZER
   TEST-TOKEN-LITERALS
   TEST-SPLIT-EDGES
   s" text-foundation-test: ok (" type TEST-N @ 1- FMT:.INT s"  assertions)" type cr ;

RUN

;package
