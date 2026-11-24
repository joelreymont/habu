\ jsonl.f - JSONL framing: SPLIT-LINE splits a memory span into physical lines.
\ It neither trims nor parses: each caller decides whether blank or malformed
\ rows are errors.
\ STORAGE CLASS. CALLER-OWNED: SPLIT-LINE lends spans of the caller's bytes and
\ owns none.

require src/lib/error.f
require src/lib/string.f

package JSONL
public

-9470 constant E-FIRST
-9479 constant E-LAST
-9471 s" a negative length, a null nonempty span or a negative position" ERROR:CODE E-SOURCE

private

10 constant LF

public

\ The physical line at pos, the position after its LF (the text's length when
\ unterminated) and true; at or past the end an empty span at pos, pos and
\ false.
: SPLIT-LINE ( str n -- str n bool )
   {: s:str pos:n :}
   s STR:LEN 0 < if E-SOURCE throw then
   s STR:LEN 0 > s STR:DATA 0= and if E-SOURCE throw then
   pos 0 < if E-SOURCE throw then
   pos s STR:LEN >= if s STR:DATA pos + 0 >str pos false exit then
   s LF pos STR:SPLIT-NEXT {: line:str next:n found:bool :}
   line next s STR:LEN min found ;

;package
