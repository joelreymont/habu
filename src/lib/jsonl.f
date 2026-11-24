split-str
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

\ The physical line at pos, the position after its LF (u when unterminated)
\ and true; at or past the end an empty span at pos, pos and false.
: SPLIT-LINE ( str n -- str n bool )
   {: a:^ u:n pos:n :}
   u 0 < if E-SOURCE throw then
   u 0 > a 0= and if E-SOURCE throw then
   pos 0 < if E-SOURCE throw then
   pos u >= if a pos + 0 pos false exit then
   a u LF pos STR:SPLIT-NEXT {: la:^ lu:n next:n found:bool :}
   la lu next u min found ;

;package
