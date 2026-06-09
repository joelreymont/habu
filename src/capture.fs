\ capture.fs — read a definition's effect and body from the input stream.
\ Parsing-aware: comments are dropped, string literals kept verbatim, and the
\ terminator is the bare `;` token (a `;]` quotation closer is NOT it).

create CAP-BUF  1024 chars allot
variable CAP-LEN
: CAP-RESET ( -- )       0 CAP-LEN ! ;
: CAP$      ( -- c-addr u )  CAP-BUF CAP-LEN @ ;
: CAP+      ( c-addr u -- )  \ append, clamped
   dup CAP-LEN @ + 1024 > if E-ARENA throw then
   {: ca u :}  ca  CAP-BUF CAP-LEN @ chars +  u move  u CAP-LEN +! ;
: CAP-SP    ( -- )       s"  " CAP+ ;
: CAP+TOK   ( c-addr u -- )  CAP+ CAP-SP ;        \ token + trailing space

\ Next whitespace token across line refills; ( a 0 ) only at true end of input.
: NEXT-SRC-TOK ( -- a u )
   begin parse-name dup 0= while  2drop refill 0= if s" " exit then  repeat ;

\ Require the next token to be `(`, then return the effect text up to `)`.
: PARSE-EFFECT ( -- c-addr u )
   NEXT-SRC-TOK s" (" compare if E-BADTYPE throw then
   [char] ) parse ;

\ Keep a string literal verbatim: WORD, one space, text up to the closing ", ".
\ parse-name already consumed the delimiter space after the word, so `parse`
\ starts at the first string char (no leading space to handle). An embedded `;`
\ inside the string is therefore part of the span, not a terminator.
: KEEP-STRING ( word-a word-u -- )
   CAP+TOK  [char] " parse  CAP+  s\" \"" CAP+  CAP-SP ;

: DISPATCH-TOK ( a u -- )
   2dup s" ("   compare 0= if 2drop [char] ) parse 2drop exit then   \ ( … ) drop
   2dup s" \"   compare 0= if 2drop source nip >in ! exit then       \ \ … drop EOL
   2dup s" .("  compare 0= if 2drop [char] ) parse 2drop exit then   \ .( … ) drop
   2dup s\" s\"" CI= if KEEP-STRING exit then
   2dup s\" .\"" CI= if KEEP-STRING exit then
   2dup s\" c\"" CI= if KEEP-STRING exit then
   2dup s" [char]" CI= if CAP+TOK NEXT-SRC-TOK CAP+TOK exit then
   2dup s" char"   CI= if CAP+TOK NEXT-SRC-TOK CAP+TOK exit then
   CAP+TOK ;                                                         \ default: keep

\ Capture the body up to (and consuming) the bare `;`. Returns the body text.
: CAPTURE-BODY ( -- c-addr u )
   CAP-RESET
   begin
     NEXT-SRC-TOK dup 0= if E-QUOT throw then     \ EOF before `;`
     2dup s" ;" compare 0= if 2drop CAP$ exit then
     DISPATCH-TOK
   again ;
