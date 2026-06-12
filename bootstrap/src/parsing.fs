\ parsing.fs — parsing-word literals: S" S\" ." .( C" CHAR [CHAR]. Fills
\ CHECK-PARSE. The checker tokenises a body by whitespace, so a string literal
\ arrives as the opener token followed by content tokens; the token that ENDS
\ with the closing char terminates the string. We consume those content tokens
\ via B-NEXT and push the literal's result types.

\ True when string a/u is non-empty and its last char equals ch.
: ENDS-WITH? ( a u ch -- f )
   >r dup 0= if 2drop r> drop false exit then
   1- chars + c@ r> = ;

\ Consume body tokens until one ends with CH; that token closes the literal.
\ Throws E-QUOT on an unterminated literal: end of body reached first.
: SKIP-TO-END ( ch -- )
   {: CH :}
   begin
     B-NEXT dup 0= if E-QUOT throw then
     CH ENDS-WITH? if exit then
   again ;

\ String content closed by a double-quote, pushing nothing.
: SKIP-STRING ( -- )  [char] " SKIP-TO-END ;

\ S" / S\" — push two slots: an addr and a u32 length.
: DO-STRING ( -- )
   SKIP-STRING
   TC-ADDR MK-CON PUSH-DTYPE
   TC-U32  MK-CON PUSH-DTYPE ;

\ ." — printing string: consume content, push nothing.
: DO-PRINT ( -- )  SKIP-STRING ;

\ .( — printing comment: consume tokens until one ends with a close paren.
: DO-PAREN ( -- )  [char] ) SKIP-TO-END ;

\ C" — counted string: push ptr u8.
: DO-COUNTED ( -- )
   SKIP-STRING
   TC-U8 MK-CON MK-PTR PUSH-DTYPE ;

\ CHAR / [CHAR] — consume the next token, the char word, push a char.
: DO-CHAR ( -- )
   B-NEXT dup 0= if E-QUOT throw then  2drop
   TC-CHAR MK-CON PUSH-DTYPE ;

\ Opener-token matchers. s\" interprets \" as a double-quote and \\ as a
\ backslash, so these build the literal tokens S"  S\"  C"  ."  without our own
\ double-quote prematurely closing the Forth string.
: (CHECK-PARSE) ( c-addr u -- f )
   2dup s\" S\""    CI= if 2drop DO-STRING  true exit then
   2dup s\" S\\\""  CI= if 2drop DO-STRING  true exit then
   2dup s\" C\""    CI= if 2drop DO-COUNTED true exit then
   2dup s\" .\""    CI= if 2drop DO-PRINT   true exit then
   2dup s" .("      CI= if 2drop DO-PAREN   true exit then
   2dup s" CHAR"    CI= if 2drop DO-CHAR    true exit then
   2dup s" [CHAR]"  CI= if 2drop DO-CHAR    true exit then
   2drop false ;
' (CHECK-PARSE) is CHECK-PARSE
