\ tokstream.f - dump a source file's code tokens and stack-effect signatures
\ through the tree's lexer (package LINT-LEX): a comment-only edit leaves the
\ stream byte-identical.   bin/hb --load tools/tokstream.f -- path ...
\ Without `--` the paths load as sources. A refused run writes an empty stream and
\ two empty streams compare equal: the caller must check the exit code.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fmt.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f
require src/lib/argv.f

package TOKSTREAM
private

\ One record per line:
\   F <len> <path>   opens each file, so a deleted file cannot pass as an empty diff.
\   C <len> <bytes>  a code token, or a `.( ... )` comment, which executes.
\   L <len> <bytes>  a string literal's body; the literal's token is only its opener.
\   S <len> <bytes>  a `( ... )` comment whose body carries `--`: a stack effect.
\ Every other comment is dropped; the lexer drops a `\` line comment before any record.
: EMIT ( ptr u8 n n -- ) {: a:ptr u:n tag:n :}
   tag emit  $20 emit  u FMT:.U  $20 emit  a u type  cr ;

: SIG? ( n -- bool ) {: k:n :}
   k LINT-LEX:CONTENT s" --" LINT-CONTAINS? ;

\ `.( body )` prints its body at load time, so it is executed source and travels
\ in the code stream, not the comment stream.
: PRINTING? ( n -- bool ) {: k:n :}
   k LINT-LEX:TOKEN s" .(" LINT-PREFIX? ;

: PAYLOAD ( n -- ) {: k:n :}
   k LINT-LEX:CONTENT dup 0= if 2drop exit then
   [char] L EMIT ;

: CODE ( n -- ) {: k:n :}
   k LINT-LEX:TOKEN [char] C EMIT
   k PAYLOAD ;

: TOK ( n -- ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:COMMENT <> if k CODE exit then
   k PRINTING? if k CODE exit then
   k SIG? if k LINT-LEX:TOKEN [char] S EMIT then ;

: ?LEXED ( ptr u8 n -- ) {: a:ptr u:n :}
   LINT-LEX:ERROR? 0= if exit then
   s" tokstream: lexer refused " type a u type
   s"  kind=" type LINT-LEX:ERROR-KIND@ FMT:.INT
   s"  line=" type LINT-LEX:ERROR-LINE@ FMT:.INT cr
   s" tokstream: source did not lex" 1 die ;

: FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u [char] F EMIT
   a u LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   a u ?LEXED
   LINT-LEX:COUNT 0 ?do i TOK loop ;

public

: RUN ( -- )
   s" tokstream.f path ...   (check the exit code)" ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   0 begin dup ARGV:POS# < while
      dup ARGV:POS$ FILE
      1+
   repeat drop ;

;package

TOKSTREAM:RUN
