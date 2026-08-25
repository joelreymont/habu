\ tokstream.f - dump a Habu source file's code-token stream and its stack-effect
\ signatures, through the tree's own lexer (package LINT-LEX).
\
\ Proof instrument for a comment-strip sweep: a comment-only edit leaves the code
\ stream byte-identical, so `tokstream before > a; tokstream after > b; cmp a b`
\ is the whole proof. Any edit that touches code, a signature, a string payload,
\ or a `.( )` printing comment changes the stream.
\
\   bin/hb --load tools/tokstream.f -- path ...
\
\ The `--` is required: without it the paths are read as more --load sources and
\ the tool sees no positional argument.
\
\ One record per line:
\   F <len> <path>   each file opens the stream, so deleting a whole file cannot
\                    pass as an empty diff.
\   C <len> <bytes>  a code token: LINT-LEX WORD or REGISTRY, plus a `.( ... )`
\                    printing comment, which EXECUTES and is therefore code.
\   L <len> <bytes>  the payload a code token carries in CONTENT - the body of a
\                    string literal. Emitted separately because the literal's own
\                    token is just its opener, so without this row an edit inside
\                    a quoted string would not show.
\   S <len> <bytes>  a `( ... )` comment whose body carries `--`: a stack effect.
\ Every other comment is dropped, which is exactly the material the sweep is
\ allowed to remove. A `\` line comment is dropped by the lexer itself and never
\ reaches a record, so `--` or `s" ..."` written inside one is not code here.
\
\ AT MOST 64 PATHS PER INVOCATION, the ARGV-MAX of lib/argv.f. A larger sweep is
\ several invocations over the same ordered list, concatenated; the F records keep
\ the result honest. THE CALLER MUST CHECK THE EXIT CODE. Over the cap the tool
\ refuses with `too many positional arguments` on stderr and exit 64, writing an
\ EMPTY stream - and two empty streams compare equal, so a sweep that ignores the
\ status reads a refusal as "comment-only".

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fmt.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require lib/argv.f

package TOKSTREAM
private

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
   s"  kind=" type LINT-LEX:ERROR-KIND@ .
   s" line=" type LINT-LEX:ERROR-LINE@ . cr
   s" tokstream: source did not lex" 1 die ;

: FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u [char] F EMIT
   a u LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   a u ?LEXED
   LINT-LEX:COUNT 0 ?do i TOK loop ;

public

: RUN ( -- )
   s" tokstream.f path ...   (at most 64 paths; check the exit code)" ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   0 begin dup ARGV:POS# < while
      dup ARGV:POS$ FILE
      1+
   repeat drop ;

;package

TOKSTREAM:RUN
