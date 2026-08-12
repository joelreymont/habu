\ judge/src.f - reading a canonical corpus source file as the definitions a code
\ generator comparison measures. One concern: turning ONE source text into the
\ per-definition facts both code generators need, so that neither column is
\ handed a hand-copied second opinion about what the corpus says.
\
\ WHAT PROBLEM THIS FILE EXISTS TO CLOSE. The comparison used to hold each
\ subject twice: once as a real definition in tools/codegen-compare-corpus4.f,
\ which the engine's emitter compiles when that file is loaded, and once as a
\ string literal in tools/codegen-compare-migrated4.f, retyped by hand with `-N`
\ on every name, which the native chain compiles. Two texts that are supposed to
\ be the same program is a claim nothing checked: a typo in the second is a
\ comparison between two different programs, reported as a code generator
\ result. This file makes the FIRST text the only one there is. The chain's word
\ is derived from the corpus file's own bytes, at run time, by a rename nobody
\ types.
\
\ WHAT IS DERIVED AND WHAT IS STATED. Everything about the program is derived:
\
\   the body        the exact bytes between the definition's `:` and its `;`,
\                   spliced, so whitespace, line comments and every literal
\                   survive as written. Only a name is ever substituted.
\   the arity       counted off the definition's own stack comment, by the type
\                   grammar signatures are written in - `ptr` takes the type
\                   after it and the two are ONE value, so `( ptr n n -- n )` is
\                   two inputs and one output, which is what a migration entry
\                   has to be told.
\   the callees     the definitions of this same file that the body names,
\                   distinct and in first-use order, which is the order they
\                   have to be published in.
\
\ Nothing above is a number a caller chose, so a caller cannot get one wrong.
\
\ WHY THE TEXT IS SPLICED RATHER THAN REBUILT FROM TOKENS. Concatenating the
\ token spellings back together with single spaces would also compile, and it
\ would quietly be a different text: a line comment inside a body would vanish,
\ and the definition the chain compiled would no longer be a span of any file. A
\ splice copies the source's own bytes and rewrites nothing but the names it was
\ asked to rewrite, so what the chain compiles IS the corpus file, with a
\ suffix.
\
\ THE LEXER IS NOT THIS FILE'S. tools/lint/source-lex.f is the one shared source
\ lexer the lint tools and the checker tooling read Habu with, and it is what
\ makes this reading STRUCTURAL rather than textual: a `\` line comment is
\ skipped entirely, a `( ... )` comment is one token whose interior is never
\ tokenised, and a string literal is ONE token whose payload is never tokenised
\ either. So `: FAKE` written inside a comment or inside a string is not a
\ definition here and cannot become one, and a `;` inside either does not close
\ one. tools/judge/src-test.f attacks exactly that, with fixtures built to fool
\ a reader that searched for text.
\
\ WHAT A DEFINITION IS. The `:` definer followed by a name, closed by the next
\ bare `;`. `;]`, `;package`, `;MATCH` and every other `;`-prefixed closer is a
\ different token and does not close a definition, which is what lets a body
\ hold a quotation. A `:` that never closes is a refusal, not a definition that
\ swallows the rest of the file. A name this file defines twice is a refusal
\ too: a caller asking for that name would be answered about one of two
\ programs, and which one is not a question a measurement should have.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/source-lex.f

package JUDGE-SRC

private

64 constant DEF-MAX               \ definitions one corpus file may publish
32 constant NAME-MAX              \ bytes of one definition name
16 constant CALL-MAX              \ distinct in-file callees one body may name
$2000 constant TEXT-MAX           \ bytes of one derived definition text
$20000 constant BYTES-MAX         \ bytes of one corpus source file

create BYTES BYTES-MAX allot
variable BYTES-U

DEF-MAX NAME-MAX * BUFFER: NAMES
create NAME-LENS DEF-MAX cells allot
create D-OPEN DEF-MAX cells allot          \ token index of the `:`
create D-CLOSE DEF-MAX cells allot         \ token index of the `;`
create D-IN DEF-MAX cells allot
create D-OUT DEF-MAX cells allot
create D-CALLS DEF-MAX cells allot         \ how many distinct callees
create D-CALLEE DEF-MAX CALL-MAX * cells allot  \ which definitions they are

create TEXT TEXT-MAX allot
variable TEXT-U
variable CURSOR                            \ where the splice stands in BYTES

variable DEF-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

: ROW-OK ( n -- n )
   dup 0 < over DEF-N @ >= or if E-JUDGE-SRC-ROW throw then ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAMES + ;

: CALL-SLOT ( n n -- ptr n ) {: k:n j:n :}
   D-CALLEE k CALL-MAX * j + cells + ;

\ ---- what the lexer's tokens mean here ---------------------------------------

: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

\ A bare `;` and nothing else closes a definition.
: CLOSER? ( n -- bool ) {: k:n :}
   k WORD? 0= if false exit then
   k LINT-LEX:TOKEN s" ;" STR= ;

: DEFINER? ( n -- bool ) {: k:n :}
   k WORD? 0= if false exit then
   k LINT-LEX:TOKEN s" :" STR= ;

: TOK-END ( n -- n ) {: k:n :}
   k LINT-LEX:TOKEN {: a:ptr u:n :}
   k LINT-LEX:BYTE@ u + ;

\ The token index of the `;` that closes the definition opened at k, or a
\ refusal. Searching forward from the body's first token is what makes an
\ unclosed `:` a refusal instead of a definition that swallows the rest of the
\ file.
: CLOSE-AT ( n -- n ) {: k:n :}
   k 2 +
   begin dup LINT-LEX:COUNT < while
      dup CLOSER? if exit then
      1+
   repeat
   drop E-JUDGE-SRC-DEF throw ;

\ ---- the arity, counted off the stack comment --------------------------------
\ A signature is a list of values written in the type grammar this repository
\ writes signatures in. Every token is one value EXCEPT `ptr`, which takes the
\ type after it: `ptr u8` is one value, `ptr ptr u8` is one value, and
\ `ptr u8 n` - a string - is two. A-NEED counts how many element types are still
\ owed to open pointers, so the whole grammar is read in one pass with one cell.

variable A-NEED
variable A-CNT
variable A-ARROW

: ARITY-TOK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" ptr" STR= {: pointer:bool :}
   A-NEED @ 0 > if A-NEED @ 1- A-NEED ! else A-CNT @ 1+ A-CNT ! then
   pointer if A-NEED @ 1+ A-NEED ! then ;

\ What a signature may hold that this reader cannot count. A quotation type is
\ written with brackets and is ONE value spread over many tokens; a reader that
\ counted its tokens would hand a migration entry a wrong arity and the compiler
\ would be blamed for the result.
: UNCOUNTABLE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [" STR= if true exit then
   a u s" ]" STR= ;

: ARITY-STEP ( ptr u8 n -- ) {: a:ptr u:n :}
   a u UNCOUNTABLE? if E-JUDGE-SRC-SIG throw then
   a u s" --" STR= 0= if a u ARITY-TOK exit then
   A-ARROW @ if E-JUDGE-SRC-SIG throw then
   true A-ARROW !
   A-CNT @ D-IN DEF-N @ SLOT !
   0 A-CNT ! 0 A-NEED ! ;

\ Walk the signature comment's body one space-delimited token at a time. The two
\ position tests are separate words so that neither reads a byte past the span
\ when the scan reaches its end: `and` evaluates both sides.
variable A-POS
variable A-END

: AT-SPACE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   A-POS @ u >= if false exit then
   A-POS @ a + c@ $20 = ;

: IN-TOKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   A-END @ u >= if false exit then
   A-END @ a + c@ $20 <> ;

: A-NEXT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   begin a u AT-SPACE? while A-POS @ 1+ A-POS ! repeat
   A-POS @ u >= if false exit then
   A-POS @ A-END !
   begin a u IN-TOKEN? while A-END @ 1+ A-END ! repeat
   true ;

: ARITY-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   0 A-POS !
   begin a u A-NEXT? while
      a A-POS @ + A-END @ A-POS @ - ARITY-STEP
      A-END @ A-POS !
   repeat ;

\ The signature of the definition opened at k, which is the token after its
\ name. A definition without one is refused: the arity is what a migration entry
\ is told, and guessing it from the body would be this file inventing the very
\ fact it exists to read.
: ARITY ( n -- ) {: k:n :}
   0 A-NEED ! 0 A-CNT ! false A-ARROW !
   k 2 + LINT-LEX:COUNT >= if E-JUDGE-SRC-SIG throw then
   k 2 + LINT-LEX:KIND@ LINT-LEX:COMMENT <> if E-JUDGE-SRC-SIG throw then
   k 2 + LINT-LEX:CONTENT ARITY-SCAN
   A-ARROW @ 0= if E-JUDGE-SRC-SIG throw then
   A-NEED @ 0<> if E-JUDGE-SRC-SIG throw then
   A-CNT @ D-OUT DEF-N @ SLOT ! ;

public

\ How many definitions the last scanned source publishes.
: DEFS ( -- n )
   DEF-N @ ;

: NAME$ ( n -- ptr u8 n ) {: k:n :}
   k ROW-OK NAME-AT
   NAME-LENS k SLOT @ ;

\ How many values definition k declares it takes and leaves, read off its own
\ stack comment.
: IN ( n -- n ) {: k:n :}
   D-IN k ROW-OK SLOT @ ;

: OUT ( n -- n ) {: k:n :}
   D-OUT k ROW-OK SLOT @ ;

\ Which definition of this file goes by a name, or -1. Names are matched whole
\ and case-sensitively.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   DEF-N @ 0 ?do
      i NAME$ a u STR= if drop i then
   loop ;

private

: KEEP-NAME ( n -- ) {: k:n :}
   k 1+ LINT-LEX:COUNT >= if E-JUDGE-SRC-DEF throw then
   k 1+ WORD? 0= if E-JUDGE-SRC-DEF throw then
   k 1+ LINT-LEX:TOKEN {: a:ptr u:n :}
   u 0= if E-JUDGE-SRC-DEF throw then
   u NAME-MAX > if E-JUDGE-SRC-CAP throw then
   a u FIND 0 >= if E-JUDGE-SRC-DUP throw then
   a  DEF-N @ NAME-AT  u STR-LEN BYTE-COPY-LEN
   u NAME-LENS DEF-N @ SLOT ! ;

: KEEP-DEF ( n -- ) {: k:n :}
   DEF-N @ DEF-MAX >= if E-JUDGE-SRC-CAP throw then
   k KEEP-NAME
   k D-OPEN DEF-N @ SLOT !
   k CLOSE-AT D-CLOSE DEF-N @ SLOT !
   k ARITY
   0 D-CALLS DEF-N @ SLOT !
   DEF-N @ 1+ DEF-N ! ;

\ ---- the callees a body names ------------------------------------------------
\ Distinct and in first-use order, because that is the order they must be
\ published in: a call site is handed the address its callee's record already
\ carries, so a callee named before it exists has no address to be handed.

: CALLED-ALREADY? ( n n -- bool ) {: k:n c:n :}
   false
   D-CALLS k SLOT @ 0 ?do
      k i CALL-SLOT @ c = if drop true then
   loop ;

: KEEP-CALL ( n n -- ) {: k:n c:n :}
   k c CALLED-ALREADY? if exit then
   D-CALLS k SLOT @ CALL-MAX >= if E-JUDGE-SRC-CAP throw then
   c  k D-CALLS k SLOT @ CALL-SLOT  !
   D-CALLS k SLOT @ 1+ D-CALLS k SLOT ! ;

: SCAN-CALLS ( n -- ) {: k:n :}
   D-OPEN k SLOT @ 2 +
   D-CLOSE k SLOT @ {: last:n :}
   begin dup last < while
      dup WORD? if
         dup LINT-LEX:TOKEN FIND {: c:n :}
         c 0 >= if k c KEEP-CALL then
      then
      1+
   repeat drop ;

public

\ How many distinct definitions of this file body k names, and which they are.
: CALLS ( n -- n ) {: k:n :}
   D-CALLS k ROW-OK SLOT @ ;

: CALL@ ( n n -- n ) {: k:n j:n :}
   j 0 < j k CALLS >= or if E-JUDGE-SRC-ROW throw then
   k j CALL-SLOT @ ;

private

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   TEXT-U @ u + TEXT-MAX > if E-JUDGE-SRC-CAP throw then
   a  TEXT TEXT-U @ +  u STR-LEN BYTE-COPY-LEN
   TEXT-U @ u + TEXT-U ! ;

\ The source bytes between where the splice stands and where a token starts:
\ the whitespace, the line comments and everything else the file wrote there.
: APPEND-GAP ( n -- ) {: stop:n :}
   stop CURSOR @ <= if exit then
   BYTES CURSOR @ + stop CURSOR @ - APPEND ;

: SPLICE-TOK ( n ptr u8 n -- ) {: k:n sa:ptr su:n :}
   k LINT-LEX:BYTE@ APPEND-GAP
   k LINT-LEX:TOKEN APPEND
   k WORD? if
      k LINT-LEX:TOKEN FIND 0 >= if sa su APPEND then
   then
   k TOK-END CURSOR ! ;

public

\ Definition k's own bytes, with every name this file defines given the caller's
\ suffix, ready to be handed to a compiler. The definition's own name is
\ suffixed too, so the derived word can be published beside the one the engine
\ compiled instead of replacing it.
: TEXT$ ( n ptr u8 n -- ptr u8 n ) {: k:n sa:ptr su:n :}
   0 TEXT-U !
   k ROW-OK drop
   D-OPEN k SLOT @ dup LINT-LEX:BYTE@ CURSOR !
   D-CLOSE k SLOT @ {: last:n :}
   begin dup last <= while
      dup sa su SPLICE-TOK
      1+
   repeat drop
   TEXT TEXT-U @ ;

private

\ Read the definitions out of whatever is in BYTES. The two public entries differ
\ only in how the bytes got there.
: SCAN-BYTES ( -- )
   0 DEF-N !
   BYTES BYTES-U @ LINT-LEX:SOURCE
   LINT-LEX:ERROR? if E-JUDGE-SRC-LEX throw then
   0 begin dup LINT-LEX:COUNT < while
      dup DEFINER? if dup KEEP-DEF then
      1+
   repeat drop
   DEF-N @ 0 ?do i SCAN-CALLS loop ;

public

\ Read a canonical corpus source TEXT and record every definition it publishes.
\ Clears everything the last scan left, so a caller holds one source at a time
\ and can never read half of one against half of another.
: SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   u BYTES-MAX > if E-JUDGE-SRC-CAP throw then
   a BYTES u STR-LEN BYTE-COPY-LEN
   u BYTES-U !
   SCAN-BYTES ;

\ The same for a FILE. The read is the lint tools' own whole-file read, so a
\ file this store cannot hold is named rather than silently truncated.
: LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BYTES BYTES-MAX READ-FILE {: ra:ptr ru:n :}
   ru BYTES-U !
   SCAN-BYTES ;

;package
