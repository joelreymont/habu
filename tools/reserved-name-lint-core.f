\ reserved-name-lint-core.f - reject definition names reserved by parser/control dispatch.
\ Also rejects number-shaped names (E-NUMERIC-DEFINITION): hb parses literals
\ before dictionary lookup (test/gate-dictionary-lib.f LITERAL-FIRST), so a name
\ like 42, .0, 1.5, -.5 or $FF is unreachable. A dot-digit tail (U.0) is rejected
\ too: one inserted space makes it a float literal.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fd.f
require src/lib/source.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/byte-buffer.f
require src/lib/json-write.f
require src/lib/source-lex.f

package RESERVED-NAME-LINT
private

$20 constant RNL-NUM-CAP
$0A constant RNL-LF
$20 constant RNL-SP
$3A constant RNL-COLON-C
$2E constant RNL-DOT-C
$2D constant RNL-MINUS-C
$24 constant RNL-DOLLAR-C

create RNL-NUM RNL-NUM-CAP allot
create RNL-LF-BUF 1 allot
create RNL-SDQ  $73 c, $22 c,
create RNL-CDQ  $63 c, $22 c,
create RNL-DOTQ $2E c, $22 c,
create RNL-JSON-BUF BUF:HDR-BYTES allot
TYPED-VARIABLE RNL-JSON-W JSON-WRITE:writer
variable RNL-JSON-READY

: RNL-JSON-WRITER ( -- ptr JSON-WRITE:writer )
   RNL-JSON-READY @ 0= if
      RNL-JSON-BUF 1 BUF:N>BLEN BUF:INIT
      1 RNL-JSON-READY !
   then
   RNL-JSON-W RNL-JSON-BUF JSON-WRITE:OPEN-BUF ;

TYPED-VARIABLE RNL-SRC-A ptr u8
variable RNL-SRC-U
variable RNL-SRC-CAP
TYPED-VARIABLE RNL-FILE-A ptr u8
variable RNL-FILE-U
variable RNL-BAD
variable RNL-I
variable RNL-IN-DEF
variable RNL-JSON
variable RNL-OUT-FD
variable RNL-NUM-I

: RNL-SRC-A@ ( -- ptr u8 )
   RNL-SRC-A @ ;

: RNL-FILE-A@ ( -- ptr u8 )
   RNL-FILE-A @ ;

: RNL-SRC-A! ( ptr u8 -- )
   RNL-SRC-A ! ;

: RNL-FILE-A! ( ptr u8 -- )
   RNL-FILE-A ! ;

public

: JSON! ( bool -- )
   RNL-JSON ! ;

: OUT-FD! ( fd -- )
   RNL-OUT-FD ! ;

private

: RNL-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   fd a u LINT-OUT-WRITE ;

: RNL-OUT ( ptr u8 n -- )
   RNL-OUT-FD @ -rot RNL-WRITE ;

: RNL-C ( n -- )
   RNL-LF-BUF c!
   RNL-LF-BUF 1 RNL-OUT ;

: RNL-NL ( -- )
   RNL-LF RNL-C ;

: RNL-U$ ( n -- ptr u8 n ) {: u:n :}
   RNL-NUM-CAP RNL-NUM-I !
   u 0= if
      RNL-NUM-I @ 1- RNL-NUM-I !
      $30 RNL-NUM RNL-NUM-I @ + c!
      RNL-NUM RNL-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod $30 +
      RNL-NUM-I @ 1- RNL-NUM-I !
      RNL-NUM RNL-NUM-I @ + c!
      10 /
   repeat drop
   RNL-NUM RNL-NUM-I @ + RNL-NUM-CAP RNL-NUM-I @ - ;

: RNL-WORD? ( n -- bool ) {: k :}
   k LINT-LEX:COUNT >= if LINT-FALSE exit then
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: RNL-TOK-END ( n -- n ) {: k :}
   k LINT-LEX:BYTE@ k LINT-LEX:TOKEN nip + ;

: RNL-COLON-DEFINER? ( n -- bool ) {: k :}
   k LINT-LEX:TOKEN s" :" LINT-STR= if LINT-TRUE exit then
   k LINT-LEX:TOKEN s" +:" LINT-STR= if LINT-TRUE exit then
   k LINT-LEX:TOKEN s" TRUSTED:" LINT-STR=CI if LINT-TRUE exit then
   k LINT-LEX:TOKEN s" KERNEL:" LINT-STR=CI ;

: RNL-DATA-DEFINER? ( n -- bool ) {: k :}
   k LINT-LEX:TOKEN s" create" LINT-STR=CI if LINT-TRUE exit then
   k LINT-LEX:TOKEN s" variable" LINT-STR=CI if LINT-TRUE exit then
   k LINT-LEX:TOKEN s" constant" LINT-STR=CI if LINT-TRUE exit then
   k LINT-LEX:TOKEN s" defer" LINT-STR=CI if LINT-TRUE exit then
   k LINT-LEX:TOKEN s" LAYOUT-BUFFER" LINT-STR=CI ;

: RNL-RESERVED-CONTROL? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" if" LINT-STR=CI if LINT-TRUE exit then
   a u s" then" LINT-STR=CI if LINT-TRUE exit then
   a u s" else" LINT-STR=CI if LINT-TRUE exit then
   a u s" begin" LINT-STR=CI if LINT-TRUE exit then
   a u s" until" LINT-STR=CI if LINT-TRUE exit then
   a u s" again" LINT-STR=CI if LINT-TRUE exit then
   a u s" while" LINT-STR=CI if LINT-TRUE exit then
   a u s" repeat" LINT-STR=CI if LINT-TRUE exit then
   a u s" case" LINT-STR=CI if LINT-TRUE exit then
   a u s" of" LINT-STR=CI if LINT-TRUE exit then
   a u s" endof" LINT-STR=CI if LINT-TRUE exit then
   a u s" endcase" LINT-STR=CI if LINT-TRUE exit then
   a u s" match" LINT-STR=CI if LINT-TRUE exit then      \ MATCH eliminator control form
   a u s" ;match" LINT-STR=CI if LINT-TRUE exit then     \ MATCH block close (;FOO convention)
   a u s" construct" LINT-STR=CI if LINT-TRUE exit then  \ private constructor token form
   a u s" do" LINT-STR=CI if LINT-TRUE exit then
   a u s" ?do" LINT-STR=CI if LINT-TRUE exit then
   a u s" loop" LINT-STR=CI if LINT-TRUE exit then
   a u s" +loop" LINT-STR=CI if LINT-TRUE exit then
   a u s" i" LINT-STR=CI if LINT-TRUE exit then
   a u s" j" LINT-STR=CI if LINT-TRUE exit then
   a u s" leave" LINT-STR=CI if LINT-TRUE exit then
   a u s" unloop" LINT-STR=CI if LINT-TRUE exit then
   a u s" exit" LINT-STR=CI if LINT-TRUE exit then
   a u s" recurse" LINT-STR=CI ;

: RNL-RESERVED-PARSER? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u RNL-SDQ 2 LINT-STR=CI if LINT-TRUE exit then
   a u RNL-CDQ 2 LINT-STR=CI if LINT-TRUE exit then
   a u RNL-DOTQ 2 LINT-STR= if LINT-TRUE exit then
   a u s" type" LINT-STR=CI if LINT-TRUE exit then
   a u s" '" LINT-STR= if LINT-TRUE exit then
   a u s" [']" LINT-STR= if LINT-TRUE exit then
   a u s" {:" LINT-STR= if LINT-TRUE exit then
   a u s" :}" LINT-STR= if LINT-TRUE exit then
   a u s" [:" LINT-STR= if LINT-TRUE exit then
   a u s" ;]" LINT-STR= if LINT-TRUE exit then
   a u s" char" LINT-STR=CI if LINT-TRUE exit then
   a u s" [char]" LINT-STR=CI if LINT-TRUE exit then
   a u s" immediate" LINT-STR=CI if LINT-TRUE exit then
   a u s" does>" LINT-STR=CI if LINT-TRUE exit then
   a u s" trusted:" LINT-STR=CI if LINT-TRUE exit then
   a u s" trust" LINT-STR=CI if LINT-TRUE exit then
   a u s" kernel:" LINT-STR=CI ;

: RNL-RESERVED-DEFINER? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" create" LINT-STR=CI if LINT-TRUE exit then
   a u s" variable" LINT-STR=CI if LINT-TRUE exit then
   a u s" constant" LINT-STR=CI if LINT-TRUE exit then
   a u s" layout-buffer" LINT-STR=CI if LINT-TRUE exit then
   a u s" package" LINT-STR=CI if LINT-TRUE exit then
   a u s" public" LINT-STR=CI if LINT-TRUE exit then
   a u s" private" LINT-STR=CI if LINT-TRUE exit then
   a u s" ;package" LINT-STR=CI if LINT-TRUE exit then
   a u s" export" LINT-STR=CI if LINT-TRUE exit then    \ EXPORT re-export keyword + hb-build directive
   a u s" newtype" LINT-STR=CI if LINT-TRUE exit then
   a u s" sumtype" LINT-STR=CI if LINT-TRUE exit then
   a u s" variant" LINT-STR=CI if LINT-TRUE exit then
   a u s" ;variant" LINT-STR=CI if LINT-TRUE exit then
   a u s" ;sumtype" LINT-STR=CI if LINT-TRUE exit then
   a u s" enum" LINT-STR=CI if LINT-TRUE exit then       \ enum block definer
   a u s" ;enum" LINT-STR=CI if LINT-TRUE exit then      \ enum block close (;FOO)
   a u s" product" LINT-STR=CI if LINT-TRUE exit then    \ product block definer
   a u s" field" LINT-STR=CI if LINT-TRUE exit then      \ product field keyword
   a u s" ;product" LINT-STR=CI if LINT-TRUE exit then   \ product block close (;FOO)
   a u s" undefine" LINT-STR=CI ;

: RNL-RESERVED-LOADER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" include" LINT-STR=CI if LINT-TRUE exit then
   a u s" included" LINT-STR=CI if LINT-TRUE exit then
   a u s" require" LINT-STR=CI if LINT-TRUE exit then
   a u s" required" LINT-STR=CI if LINT-TRUE exit then
   a u s" provided" LINT-STR=CI ;

\ tools/check.f reads a target predicate's spelling as the engine's answer to
\ skip the arms of an `if` the engine never runs (src/habu/verify-source.f
\ TARGET-GUARD).
: RNL-RESERVED-TARGET? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" hb-target-linux?" LINT-STR=CI if LINT-TRUE exit then
   a u s" hb-target-macos?" LINT-STR=CI if LINT-TRUE exit then
   a u s" hb-target-linux-x86-64?" LINT-STR=CI ;

: RNL-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u RNL-RESERVED-CONTROL? if LINT-TRUE exit then
   a u RNL-RESERVED-PARSER? if LINT-TRUE exit then
   a u RNL-RESERVED-DEFINER? if LINT-TRUE exit then
   a u RNL-RESERVED-LOADER? if LINT-TRUE exit then
   a u RNL-RESERVED-TARGET? ;

\ ---- number-shaped names (numeric parse wins over the dictionary) ---------
: RNL-DIGIT? ( n -- bool )
   dup $2F > swap $3A < and ;

: RNL-HEX-LETTER? ( n -- bool ) {: c:n :}
   c $40 > c $47 < and if LINT-TRUE exit then
   c $60 > c $67 < and ;

: RNL-HEX-DIGIT? ( n -- bool ) {: c:n :}
   c RNL-DIGIT? if LINT-TRUE exit then
   c RNL-HEX-LETTER? ;

: RNL-DIGITS? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ nonempty, all decimal digits
   u 0= if LINT-FALSE exit then
   u 0 ?do
      a i + c@ RNL-DIGIT? 0= if LINT-FALSE unloop exit then
   loop LINT-TRUE ;

: RNL-DIGITS-OPT? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ empty or all digits
   u 0= if LINT-TRUE exit then
   a u RNL-DIGITS? ;

: RNL-HEX-DIGITS? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ nonempty, all hex digits
   u 0= if LINT-FALSE exit then
   u 0 ?do
      a i + c@ RNL-HEX-DIGIT? 0= if LINT-FALSE unloop exit then
   loop LINT-TRUE ;

: RNL-FLOAT-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n dot:n :}   \ digits* '.'@dot digits+
   a dot RNL-DIGITS-OPT?
   a dot 1+ + u dot 1+ - RNL-DIGITS?
   and ;

: RNL-FLOAT-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RNL-DOT-C LINT-INDEX-OF MATCH option
     none OF LINT-FALSE ENDOF
     some OF a u rot RNL-FLOAT-AT? ENDOF
   ;MATCH ;

: RNL-HEX-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 < if LINT-FALSE exit then
   a c@ RNL-DOLLAR-C <> if LINT-FALSE exit then
   a 1+ u 1- RNL-HEX-DIGITS? ;

: RNL-NUM-BODY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RNL-DIGITS? if LINT-TRUE exit then
   a u RNL-FLOAT-NAME? if LINT-TRUE exit then
   a u RNL-HEX-NAME? ;

: RNL-NUM-CLAIMED? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ number parser owns the token
   a u RNL-NUM-BODY? if LINT-TRUE exit then
   u 2 < if LINT-FALSE exit then
   a c@ RNL-MINUS-C <> if LINT-FALSE exit then
   a 1+ u 1- RNL-NUM-BODY? ;

: RNL-DOT-DIGIT? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ contains a ".<digit>" tail
   u 1- 0 ?do
      a i + c@ RNL-DOT-C =
      a i + 1+ c@ RNL-DIGIT? and if LINT-TRUE unloop exit then
   loop LINT-FALSE ;

: RNL-NUMERIC-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RNL-NUM-CLAIMED? if LINT-TRUE exit then
   a u RNL-DOT-DIGIT? ;

: RNL-JSON-FINDING-AS ( n ptr u8 n ptr u8 n ptr u8 n -- )
   {: k:n code:ptr codeu:n why:ptr whyu:n fix:ptr fixu:n :}
   RNL-JSON-WRITER JSON-WRITE:OBJECT-START
   s" schema_version" 1 JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" code" code codeu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" file" RNL-FILE-A@ RNL-FILE-U @ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" line" k LINT-LEX:LINE@ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" column" k LINT-LEX:COL@ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" byte_start" k LINT-LEX:BYTE@ JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" byte_end" k RNL-TOK-END JSON-WRITE:FIELD-U JSON-WRITE:COMMA
   s" word" k LINT-LEX:TOKEN JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" reason" why whyu JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" suggestion" fix fixu JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END JSON-WRITE:$ RNL-OUT RNL-NL ;

: RNL-JSON-FINDING ( n -- )
   s" E-RESERVED-DEFINITION"
   s" parser/control words cannot be source definition names"
   s" Rename the definition after prefix stripping; use names like IX/JX for loop counters."
   RNL-JSON-FINDING-AS ;

: RNL-JSON-NUM-FINDING ( n -- )
   s" E-NUMERIC-DEFINITION"
   s" numeric literals parse before dictionary lookup; a number-shaped name is unreachable or float-confusable"
   s" Rename so the name cannot parse as a number: avoid digit-only, dot-digit, and $hex shapes."
   RNL-JSON-FINDING-AS ;

: RNL-TEXT-FINDING-AS ( n ptr u8 n ptr u8 n -- )
   {: k:n code:ptr codeu:n why:ptr whyu:n :}
   code codeu RNL-OUT
   RNL-SP RNL-C
   RNL-FILE-A@ RNL-FILE-U @ RNL-OUT
   RNL-COLON-C RNL-C k LINT-LEX:LINE@ RNL-U$ RNL-OUT
   RNL-COLON-C RNL-C k LINT-LEX:COL@ RNL-U$ RNL-OUT
   s" : `" RNL-OUT
   k LINT-LEX:TOKEN RNL-OUT
   why whyu RNL-OUT
   RNL-NL ;

: RNL-TEXT-FINDING ( n -- )
   s" E-RESERVED-DEFINITION"
   s" ` is reserved by parser/control dispatch; rename after prefix stripping"
   RNL-TEXT-FINDING-AS ;

: RNL-TEXT-NUM-FINDING ( n -- )
   s" E-NUMERIC-DEFINITION"
   s" ` is number-shaped; numeric literals parse before dictionary lookup, rename it"
   RNL-TEXT-FINDING-AS ;

: RNL-REPORT ( n -- ) {: k :}
   RNL-BAD @ 1+ RNL-BAD !
   RNL-JSON @ if k RNL-JSON-FINDING exit then
   k RNL-TEXT-FINDING ;

: RNL-REPORT-NUM ( n -- ) {: k:n :}
   RNL-BAD @ 1+ RNL-BAD !
   RNL-JSON @ if k RNL-JSON-NUM-FINDING exit then
   k RNL-TEXT-NUM-FINDING ;

: RNL-CHECK-NAME ( n -- ) {: k:n :}
   k RNL-WORD? 0= if exit then
   k LINT-LEX:TOKEN RNL-RESERVED? if k RNL-REPORT exit then
   k LINT-LEX:TOKEN RNL-NUMERIC-NAME? if k RNL-REPORT-NUM then ;

: RNL-HANDLE-IN-DEF ( -- )
   RNL-I @ LINT-LEX:TOKEN s" ;" LINT-STR= if 0 RNL-IN-DEF ! then ;

\ A definer reads its name with parse-name, so the lexer reads the token after
\ one again by that rule and marks it an operand: `: \` defines `\`, a reserved
\ word on the next line is not its name, and the scan skips the name.
: RNL-NAME ( -- )
   RNL-I @ LINT-LEX:OPERAND
   RNL-I @ 1+ RNL-CHECK-NAME ;

: RNL-HANDLE-TOP ( -- )
   RNL-I @ RNL-COLON-DEFINER? if
      RNL-NAME
      -1 RNL-IN-DEF !
      exit
   then
   RNL-I @ RNL-DATA-DEFINER? if RNL-NAME then ;

\ An operand is data in either state: `char : constant COLON` defines COLON,
\ not a word named `constant`, `[char] ;` ends no definition, and after
\ `create char` the next `:` starts one.
: RNL-SCAN-TOKEN ( -- )
   RNL-I @ RNL-WORD? 0= if exit then
   RNL-I @ LINT-LEX:OPERAND? if exit then
   RNL-IN-DEF @ if RNL-HANDLE-IN-DEF else RNL-HANDLE-TOP then ;

: RNL-SCAN ( -- )
   0 RNL-I !
   0 RNL-IN-DEF !
   begin RNL-I @ LINT-LEX:COUNT < while
      RNL-SCAN-TOKEN
      RNL-I @ 1+ RNL-I !
   repeat ;

\ Room for n bytes of the source being read, in a region of the read's own: a
\ larger region takes the bytes the last one holds, and the last is released
\ when this read made it. A region is made only for more than the read holds,
\ so it is never empty.
: RNL-SRC-ROOM ( n -- ptr u8 ) {: need:n :}
   need RNL-SRC-CAP @ > if
      RNL-SRC-A@ RNL-SRC-CAP @ {: old:ptr oldcap:n :}
      need MEM-ALLOC-64K-SPAN RNL-SRC-CAP ! {: fresh:ptr :}
      old fresh oldcap BYTE-COPY
      oldcap 0 > if old oldcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES then
      fresh RNL-SRC-A!
   then
   RNL-SRC-A@ ;

\ Each read takes a region of its own, so the bytes an earlier read left stay
\ where they are. The file is read to its end however it grows while it is
\ read: its size, whose refusal (E-FS-STAT) stays a missing or irregular
\ file's, is only the first room (src/lib/source.f READ-WHOLE-SAMPLED).
: RNL-LOAD-SOURCE ( ptr u8 n -- ) {: path:ptr pathu :}
   0 RNL-SRC-CAP !
   path pathu  path pathu FS:FILE-SIZE  [: RNL-SRC-ROOM ;] SOURCE:READ-WHOLE-SAMPLED RNL-SRC-U ! ;

public

: RESET ( -- )
   0 RNL-BAD !
   0 RNL-JSON !
   FD:STDOUT OUT-FD! ;

: FILE-AS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu label:ptr labelu :}
   label RNL-FILE-A! labelu RNL-FILE-U !
   path pathu RNL-LOAD-SOURCE
   RNL-SRC-A@ RNL-SRC-U @ LINT-LEX:SOURCE
   RNL-SCAN ;

: FILE ( ptr u8 n -- )
   2dup FILE-AS ;

: FINISH ( -- )
   RNL-BAD @ 0 > if 1 throw then ;

;package
