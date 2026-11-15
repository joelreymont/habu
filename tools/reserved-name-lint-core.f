\ reserved-name-lint-core.f - reject definition names reserved by parser/control dispatch.
\ Also rejects number-shaped names (E-NUMERIC-DEFINITION), as hb does, and a
\ dot-digit tail (U.0): one inserted space makes it a float literal.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/source.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f

package RESERVED-NAME-LINT
private

$2E constant RNL-DOT-C
$2D constant RNL-MINUS-C
$24 constant RNL-DOLLAR-C

create RNL-SDQ  $73 c, $22 c,
create RNL-CDQ  $63 c, $22 c,
create RNL-DOTQ $2E c, $22 c,
TYPED-VARIABLE RNL-SRC-A ptr u8
variable RNL-SRC-U
variable RNL-SRC-CAP
variable RNL-BAD
variable RNL-I
variable RNL-IN-DEF

: RNL-SRC-A@ ( -- ptr u8 )
   RNL-SRC-A @ ;

: RNL-SRC-A! ( ptr u8 -- )
   RNL-SRC-A ! ;

: RNL-WORD? ( n -- bool ) {: k :}
   k LINT-LEX:COUNT >= if LINT-FALSE exit then
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

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
   a u s" [if]" LINT-STR=CI if LINT-TRUE exit then
   a u s" [then]" LINT-STR=CI if LINT-TRUE exit then
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
   a u s" [package]" LINT-STR=CI if LINT-TRUE exit then
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
   a u s" seal-package" LINT-STR=CI if LINT-TRUE exit then
   a u s" export" LINT-STR=CI if LINT-TRUE exit then    \ EXPORT re-exports a word from a package
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

\ The verifier takes a target predicate's spelling as the engine's answer, for
\ a body `if` and a top-level `[if]` (src/habu/verify-source.f TARGET-GUARD).
: RNL-RESERVED-TARGET? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" hb-target-linux?" LINT-STR=CI if LINT-TRUE exit then
   a u s" hb-target-macos?" LINT-STR=CI if LINT-TRUE exit then
   a u s" hb-target-linux-x86-64?" LINT-STR=CI if LINT-TRUE exit then
   a u s" hb-target-linux-kernel?" LINT-STR=CI if LINT-TRUE exit then
   a u s" hb-target-wasm?" LINT-STR=CI ;

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

\ A finding is one rejection record at the name token k, kept so the scan goes
\ on; the file being scanned is armed.
: RNL-FINDING ( n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: k:n code:ptr cu:n why:ptr wu:n reason:ptr ru:n fix:ptr fu:n :}
   RNL-BAD @ 1+ RNL-BAD !
   code cu REJECTION:OPEN
   s" `" REJECTION:EMIT-STR  k LINT-LEX:TOKEN REJECTION:EMIT-STR
   why wu REJECTION:EMIT-STR
   k LINT-LEX:TOKEN REJECTION:AT  s" defined here" REJECTION:EMIT-STR
   s" reason" REJECTION:NOTE  reason ru REJECTION:EMIT-STR
   REJECTION:HELP  fix fu REJECTION:EMIT-STR
   REJECTION:KEEP ;

: RNL-REPORT ( n -- )
   s" E-RESERVED-DEFINITION"
   s" ` is reserved by parser/control dispatch"
   s" parser/control words cannot be source definition names"
   s" rename the definition after prefix stripping; use names like IX/JX for loop counters"
   RNL-FINDING ;

: RNL-REPORT-NUM ( n -- )
   s" E-NUMERIC-DEFINITION"
   s" ` is number-shaped"
   s" numeric literals parse before dictionary lookup; a number-shaped name is unreachable or float-confusable"
   s" rename so the name cannot parse as a number: avoid digit-only, dot-digit and $hex shapes"
   RNL-FINDING ;

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
   0 RNL-BAD ! ;

\ PATH is read; LABEL names it in every record.
: FILE-AS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu label:ptr labelu :}
   path pathu RNL-LOAD-SOURCE
   RNL-SRC-A@ RNL-SRC-U @ LINT-LEX:SOURCE
   label labelu REJECTION:FILE!
   RNL-SRC-A@ RNL-SRC-U @ 1 1 0 REJECTION:SOURCE!
   [: RNL-SCAN ;] catch
   REJECTION:SOURCE-OFF
   dup 0<> if throw then drop ;

: FILE ( ptr u8 n -- )
   2dup FILE-AS ;

: FINISH ( -- )
   RNL-BAD @ 0 > if CHECKER-REJECT-RC throw then ;

;package
