\ diff.f - checked streaming parser for `jj diff --git` unified diffs.
\ LINE emits borrowed file/content spans plus hunk new-line metadata.  The
\ parser preserves raw path bytes, resolves ambiguous heads from ordered body
\ metadata, and emits content only while exact old/new hunk counts remain.
\ Metadata-only sections finish only after their complete form; indexed text
\ sections require a changed hunk and indexed binary sections are terminal.
\ Owns the tool-local unified-diff error block -7400..-7499.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/adt/option.f

-7400 constant E-DIFF-SYNTAX

package DIFF
public

ENUM event
   none
   file
   hunk
   add
   context
   delete
;ENUM

private

ENUM state
   idle
   header
   new-header
   file-body
   hunk-body
   after-hunk
   terminal
;ENUM

ENUM meta
   normal
   mode-old
   rename-from
   copy-from
   new-file
   delete-file
;ENUM

$20 constant SPACE-C
$25 constant PERCENT-C
$2B constant PLUS-C
$2C constant COMMA-C
$2D constant MINUS-C
$30 constant ZERO-C
$37 constant SEVEN-C
$39 constant NINE-C
$40 constant AT-C
$61 constant A-C
$66 constant F-C
6 constant MODE-U

1 LAYOUT-BUFFER ST-V state
1 LAYOUT-BUFFER META-V meta
variable HEAD-A
variable HEAD-U
variable HEAD-OLD-A
variable HEAD-OLD-U
variable HEAD-NEW-A
variable HEAD-NEW-U
variable OLD-LEFT
variable NEW-LEFT
variable MARK-OK
variable HUNK-CHANGE
variable COMPLETE
variable INDEXED
variable REPLACED
variable MODED
variable OLD-MODE
variable HEAD-RESOLVED
variable SIMILAR
variable SIM-PENDING
variable SIM-REPLACE
variable SIM-BODY

: ST-AT ( -- ptr state )
   0 ST-V ;

: ST@ ( -- state )
   ST-AT @ ;

: ST! ( state -- )
   ST-AT ! ;

: META-AT ( -- ptr meta )
   0 META-V ;

: META@ ( -- meta )
   META-AT @ ;

: META! ( meta -- )
   META-AT ! ;

: HEAD-A-FIELD ( -- ptr ptr u8 )
   HEAD-A 0 ptr-field ;

: HEAD-OLD-A-FIELD ( -- ptr ptr u8 )
   HEAD-OLD-A 0 ptr-field ;

: HEAD-NEW-A-FIELD ( -- ptr ptr u8 )
   HEAD-NEW-A 0 ptr-field ;

: HEAD-A@ ( -- ptr u8 )
   HEAD-A-FIELD @ ;

: HEAD-OLD-A@ ( -- ptr u8 )
   HEAD-OLD-A-FIELD @ ;

: HEAD-NEW-A@ ( -- ptr u8 )
   HEAD-NEW-A-FIELD @ ;

: HEAD-OLD-A! ( ptr u8 -- )
   HEAD-OLD-A-FIELD ! ;

: HEAD-NEW-A! ( ptr u8 -- )
   HEAD-NEW-A-FIELD ! ;

: HEAD-A! ( ptr u8 -- )
   HEAD-A-FIELD ! ;

: HEAD$ ( -- ptr u8 n )
   HEAD-A@ HEAD-U @ ;

: HEAD-OLD$ ( -- ptr u8 n )
   HEAD-OLD-A@ HEAD-OLD-U @ ;

: HEAD-NEW$ ( -- ptr u8 n )
   HEAD-NEW-A@ HEAD-NEW-U @ ;

: META-NORMAL! ( -- )
   construct meta normal META! ;

: COMPLETE! ( -- )
   true COMPLETE ! ;

: INCOMPLETE! ( -- )
   false COMPLETE ! ;

: COMPLETE? ( -- bool )
   COMPLETE @ ;

: EMPTY ( -- ptr u8 n )
   s" " ;

: NONE-EVENT ( -- ptr u8 n n event )
   EMPTY 0 DIFF-EVENT:NONE ;

: DIFF-HEAD? ( ptr u8 n -- bool )
   s" diff --git " STARTS-WITH? ;

: OLD-HEAD? ( ptr u8 n -- bool )
   s" --- " STARTS-WITH? ;

: NEW-HEAD? ( ptr u8 n -- bool )
   s" +++ " STARTS-WITH? ;

: HUNK-HEAD? ( ptr u8 n -- bool )
   s" @@ " STARTS-WITH? ;

: MARKER? ( ptr u8 n -- bool )
   s" \ No newline at end of file" STR= ;

: OLD-PATH-EVENT ( -- ptr u8 n n event )
   HEAD-OLD$ 0 DIFF-EVENT:FILE ;

: NEW-PATH-EVENT ( -- ptr u8 n n event )
   HEAD-NEW$ 0 DIFF-EVENT:FILE ;

: HEAD-SAME? ( -- bool )
   HEAD-OLD$ HEAD-NEW$ STR= ;

: IDENTITY-READY? ( -- bool )
   HEAD-SAME? if true exit then
   REPLACED @ ;

: COMPLETE-IDENTITY! ( -- )
   IDENTITY-READY? if COMPLETE! else INCOMPLETE! then ;

: COMPLETE-METADATA! ( -- )
   SIM-PENDING @ if INCOMPLETE! else COMPLETE-IDENTITY! then ;

: COMPLETE-REPLACEMENT! ( -- )
   false SIM-REPLACE !
   SIM-BODY @ if
      INCOMPLETE!
   else
      false SIM-PENDING !
      COMPLETE!
   then ;

: PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > ;

: AFTER-PATH ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: a:ptr u:n prefix:ptr prefixu:n :}
   a u prefix prefixu STARTS-WITH? 0= if E-DIFF-SYNTAX throw then
   a prefixu + u prefixu - dup 0= if E-DIFF-SYNTAX throw then ;

: UNIQUE-SPLIT ( ptr u8 n ptr u8 n -- n bool )
   {: a:ptr u:n sep:ptr sepu:n :}
   a u sep sepu FIND-SUB MATCH option
      none OF E-DIFF-SYNTAX throw ENDOF
      some OF IDX>N {: split:n :}
         a split 1+ + u split 1+ - sep sepu FIND-SUB MATCH option
            none OF split true ENDOF
            some OF drop split false ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: HEAD-PATHS! ( ptr u8 n ptr u8 n -- )
   {: oa:ptr ou:n na:ptr nu:n :}
   oa HEAD-OLD-A! ou HEAD-OLD-U !
   na HEAD-NEW-A! nu HEAD-NEW-U ! ;

: HEAD-MATCH? ( ptr u8 n ptr u8 n -- bool )
   {: oa:ptr ou:n na:ptr nu:n :}
   HEAD$ {: a:ptr u:n :}
   s" a/" {: oldp:ptr oldpu:n :}
   s"  b/" {: sep:ptr sepu:n :}
   oldpu ou + sepu + nu + u <> if false exit then
   a oldpu oldp oldpu STR= 0= if false exit then
   a oldpu + ou oa ou STR= 0= if false exit then
   a oldpu ou + + sepu sep sepu STR= 0= if false exit then
   a oldpu ou + sepu + + nu na nu STR= ;

: HEAD-VALIDATE ( ptr u8 n ptr u8 n -- )
   {: oa:ptr ou:n na:ptr nu:n :}
   oa ou na nu HEAD-MATCH? 0= if E-DIFF-SYNTAX throw then
   oa ou na nu HEAD-PATHS!
   true HEAD-RESOLVED ! ;

: HEAD-SAME-VALIDATE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u a u HEAD-VALIDATE ;

: HEAD-SAME-INFER ( -- bool )
   HEAD-RESOLVED @ if HEAD-SAME? exit then
   HEAD$ {: a:ptr u:n :}
   s" a/" {: prefix:ptr prefixu:n :}
   s"  b/" {: sep:ptr sepu:n :}
   prefixu sepu + {: fixed:n :}
   u fixed < if false exit then
   u fixed - dup 2 mod 0<> if drop false exit then
   2 / {: pathu:n :}
   a u prefix prefixu STARTS-WITH? 0= if false exit then
   a prefixu pathu + + sepu sep sepu STR= 0= if false exit then
   a prefixu + {: oa:ptr :}
   a prefixu pathu + sepu + + {: na:ptr :}
   oa pathu na pathu STR= 0= if false exit then
   oa pathu na pathu HEAD-PATHS!
   true HEAD-RESOLVED !
   true ;

: HEAD-PARSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" diff --git " {: prefix:ptr prefixu:n :}
   a u prefix prefixu STARTS-WITH? 0= if E-DIFF-SYNTAX throw then
   a prefixu + u prefixu - {: body:ptr bodyu:n :}
   body bodyu s" a/" STARTS-WITH? 0= if E-DIFF-SYNTAX throw then
   body HEAD-A! bodyu HEAD-U !
   s"  b/" {: sep:ptr sepu:n :}
   body bodyu sep sepu UNIQUE-SPLIT {: split:n unique:bool :}
   unique HEAD-RESOLVED !
   unique 0= if
      0 HEAD-OLD-U ! 0 HEAD-NEW-U !
      exit
   then
   s" a/" nip {: oldpu:n :}
   split oldpu <= if E-DIFF-SYNTAX throw then
   body oldpu + split oldpu - {: oa:ptr ou:n :}
   body split + sepu + bodyu split sepu + - {: na:ptr nu:n :}
   oa ou PATH? 0= if E-DIFF-SYNTAX throw then
   na nu PATH? 0= if E-DIFF-SYNTAX throw then
   oa ou na nu HEAD-PATHS! ;

: HEX-C? ( n -- bool ) {: c:n :}
   c ZERO-C >= c NINE-C <= and if true exit then
   c A-C >= c F-C <= and ;

: HEX$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   0 begin dup u < while
      dup a + c@ HEX-C? 0= if drop false exit then
      1+
   repeat drop true ;

: DIGITS$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   0 begin dup u < while
      dup a + c@ STR-DIGIT? 0= if drop false exit then
      1+
   repeat drop true ;

: OCTAL$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   0 begin dup u < while
      dup a + c@ dup ZERO-C < swap SEVEN-C > or if drop false exit then
      1+
   repeat drop true ;

: INDEX-RIGHT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SPACE-C INDEX-OF MATCH option
      none OF a u HEX$? ENDOF
      some OF IDX>N {: split:n :}
         a split HEX$?
         u split 1+ - MODE-U = and
         a split 1+ + u split 1+ - OCTAL$? and
      ENDOF
   ;MATCH ;

: INDEX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" index " {: prefix:ptr prefixu:n :}
   a u prefix prefixu STARTS-WITH? 0= if false exit then
   a prefixu + u prefixu - {: body:ptr bodyu:n :}
   s" .." {: sep:ptr sepu:n :}
   body bodyu sep sepu FIND-SUB MATCH option
      none OF false ENDOF
      some OF IDX>N {: split:n :}
         body split HEX$?
         body split + sepu + bodyu split sepu + - INDEX-RIGHT? and
      ENDOF
   ;MATCH ;

: PERCENT-BODY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 <= if false exit then
   a u 1- + c@ PERCENT-C <> if false exit then
   a u 1- DIGITS$? 0= if false exit then
   a u 1- STR>NUMBER? MATCH option
      none OF false ENDOF
      some OF dup 0 >= swap 100 <= and ENDOF
   ;MATCH ;

: SIMILARITY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" similarity index " {: sim:ptr simu:n :}
   a u sim simu STARTS-WITH? if
      a simu + u simu - PERCENT-BODY? exit
   then
   s" dissimilarity index " {: dis:ptr disu:n :}
   a u dis disu STARTS-WITH? if
      a disu + u disu - PERCENT-BODY? exit
   then
   false ;

: DIGIT-END ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   start begin dup u < while
      dup a + c@ STR-DIGIT? 0= if exit then
      1+
   repeat ;

: PARSE-U ( ptr u8 n n n -- n ) {: a:ptr u:n start:n end:n :}
   end start <= if E-DIFF-SYNTAX throw then
   a start + end start - STR>NUMBER? MATCH option
      none OF E-DIFF-SYNTAX throw ENDOF
      some OF ENDOF
   ;MATCH ;

: RANGE ( ptr u8 n n n -- n n n ) {: a:ptr u:n at:n sign:n :}
   at u >= if E-DIFF-SYNTAX throw then
   a at + c@ sign <> if E-DIFF-SYNTAX throw then
   at 1+ {: first:n :}
   a u first DIGIT-END {: last:n :}
   a u first last PARSE-U {: start:n :}
   last u < if
      a last + c@ COMMA-C = if
         last 1+ {: cf:n :}
         a u cf DIGIT-END {: cl:n :}
         a u cf cl PARSE-U {: count:n :}
         start count cl exit
      then
   then
   start 1 last ;

: RANGE-VALID? ( n n -- bool ) {: start:n count:n :}
   count 0= if start 0 >= exit then
   start 0 > ;

: HUNK-PARSE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" @@ -" STARTS-WITH? 0= if E-DIFF-SYNTAX throw then
   a u 3 MINUS-C RANGE {: old-start:n old-count:n at:n :}
   old-start old-count RANGE-VALID? 0= if E-DIFF-SYNTAX throw then
   at u >= if E-DIFF-SYNTAX throw then
   a at + c@ SPACE-C <> if E-DIFF-SYNTAX throw then
   a u at 1+ PLUS-C RANGE {: new-start:n new-count:n tail:n :}
   new-start new-count RANGE-VALID? 0= if E-DIFF-SYNTAX throw then
   tail 3 + u > if E-DIFF-SYNTAX throw then
   a tail + c@ SPACE-C <> if E-DIFF-SYNTAX throw then
   a tail 1+ + c@ AT-C <> if E-DIFF-SYNTAX throw then
   a tail 2 + + c@ AT-C <> if E-DIFF-SYNTAX throw then
   tail 3 + u < if
      a tail 3 + + c@ SPACE-C <> if E-DIFF-SYNTAX throw then
   then
   old-count OLD-LEFT !
   new-count NEW-LEFT !
   false MARK-OK !
   false HUNK-CHANGE !
   old-count 0= new-count 0= and if
      E-DIFF-SYNTAX throw
   then
   construct state hunk-body ST!
   new-start ;

: HUNK-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u HUNK-PARSE {: line:n :}
   a u line DIFF-EVENT:HUNK ;

: HUNK-DONE ( -- )
   OLD-LEFT @ 0= NEW-LEFT @ 0= and if
      HUNK-CHANGE @ 0= if E-DIFF-SYNTAX throw then
      construct state after-hunk ST!
   then ;

: MARK-ALLOW ( -- )
   true MARK-OK ! ;

: OLD-USE ( -- )
   OLD-LEFT @ 0 <= if E-DIFF-SYNTAX throw then
   OLD-LEFT @ 1- OLD-LEFT ! ;

: NEW-USE ( -- )
   NEW-LEFT @ 0 <= if E-DIFF-SYNTAX throw then
   NEW-LEFT @ 1- NEW-LEFT ! ;

: ADD-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   NEW-USE true HUNK-CHANGE ! MARK-ALLOW HUNK-DONE
   a 1+ u 1- 0 DIFF-EVENT:ADD ;

: DELETE-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   OLD-USE true HUNK-CHANGE ! MARK-ALLOW HUNK-DONE
   a 1+ u 1- 0 DIFF-EVENT:DELETE ;

: CONTEXT-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   OLD-USE NEW-USE MARK-ALLOW HUNK-DONE
   a 1+ u 1- 0 DIFF-EVENT:CONTEXT ;

: MARKER-EVENT ( -- ptr u8 n n event )
   MARK-OK @ 0= if E-DIFF-SYNTAX throw then
   false MARK-OK !
   NONE-EVENT ;

: HUNK-LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u MARKER? if MARKER-EVENT exit then
   u 0= if E-DIFF-SYNTAX throw then
   a c@ dup PLUS-C = if drop a u ADD-EVENT exit then
   dup MINUS-C = if drop a u DELETE-EVENT exit then
   SPACE-C = if a u CONTEXT-EVENT exit then
   E-DIFF-SYNTAX throw ;

: NEED-NORMAL ( -- )
   META@ MATCH meta
      normal      OF ENDOF
      mode-old    OF E-DIFF-SYNTAX throw ENDOF
      rename-from OF E-DIFF-SYNTAX throw ENDOF
      copy-from   OF E-DIFF-SYNTAX throw ENDOF
      new-file    OF E-DIFF-SYNTAX throw ENDOF
      delete-file OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH ;

: NEED-MODE-OLD ( -- )
   META@ MATCH meta
      normal      OF E-DIFF-SYNTAX throw ENDOF
      mode-old    OF ENDOF
      rename-from OF E-DIFF-SYNTAX throw ENDOF
      copy-from   OF E-DIFF-SYNTAX throw ENDOF
      new-file    OF E-DIFF-SYNTAX throw ENDOF
      delete-file OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH ;

: NEED-RENAME-FROM ( -- )
   META@ MATCH meta
      normal      OF E-DIFF-SYNTAX throw ENDOF
      mode-old    OF E-DIFF-SYNTAX throw ENDOF
      rename-from OF ENDOF
      copy-from   OF E-DIFF-SYNTAX throw ENDOF
      new-file    OF E-DIFF-SYNTAX throw ENDOF
      delete-file OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH ;

: NEED-COPY-FROM ( -- )
   META@ MATCH meta
      normal      OF E-DIFF-SYNTAX throw ENDOF
      mode-old    OF E-DIFF-SYNTAX throw ENDOF
      rename-from OF E-DIFF-SYNTAX throw ENDOF
      copy-from   OF ENDOF
      new-file    OF E-DIFF-SYNTAX throw ENDOF
      delete-file OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH ;

: NEED-PRE-INDEX ( -- )
   INDEXED @ if E-DIFF-SYNTAX throw then ;

: MODE-VALUE ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n prefix:ptr prefixu:n :}
   a u prefix prefixu STARTS-WITH? 0= if E-DIFF-SYNTAX throw then
   u prefixu - MODE-U <> if E-DIFF-SYNTAX throw then
   a prefixu + MODE-U OCTAL$? 0= if E-DIFF-SYNTAX throw then
   a prefixu + MODE-U STR>NUMBER? MATCH option
      none OF E-DIFF-SYNTAX throw ENDOF
      some OF ENDOF
   ;MATCH ;

: MODE-OLD-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u s" old mode " MODE-VALUE OLD-MODE !
   NEED-PRE-INDEX
   NEED-NORMAL
   SIM-PENDING @ if E-DIFF-SYNTAX throw then
   MODED @ if E-DIFF-SYNTAX throw then
   REPLACED @ 0= if
      HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
   then
   IDENTITY-READY? 0= if E-DIFF-SYNTAX throw then
   construct meta mode-old META!
   INCOMPLETE!
   OLD-PATH-EVENT ;

: MODE-NEW-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u s" new mode " MODE-VALUE OLD-MODE @ = if E-DIFF-SYNTAX throw then
   NEED-PRE-INDEX
   NEED-MODE-OLD
   META-NORMAL!
   true MODED !
   COMPLETE-METADATA!
   NEW-PATH-EVENT ;

: RENAME-OLD-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   NEED-PRE-INDEX
   NEED-NORMAL
   REPLACED @ if E-DIFF-SYNTAX throw then
   MODED @ if E-DIFF-SYNTAX throw then
   a u s" rename from " AFTER-PATH {: pa:ptr pu:n :}
   pa HEAD-OLD-A! pu HEAD-OLD-U !
   construct meta rename-from META!
   INCOMPLETE!
   OLD-PATH-EVENT ;

: RENAME-NEW-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   NEED-PRE-INDEX
   NEED-RENAME-FROM
   a u s" rename to " AFTER-PATH {: pa:ptr pu:n :}
   HEAD-OLD$ pa pu HEAD-VALIDATE
   META-NORMAL!
   true REPLACED !
   COMPLETE-REPLACEMENT!
   NEW-PATH-EVENT ;

: COPY-OLD-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   NEED-PRE-INDEX
   NEED-NORMAL
   REPLACED @ if E-DIFF-SYNTAX throw then
   MODED @ if E-DIFF-SYNTAX throw then
   a u s" copy from " AFTER-PATH {: pa:ptr pu:n :}
   pa HEAD-OLD-A! pu HEAD-OLD-U !
   construct meta copy-from META!
   INCOMPLETE!
   OLD-PATH-EVENT ;

: COPY-NEW-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   NEED-PRE-INDEX
   NEED-COPY-FROM
   a u s" copy to " AFTER-PATH {: pa:ptr pu:n :}
   HEAD-OLD$ pa pu HEAD-VALIDATE
   META-NORMAL!
   true REPLACED !
   COMPLETE-REPLACEMENT!
   NEW-PATH-EVENT ;

: NEW-FILE-MODE-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u s" new file mode " MODE-VALUE drop
   NEED-PRE-INDEX
   NEED-NORMAL
   SIM-PENDING @ if E-DIFF-SYNTAX throw then
   REPLACED @ if E-DIFF-SYNTAX throw then
   MODED @ if E-DIFF-SYNTAX throw then
   construct meta new-file META!
   INCOMPLETE!
   NONE-EVENT ;

: DELETE-FILE-MODE-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u s" deleted file mode " MODE-VALUE drop
   NEED-PRE-INDEX
   NEED-NORMAL
   SIM-PENDING @ if E-DIFF-SYNTAX throw then
   REPLACED @ if E-DIFF-SYNTAX throw then
   MODED @ if E-DIFF-SYNTAX throw then
   construct meta delete-file META!
   INCOMPLETE!
   NONE-EVENT ;

: INDEX-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u INDEX? 0= if E-DIFF-SYNTAX throw then
   INDEXED @ if E-DIFF-SYNTAX throw then
   SIM-PENDING @ if
      SIM-REPLACE @ if E-DIFF-SYNTAX throw then
      false SIM-PENDING !
      false SIM-BODY !
   then
   true INDEXED !
   META@ MATCH meta
      normal OF
         INCOMPLETE!
         NONE-EVENT
      ENDOF
      new-file OF
         a u s" index 0000000000..e69de29bb2" STR= if
            HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
            META-NORMAL! COMPLETE!
            construct state terminal ST!
            NEW-PATH-EVENT
         else
            NONE-EVENT
         then
      ENDOF
      delete-file OF
         a u s" index e69de29bb2..0000000000" STR= {: empty:bool :}
         empty if
            HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
            META-NORMAL! COMPLETE!
            construct state terminal ST!
         then
         empty if
            OLD-PATH-EVENT
         else
            NONE-EVENT
         then
      ENDOF
      mode-old    OF E-DIFF-SYNTAX throw ENDOF
      rename-from OF E-DIFF-SYNTAX throw ENDOF
      copy-from   OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH ;

: SIMILARITY-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   NEED-PRE-INDEX
   NEED-NORMAL
   REPLACED @ if E-DIFF-SYNTAX throw then
   MODED @ if E-DIFF-SYNTAX throw then
   SIMILAR @ if E-DIFF-SYNTAX throw then
   a u SIMILARITY? 0= if E-DIFF-SYNTAX throw then
   true SIMILAR !
   true SIM-PENDING !
   a u s" similarity index " STARTS-WITH? {: replacement:bool :}
   replacement SIM-REPLACE !
   replacement if
      a u s" similarity index 100%" STR= 0=
   else
      true
   then SIM-BODY !
   INCOMPLETE!
   NONE-EVENT ;

: BINARY-NORMAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   HEAD-OLD$ {: oa:ptr ou:n :}
   HEAD-NEW$ {: na:ptr nu:n :}
   s" Binary files a/" {: prefix:ptr prefixu:n :}
   s"  and b/" {: sep:ptr sepu:n :}
   s"  differ" {: suffix:ptr suffixu:n :}
   prefixu ou + sepu + nu + suffixu + u <> if false exit then
   a prefixu prefix prefixu STR= 0= if false exit then
   a prefixu + ou oa ou STR= 0= if false exit then
   a prefixu ou + + sepu sep sepu STR= 0= if false exit then
   a prefixu ou + sepu + + nu na nu STR= 0= if false exit then
   a prefixu ou + sepu + nu + + suffixu suffix suffixu STR= ;

: BINARY-NEW? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   HEAD-NEW$ {: na:ptr nu:n :}
   s" Binary files /dev/null and b/" {: prefix:ptr prefixu:n :}
   s"  differ" {: suffix:ptr suffixu:n :}
   prefixu nu + suffixu + u <> if false exit then
   a prefixu prefix prefixu STR= 0= if false exit then
   a prefixu + nu na nu STR= 0= if false exit then
   a prefixu nu + + suffixu suffix suffixu STR= ;

: BINARY-DELETE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   HEAD-OLD$ {: oa:ptr ou:n :}
   s" Binary files a/" {: prefix:ptr prefixu:n :}
   s"  and /dev/null differ" {: suffix:ptr suffixu:n :}
   prefixu ou + suffixu + u <> if false exit then
   a prefixu prefix prefixu STR= 0= if false exit then
   a prefixu + ou oa ou STR= 0= if false exit then
   a prefixu ou + + suffixu suffix suffixu STR= ;

: BINARY-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   INDEXED @ 0= if E-DIFF-SYNTAX throw then
   META@ MATCH meta
      normal OF
         HEAD-RESOLVED @ 0= if
            HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
         then
         IDENTITY-READY? 0= if E-DIFF-SYNTAX throw then
         a u BINARY-NORMAL? 0= if E-DIFF-SYNTAX throw then
         COMPLETE!
         NEW-PATH-EVENT
      ENDOF
      new-file OF
         HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
         a u BINARY-NEW? 0= if E-DIFF-SYNTAX throw then
         META-NORMAL! COMPLETE!
         NEW-PATH-EVENT
      ENDOF
      delete-file OF
         HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
         a u BINARY-DELETE? 0= if E-DIFF-SYNTAX throw then
         META-NORMAL! COMPLETE!
         OLD-PATH-EVENT
      ENDOF
      mode-old    OF E-DIFF-SYNTAX throw ENDOF
      rename-from OF E-DIFF-SYNTAX throw ENDOF
      copy-from   OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH
   construct state terminal ST! ;

: OLD-TEXT-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   INDEXED @ 0= if E-DIFF-SYNTAX throw then
   META@ MATCH meta
      normal OF
         a u s" --- a/" AFTER-PATH {: pa:ptr pu:n :}
         pa HEAD-OLD-A! pu HEAD-OLD-U !
         OLD-PATH-EVENT
      ENDOF
      new-file OF
         a u s" --- /dev/null" STR= 0= if E-DIFF-SYNTAX throw then
         NONE-EVENT
      ENDOF
      delete-file OF
         a u s" --- a/" AFTER-PATH {: pa:ptr pu:n :}
         pa HEAD-OLD-A! pu HEAD-OLD-U !
         OLD-PATH-EVENT
      ENDOF
      mode-old    OF E-DIFF-SYNTAX throw ENDOF
      rename-from OF E-DIFF-SYNTAX throw ENDOF
      copy-from   OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH ;

: START-HEAD ( ptr u8 n -- ptr u8 n n event )
   HEAD-PARSE
   construct state header ST!
   META-NORMAL!
   INCOMPLETE!
   false INDEXED !
   false REPLACED !
   false MODED !
   0 OLD-MODE !
   false SIMILAR !
   false SIM-PENDING !
   false SIM-REPLACE !
   false SIM-BODY !
   NONE-EVENT ;

: HEAD-LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u DIFF-HEAD? if
      COMPLETE? 0= if E-DIFF-SYNTAX throw then
      a u START-HEAD exit
   then
   a u s" similarity index " STARTS-WITH? if a u SIMILARITY-EVENT exit then
   a u s" dissimilarity index " STARTS-WITH? if a u SIMILARITY-EVENT exit then
   a u s" old mode " STARTS-WITH? if a u MODE-OLD-EVENT exit then
   a u s" new mode " STARTS-WITH? if a u MODE-NEW-EVENT exit then
   a u s" rename from " STARTS-WITH? if a u RENAME-OLD-EVENT exit then
   a u s" rename to " STARTS-WITH? if a u RENAME-NEW-EVENT exit then
   a u s" copy from " STARTS-WITH? if a u COPY-OLD-EVENT exit then
   a u s" copy to " STARTS-WITH? if a u COPY-NEW-EVENT exit then
   a u s" new file mode " STARTS-WITH? if a u NEW-FILE-MODE-EVENT exit then
   a u s" deleted file mode " STARTS-WITH? if a u DELETE-FILE-MODE-EVENT exit then
   a u s" index " STARTS-WITH? if a u INDEX-EVENT exit then
   a u s" Binary files " STARTS-WITH? if a u BINARY-EVENT exit then
   a u OLD-HEAD? if
      a u OLD-TEXT-EVENT
      construct state new-header ST!
      exit
   then
   E-DIFF-SYNTAX throw ;

: NEW-LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u NEW-HEAD? 0= if E-DIFF-SYNTAX throw then
   META@ MATCH meta
      normal OF
         a u s" +++ b/" AFTER-PATH {: pa:ptr pu:n :}
         HEAD-OLD$ pa pu HEAD-VALIDATE
         IDENTITY-READY? 0= if E-DIFF-SYNTAX throw then
         NEW-PATH-EVENT
      ENDOF
      new-file OF
         a u s" +++ b/" AFTER-PATH {: pa:ptr pu:n :}
         pa pu HEAD-SAME-VALIDATE
         META-NORMAL!
         NEW-PATH-EVENT
      ENDOF
      delete-file OF
         a u s" +++ /dev/null" STR= 0= if E-DIFF-SYNTAX throw then
         HEAD-OLD$ HEAD-SAME-VALIDATE
         META-NORMAL!
         NONE-EVENT
      ENDOF
      mode-old    OF E-DIFF-SYNTAX throw ENDOF
      rename-from OF E-DIFF-SYNTAX throw ENDOF
      copy-from   OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH
   construct state file-body ST!
   COMPLETE! ;

: FILE-LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u HUNK-HEAD? if a u HUNK-EVENT exit then
   E-DIFF-SYNTAX throw ;

: AFTER-LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u MARKER? if MARKER-EVENT exit then
   a u HUNK-HEAD? if a u HUNK-EVENT exit then
   a u DIFF-HEAD? if a u START-HEAD exit then
   E-DIFF-SYNTAX throw ;

: TERMINAL-LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u DIFF-HEAD? 0= if E-DIFF-SYNTAX throw then
   a u START-HEAD ;

: IDLE-LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u DIFF-HEAD? 0= if E-DIFF-SYNTAX throw then
   a u START-HEAD ;

public

: RESET ( -- )
   construct state idle ST!
   META-NORMAL!
   0 HEAD-U !
   0 HEAD-OLD-U !
   0 HEAD-NEW-U !
   0 OLD-LEFT !
   0 NEW-LEFT !
   false MARK-OK !
   false HUNK-CHANGE !
   false COMPLETE !
   false INDEXED !
   false REPLACED !
   false MODED !
   0 OLD-MODE !
   false HEAD-RESOLVED !
   false SIMILAR !
   false SIM-PENDING !
   false SIM-REPLACE !
   false SIM-BODY ! ;

: LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   ST@ MATCH state
      idle       OF a u IDLE-LINE  ENDOF
      header     OF a u HEAD-LINE  ENDOF
      new-header OF a u NEW-LINE   ENDOF
      file-body  OF a u FILE-LINE  ENDOF
      hunk-body  OF a u HUNK-LINE  ENDOF
      after-hunk OF a u AFTER-LINE ENDOF
      terminal   OF a u TERMINAL-LINE ENDOF
   ;MATCH ;

: FINISH ( -- )
   \ HEADER is terminal only after a complete metadata-only form.
   ST@ MATCH state
      idle       OF ENDOF
      header     OF COMPLETE? 0= if E-DIFF-SYNTAX throw then ENDOF
      new-header OF E-DIFF-SYNTAX throw ENDOF
      file-body  OF E-DIFF-SYNTAX throw ENDOF
      hunk-body  OF E-DIFF-SYNTAX throw ENDOF
      after-hunk OF ENDOF
      terminal   OF ENDOF
   ;MATCH ;

;package
