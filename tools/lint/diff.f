\ diff.f - checked byte-exact validator for jj Git-format sections.
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
-7401 constant E-DIFF-FRAME-CAP
-7402 constant E-DIFF-CAPTURE
-7403 constant E-DIFF-CAPTURE-STDERR
-7404 constant E-DIFF-CAPTURE-ID

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

ENUM status
   modified
   added
   removed
   renamed
   copied
;ENUM

ENUM form
   text
   binary
   mode
   empty
   pure
   gitlink
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
1 LAYOUT-BUFFER STATUS-V status
PTR-VARIABLE HEAD-A
variable HEAD-U
PTR-VARIABLE HEAD-OLD-A
variable HEAD-OLD-U
PTR-VARIABLE HEAD-NEW-A
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
variable FRAMED
variable FRAME-OLD-PRESENT
variable FRAME-NEW-PRESENT
PTR-VARIABLE FRAME-OLD-A
variable FRAME-OLD-U
PTR-VARIABLE FRAME-NEW-A
variable FRAME-NEW-U
PTR-VARIABLE RAW-A
variable RAW-U
variable RAW-I
variable SAW-NEW-FILE
variable SAW-DELETE-FILE
variable SAW-RENAME
variable SAW-COPY
variable SAW-BINARY
variable SAW-HUNK
variable SAW-EMPTY
variable SAW-GITLINK
variable ENTRY-GITLINK
variable SCAN-NEXT
PTR-VARIABLE SCAN-OLD-A
variable SCAN-OLD-U
PTR-VARIABLE SCAN-NEW-A
variable SCAN-NEW-U

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

: STATUS-AT ( -- ptr status )
   0 STATUS-V ;

: STATUS@ ( -- status )
   STATUS-AT @ ;

: STATUS! ( status -- )
   STATUS-AT ! ;

: FRAME-OLD-A@ ( -- ptr u8 )
   FRAME-OLD-A @ ;

: FRAME-NEW-A@ ( -- ptr u8 )
   FRAME-NEW-A @ ;

: RAW-A@ ( -- ptr u8 )
   RAW-A @ ;

: FRAME-OLD-A! ( ptr u8 -- )
   FRAME-OLD-A ! ;

: FRAME-NEW-A! ( ptr u8 -- )
   FRAME-NEW-A ! ;

: RAW-A! ( ptr u8 -- )
   RAW-A ! ;

: FRAME-OLD$ ( -- ptr u8 n )
   FRAME-OLD-A@ FRAME-OLD-U @ ;

: FRAME-NEW$ ( -- ptr u8 n )
   FRAME-NEW-A@ FRAME-NEW-U @ ;

: FRAME-HEAD-OLD$ ( -- ptr u8 n )
   FRAME-OLD-PRESENT @ if FRAME-OLD$ else FRAME-NEW$ then ;

: FRAME-HEAD-NEW$ ( -- ptr u8 n )
   FRAME-NEW-PRESENT @ if FRAME-NEW$ else FRAME-OLD$ then ;

: RAW$ ( -- ptr u8 n )
   RAW-A@ RAW-U @ ;

: FRAME-OLD? ( -- bool )
   FRAME-OLD-PRESENT @ if true else false then ;

: FRAME-NEW? ( -- bool )
   FRAME-NEW-PRESENT @ if true else false then ;

: HEAD-A@ ( -- ptr u8 )
   HEAD-A @ ;

: HEAD-OLD-A@ ( -- ptr u8 )
   HEAD-OLD-A @ ;

: HEAD-NEW-A@ ( -- ptr u8 )
   HEAD-NEW-A @ ;

: HEAD-OLD-A! ( ptr u8 -- )
   HEAD-OLD-A ! ;

: HEAD-NEW-A! ( ptr u8 -- )
   HEAD-NEW-A ! ;

: HEAD-A! ( ptr u8 -- )
   HEAD-A ! ;

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

: FRAME-HEAD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   FRAME-HEAD-OLD$ {: oa:ptr ou:n :}
   FRAME-HEAD-NEW$ {: na:ptr nu:n :}
   s" diff --git a/" {: prefix:ptr prefixu:n :}
   s"  b/" {: sep:ptr sepu:n :}
   prefixu ou + sepu + nu + u <> if false exit then
   a prefixu prefix prefixu STR= 0= if false exit then
   a prefixu + ou oa ou STR= 0= if false exit then
   a prefixu ou + + sepu sep sepu STR= 0= if false exit then
   a prefixu ou + sepu + + nu na nu STR= ;

: FRAME-HEAD-PARSE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FRAME-HEAD? 0= if E-DIFF-SYNTAX throw then
   s" diff --git " nip {: prefixu:n :}
   a prefixu + HEAD-A! u prefixu - HEAD-U !
   FRAME-HEAD-OLD$ FRAME-HEAD-NEW$ HEAD-PATHS!
   true HEAD-RESOLVED ! ;

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

: GITLINK-SUFFIX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s"  040000" {: suffix:ptr suffixu:n :}
   u suffixu < if false exit then
   a u suffixu - + suffixu suffix suffixu STR= ;

: GITLINK-MODE? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n prefix:ptr prefixu:n :}
   u prefixu - MODE-U <> if false exit then
   a u prefix prefixu STARTS-WITH? 0= if false exit then
   a prefixu + MODE-U s" 040000" STR= ;

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
   true SAW-HUNK !
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
   true SAW-RENAME !
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
   true SAW-COPY !
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
   a u s" new file mode " GITLINK-MODE? ENTRY-GITLINK !
   NEED-PRE-INDEX
   NEED-NORMAL
   SIM-PENDING @ if E-DIFF-SYNTAX throw then
   REPLACED @ if E-DIFF-SYNTAX throw then
   MODED @ if E-DIFF-SYNTAX throw then
   construct meta new-file META!
   true SAW-NEW-FILE !
   INCOMPLETE!
   NONE-EVENT ;

: DELETE-FILE-MODE-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u s" deleted file mode " MODE-VALUE drop
   a u s" deleted file mode " GITLINK-MODE? ENTRY-GITLINK !
   NEED-PRE-INDEX
   NEED-NORMAL
   SIM-PENDING @ if E-DIFF-SYNTAX throw then
   REPLACED @ if E-DIFF-SYNTAX throw then
   MODED @ if E-DIFF-SYNTAX throw then
   construct meta delete-file META!
   true SAW-DELETE-FILE !
   INCOMPLETE!
   NONE-EVENT ;

: GITLINK-EVENT ( -- ptr u8 n n event )
   META@ MATCH meta
      normal OF
         HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
      ENDOF
      new-file OF
         ENTRY-GITLINK @ 0= if E-DIFF-SYNTAX throw then
         HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
      ENDOF
      delete-file OF
         ENTRY-GITLINK @ 0= if E-DIFF-SYNTAX throw then
         HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
      ENDOF
      mode-old    OF E-DIFF-SYNTAX throw ENDOF
      rename-from OF E-DIFF-SYNTAX throw ENDOF
      copy-from   OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH
   META-NORMAL!
   true SAW-GITLINK !
   COMPLETE!
   construct state terminal ST!
   STATUS@ MATCH status
      modified OF NEW-PATH-EVENT ENDOF
      added    OF NEW-PATH-EVENT ENDOF
      removed  OF OLD-PATH-EVENT ENDOF
      renamed  OF E-DIFF-SYNTAX throw ENDOF
      copied   OF E-DIFF-SYNTAX throw ENDOF
   ;MATCH ;

: INDEX-EVENT ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   a u INDEX? 0= if E-DIFF-SYNTAX throw then
   INDEXED @ if E-DIFF-SYNTAX throw then
   SIM-PENDING @ if
      SIM-REPLACE @ if E-DIFF-SYNTAX throw then
      false SIM-PENDING !
      false SIM-BODY !
   then
   true INDEXED !
   a u GITLINK-SUFFIX? ENTRY-GITLINK @ or if GITLINK-EVENT exit then
   META@ MATCH meta
      normal OF
         INCOMPLETE!
         NONE-EVENT
      ENDOF
      new-file OF
         a u s" index 0000000000..e69de29bb2" STR= if
            HEAD-SAME-INFER 0= if E-DIFF-SYNTAX throw then
            true SAW-EMPTY !
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
            true SAW-EMPTY !
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
   true SAW-BINARY !
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
   FRAMED @ if FRAME-HEAD-PARSE else HEAD-PARSE then
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

private

: RAW-RESET ( -- )
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
   false SIM-BODY !
   false FRAMED !
   false FRAME-OLD-PRESENT !
   false FRAME-NEW-PRESENT !
   0 FRAME-OLD-U !
   0 FRAME-NEW-U !
   0 RAW-U !
   0 RAW-I !
   false SAW-NEW-FILE !
   false SAW-DELETE-FILE !
   false SAW-RENAME !
   false SAW-COPY !
   false SAW-BINARY !
   false SAW-HUNK !
   false SAW-EMPTY !
   false SAW-GITLINK !
   false ENTRY-GITLINK ! ;

: RAW-LINE ( ptr u8 n -- ptr u8 n n event ) {: a:ptr u:n :}
   ST@ MATCH state
      idle       OF a u IDLE-LINE  ENDOF
      header     OF a u HEAD-LINE  ENDOF
      new-header OF a u NEW-LINE   ENDOF
      file-body  OF a u FILE-LINE  ENDOF
      hunk-body  OF a u HUNK-LINE  ENDOF
      after-hunk OF a u AFTER-LINE ENDOF
      terminal   OF a u TERMINAL-LINE ENDOF
   ;MATCH ;

: RAW-FINISH ( -- )
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

$0A constant LF-C

: RAW-AT ( -- ptr u8 )
   RAW-A@ RAW-I @ + ;

: RAW-LEFT ( -- n )
   RAW-U @ RAW-I @ - ;

: RAW-PREFIX? ( ptr u8 n -- bool )
   RAW-AT RAW-LEFT 2swap STARTS-WITH? ;

: RAW-SEG? ( n ptr u8 n -- bool ) {: off:n a:ptr u:n :}
   off 0 < if false exit then
   off u + RAW-LEFT > if false exit then
   RAW-AT off + u a u STR= ;

: RAW-DELIM! ( n -- ) {: lineu:n :}
   lineu RAW-LEFT >= if E-DIFF-SYNTAX throw then
   RAW-AT lineu + c@ LF-C <> if E-DIFF-SYNTAX throw then
   RAW-I @ lineu + 1+ RAW-I ! ;

: RAW-TAKE3 ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: prefix:ptr prefixu:n body:ptr bodyu:n suffix:ptr suffixu:n :}
   prefixu bodyu + suffixu + {: lineu:n :}
   0 prefix prefixu RAW-SEG? 0= if E-DIFF-SYNTAX throw then
   prefixu body bodyu RAW-SEG? 0= if E-DIFF-SYNTAX throw then
   prefixu bodyu + suffix suffixu RAW-SEG? 0= if E-DIFF-SYNTAX throw then
   RAW-AT lineu dup RAW-DELIM! ;

: RAW-TAKE5 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: p:ptr pu:n a:ptr au:n mid:ptr midu:n b:ptr bu:n suffix:ptr suffixu:n :}
   pu au + midu + bu + suffixu + {: lineu:n :}
   0 p pu RAW-SEG? 0= if E-DIFF-SYNTAX throw then
   pu a au RAW-SEG? 0= if E-DIFF-SYNTAX throw then
   pu au + mid midu RAW-SEG? 0= if E-DIFF-SYNTAX throw then
   pu au + midu + b bu RAW-SEG? 0= if E-DIFF-SYNTAX throw then
   pu au + midu + bu + suffix suffixu RAW-SEG? 0= if E-DIFF-SYNTAX throw then
   RAW-AT lineu dup RAW-DELIM! ;

: RAW-ORDINARY ( -- ptr u8 n )
   RAW-I @ {: start:n :}
   begin RAW-I @ RAW-U @ < while
      RAW-A@ RAW-I @ + c@ LF-C = if
         RAW-A@ start + RAW-I @ start -
         RAW-I @ 1+ RAW-I !
         exit
      then
      RAW-I @ 1+ RAW-I !
   repeat
   E-DIFF-SYNTAX throw ;

: RAW-HEAD ( -- ptr u8 n )
   s" diff --git a/" FRAME-HEAD-OLD$
   s"  b/" FRAME-HEAD-NEW$ EMPTY RAW-TAKE5 ;

: RAW-RENAME-FROM ( -- ptr u8 n )
   s" rename from " FRAME-OLD$ EMPTY RAW-TAKE3 ;

: RAW-RENAME-TO ( -- ptr u8 n )
   s" rename to " FRAME-NEW$ EMPTY RAW-TAKE3 ;

: RAW-COPY-FROM ( -- ptr u8 n )
   s" copy from " FRAME-OLD$ EMPTY RAW-TAKE3 ;

: RAW-COPY-TO ( -- ptr u8 n )
   s" copy to " FRAME-NEW$ EMPTY RAW-TAKE3 ;

: RAW-OLD-TEXT ( -- ptr u8 n )
   STATUS@ MATCH status
      added    OF s" --- /dev/null" EMPTY EMPTY RAW-TAKE3 ENDOF
      modified OF s" --- a/" FRAME-OLD$ EMPTY RAW-TAKE3 ENDOF
      removed  OF s" --- a/" FRAME-OLD$ EMPTY RAW-TAKE3 ENDOF
      renamed  OF s" --- a/" FRAME-OLD$ EMPTY RAW-TAKE3 ENDOF
      copied   OF s" --- a/" FRAME-OLD$ EMPTY RAW-TAKE3 ENDOF
   ;MATCH ;

: RAW-NEW-TEXT ( -- ptr u8 n )
   STATUS@ MATCH status
      added    OF s" +++ b/" FRAME-NEW$ EMPTY RAW-TAKE3 ENDOF
      modified OF s" +++ b/" FRAME-NEW$ EMPTY RAW-TAKE3 ENDOF
      removed  OF s" +++ /dev/null" EMPTY EMPTY RAW-TAKE3 ENDOF
      renamed  OF s" +++ b/" FRAME-NEW$ EMPTY RAW-TAKE3 ENDOF
      copied   OF s" +++ b/" FRAME-NEW$ EMPTY RAW-TAKE3 ENDOF
   ;MATCH ;

: RAW-BINARY ( -- ptr u8 n )
   STATUS@ MATCH status
      added OF
         s" Binary files /dev/null and b/" FRAME-NEW$ s"  differ" RAW-TAKE3
      ENDOF
      removed OF
         s" Binary files a/" FRAME-OLD$ s"  and /dev/null differ" RAW-TAKE3
      ENDOF
      modified OF
         s" Binary files a/" FRAME-OLD$ s"  and b/" FRAME-NEW$ s"  differ" RAW-TAKE5
      ENDOF
      renamed OF
         s" Binary files a/" FRAME-OLD$ s"  and b/" FRAME-NEW$ s"  differ" RAW-TAKE5
      ENDOF
      copied OF
         s" Binary files a/" FRAME-OLD$ s"  and b/" FRAME-NEW$ s"  differ" RAW-TAKE5
      ENDOF
   ;MATCH ;

: RAW-HEADER ( -- ptr u8 n )
   s" rename from " RAW-PREFIX? if RAW-RENAME-FROM exit then
   s" rename to " RAW-PREFIX? if RAW-RENAME-TO exit then
   s" copy from " RAW-PREFIX? if RAW-COPY-FROM exit then
   s" copy to " RAW-PREFIX? if RAW-COPY-TO exit then
   s" Binary files " RAW-PREFIX? if RAW-BINARY exit then
   s" --- " RAW-PREFIX? if RAW-OLD-TEXT exit then
   RAW-ORDINARY ;

: RAW-NEXT-LINE ( -- ptr u8 n )
   ST@ MATCH state
      idle       OF RAW-HEAD ENDOF
      header     OF RAW-HEADER ENDOF
      new-header OF RAW-NEW-TEXT ENDOF
      file-body  OF RAW-ORDINARY ENDOF
      hunk-body  OF RAW-ORDINARY ENDOF
      after-hunk OF RAW-ORDINARY ENDOF
      terminal   OF RAW-ORDINARY ENDOF
   ;MATCH ;

: RAW-STEP ( -- ptr u8 n n event )
   RAW-NEXT-LINE RAW-LINE ;

: FRAME-PATHS-VALID? ( -- bool )
   FRAME-OLD? if FRAME-OLD-U @ 0 > else FRAME-OLD-U @ 0= then
   FRAME-NEW? if FRAME-NEW-U @ 0 > else FRAME-NEW-U @ 0= then and ;

: NO-REPLACEMENT? ( -- bool )
   SAW-RENAME @ 0=
   SAW-COPY @ 0= and ;

: NO-FILE-KIND? ( -- bool )
   SAW-NEW-FILE @ 0=
   SAW-DELETE-FILE @ 0= and ;

: STATUS-VALID? ( -- bool )
   STATUS@ MATCH status
      modified OF
         FRAME-OLD? FRAME-NEW? and
         FRAME-OLD$ FRAME-NEW$ STR= and
         NO-REPLACEMENT? and NO-FILE-KIND? and
      ENDOF
      added OF
         FRAME-OLD? 0= FRAME-NEW? and
         SAW-NEW-FILE @ 0 <> and SAW-DELETE-FILE @ 0= and
         NO-REPLACEMENT? and
      ENDOF
      removed OF
         FRAME-OLD? FRAME-NEW? 0= and
         SAW-DELETE-FILE @ 0 <> and SAW-NEW-FILE @ 0= and
         NO-REPLACEMENT? and
      ENDOF
      renamed OF
         FRAME-OLD? FRAME-NEW? and
         FRAME-OLD$ FRAME-NEW$ STR= 0= and
         SAW-RENAME @ 0 <> and SAW-COPY @ 0= and NO-FILE-KIND? and
      ENDOF
      copied OF
         FRAME-OLD? FRAME-NEW? and
         FRAME-OLD$ FRAME-NEW$ STR= 0= and
         SAW-COPY @ 0 <> and SAW-RENAME @ 0= and NO-FILE-KIND? and
      ENDOF
   ;MATCH ;

: RAW-FORM ( -- form bool )
   SAW-HUNK @ if DIFF-FORM:TEXT true exit then
   SAW-BINARY @ if DIFF-FORM:BINARY true exit then
   SAW-EMPTY @ if DIFF-FORM:EMPTY false exit then
   SAW-GITLINK @ if DIFF-FORM:GITLINK false exit then
   REPLACED @ 0 <> INDEXED @ 0= and if DIFF-FORM:PURE false exit then
   MODED @ 0 <> INDEXED @ 0= and if DIFF-FORM:MODE false exit then
   E-DIFF-SYNTAX throw ;

: RAW-BEGIN ( status bool ptr u8 n bool ptr u8 n ptr u8 n -- )
   {: change:status old?:bool oa:ptr ou:n new?:bool na:ptr nu:n raw:ptr rawu:n :}
   RAW-RESET
   change STATUS!
   old? FRAME-OLD-PRESENT !
   new? FRAME-NEW-PRESENT !
   oa FRAME-OLD-A! ou FRAME-OLD-U !
   na FRAME-NEW-A! nu FRAME-NEW-U !
   raw RAW-A! rawu RAW-U !
   FRAME-PATHS-VALID? 0= if E-DIFF-SYNTAX throw then
   rawu 0 <= if E-DIFF-SYNTAX throw then
   true FRAMED ! ;

: RAW-END ( -- )
   RAW-FINISH
   STATUS-VALID? 0= if E-DIFF-SYNTAX throw then ;

: SCAN-OLD$ ( -- ptr u8 n )
   SCAN-OLD-A @ SCAN-OLD-U @ ;

: SCAN-NEW$ ( -- ptr u8 n )
   SCAN-NEW-A @ SCAN-NEW-U @ ;

: SCAN-SEG? ( n ptr u8 n -- bool ) {: off:n a:ptr u:n :}
   off 0 < if false exit then
   off u + off < if false exit then
   off u + RAW-U @ > if false exit then
   RAW-A@ off + u a u STR= ;

: SCAN-HEAD? ( -- bool )
   SCAN-OLD$ {: oa:ptr ou:n :}
   SCAN-NEW$ {: na:ptr nu:n :}
   s" diff --git a/" {: prefix:ptr prefixu:n :}
   s"  b/" {: mid:ptr midu:n :}
   prefixu ou + midu + nu + {: lineu:n :}
   RAW-I @ lineu + RAW-I @ < if false exit then
   RAW-I @ lineu + RAW-U @ >= if false exit then
   RAW-I @ prefix prefixu SCAN-SEG? 0= if false exit then
   RAW-I @ prefixu + oa ou SCAN-SEG? 0= if false exit then
   RAW-I @ prefixu + ou + mid midu SCAN-SEG? 0= if false exit then
   RAW-I @ prefixu + ou + midu + na nu SCAN-SEG? 0= if false exit then
   RAW-A@ RAW-I @ lineu + + c@ LF-C = ;

: SCAN-END? ( -- bool )
   ST@ MATCH state
      idle       OF false ENDOF
      header     OF COMPLETE? ENDOF
      new-header OF false ENDOF
      file-body  OF false ENDOF
      hunk-body  OF false ENDOF
      after-hunk OF true ENDOF
      terminal   OF true ENDOF
   ;MATCH ;

: SCAN-BOUNDARY? ( -- bool )
   SCAN-NEXT @ 0= if false exit then
   RAW-I @ 0= if false exit then
   SCAN-END? 0= if false exit then
   SCAN-HEAD? ;

: SCAN-NEXT! ( bool ptr u8 n bool ptr u8 n -- )
   {: old?:bool oa:ptr ou:n new?:bool na:ptr nu:n :}
   old? if oa else na then SCAN-OLD-A !
   old? if ou else nu then SCAN-OLD-U !
   new? if na else oa then SCAN-NEW-A !
   new? if nu else ou then SCAN-NEW-U ! ;

public

: OBJECT-ID? ( ptr u8 n -- bool )
   HEX$? ;

: VALIDATE-SECTION ( status bool ptr u8 n bool ptr u8 n ptr u8 n -- form bool )
   RAW-BEGIN
   begin RAW-I @ RAW-U @ < while
      RAW-STEP drop drop 2drop
   repeat
   RAW-END
   RAW-FORM ;

: SCAN-SECTION ( status bool ptr u8 n bool ptr u8 n bool bool ptr u8 n bool ptr u8 n ptr u8 n -- n form bool )
   {: change:status old?:bool oa:ptr ou:n new?:bool na:ptr nu:n next?:bool next-old?:bool noa:ptr nou:n next-new?:bool nna:ptr nnu:n raw:ptr rawu:n :}
   next? SCAN-NEXT !
   next-old? noa nou next-new? nna nnu SCAN-NEXT!
   change old? oa ou new? na nu raw rawu RAW-BEGIN
   begin RAW-I @ RAW-U @ < while
      SCAN-BOUNDARY? if
         RAW-END
         RAW-I @ RAW-FORM exit
      then
      RAW-STEP drop drop 2drop
   repeat
   next? if E-DIFF-SYNTAX throw then
   RAW-END
   RAW-I @ RAW-FORM ;

;package
