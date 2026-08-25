\ diff.f - checked canonical parser for `jj diff --git`.
\ Event accessors borrow LINE/FINISH input spans until the next parser call.
\ Paths admit printable non-DEL bytes and canonical nonempty segments; raw
\ controls, embedded line breaks, `.` segments, and `..` segments fail closed.

require lib/prelude.f
require lib/string.f
require lib/adt/option.f
require tools/lint/diff-error.f

package DIFF
public

ENUM form
   modify
   add-file
   delete-file
   rename
   copy
   mode
   binary
;ENUM

ENUM event none section hunk add context delete ;ENUM

private

ENUM phase idle meta new-head body hunk terminal ;ENUM
ENUM file-mode regular executable symlink gitlink ;ENUM
ENUM mark-side no-mark old-side new-side both-sides ;ENUM

$20 constant SPACE-C
$0A constant LF-C
$0D constant CR-C
$2C constant COMMA-C
$2E constant DOT-C
$2F constant SLASH-C
$30 constant ZERO-C
$39 constant NINE-C
$40 constant AT-C
$61 constant A-C
$66 constant F-C
$7F constant DELETE-C
7 constant HASH-MIN
$40 constant HASH-MAX

1 LAYOUT-BUFFER PHASE-V phase
1 LAYOUT-BUFFER MODE-V file-mode
1 LAYOUT-BUFFER EVENT-V event
1 LAYOUT-BUFFER EVENT-FORM-V form
1 LAYOUT-BUFFER MARK-SIDE-V mark-side

variable HEAD-A
variable HEAD-U
variable OLD-A
variable OLD-U
variable NEW-A
variable NEW-U
variable OLD-LEFT
variable NEW-LEFT
variable INDEXED
variable INDEX-MODE
variable INDEX-OLD-ZERO
variable INDEX-NEW-ZERO
variable INDEX-OLD-EMPTY
variable INDEX-NEW-EMPTY
variable OLD-MODED
variable NEW-MODED
variable NEW-FILE
variable DELETE-FILE
variable RENAMED-FROM
variable RENAMED-TO
variable COPIED-FROM
variable COPIED-TO
variable SIMILAR
variable DISSIMILAR
variable SIM-N
variable HUNK-SEEN
variable HUNK-CHANGED
variable PREV-OLD-END
variable PREV-NEW-END
variable EVENT-OLD-A
variable EVENT-OLD-U
variable EVENT-NEW-A
variable EVENT-NEW-U
variable EVENT-BODY
variable EVENT-N
variable EVENT-A
variable EVENT-U
variable SEG-FIRST
variable SEG-END

: PTR-SLOT ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: HEAD-A@ ( -- ptr u8 )
   HEAD-A PTR-SLOT @ ;

: OLD-A@ ( -- ptr u8 )
   OLD-A PTR-SLOT @ ;

: NEW-A@ ( -- ptr u8 )
   NEW-A PTR-SLOT @ ;

: EVENT-OLD-A@ ( -- ptr u8 )
   EVENT-OLD-A PTR-SLOT @ ;

: EVENT-NEW-A@ ( -- ptr u8 )
   EVENT-NEW-A PTR-SLOT @ ;

: EVENT-A@ ( -- ptr u8 )
   EVENT-A PTR-SLOT @ ;

: HEAD-A! ( ptr u8 -- )
   HEAD-A PTR-SLOT ! ;

: OLD-A! ( ptr u8 -- )
   OLD-A PTR-SLOT ! ;

: NEW-A! ( ptr u8 -- )
   NEW-A PTR-SLOT ! ;

: EVENT-OLD-A! ( ptr u8 -- )
   EVENT-OLD-A PTR-SLOT ! ;

: EVENT-NEW-A! ( ptr u8 -- )
   EVENT-NEW-A PTR-SLOT ! ;

: EVENT-A! ( ptr u8 -- )
   EVENT-A PTR-SLOT ! ;

: HEAD$ ( -- ptr u8 n )
   HEAD-A@ HEAD-U @ ;

: OLD$ ( -- ptr u8 n )
   OLD-A@ OLD-U @ ;

: NEW$ ( -- ptr u8 n )
   NEW-A@ NEW-U @ ;

: PHASE-AT ( -- ptr phase )
   0 PHASE-V ;

: PHASE@ ( -- phase )
   PHASE-AT @ ;

: PHASE! ( phase -- )
   PHASE-AT ! ;

: MODE-AT ( -- ptr file-mode )
   0 MODE-V ;

: MODE@ ( -- file-mode )
   MODE-AT @ ;

: MODE! ( file-mode -- )
   MODE-AT ! ;

: EVENT-AT ( -- ptr event )
   0 EVENT-V ;

: EVENT@ ( -- event )
   EVENT-AT @ ;

: EVENT! ( event -- event )
   dup EVENT-AT ! ;

: EVENT-FORM-AT ( -- ptr form )
   0 EVENT-FORM-V ;

: EVENT-FORM@ ( -- form )
   EVENT-FORM-AT @ ;

: EVENT-FORM! ( form -- )
   EVENT-FORM-AT ! ;

: MARK-SIDE-AT ( -- ptr mark-side )
   0 MARK-SIDE-V ;

: MARK-SIDE@ ( -- mark-side )
   MARK-SIDE-AT @ ;

: MARK-SIDE! ( mark-side -- )
   MARK-SIDE-AT ! ;

: FAIL ( -- )
   E-DIFF-SYNTAX throw ;

: ON? ( ptr a -- bool )
   @ 0<> ;

: NONE ( -- event )
   construct event none EVENT! ;

: SECTION-SPANS ( ptr u8 n ptr u8 n form bool -- event )
   {: oa:ptr ou:n na:ptr nu:n kind:form present:bool :}
   oa EVENT-OLD-A! ou EVENT-OLD-U !
   na EVENT-NEW-A! nu EVENT-NEW-U !
   kind EVENT-FORM! present EVENT-BODY !
   construct event section EVENT! ;

: SECTION ( form bool -- event ) {: kind:form present:bool :}
   OLD$ NEW$ kind present SECTION-SPANS ;

: HUNK-EVENT ( n -- event )
   EVENT-N !
   construct event hunk EVENT! ;

: CONTENT-EVENT ( ptr u8 n event -- event ) {: a:ptr u:n kind:event :}
   a EVENT-A! u EVENT-U !
   kind EVENT! ;

: SET-OLD ( ptr u8 n -- ) {: a:ptr u:n :}
   a OLD-A! u OLD-U ! ;

: SET-NEW ( ptr u8 n -- ) {: a:ptr u:n :}
   a NEW-A! u NEW-U ! ;

: EMPTY-OLD ( -- )
   s" " SET-OLD ;

: EMPTY-NEW ( -- )
   s" " SET-NEW ;

: BYTE-SAFE? ( n -- bool ) {: c:n :}
   c SPACE-C < if false exit then
   c DELETE-C <> ;

: BYTES-SAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ BYTE-SAFE? 0= if drop false exit then
      1+
   repeat drop true ;

: LINE-SAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ dup 0= swap CR-C = or if drop false exit then
      1+
   repeat drop true ;

: SEG-SAFE? ( ptr u8 n n -- bool ) {: a:ptr first:n last:n :}
   last first - {: len:n :}
   len 0= if false exit then
   len 1 = if a first + c@ DOT-C <> exit then
   len 2 = if
      a first + c@ DOT-C =
      a first 1+ + c@ DOT-C = and 0= exit
   then
   true ;

: PATH-SEGS-SAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 SEG-FIRST !
   0 begin dup u < while
      dup a + c@ SLASH-C = if
         dup SEG-END !
         a SEG-FIRST @ SEG-END @ SEG-SAFE? 0= if drop false exit then
         dup 1+ SEG-FIRST !
      then
      1+
   repeat drop
   a SEG-FIRST @ u SEG-SAFE? ;

: PATH-SAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   a u BYTES-SAFE? 0= if false exit then
   a u PATH-SEGS-SAFE? ;

: NEED-PATH ( ptr u8 n -- ptr u8 n ) 2dup PATH-SAFE? 0= if FAIL then ;

: PREFIX-AFTER ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: a:ptr u:n prefix:ptr prefixu:n :}
   a u prefix prefixu STARTS-WITH? 0= if FAIL then
   a prefixu + u prefixu - NEED-PATH ;

: HEAD-EXACT? ( ptr u8 n ptr u8 n -- bool )
   {: oa:ptr ou:n na:ptr nu:n :}
   HEAD$ {: a:ptr u:n :}
   2 ou + 3 + nu + u <> if false exit then
   a u s" a/" STARTS-WITH? 0= if false exit then
   a 2 + ou oa ou STR= 0= if false exit then
   a 2 ou + + 3 s"  b/" STR= 0= if false exit then
   a 2 ou + 3 + + nu na nu STR= ;

: HEAD-VALIDATE ( ptr u8 n ptr u8 n -- )
   {: oa:ptr ou:n na:ptr nu:n :}
   oa ou NEED-PATH 2drop
   na nu NEED-PATH 2drop
   oa ou na nu HEAD-EXACT? 0= if FAIL then ;

: SAME-CANDIDATE? ( n -- bool ) {: split:n :}
   HEAD$ {: a:ptr u:n :}
   split 2 <= if false exit then
   split 3 + u >= if false exit then
   a split + 3 s"  b/" STR= 0= if false exit then
   split 2 - {: ou:n :}
   u split 3 + - {: nu:n :}
   ou nu <> if false exit then
   a 2 + ou a split 3 + + nu STR= 0= if false exit then
   a 2 + ou PATH-SAFE? ;

: HEAD-SAME-INFER ( -- )
   HEAD$ {: a:ptr u:n :}
   u 5 < if FAIL then
   u 5 - dup 2 mod 0<> if drop FAIL then
   2 / 2 + {: split:n :}
   split SAME-CANDIDATE? 0= if FAIL then
   a 2 + split 2 - {: pa:ptr pu:n :}
   pa pu SET-OLD
   pa pu SET-NEW ;

: HEAD-PARSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" diff --git " {: prefix:ptr prefixu:n :}
   a u prefix prefixu STARTS-WITH? 0= if FAIL then
   a prefixu + u prefixu - {: body:ptr bodyu:n :}
   bodyu 6 < if FAIL then
   body bodyu BYTES-SAFE? 0= if FAIL then
   body bodyu s" a/" STARTS-WITH? 0= if FAIL then
   body HEAD-A! bodyu HEAD-U ! ;

: MODE-OF ( ptr u8 n -- file-mode ) {: a:ptr u:n :}
   a u s" 100644" STR= if construct file-mode regular exit then
   a u s" 100755" STR= if construct file-mode executable exit then
   a u s" 120000" STR= if construct file-mode symlink exit then
   a u s" 160000" STR= if construct file-mode gitlink exit then
   FAIL ;

: MODE# ( file-mode -- n )
   MATCH file-mode
      regular    OF 0 ENDOF
      executable OF 1 ENDOF
      symlink    OF 2 ENDOF
      gitlink    OF 3 ENDOF
   ;MATCH ;

: MODE= ( file-mode file-mode -- bool )
   MODE# swap MODE# = ;

: HEX-C? ( n -- bool ) {: c:n :}
   c ZERO-C >= c NINE-C <= and if true exit then
   c A-C >= c F-C <= and ;

: HASH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u HASH-MIN < if false exit then
   u HASH-MAX > if false exit then
   0 begin dup u < while
      dup a + c@ HEX-C? 0= if drop false exit then
      1+
   repeat drop true ;

: ZERO-HASH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ ZERO-C <> if drop false exit then
      1+
   repeat drop true ;

: EMPTY-HASH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" e69de29bb2d1d6434b8b29ae775ad8c2e48c5391" a u STARTS-WITH? if true exit then
   s" e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" a u STARTS-WITH? ;

: UNIQUE-DOTS ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" .." FIND-SUB MATCH option
      none OF FAIL ENDOF
      some OF IDX>N {: split:n :}
         a split 2 + + u split 2 + - s" .." FIND-SUB MATCH option
            none OF split ENDOF
            some OF drop FAIL ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: INDEX-MODE! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MODE-OF drop
   true INDEX-MODE ! ;

: INDEX-RIGHT ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   false INDEX-MODE !
   a u SPACE-C INDEX-OF MATCH option
      none OF a u ENDOF
      some OF IDX>N {: split:n :}
         a split 1+ + u split 1+ - {: ma:ptr mu:n :}
         ma mu INDEX-MODE!
         a split
      ENDOF
   ;MATCH ;

: INDEX-HASHES ( ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr u:n :}
   a u UNIQUE-DOTS {: split:n :}
   a split {: oa:ptr ou:n :}
   a split 2 + + u split 2 + - INDEX-RIGHT {: na:ptr nu:n :}
   oa ou HASH? 0= if FAIL then
   na nu HASH? 0= if FAIL then
   ou nu <> if FAIL then
   oa ou na nu ;

: INDEX-SEMANTICS! ( ptr u8 n ptr u8 n -- )
   {: oa:ptr ou:n na:ptr nu:n :}
   oa ou na nu STR= if FAIL then
   oa ou ZERO-HASH? INDEX-OLD-ZERO !
   na nu ZERO-HASH? INDEX-NEW-ZERO !
   oa ou EMPTY-HASH? INDEX-OLD-EMPTY !
   na nu EMPTY-HASH? INDEX-NEW-EMPTY !
   NEW-FILE ON? if
      INDEX-OLD-ZERO ON? 0= INDEX-NEW-ZERO ON? or if FAIL then
      exit
   then
   DELETE-FILE ON? if
      INDEX-OLD-ZERO ON? INDEX-NEW-ZERO ON? 0= or if FAIL then
      exit
   then
   INDEX-OLD-ZERO ON? INDEX-NEW-ZERO ON? or if FAIL then ;

: INDEX-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   INDEXED ON? if FAIL then
   a u s" index " PREFIX-AFTER INDEX-HASHES INDEX-SEMANTICS!
   OLD-MODED ON? NEW-FILE ON? or DELETE-FILE ON? or if
      INDEX-MODE ON? if FAIL then
   else
      INDEX-MODE ON? 0= if FAIL then
   then
   true INDEXED ! ;

: DECIMAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   u 1 > if a c@ ZERO-C = if false exit then then
   0 begin dup u < while
      dup a + c@ dup ZERO-C < swap NINE-C > or if drop false exit then
      1+
   repeat drop true ;

: PERCENT ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n prefix:ptr prefixu:n :}
   a u prefix prefixu STARTS-WITH? 0= if FAIL then
   u prefixu 1+ <= if FAIL then
   a u 1- + c@ $25 <> if FAIL then
   a prefixu + u prefixu - 1- {: na:ptr nu:n :}
   na nu DECIMAL? 0= if FAIL then
   na nu STR>NUMBER? MATCH option
      none OF FAIL ENDOF
      some OF dup 100 > if drop FAIL then ENDOF
   ;MATCH ;

: NEED-META-OPEN ( -- )
   INDEXED ON? if FAIL then ;

: ADD-DELETE? ( -- bool )
   NEW-FILE ON? DELETE-FILE ON? or ;

: RENAME-COPY? ( -- bool )
   RENAMED-FROM ON? COPIED-FROM ON? or ;

: NAME-META? ( -- bool )
   RENAME-COPY? SIMILAR ON? or DISSIMILAR ON? or ;

: OLD-MODE-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   OLD-MODED ON? if FAIL then
   ADD-DELETE? if FAIL then
   a u s" old mode " PREFIX-AFTER MODE-OF MODE!
   true OLD-MODED ! ;

: NEW-MODE-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   OLD-MODED ON? 0= NEW-MODED ON? or if FAIL then
   a u s" new mode " PREFIX-AFTER MODE-OF MODE@ MODE= if FAIL then
   true NEW-MODED ! ;

: NEW-FILE-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   ADD-DELETE? OLD-MODED ON? or NAME-META? or if FAIL then
   a u s" new file mode " PREFIX-AFTER MODE-OF drop
   true NEW-FILE ! ;

: DELETE-FILE-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   ADD-DELETE? OLD-MODED ON? or NAME-META? or if FAIL then
   a u s" deleted file mode " PREFIX-AFTER MODE-OF drop
   true DELETE-FILE ! ;

: SIMILARITY-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   SIMILAR ON? DISSIMILAR ON? or ADD-DELETE? or if FAIL then
   a u s" similarity index " PERCENT SIM-N !
   true SIMILAR ! ;

: DISSIMILARITY-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   SIMILAR ON? DISSIMILAR ON? or ADD-DELETE? or RENAME-COPY? or if FAIL then
   a u s" dissimilarity index " PERCENT SIM-N !
   true DISSIMILAR ! ;

: RENAME-FROM-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   RENAME-COPY? ADD-DELETE? or DISSIMILAR ON? or if FAIL then
   a u s" rename from " PREFIX-AFTER SET-OLD
   true RENAMED-FROM ! ;

: RENAME-TO-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   RENAMED-FROM ON? 0= RENAMED-TO ON? or if FAIL then
   a u s" rename to " PREFIX-AFTER SET-NEW
   OLD$ NEW$ STR= if FAIL then
   OLD$ NEW$ HEAD-VALIDATE
   true RENAMED-TO ! ;

: COPY-FROM-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   RENAME-COPY? ADD-DELETE? or DISSIMILAR ON? or if FAIL then
   a u s" copy from " PREFIX-AFTER SET-OLD
   true COPIED-FROM ! ;

: COPY-TO-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-META-OPEN
   COPIED-FROM ON? 0= COPIED-TO ON? or if FAIL then
   a u s" copy to " PREFIX-AFTER SET-NEW
   OLD$ NEW$ STR= if FAIL then
   OLD$ NEW$ HEAD-VALIDATE
   true COPIED-TO ! ;

: PAIRS-COMPLETE ( -- )
   OLD-MODED @ NEW-MODED @ <> if FAIL then
   RENAMED-FROM @ RENAMED-TO @ <> if FAIL then
   COPIED-FROM @ COPIED-TO @ <> if FAIL then
   RENAMED-FROM ON? COPIED-FROM ON? and if FAIL then
   SIMILAR ON? if
      RENAMED-FROM ON? COPIED-FROM ON? or 0= if FAIL then
   then
   DISSIMILAR ON? if
      RENAMED-FROM ON? COPIED-FROM ON? or if FAIL then
   then ;

: SELECT-FORM ( -- form )
   RENAMED-FROM ON? if construct form rename exit then
   COPIED-FROM ON? if construct form copy exit then
   NEW-FILE ON? if construct form add-file exit then
   DELETE-FILE ON? if construct form delete-file exit then
   OLD-MODED ON? if construct form mode exit then
   construct form modify ;

: ADD-IDENTITY ( -- )
   HEAD-SAME-INFER
   NEW$ {: a:ptr u:n :}
   EMPTY-OLD
   a u SET-NEW ;

: DELETE-IDENTITY ( -- )
   HEAD-SAME-INFER
   OLD$ {: a:ptr u:n :}
   a u SET-OLD
   EMPTY-NEW ;

: PENDING-FORM ( -- form )
   PAIRS-COMPLETE
   INDEXED ON? if
      NEW-FILE ON? if
         INDEX-NEW-EMPTY ON? 0= if FAIL then
         ADD-IDENTITY construct form add-file exit
      then
      DELETE-FILE ON? if
         INDEX-OLD-EMPTY ON? 0= if FAIL then
         DELETE-IDENTITY construct form delete-file exit
      then
      FAIL
   then
   RENAMED-FROM ON? if
      SIMILAR ON? if SIM-N @ 100 <> if FAIL then then
      construct form rename exit
   then
   COPIED-FROM ON? if
      SIMILAR ON? if SIM-N @ 100 <> if FAIL then then
      construct form copy exit
   then
   OLD-MODED ON? if
      HEAD-SAME-INFER
      construct form mode exit
   then
   FAIL ;

: RESET-SECTION ( -- )
   0 OLD-U ! 0 NEW-U !
   false INDEXED ! false INDEX-MODE !
   false INDEX-OLD-ZERO ! false INDEX-NEW-ZERO !
   false INDEX-OLD-EMPTY ! false INDEX-NEW-EMPTY !
   false OLD-MODED ! false NEW-MODED !
   false NEW-FILE ! false DELETE-FILE !
   false RENAMED-FROM ! false RENAMED-TO !
   false COPIED-FROM ! false COPIED-TO !
   false SIMILAR ! false DISSIMILAR !
   0 SIM-N !
   false HUNK-SEEN ! false HUNK-CHANGED !
   construct mark-side no-mark MARK-SIDE!
   0 OLD-LEFT ! 0 NEW-LEFT !
   0 PREV-OLD-END ! 0 PREV-NEW-END ! ;

: START-HEAD ( ptr u8 n -- )
   HEAD-PARSE
   RESET-SECTION
   construct phase meta PHASE! ;

: CLOSE-AND-START ( ptr u8 n -- event ) {: a:ptr u:n :}
   PENDING-FORM {: kind:form :}
   OLD$ {: oa:ptr ou:n :}
   NEW$ {: na:ptr nu:n :}
   a u START-HEAD
   oa ou na nu kind false SECTION-SPANS ;

: NEED-TEXT-META ( -- )
   PAIRS-COMPLETE
   INDEXED ON? 0= if FAIL then
   SIMILAR ON? if SIM-N @ 100 = if FAIL then then
   NEW-FILE ON? if INDEX-NEW-EMPTY ON? if FAIL then then
   DELETE-FILE ON? if INDEX-OLD-EMPTY ON? if FAIL then then ;

: OLD-HEAD-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   NEED-TEXT-META
   NEW-FILE ON? if
      a u s" --- /dev/null" STR= 0= if FAIL then
   else
      a u s" --- a/" PREFIX-AFTER {: pa:ptr pu:n :}
      RENAMED-FROM ON? COPIED-FROM ON? or if
         pa pu OLD$ STR= 0= if FAIL then
      else
         pa pu SET-OLD
      then
   then
   construct phase new-head PHASE! ;

: NEW-HEAD-LINE ( ptr u8 n -- event ) {: a:ptr u:n :}
   DELETE-FILE ON? if
      a u s" +++ /dev/null" STR= 0= if FAIL then
      EMPTY-NEW
      OLD$ 2dup HEAD-VALIDATE
      EMPTY-NEW
   else
      a u s" +++ b/" PREFIX-AFTER {: pa:ptr pu:n :}
      RENAMED-FROM ON? COPIED-FROM ON? or if
         pa pu NEW$ STR= 0= if FAIL then
      else
         pa pu SET-NEW
      then
      NEW-FILE ON? if NEW$ 2dup HEAD-VALIDATE else OLD$ NEW$ HEAD-VALIDATE then
      NEW-FILE ON? if EMPTY-OLD then
   then
   construct phase body PHASE!
   SELECT-FORM true SECTION ;

: DIGIT-END ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   start begin dup u < while
      dup a + c@ dup ZERO-C < swap NINE-C > or if exit then
      1+
   repeat ;

: PARSE-U ( ptr u8 n n n -- n ) {: a:ptr u:n first:n last:n :}
   last first <= if FAIL then
   a first + last first - DECIMAL? 0= if FAIL then
   a first + last first - STR>NUMBER? MATCH option
      none OF FAIL ENDOF
      some OF ENDOF
   ;MATCH ;

: RANGE ( ptr u8 n n n -- n n n ) {: a:ptr u:n at:n sign:n :}
   at u >= if FAIL then
   a at + c@ sign <> if FAIL then
   at 1+ {: first:n :}
   a u first DIGIT-END {: last:n :}
   a u first last PARSE-U {: start:n :}
   last u < if
      a last + c@ COMMA-C = if
         last 1+ {: cf:n :}
         a u cf DIGIT-END {: cl:n :}
         a u cf cl PARSE-U
         start swap cl exit
      then
   then
   start 1 last ;

: RANGE-OK? ( n n -- bool ) {: start:n count:n :}
   count 0= if start 0 >= exit then
   start 0 > ;

: RANGE-END ( n n -- n ) {: start:n count:n :}
   start count + dup start < if drop FAIL then ;

: HUNK-ORDER! ( n n n n -- )
   {: old-start:n old-count:n new-start:n new-count:n :}
   HUNK-SEEN ON? if
      old-start PREV-OLD-END @ < if FAIL then
      new-start PREV-NEW-END @ < if FAIL then
   then
   old-start old-count RANGE-END PREV-OLD-END !
   new-start new-count RANGE-END PREV-NEW-END ! ;

: HUNK-FORM-VALIDATE ( n n n n -- )
   {: old-start:n old-count:n new-start:n new-count:n :}
   NEW-FILE ON? if
      old-start 0<> old-count 0<> or new-count 0= or if FAIL then
      exit
   then
   DELETE-FILE ON? if
      new-start 0<> new-count 0<> or old-count 0= or if FAIL then
   then ;

: HUNK-PARSE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" @@ -" STARTS-WITH? 0= if FAIL then
   a u 3 $2D RANGE {: old-start:n old-count:n at:n :}
   old-start old-count RANGE-OK? 0= if FAIL then
   at u >= if FAIL then
   a at + c@ SPACE-C <> if FAIL then
   a u at 1+ $2B RANGE {: new-start:n new-count:n tail:n :}
   new-start new-count RANGE-OK? 0= if FAIL then
   tail 3 + u > if FAIL then
   a tail + c@ SPACE-C <> if FAIL then
   a tail 1+ + c@ AT-C <> if FAIL then
   a tail 2 + + c@ AT-C <> if FAIL then
   tail 3 + u < if a tail 3 + + c@ SPACE-C <> if FAIL then then
   old-count 0= new-count 0= and if FAIL then
   old-start old-count new-start new-count HUNK-FORM-VALIDATE
   old-start old-count new-start new-count HUNK-ORDER!
   old-count OLD-LEFT ! new-count NEW-LEFT !
   false HUNK-CHANGED ! construct mark-side no-mark MARK-SIDE!
   true HUNK-SEEN !
   construct phase hunk PHASE!
   new-start ;

: HUNK-DONE? ( -- bool )
   OLD-LEFT @ 0= NEW-LEFT @ 0= and ;

: NEED-HUNK-DONE ( -- )
   HUNK-DONE? 0= if FAIL then
   HUNK-CHANGED @ 0= if FAIL then ;

: USE-OLD ( -- )
   OLD-LEFT @ 0 <= if FAIL then
   OLD-LEFT @ 1- OLD-LEFT ! ;

: USE-NEW ( -- )
   NEW-LEFT @ 0 <= if FAIL then
   NEW-LEFT @ 1- NEW-LEFT ! ;

: MARK-USED ( mark-side -- )
   MARK-SIDE! ;

: ADD-EVENT ( ptr u8 n -- event ) {: a:ptr u:n :}
   DELETE-FILE ON? if FAIL then
   USE-NEW true HUNK-CHANGED !
   construct mark-side new-side MARK-USED
   a 1+ u 1- construct event add CONTENT-EVENT ;

: DELETE-EVENT ( ptr u8 n -- event ) {: a:ptr u:n :}
   NEW-FILE ON? if FAIL then
   USE-OLD true HUNK-CHANGED !
   construct mark-side old-side MARK-USED
   a 1+ u 1- construct event delete CONTENT-EVENT ;

: CONTEXT-EVENT ( ptr u8 n -- event ) {: a:ptr u:n :}
   ADD-DELETE? if FAIL then
   USE-OLD USE-NEW
   construct mark-side both-sides MARK-USED
   a 1+ u 1- construct event context CONTENT-EVENT ;

: MARKER? ( ptr u8 n -- bool )
   s" \ No newline at end of file" STR= ;

: MARKER-EVENT ( -- event )
   MARK-SIDE@ MATCH mark-side
      no-mark OF FAIL ENDOF
      old-side OF OLD-LEFT @ 0<> if FAIL then ENDOF
      new-side OF NEW-LEFT @ 0<> if FAIL then ENDOF
      both-sides OF HUNK-DONE? 0= if FAIL then ENDOF
   ;MATCH
   construct mark-side no-mark MARK-SIDE!
   NONE ;

: HUNK-CONTENT ( ptr u8 n -- event ) {: a:ptr u:n :}
   a u MARKER? if MARKER-EVENT exit then
   u 0= if FAIL then
   a c@ dup $2B = if drop a u ADD-EVENT exit then
   dup $2D = if drop a u DELETE-EVENT exit then
   SPACE-C = if a u CONTEXT-EVENT exit then
   FAIL ;

: BINARY-NORMAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OLD$ {: oa:ptr ou:n :}
   NEW$ {: na:ptr nu:n :}
   15 ou + 7 + nu + 7 + u <> if false exit then
   a u s" Binary files a/" STARTS-WITH? 0= if false exit then
   a 15 + ou oa ou STR= 0= if false exit then
   a 15 ou + + 7 s"  and b/" STR= 0= if false exit then
   a 15 ou + 7 + + nu na nu STR= 0= if false exit then
   a 15 ou + 7 + nu + + 7 s"  differ" STR= ;

: BINARY-ADD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   NEW$ {: na:ptr nu:n :}
   29 nu + 7 + u <> if false exit then
   a u s" Binary files /dev/null and b/" STARTS-WITH? 0= if false exit then
   a 29 + nu na nu STR= 0= if false exit then
   a 29 nu + + 7 s"  differ" STR= ;

: BINARY-DELETE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OLD$ {: oa:ptr ou:n :}
   15 ou + 21 + u <> if false exit then
   a u s" Binary files a/" STARTS-WITH? 0= if false exit then
   a 15 + ou oa ou STR= 0= if false exit then
   a 15 ou + + 21 s"  and /dev/null differ" STR= ;

: BINARY-LINE ( ptr u8 n -- event ) {: a:ptr u:n :}
   NEED-TEXT-META
   NEW-FILE ON? if
      HEAD-SAME-INFER NEW$ {: na:ptr nu:n :}
      EMPTY-OLD na nu SET-NEW
      a u BINARY-ADD? 0= if FAIL then
   else DELETE-FILE ON? if
      HEAD-SAME-INFER OLD$ {: oa:ptr ou:n :}
      oa ou SET-OLD EMPTY-NEW
      a u BINARY-DELETE? 0= if FAIL then
   else
      RENAMED-FROM ON? COPIED-FROM ON? or 0= if HEAD-SAME-INFER then
      a u BINARY-NORMAL? 0= if FAIL then
   then then
   construct phase terminal PHASE!
   construct form binary false SECTION ;

: META-LINE ( ptr u8 n -- event ) {: a:ptr u:n :}
   a u s" diff --git " STARTS-WITH? if a u CLOSE-AND-START exit then
   a u s" old mode " STARTS-WITH? if a u OLD-MODE-LINE NONE exit then
   a u s" new mode " STARTS-WITH? if a u NEW-MODE-LINE NONE exit then
   a u s" new file mode " STARTS-WITH? if a u NEW-FILE-LINE NONE exit then
   a u s" deleted file mode " STARTS-WITH? if a u DELETE-FILE-LINE NONE exit then
   a u s" similarity index " STARTS-WITH? if a u SIMILARITY-LINE NONE exit then
   a u s" dissimilarity index " STARTS-WITH? if a u DISSIMILARITY-LINE NONE exit then
   a u s" rename from " STARTS-WITH? if a u RENAME-FROM-LINE NONE exit then
   a u s" rename to " STARTS-WITH? if a u RENAME-TO-LINE NONE exit then
   a u s" copy from " STARTS-WITH? if a u COPY-FROM-LINE NONE exit then
   a u s" copy to " STARTS-WITH? if a u COPY-TO-LINE NONE exit then
   a u s" index " STARTS-WITH? if a u INDEX-LINE NONE exit then
   a u s" --- " STARTS-WITH? if a u OLD-HEAD-LINE NONE exit then
   a u s" Binary files " STARTS-WITH? if a u BINARY-LINE exit then
   FAIL ;

: BODY-LINE ( ptr u8 n -- event ) {: a:ptr u:n :}
   a u s" @@ -" STARTS-WITH? if
      HUNK-SEEN ON? if NEED-HUNK-DONE then
      a u HUNK-PARSE HUNK-EVENT exit
   then
   a u s" diff --git " STARTS-WITH? if FAIL then
   FAIL ;

: HUNK-LINE ( ptr u8 n -- event ) {: a:ptr u:n :}
   HUNK-DONE? if
      a u s" @@ -" STARTS-WITH? if
         NEED-HUNK-DONE
         a u HUNK-PARSE HUNK-EVENT exit
      then
      a u s" diff --git " STARTS-WITH? if
         NEED-HUNK-DONE
         a u START-HEAD
         NONE exit
      then
   then
   a u HUNK-CONTENT ;

: TERMINAL-LINE ( ptr u8 n -- event ) {: a:ptr u:n :}
   a u s" diff --git " STARTS-WITH? 0= if FAIL then
   a u START-HEAD
   NONE ;

: EVENT# ( event -- n )
   MATCH event
      none OF 0 ENDOF
      section OF 1 ENDOF
      hunk OF 2 ENDOF
      add OF 3 ENDOF
      context OF 4 ENDOF
      delete OF 5 ENDOF
   ;MATCH ;

: NEED-EVENT ( event -- )
   EVENT# EVENT@ EVENT# <> if FAIL then ;

: CONTENT? ( event -- bool )
   MATCH event
      none OF false ENDOF
      section OF false ENDOF
      hunk OF false ENDOF
      add OF true ENDOF
      context OF true ENDOF
      delete OF true ENDOF
   ;MATCH ;

public

: SECTION-OLD$ ( -- ptr u8 n )
   construct event section NEED-EVENT
   EVENT-OLD-A@ EVENT-OLD-U @ ;

: SECTION-NEW$ ( -- ptr u8 n )
   construct event section NEED-EVENT
   EVENT-NEW-A@ EVENT-NEW-U @ ;

: SECTION-FORM ( -- form )
   construct event section NEED-EVENT
   EVENT-FORM@ ;

: SECTION-BODY? ( -- bool )
   construct event section NEED-EVENT
   EVENT-BODY @ 0<> ;

: HUNK-NEW-START ( -- n )
   construct event hunk NEED-EVENT
   EVENT-N @ ;

: HUNK-NEW-COUNT ( -- n )
   construct event hunk NEED-EVENT
   NEW-LEFT @ ;

: CONTENT$ ( -- ptr u8 n )
   EVENT@ CONTENT? 0= if FAIL then
   EVENT-A@ EVENT-U @ ;

: SOURCE-VALIDATE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   a u 1- + c@ LF-C <> if FAIL then ;

: RESET ( -- )
   RESET-SECTION
   0 HEAD-U ! construct event none EVENT-AT !
   construct phase idle PHASE! ;

: LINE ( ptr u8 n -- event ) {: a:ptr u:n :}
   a u LINE-SAFE? 0= if FAIL then
   PHASE@ MATCH phase
      idle OF
         a u s" diff --git " STARTS-WITH? 0= if FAIL then
         a u START-HEAD
         NONE
      ENDOF
      meta OF a u META-LINE ENDOF
      new-head OF a u NEW-HEAD-LINE ENDOF
      body OF a u BODY-LINE ENDOF
      hunk OF a u HUNK-LINE ENDOF
      terminal OF a u TERMINAL-LINE ENDOF
   ;MATCH ;

: FINISH ( -- event )
   PHASE@ MATCH phase
      idle OF NONE ENDOF
      meta OF PENDING-FORM false SECTION construct phase terminal PHASE! ENDOF
      new-head OF FAIL ENDOF
      body OF FAIL ENDOF
      hunk OF NEED-HUNK-DONE NONE ENDOF
      terminal OF NONE ENDOF
   ;MATCH ;

;package
