\ verify-diagnostics.f - schema-1 fault records for quiet source verification.
\ Returned bytes belong to this package and last until its next record.

require src/lib/json.f
require src/habu/verify-source.f

package VERIFY-DIAGNOSTICS
private

create OUT BUF:HDR-BYTES allot
TYPED-VARIABLE W JSON:writer
variable READY

: START ( -- ptr JSON:writer )
   READY @ 0= if
      OUT 256 BUF:N>BLEN BUF:INIT
      1 READY !
   then
   W OUT JSON:WRITE-OPEN-BUF JSON:WRITE-OBJECT-START
   s" schema_version" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA ;

: END$ ( ptr JSON:writer -- ptr u8 n )
   JSON:WRITE-OBJECT-END JSON:WRITTEN$ ;

: ORIGIN ( ptr u8 n -- n n ) {: src:ptr at:n :}
   1 0 at 0 ?do
      src i + c@ 10 = if drop 1+ i 1+ then
   loop
   at swap - 1+ ;

: TOKEN-END ( ptr u8 n n -- n ) {: src:ptr u:n at:n :}
   at begin dup u < if src over + c@ 32 > else false then while
      1+
   repeat ;

public

\ The code, repair class, suggestion and short message of a statement that
\ threw the given code. The message is empty for E-STATEMENT-THROW, which only
\ the throw number tells apart.
: THROW-CODE$ ( n -- ptr u8 n ptr u8 n ptr u8 n ptr u8 n ) {: rc:n :}
   rc VERIFY:E-MISSING-SIGNATURE = if
      s" E-MISSING-SIGNATURE" s" add_signature"
      s" Write the definer's stack effect as ( inputs -- outputs ) right after the name, or right after does> for its clause."
      s" missing signature" exit
   then
   rc ENGINE-ERROR:SEAL-PACKAGE = if
      s" E-SEALED-PACKAGE" s" use_own_package"
      s" The package is sealed: define the word in a package of your own."
      s" sealed package" exit
   then
   rc VERIFY:E-SECTION-WORD = if
      s" E-SECTION-WORD" s" rename_section_word_clash"
      s" The token is the section word and a word of that name is in scope: rename the word or call it qualified."
      s" section word with a word of that name in scope" exit
   then
   s" E-STATEMENT-THROW" s" unknown_rejection"
   s" Inspect the token, signature, and raw stack evidence." s" " ;

: THROW-RECORD$ ( n n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: rc:n at:n name:ptr nameu:n src:ptr srcu:n why:ptr whyu:n :}
   src at ORIGIN {: line:n col:n :}
   src srcu at TOKEN-END {: last:n :}
   rc THROW-CODE$ nip {: code:ptr codeu:n class:ptr classu:n sug:ptr sugu:n msgu:n :}
   START
   s" code" code codeu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" repair_class" class classu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" verdict" s" rejected" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" token" src at + last at - JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   whyu 0<> if
      s" reason" why whyu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   then
   s" file" name nameu JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" line" line JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" column" col JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" byte_start" at JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   s" byte_end" last JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   msgu 0= if
      s" throw_code" rc JSON:WRITE-FIELD-INT JSON:WRITE-COMMA
   then
   s" suggestion" sug sugu JSON:WRITE-FIELD-S
   END$ ;

;package
