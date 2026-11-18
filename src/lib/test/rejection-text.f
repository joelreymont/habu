split-str
\ rejection-text.f - rejection records as text a test compares, package
\ REJECTION-TEXT. FLAT drops each record's gutter, so expected text holds no
\ width that moves when lines are added above the record. A record runs from a
\ line starting `error[` or `warning[` to the next empty line; its gutter is the
\ blanks before its `-->`, 1 with none, and each later line that starts with that
\ many blanks or digits and a blank loses them and the blank.
\ STORAGE CLASS. PROCESS-WIDE: one buffer, which each FLAT reuses.

require src/lib/byte-buffer.f
require src/lib/string.f

package REJECTION-TEXT
private

create OUT BUF:HDR-BYTES allot
variable GUTTER
\ true from a record's header to the empty line after it
variable IN-RECORD

: EMIT+ ( str -- ) BUF:N>BLEN OUT BUF:APPEND-SPAN ;

: HEADER? ( str -- bool ) {: a:^ u:n :}
   a u s" error[" STARTS-WITH?  a u s" warning[" STARTS-WITH? or ;

\ Whether a u begins with the gutter: GUTTER blanks or digits, then a blank.
: GUTTERED? ( str -- bool ) {: a:^ u:n :}
   GUTTER @ {: g:n :}
   u g <= IF false EXIT THEN
   a g + c@ 32 <> IF false EXIT THEN
   g 0 ?DO
      a i + c@ {: c:n :}
      c 32 =  c STR-DIGIT? or 0= IF false unloop EXIT THEN
   LOOP true ;

\ The line a u, its newline excluded, onto OUT, its record's gutter dropped.
: LINE+ ( str -- ) {: a:^ u:n :}
   a u HEADER? IF 1 GUTTER !  true IN-RECORD !  a u EMIT+ EXIT THEN
   IN-RECORD @ 0= IF a u EMIT+ EXIT THEN
   u 0= IF false IN-RECORD ! EXIT THEN
   a u LTRIM {: t:^ tu:n :}
   t tu s" -->" STARTS-WITH? IF u tu - GUTTER !  t tu EMIT+ EXIT THEN
   a u GUTTERED? IF GUTTER @ 1 + {: g:n :}  a g +  u g -  EMIT+ EXIT THEN
   a u EMIT+ ;

public

\ The text a u with each record's gutter dropped, valid until the next FLAT.
: FLAT ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   OUT BUF:LIVE? 0= IF OUT 256 BUF:N>BLEN BUF:INIT THEN
   OUT BUF:CLEAR  false IN-RECORD !  1 GUTTER !
   a u 10 0 BEGIN SPLIT-NEXT WHILE {: p:^ pu:n next:n :}
      p pu LINE+
      next u <= IF 10 OUT BUF:APPEND-BYTE THEN
      a u 10 next
   REPEAT drop 2drop
   OUT BUF:SPAN$ BUF:BLEN>N ;

\ Whether text a u, its records' gutters dropped, holds e eu.
: HAS? ( ^u8 n ^u8 n -- bool ) {: a:^ u:n e:^ eu:n :}
   a u FLAT e eu CONTAINS? ;

;package
