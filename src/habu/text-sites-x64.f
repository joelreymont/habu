\ Read the exact x86-64 engine-text sites retained by X64CODE:TEXT-SITES,.
\ CODE-END-CELL points at their count header; fixed text VAs survive snapshots.
require lib/le.f
require src/habu/layout.f
require src/os/linux-x86-64/target-layout.f

package X64TEXT

private

74 constant SITE-RC
5 constant ROW-BYTES
16 constant FOOTER-BYTES
$3145544953343658 constant MAGIC
variable PREV

TRUSTED: RAW-PTR ( n -- ptr u8 ) ;

: BAD ( -- ) s" x64text: malformed engine text sites" SITE-RC die ;
: BASE-N ( -- n ) data-base RBASE-CELL + @ ;
: CODE-END-N ( -- n ) data-base CODE-END-CELL + @ ;
: RX-END-N ( -- n )
   BASE-N X64LAYOUT:CODE-OFF - {: image:n :}
   image $60 + RAW-PTR LE:U64@ {: size:n :}
   size 0 < size REGION-OFF > or if BAD then
   image size + ;
: ROW ( n n -- ptr u8 ) {: table:n idx:n :}
   table 4 + idx ROW-BYTES * + RAW-PTR ;
: WIDTH ( n -- n ) {: kind:n :}
   kind 0= if 1 exit then
   kind 1 = if 4 exit then
   kind 2 = if 8 exit then
   BAD ;
: ROUND-PAGE ( n -- n )
   PROT-PAGE-MAX 1- + PROT-PAGE-MAX 1- invert and ;

\ Validate the complete table and its immutable footer before yielding a site.
: TABLE ( -- n n )
   BASE-N {: base:n :}
   CODE-END-N {: table:n :}
   RX-END-N {: rx-end:n :}
   table base < table rx-end >= or if BAD then
   table 4 + rx-end > if BAD then
   table RAW-PTR LE:U32@ {: count:n :}
   count rx-end table - 4 - FOOTER-BYTES - ROW-BYTES / > if BAD then
   table 4 + count ROW-BYTES * + FOOTER-BYTES + ROUND-PAGE {: end:n :}
   end rx-end > if BAD then
   end FOOTER-BYTES - RAW-PTR {: footer:ptr :}
   footer LE:U32@ table base - <> if BAD then
   footer 4 + LE:U32@ count <> if BAD then
   footer 8 + LE:U64@ MAGIC <> if BAD then
   -1 PREV !
   count 0 ?do
      table i ROW {: row:ptr :}
      row LE:U32@ {: off:n :}
      row 4 + c@ WIDTH {: width:n :}
      off PREV @ <= off width + table base - > or if BAD then
      off PREV !
   loop
   table count ;

public

\ The callback receives a source field pointer and kind: 3 rel8, 4 rel32,
\ 5 abs64. These values do not overlap live REGION site kinds 1/2.
: EACH-IN-SPAN ( ptr u8 ptr u8 [ ptr u8 n -- ] -- )
   {: first:ptr last:ptr q :}
   TABLE {: table:n count:n :}
   first BASE-N RAW-PTR < last CODE-END-N RAW-PTR > or if BAD then
   count 0 ?do
      table i ROW {: row:ptr :}
      BASE-N row LE:U32@ + RAW-PTR {: site:ptr :}
      site first >= site last < and if
         site row 4 + c@ 3 + q execute
      then
   loop ;

;package
