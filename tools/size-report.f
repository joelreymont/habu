\ size-report.f - render bin/hb size-attribution manifest from an engine size map.
\
\ The size map is the per-emitter-phase byte breakdown that src/habu/engine-size.f
\ prints during the final EMIT-FORTH pass when HABU_ENGINE_SIZE_MAP is set. Capture
\ it from a build, then render committed-manifest-style rows plus the code total,
\ engine file size, header/pad remainder, and distance-to-page-floor:
\
\   HABU_ENGINE_SIZE_MAP=1 bin/hb --load ...build... -- stdin > MAP 2>&1
\   bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f \
\     lib/adt/option.f tools/size-report.f -- MAP [ENGINE]
\
\ A build emits the map once per EMIT-FORTH pass; the final (installed-engine)
\ block wins, so LOAD keeps the last block only. ENGINE defaults to bin/hb.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/adt/option.f

package SIZE-REPORT

$4000 constant PAGE            \ 16 KiB page floor (campaign page unit; 4 KiB divides it)
256 constant ROW-CAP
10 constant NL
$20 constant SP
74 constant RC-IO

create ROW-NAME-A ROW-CAP cells allot
create ROW-NAME-U ROW-CAP cells allot
create ROW-VAL    ROW-CAP cells allot
variable ROW-N
variable MAP-A
variable MAP-U
variable LI-BEST
variable SCAN-CUR

: BLOCK-HEAD$ ( -- ptr u8 n )   s" main/startup" ;
: SRC-ROW$ ( -- ptr u8 n )      s" baked-source" ;

: SLOT ( n ptr a -- ptr a )
   {: idx:n base:ptr :}
   base idx cells + ;

: VALIDATE ( n -- n )
   {: idx:n :}
   idx 0 < idx ROW-N @ >= or if s" size-report: row out of range" RC-IO die then
   idx ;

: ROW-RESET ( -- )
   0 ROW-N ! ;

: ROW+ ( ptr u8 n n -- ) {: a:ptr u:n v:n :}
   ROW-N @ ROW-CAP >= if s" size-report: row capacity" RC-IO die then
   a ROW-N @ ROW-NAME-A SLOT !
   u ROW-N @ ROW-NAME-U SLOT !
   v ROW-N @ ROW-VAL SLOT !
   ROW-N @ 1+ ROW-N ! ;

\ Last byte offset in (a,u) where needle (na,nu) begins, else -1.
: LAST-INDEX ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n na:ptr nu:n :}
   -1 LI-BEST !
   0 begin dup u nu - <= while
      dup a + nu na nu STR= if dup LI-BEST ! then
      1+
   repeat drop LI-BEST @ ;

\ Byte offset of the final block: the last line starting with "main/startup".
: BLOCK-START ( -- n )
   MAP-A @ MAP-U @ BLOCK-HEAD$ LAST-INDEX
   dup 0 < if s" size-report: no size map in input" RC-IO die then ;

\ "name<sp>value" -> record a row; a line without a numeric value token is
\ skipped (build-log noise between the map rows is tolerated).
: PARSE-ROW ( ptr u8 n -- ) {: la:ptr lu:n :}
   la lu SP 0 SPLIT-NEXT {: na:ptr nu:n nx:n more:bool :}
   more 0= if exit then
   la lu SP nx SPLIT-NEXT {: va:ptr vu:n vnx:n vmore:bool :}
   vmore 0= if exit then
   va vu STR>NUMBER?
   MATCH option
      none OF ENDOF
      some OF {: v:n :} na nu v ROW+ ENDOF
   ;MATCH ;

\ Iterate lines of the final block, recording rows.
: SCAN-ROWS ( -- )
   ROW-RESET
   MAP-A @ MAP-U @ BLOCK-START {: a:ptr u:n start:n :}
   start SCAN-CUR !
   begin SCAN-CUR @ u <= while
      a u NL SCAN-CUR @ SPLIT-NEXT {: la:ptr lu:n nx:n more:bool :}
      more 0= if exit then
      lu 0 > if la lu PARSE-ROW then
      nx SCAN-CUR !
   repeat ;

public

: LOAD-BYTES ( ptr u8 n -- ) {: ma:ptr mu:n :}
   ma MAP-A ! mu MAP-U !
   SCAN-ROWS ;

: LOAD ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu FILE-SIZE 1+ MEM-ALLOC-64K-SPAN {: buf:ptr cap:n :}
   pa pu buf cap READ-ALL {: got:n :}
   buf got LOAD-BYTES ;

: COUNT ( -- n )
   ROW-N @ ;

: NAME$ ( n -- ptr u8 n )
   VALIDATE {: idx:n :}
   idx ROW-NAME-A SLOT @
   idx ROW-NAME-U SLOT @ ;

: VAL@ ( n -- n )
   VALIDATE ROW-VAL SLOT @ ;

\ SOME value for the named row (last occurrence), else NONE.
: FIND ( ptr u8 n -- option<n> ) {: qa:ptr qu:n :}
   0 begin dup ROW-N @ < while
      dup NAME$ qa qu STR= if VAL@ OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

\ Total attributed engine code = every row except the source blob.
: CODE-TOTAL ( -- n )
   0 0 begin dup ROW-N @ < while
      dup NAME$ SRC-ROW$ STR= 0= if dup VAL@ rot + swap then
      1+
   repeat drop ;

: .ROWS ( -- )
   0 begin dup ROW-N @ < while
      dup NAME$ type SP emit dup VAL@ .
      1+
   repeat drop ;

\ Committed-manifest-style attribution for ENGINE (ptr u8 n): per-phase rows,
\ code total, engine file size, header/pad remainder, and distance-to-page-floor
\ (bytes above the previous 16 KiB floor = the shave that recovers a page).
: PRINT ( ptr u8 n -- ) {: ea:ptr eu:n :}
   .ROWS
   s" code-total " type CODE-TOTAL .
   ea eu FILE-SIZE {: fsz:n :}
   s" engine " type fsz .
   s" header+pad " type fsz CODE-TOTAL - .
   s" page-floor-above " type fsz PAGE mod . ;

;package
