\ size-report.f - render bin/hb size-attribution manifest from an engine size map.
\
\ The size map is the per-region byte breakdown that src/habu/engine-size.f prints
\ during the final image emission (src/habu/driver-io.f DRV-SIZE-MAP) when
\ HABU_ENGINE_SIZE_MAP is set. It carries one row per emitter phase plus the
\ container rows the driver attributes post-sign - the header/load commands
\ (container/header), the zero fill to the __TEXT page (container/text-pad), and
\ the target tail (Mach-O: container/data-const + container/linkedit +
\ container/signature; ELF: container/rw-segment). Every byte of the file lands on
\ a named row, so the rows sum to the exact engine file size.
\
\   HABU_ENGINE_SIZE_MAP=1 <stdin-metabuild-host> > MAP 2>&1
\   bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f \
\     lib/adt/option.f tools/size-report.f tools/size-report-main.f -- MAP [ENGINE]
\
\ The map's final block wins (LOAD keeps the last block); ENGINE defaults to bin/hb.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/adt/option.f

package SIZE-REPORT

$4000 constant MACOS-PAGE      \ __TEXT segment page on macOS (16 KiB)
$1000 constant LINUX-PAGE      \ text segment page on Linux (4 KiB)
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
variable ACC

: BLOCK-HEAD$ ( -- ptr u8 n )   s" main/startup" ;
: SRC-ROW$ ( -- ptr u8 n )      s" baked-source" ;
: CONTAINER$ ( -- ptr u8 n )    s" container/" ;
: RESIDUE-ROW$ ( -- ptr u8 n )  s" unattributed-residue" ;

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

\ A container row is one the driver attributed post-__text (header, page pad, and
\ the target tail); the rest are the emitter-phase code rows plus baked-source.
: CONTAINER? ( n -- bool )
   NAME$ CONTAINER$ STARTS-WITH? ;

: BAKED? ( n -- bool )
   NAME$ SRC-ROW$ STR= ;

\ Emitter-phase machine code: neither a container region nor the baked data blob.
: PHASE-CODE? ( n -- bool ) {: idx:n :}
   idx CONTAINER? 0= idx BAKED? 0= and ;

private
\ One row-summing loop over the rows a predicate accepts; the four public totals
\ below are this loop under a fixed row predicate.
\ typed-local-lint: allow-bare-local - q carries the row predicate effect from the stack signature.
: SUM-ROWS ( [ n -- bool ] -- n ) {: q :}
   0 ACC !
   0 begin dup ROW-N @ < while
      dup q execute if dup VAL@ ACC @ + ACC ! then
      1+
   repeat drop ACC @ ;
public

: SUM-ALL ( -- n )        [: drop true ;] SUM-ROWS ;
: SUM-CONTAINER ( -- n )  [: CONTAINER? ;] SUM-ROWS ;

\ Whole emitted __text = every non-container row (emitter phases + baked-source).
: SUM-TEXT ( -- n )       [: CONTAINER? 0= ;] SUM-ROWS ;

\ Emitter-phase code only (excludes baked-source data).
: CODE-TOTAL ( -- n )     [: PHASE-CODE? ;] SUM-ROWS ;

: HEADER-BYTES ( -- n )
   s" container/header" FIND MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

: PAGE ( -- n )
   HB-TARGET-MACOS? if MACOS-PAGE exit then
   HB-TARGET-LINUX? if LINUX-PAGE exit then
   s" size-report: unknown build target" RC-IO die ;

\ Bytes of the text segment (header + __text) above its page floor: the exact
\ shave of emitted code that recovers one page from the file.
: FLOOR-DIST ( -- n )
   HEADER-BYTES SUM-TEXT + PAGE mod ;

\ Residue = the file bytes no row attributes. Zero when the map is complete.
: RESIDUE ( ptr u8 n -- n ) {: ea:ptr eu:n :}
   ea eu FILE-SIZE SUM-ALL - ;

: .KV ( ptr u8 n n -- ) {: la:ptr lu:n v:n :}
   la lu type v . ;

: .ROWS ( -- )
   0 begin dup ROW-N @ < while
      dup NAME$ type SP emit dup VAL@ .
      1+
   repeat drop ;

\ Full attribution for ENGINE (ptr u8 n): the per-region rows, the code/text/
\ container subtotals, the file size, distance-to-page-floor, and the residue. A
\ nonzero residue is emitted as its own named row (unattributed-residue), never
\ folded into a remainder.
: PRINT ( ptr u8 n -- ) {: ea:ptr eu:n :}
   .ROWS
   s" code-total " CODE-TOTAL .KV
   s" engine-text " SUM-TEXT .KV
   s" container-total " SUM-CONTAINER .KV
   s" attributed " SUM-ALL .KV
   ea eu FILE-SIZE {: fsz:n :}
   s" engine-file " fsz .KV
   s" page-floor-distance " FLOOR-DIST .KV
   fsz SUM-ALL - {: residue:n :}
   residue 0 <> if RESIDUE-ROW$ residue .KV then ;

\ Fail-closed reconciliation: exit status 0 only when every byte is attributed.
\ PRINT already itemises the unattributed-residue row; this enforces the exit
\ status so an under-reporting map is a loud failure, not a silent remainder.
: RECONCILE ( ptr u8 n -- ) {: ea:ptr eu:n :}
   ea eu RESIDUE 0 <> if
      s" size-report: engine bytes not fully attributed" RC-IO die
   then ;

;package
