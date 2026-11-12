\ arm64-link-data-bad.f - a program whose link LINKER:WRITE refuses, one per
\ process, chosen by the first script argument: `data`, a cell declared as a
\ DATA pointer that holds an address outside the build's space, read by the
\ entry, since a stripped link checks only the data its entry reaches;
\ `header`, a build's xt! into a header cell the link writes; `eval`, an entry
\ that calls `evaluate-closed`; `dict`, an entry that calls `search-wl`. The
\ second argument is the kernel object the link adopts, the third the path it
\ would write. test/compiler/arm64-link-data.f runs each and checks it.

require src/lib/error.f
require src/lib/string.f
require src/os/script-argv.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f

package ARM64-LINK-DATA-BAD

CAST: ENTRY-XT ( n -- [ -- ] )

: WORD-XT ( ptr u8 n -- [ -- ] ) XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT ;

: WRITE ( -- )
   s" ARM64-LINK-DATA-BAD-ARMED" type cr
   2 SCRIPT-ARGV$ LINKER:WRITE ;


: DATA-BODY ( -- )
   s" variable LD-BAD  1 LD-BAD !  LD-BAD ptr-cell-mark" evaluate-closed
   s" : LD-BAD-READ ( -- ) LD-BAD @ drop ;" evaluate-closed
   s" LD-BAD-READ" WORD-XT  data-base APP-ENTRY:XT-CELL + xt!  WRITE
   s" arm64-link-data-bad: the DATA pointer was not refused" 1 die ;


: HEADER-BODY ( -- )
   s" : LD-HB ( -- ) ;" evaluate-closed
   s" LD-HB" WORD-XT  data-base APP-ENTRY:XT-CELL + xt!
   s" LD-HB" WORD-XT  data-base UNCGH-CELL + xt!  WRITE
   s" arm64-link-data-bad: the header store was not refused" 1 die ;


: EVAL-BODY ( -- )
   s\" : LD-EV ( -- ) s\" \" evaluate-closed ;" evaluate-closed
   s" LD-EV" WORD-XT  data-base APP-ENTRY:XT-CELL + xt!  WRITE
   s" arm64-link-data-bad: the evaluate-closed call was not refused" 1 die ;


: DICT-BODY ( -- )
   s\" : LD-DW ( -- ) s\" LD-DW\" 0 search-wl drop ;" evaluate-closed
   s" LD-DW" WORD-XT  data-base APP-ENTRY:XT-CELL + xt!  WRITE
   s" arm64-link-data-bad: the wordlist lookup was not refused" 1 die ;


public

: RUN ( -- )
   1 SCRIPT-ARGV$ LINKER:KERNEL
   0 SCRIPT-ARGV$ {: at:ptr u:n :}
   at u s" data" STR= if s" app" RTARGET:HOST-TARGET [: DATA-BODY ;] LINKER:WITH exit then
   at u s" header" STR= if s" app" RTARGET:HOST-TARGET [: HEADER-BODY ;] LINKER:WITH exit then
   at u s" eval" STR= if s" app" RTARGET:HOST-TARGET [: EVAL-BODY ;] LINKER:WITH exit then
   at u s" dict" STR= if s" app" RTARGET:HOST-TARGET [: DICT-BODY ;] LINKER:WITH exit then
   s" arm64-link-data-bad: the case is data, header, eval or dict" 1 die ;

;package

ARM64-LINK-DATA-BAD:RUN
