split-str
\ arm64-link-data-bad.f - a program whose link LINKER:WRITE refuses, one per
\ process, chosen by the first script argument: `data`, a cell declared as a
\ DATA pointer that holds an address outside the build's space, read by the
\ entry, since a stripped link checks only the data its entry reaches;
\ `header`, an empty entry, refused only by a malformed kernel object when the
\ link writes the header cells; `eval`, an entry that calls `evaluate-closed`.
\ The second argument is the kernel object the link adopts, the third the path
\ it would write.
\ test/compiler/arm64-link-data.f runs each and checks it.

require src/core/exit-status.f
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

: WORD-XT ( str -- [ -- ] ) XREF-FIND XREF-START ENTRY-XT ;

: WRITE ( -- )
   s" ARM64-LINK-DATA-BAD-ARMED" type cr
   2 SCRIPT-ARGV$ LINKER:WRITE ;


: DATA-BODY ( -- )
   s" variable LD-BAD  1 LD-BAD !  LD-BAD ptr-cell-mark" evaluate-closed
   s" : LD-BAD-READ ( -- ) LD-BAD @ drop ;" evaluate-closed
   s" LD-BAD-READ" WORD-XT  APP-ENTRY:XT!  WRITE
   s" arm64-link-data-bad: the DATA pointer was not refused" EXIT-STATUS:FAILURE die ;


: HEADER-BODY ( -- )
   s" : LD-HB ( -- ) ;" evaluate-closed
   s" LD-HB" WORD-XT  APP-ENTRY:XT!  WRITE
   s" arm64-link-data-bad: the kernel object was not refused" EXIT-STATUS:FAILURE die ;


: EVAL-BODY ( -- )
   s\" : LD-EV ( -- ) s\" \" evaluate-closed ;" evaluate-closed
   s" LD-EV" WORD-XT  APP-ENTRY:XT!  WRITE
   s" arm64-link-data-bad: the evaluate-closed call was not refused" EXIT-STATUS:FAILURE die ;


public

: RUN ( -- )
   1 SCRIPT-ARGV$ LINKER:KERNEL
   0 SCRIPT-ARGV$ {: at:^ u:n :}
   at u s" data" STR= if s" app" RTARGET:HOST-TARGET [: DATA-BODY ;] LINKER:WITH exit then
   at u s" header" STR= if s" app" RTARGET:HOST-TARGET [: HEADER-BODY ;] LINKER:WITH exit then
   at u s" eval" STR= if s" app" RTARGET:HOST-TARGET [: EVAL-BODY ;] LINKER:WITH exit then
   s" arm64-link-data-bad: the case is data, header or eval" EXIT-STATUS:FAILURE die ;

;package

ARM64-LINK-DATA-BAD:RUN
