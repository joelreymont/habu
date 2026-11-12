\ arm64-place-unsafe-bad.f - a stripped program rooted at PL-MAKE, whose own body
\ calls `create`, which a stripped program cannot run: its link exits 74 before
\ it writes. The script arguments are the kernel object the link adopts and the
\ program's path. test/compiler/arm64-place.f runs it and checks the refusal.

require src/lib/error.f
require src/lib/string.f
require src/os/script-argv.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/target/model.f
require src/habu/linker.f
require src/habu/link-data.f

package ARM64-PLACE-UNSAFE

CAST: ENTRY-XT ( n -- [ -- ] )


: BODY ( -- )
   s" test/compiler/arm64-place-fixture.f" included
   s" PL-MAKE" XREF-FIND-INDEX XREF-REC XREF-START ENTRY-XT
   data-base APP-ENTRY:XT-CELL + xt!
   s" ARM64-PLACE-UNSAFE-ARMED" type cr
   1 SCRIPT-ARGV$ LINKER:WRITE
   s" arm64-place-unsafe: PL-MAKE was not refused" 1 die ;


public

: RUN ( -- )
   0 SCRIPT-ARGV$ LINKER:KERNEL
   s" app" RTARGET:HOST-TARGET [: BODY ;] LINKER:WITH ;

;package

ARM64-PLACE-UNSAFE:RUN
