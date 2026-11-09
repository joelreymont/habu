\ next-hb.f - builds the next arm64 hb from this tree (tools/hb-build-core.f
\ HB-BUILD-CORE:ENGINE) and writes the program the argument names, with its
\ map beside it.
\ Run from the tree root: bin/hb --load tools/next-hb.f -- <program>

require src/lib/error.f
require src/habu/linker.f
require tools/hb-build-core.f

package NEXT-HB
private

: BUILD ( -- )
   0 SCRIPT-ARGV$ HB-BUILD-CORE:ENGINE
   s" compiled " type LINKER:COMPILED . cr ;


: FAILED ( n -- )
   {: rc:n :}
   s" next-hb: " type HB-BUILD-CORE:ROW$ type s"  threw " type rc .
   rc ERROR:NAME$ type cr
   s" next-hb: build failed" 1 die ;

public

: RUN ( -- )
   SCRIPT-ARGC 1 <> if s" usage: hb --load tools/next-hb.f -- <program>" 64 die then
   HB-TARGET-LINUX-X86-64? if s" next-hb: builds arm64 only" 64 die then
   ['] BUILD catch {: rc:n :}
   rc 0<> if rc FAILED then ;

;package

NEXT-HB:RUN
