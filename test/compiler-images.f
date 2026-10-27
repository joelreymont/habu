\ The gate's compiler bundle is one image family. Its outputs live under the
\ gate root, whose lifetime extends past every row's private TMPDIR.
require src/lib/errors.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-id.f
require test/image-grant.f
require test/suite-budget.f

package COMPILER-IMAGES
private
$10000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

public
: ROOT$ ( -- ptr u8 n )
   s" HABU_GATE_COMPILER_IMAGES" GETENV ;

: BUILD ( -- )
   s" compiler-images" IMAGE-GRANT:CHECK
   ROOT$ nip 0= if
      s" suite compiler images: HABU_GATE_COMPILER_IMAGES is not set" 75 die
   then
   PROC-ARGV-ENV-RESET
   s" HABU_GATE_COMPILER_BUNDLE" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/compiler-images-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ROOT$ >LEN PROC-ARGV+
   ENGINE-ID:PATH$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   rc PROC-TIMEOUT-RC = if
      s" suite compiler images: initial build ran out of time" type cr
      E-PROC-TIMEOUT throw
   then
   rc 0<> if s" suite compiler images: initial build failed" rc die then ;
;package
