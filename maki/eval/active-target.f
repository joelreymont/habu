\ maki/eval/active-target.f - resolve THIS process's PTX target by probing the
\ CUDA driver, so the device path never hardcodes an sm_ arch.
\
\ cuInit + cuDeviceGet(0), then cuDeviceGetAttribute reads the compute-capability
\ major/minor (attribute ids 75/76). The (major,minor) pair maps to a registered
\ target descriptor: (8,7) -> sm_87 (Orin), (12,1) -> sm_121a (GB10). ANY other
\ pair is a named throw - fail closed, never a default target. The result is
\ interned once per process. Lives here in maki/eval (beside the device goldens
\ that consume it), not in lib: lib never requires maki.

require lib/errors.f
require maki/cuda-driver.f
require maki/target/target.f

-5429 constant E-ACTIVE-TARGET    \ compute capability maps to no registered target

package ATGT

private

75 constant CC-MAJOR-ATTR
76 constant CC-MINOR-ATTR

variable AT-DEV
variable AT-MAJ
variable AT-MIN
variable AT-READY                 \ 0 until the probe has resolved (and interned) the id
TYPED-VARIABLE AT-ID CAD-KIND:target-id

: PROBE ( -- n n )                \ (major minor) from the current CUDA device
   CUDA:OPEN
   0 CUDA:CUINIT CUDA:RC0
   AT-DEV 0 >IDX CUDA:CUDEVICEGET CUDA:RC0
   0 AT-MAJ !  0 AT-MIN !         \ zero the 8-byte cells; the attribute writes 4
   AT-MAJ CC-MAJOR-ATTR AT-DEV @ >CUDA-DEV CUDA:CUDEVICEGETATTRIBUTE CUDA:RC0
   AT-MIN CC-MINOR-ATTR AT-DEV @ >CUDA-DEV CUDA:CUDEVICEGETATTRIBUTE CUDA:RC0
   AT-MAJ @ AT-MIN @ ;

: MAP ( n n -- CAD-KIND:target-id )   \ compute capability -> registered target
   {: maj:n min:n :}
   maj 8  = min 7 = and if TARGET:SM87   exit then
   maj 12 = min 1 = and if TARGET:SM121A exit then
   E-ACTIVE-TARGET throw ;

public

\ the interned target for this process's GPU (resolved once, then cached)
: ID ( -- CAD-KIND:target-id )
   AT-READY @ if AT-ID @ exit then
   PROBE MAP dup AT-ID !  -1 AT-READY ! ;

\ the ptxas arch label (e.g. sm_121a) and the .version its ptxas requires
: LABEL$ ( -- ptr u8 n )   ID TARGET:LABEL$ ;
: VER$   ( -- ptr u8 n )   ID TARGET:ARCH-VER$ ;

;package
