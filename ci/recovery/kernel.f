0 set-tier
require tools/hb-build-core.f
require src/lib/fs-mutate.f

package RECOVERY-KERNEL
private
: BUILD ( -- )
   true RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   HB-BUILD-CORE:KERNEL$ 1 SCRIPT-ARGV$ FS:COPY-FILE-STREAM ;
public
: RUN ( -- )
   SCRIPT-ARGC 2 <> if s" recovery-kernel: source-root output.hko" 64 die then
   0 SCRIPT-ARGV$ ['] BUILD SOURCE-ROOT:WITH ;
RUN
;package
