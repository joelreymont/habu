0 set-tier
require tools/hb-build-core.f

package HB-BUILD-CORE
public
: RECOVERY-LINK ( -- )
   RTARGET:HOST-TARGET {: target:RTARGET:resolved-target :}
   target TARGET !
   1 SCRIPT-ARGV$ OUT-LEN ! OUT-PATH !
   PATHS ENGINE-END!
   2 SCRIPT-ARGV$ LINKER:KERNEL
   NULL-PTR ROW-PATH ! 0 ROW-LEN !
   s" engine" target ['] ENGINE-BODY LINKER:WITH ;
: RECOVERY-RUN ( -- )
   SCRIPT-ARGC 3 <> if s" recovery-engine: source-root output-hb input.hko" 64 die then
   0 SCRIPT-ARGV$ ['] RECOVERY-LINK SOURCE-ROOT:WITH ;
;package

HB-BUILD-CORE:RECOVERY-RUN
