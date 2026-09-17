\ process-env-overflow-child.f - drives the child environment table past its
\ ceiling on purpose.
\
\ The test spawns this program with a three-entry environment, so the ceiling
\ it must refuse at is exactly three inherited rows plus PROC-ENV-EXTRA. It
\ prints nothing: what the test reads is the refusal line lib/process-env.f
\ writes to stderr before throwing E-PROC-ENV.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

$4000 constant OVF-ROWS                  \ past any ceiling this envp can produce
variable OVF-I

: OVF-MAIN ( -- )
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   0 OVF-I !
   begin OVF-I @ OVF-ROWS < while
      s" HABU_OVERFLOW" >LEN s" x" >LEN PROC-ENV+
      OVF-I @ 1 + OVF-I !
   repeat
   s" overflow-child: no refusal" type cr ;

OVF-MAIN
