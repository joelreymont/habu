\ diagnose-hb-test.f - fixtures for the hb-outside-repo diagnostic.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f tools/diagnose-hb-core.f
\   tools/diagnose-hb-test.f

require tools/diagnose-hb-core.f
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

package DHT

$4000 constant CAP
10000 constant TIMEOUT-MS
18 constant COMMON-N

variable OUT-A
variable ERR-A
create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U

: OUT-A-FIELD ( -- ptr ptr u8 )
   OUT-A 0 ptr-field ;

: ERR-A-FIELD ( -- ptr ptr u8 )
   ERR-A 0 ptr-field ;

: OUT@ ( -- ptr u8 )
   OUT-A-FIELD @ ;

: ERR@ ( -- ptr u8 )
   ERR-A-FIELD @ ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: TARGET-N ( -- n )
   HB-TARGET-LINUX? if 2 exit then
   HB-TARGET-MACOS? if 2 exit then
   0 ;

: SETUP ( -- )
   CLEANUP-RESET
   CAP MEM-ALLOC-BYTES drop OUT-A-FIELD !
   CAP MEM-ALLOC-BYTES drop ERR-A-FIELD !
   s" habu-diagnose" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U !
   ROOT$ CLEANUP-TREE+ ;

\ An outside root names the first unresolved prefix file and counts them all.
: OUTSIDE-NAMES-MISSING ( -- )
   ROOT$ DIAGNOSE:ROOT!
   DIAGNOSE:SCAN
   s" outside root is flagged missing" T-LABEL
   DIAGNOSE:MISSING? TTRUE
   s" outside root names first prefix file" T-LABEL
   DIAGNOSE:MISSING$ s" src/core/util.f" T$=
   s" all baked prefix sources are checked" T-LABEL
   DIAGNOSE:CHECKED# COMMON-N TARGET-N + T= ;

\ The repo root (test cwd) resolves every prefix source.
: REPO-ROOT-OK ( -- )
   s" ." DIAGNOSE:ROOT!
   DIAGNOSE:SCAN
   s" repo root resolves all prefix sources" T-LABEL
   DIAGNOSE:MISSING? TFALSE ;

: ARGV+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: BUILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" ARGV+
   s" lib/errors.f" ARGV+
   s" lib/string.f" ARGV+
   s" lib/memory.f" ARGV+
   s" lib/fs.f" ARGV+
   s" tools/diagnose-hb-core.f" ARGV+
   s" tools/diagnose-hb.f" ARGV+
   s" --" ARGV+
   ROOT$ ARGV+ ;

\ The CLI entry exits nonzero and prints the unresolved file to stdout.
: CLI-REPORTS ( -- )
   BUILD-ARGV
   s" bin/hb" >LEN OUT@ CAP >LEN ERR@ CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME {: outu:len erru:len kind:n code:n :}
   s" cli exits nonzero outside repo" T-LABEL
   code 0 T<>
   s" cli stdout names the prefix file" T-LABEL
   OUT@ outu LEN>N s" src/core/util.f" CONTAINS? TTRUE ;

: MAIN ( -- )
   T-RESET
   SETUP
   OUTSIDE-NAMES-MISSING
   REPO-ROOT-OK
   CLI-REPORTS
   CLEANUP-RUN
   T-REPORT ;

MAIN

end-package
