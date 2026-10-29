\ standalone-load-test.f - prove tool and build-side entries load in isolation.
\ Run: bin/hb --load tools/standalone-load-test.f
\ A support or tool entry must `require` every dependency its own top-level path
\ uses, so `bin/hb --load <entry>` works without test/run.f's load order. Each
\ entry runs in a fresh child with empty stdin; exit 0 proves it pulled its
\ whole closure itself.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/outcome.f

2048 constant SL-CAP
20000 constant SL-TIMEOUT-MS

create SL-OUT SL-CAP allot
create SL-ERR SL-CAP allot
create SL-EMPTY 1 allot                 \ zero-length stdin
: SL-LOADS ( ptr u8 n -- )
   {: p:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   p u >LEN PROC-ARGV+
   s" bin/hb" >LEN  SL-EMPTY 0 >LEN  SL-OUT SL-CAP >LEN
   SL-ERR SL-CAP >LEN  SL-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME
   {: outu:len erru:len oc :}
   p u SL-OUT outu LEN>N SL-ERR erru LEN>N oc 0 T-OUTCOME-EXITED= ;

: SL-MAIN ( -- )
   T-RESET
   s" tools/lint/text.f loads standalone" T-LABEL
   s" tools/lint/text.f" SL-LOADS
   s" tools/lint/intern.f loads standalone" T-LABEL
   s" tools/lint/intern.f" SL-LOADS
   s" tools/public-signatures-core.f loads standalone" T-LABEL
   s" tools/public-signatures-core.f" SL-LOADS
   s" src/habu/aot-decl.f loads standalone" T-LABEL
   s" src/habu/aot-decl.f" SL-LOADS
   s" src/habu/aot-capture.f loads standalone" T-LABEL
   s" src/habu/aot-capture.f" SL-LOADS
   s" src/habu/aot-file.f loads standalone" T-LABEL
   s" src/habu/aot-file.f" SL-LOADS
   s" src/habu/aot-owned.f loads standalone" T-LABEL
   s" src/habu/aot-owned.f" SL-LOADS
   s" src/arch/arm64/mnem.f loads standalone" T-LABEL
   s" src/arch/arm64/mnem.f" SL-LOADS
   s" src/habu/aot-lib.f loads standalone" T-LABEL
   s" src/habu/aot-lib.f" SL-LOADS
   T-REPORT
   s" standalone-load-test: ok" type cr ;

SL-MAIN
