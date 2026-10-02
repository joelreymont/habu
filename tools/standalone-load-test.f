\ standalone-load-test.f - prove tool and build-side entries load in isolation.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f tools/standalone-load-test.f
\
\ Regression guard for habu-standalone-support-load-7c3d9f16: a support/tool entry
\ must `require` the exact deps its own top-level path uses, so `bin/hb --load
\ <entry>` works WITHOUT the resident test/run.f DAG ordering. Each entry is
\ spawned in a fresh child engine with empty stdin; a clean exit 0 proves the
\ entry pulled its whole dependency closure itself. Before the fix,
\ tools/public-signatures-core.f died E-UNDEFINED: COPY-UPPER (its tools/lint/text.f
\ dep was unrequired) and tools/lint/intern.f was likewise dep-implicit.
\ src/habu/aot-capture.f died `using: unknown package: AOT-BUF` (aot-decl.f and
\ aot-arm.f unrequired), and src/habu/aot-decl.f died E-UNDEFINED:
\ AOT-SECTION-CAP (src/arch/arm64/icode.f unrequired); every other loader
\ brought those files in first. src/habu/aot-file.f, and src/habu/aot-owned.f
\ through it, died the same way (aot-decl.f unrequired), src/arch/arm64/mnem.f
\ died E-UNDEFINED: EMITW (icode.f unrequired), and src/habu/aot-lib.f died
\ E-UNDEFINED: XDS (mnem.f unrequired; its loaders brought it in through
\ src/habu/app-image.f).

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

2048 constant SL-CAP
20000 constant SL-TIMEOUT-MS

create SL-OUT SL-CAP allot
create SL-ERR SL-CAP allot
create SL-EMPTY 1 allot                 \ zero-length stdin
variable SL-RC
variable SL-EXITED

: SL-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF SL-RC ! 0 0= SL-EXITED ! ENDOF
     signaled OF SL-RC ! 0 0= 0= SL-EXITED ! ENDOF
     timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH
   LEN>N drop LEN>N drop ;

\ Spawn `bin/hb --load <path>` with empty stdin; assert a clean exit 0.
: SL-LOADS ( ptr u8 n -- ) {: p:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   p u >LEN PROC-ARGV+
   s" bin/hb" >LEN  SL-EMPTY 0 >LEN  SL-OUT SL-CAP >LEN
   SL-ERR SL-CAP >LEN  SL-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME SL-STORE!
   SL-EXITED @ TTRUE  SL-RC @ 0 T= ;

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
