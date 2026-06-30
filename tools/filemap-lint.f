\ filemap-lint.f - keep FILEMAP.md useful as an agent navigation index.
\ Load after lib/errors.f, lib/memory.f, lib/vector.f, tools/lint/text.f,
\ tools/lint/intern.f, tools/lint/token.f, and tools/lint/lib.f.

create FM-BUF $20000 allot
create FM-NUM 32 allot

variable FM-LEN
variable FM-I
variable FM-START
variable FM-BAD
variable FM-NUM-L

: FM-NL ( -- ) 10 emit ;

: FM-PATHISH? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" /" LINT-CONTAINS? IF LINT-TRUE exit THEN
   a u s" .md" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .sh" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .f" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .fs" HAS-EXT? IF LINT-TRUE exit THEN
   a u s" .tsv" HAS-EXT? ;

: FM-EXISTS? ( ptr u8 n -- bool )
   LINT-PATHZ PATHBUF 0 access 0= ;

: FM-PRINT-PATH ( ptr u8 n -- )
   96 emit type 96 emit ;

: FM-STALE ( ptr u8 n -- )
   s" FILEMAP-STALE FILEMAP.md: " type
   2dup FM-PRINT-PATH
   s"  does not exist" type FM-NL
   2drop
   FM-BAD @ 1+ FM-BAD ! ;

: FM-MISSING ( ptr u8 n -- )
   s" FILEMAP-MISSING FILEMAP.md: required entry " type
   2dup FM-PRINT-PATH
   s"  is absent" type FM-NL
   2drop
   FM-BAD @ 1+ FM-BAD ! ;

: FM-SCAN-BTICK ( -- )
   FM-I @ 1+ FM-START !
   FM-I @ 1+ FM-I !
   begin FM-I @ FM-LEN @ < while
      FM-BUF FM-I @ + c@ 96 = IF
         FM-BUF FM-START @ +  FM-I @ FM-START @ -  2dup FM-PATHISH? IF INTERN drop ELSE 2drop THEN
         FM-I @ 1+ FM-I !
         exit
      THEN
      FM-I @ 1+ FM-I !
   repeat ;

: FM-SCAN-PATHS ( -- )
   INTERN-RESET
   s" FILEMAP.md" FM-BUF $20000 READ-FILE nip FM-LEN !
   0 FM-I !
   begin FM-I @ FM-LEN @ < while
      FM-BUF FM-I @ + c@ 96 = IF
         FM-SCAN-BTICK
      ELSE
         FM-I @ 1+ FM-I !
      THEN
   repeat ;

: FM-CHECK-PATHS ( -- )
   0 begin dup INTERN# < while
      dup INTERN$ 2dup FM-EXISTS? 0= IF FM-STALE ELSE 2drop THEN
      1+
   repeat drop ;

: FM-REQ ( ptr u8 n -- )
   2dup INTERN? 0= IF FM-MISSING ELSE 2drop THEN ;

: FM-CHECK-REQUIRED-ROOT ( -- )
   s" AGENTS.md" FM-REQ
   s" LLM.md" FM-REQ
   s" LESSONS.md" FM-REQ
   s" STATUS.md" FM-REQ
   s" TRUSTED.md" FM-REQ
   s" docs/parallel-agents.md" FM-REQ
   s" docs/seed.md" FM-REQ ;

: FM-CHECK-REQUIRED-CORE ( -- )
   s" src/core/checker.f" FM-REQ
   s" src/core/render.f" FM-REQ
   s" src/core/exec-vector.f" FM-REQ
   s" src/core/sha256.f" FM-REQ
   s" src/os/linux/layout.f" FM-REQ
   s" src/os/macos/layout.f" FM-REQ
   s" src/os/image-bytes.f" FM-REQ
   s" src/os/linux/repl-term.f" FM-REQ
   s" src/os/macos/repl-term.f" FM-REQ
   s" src/habu/aot.f" FM-REQ
   s" src/habu/build.f" FM-REQ
   s" src/habu/maker.f" FM-REQ
   s" src/habu/debug-watch.f" FM-REQ
   s" src/habu/stepper.f" FM-REQ
   s" src/habu/debug.f" FM-REQ
   s" src/habu/layout.f" FM-REQ
   s" src/habu/hide.f" FM-REQ
   s" src/habu/xref.f" FM-REQ
   s" src/arch/ptx/emit.f" FM-REQ
   s" src/arch/arm64/disasm.f" FM-REQ ;

: FM-CHECK-REQUIRED-BUILD ( -- )
   s" tools/check-core.f" FM-REQ
   s" tools/check.f" FM-REQ
   s" tools/check-main.f" FM-REQ
   s" tools/check-test.f" FM-REQ
   s" tools/sha256-file-test.f" FM-REQ
   s" lib/content-key.f" FM-REQ
   s" lib/content-key-test.f" FM-REQ
   s" tools/hb-cli-contracts-test.f" FM-REQ
   s" tools/hb-baseline-contracts-test.f" FM-REQ
   s" tools/seed.f" FM-REQ
   s" tools/seed-main.f" FM-REQ
   s" tools/seed-test.f" FM-REQ
   s" tools/hb-build-lib.f" FM-REQ
   s" tools/hb-build-direct-lints.f" FM-REQ
   s" tools/hb-build.f" FM-REQ
   s" tools/hb-build-test.f" FM-REQ
   s" tools/warm-image-gate-stats.f" FM-REQ
   s" tools/imgdump.f" FM-REQ
   s" tools/imgdump-test.f" FM-REQ
   s" tools/ptx/saxpy.f" FM-REQ
   s" tools/ptx/saxpy-test.f" FM-REQ
   s" tools/ptx/ptxas-smoke.f" FM-REQ
   s" tools/build-fixpoint.f" FM-REQ
   s" tools/build-fixpoint-main.f" FM-REQ
   s" tools/build-fixpoint-test.f" FM-REQ ;

: FM-CHECK-REQUIRED-DIAG ( -- )
   s" tools/lint/json-writer.f" FM-REQ
   s" tools/lint/source-lex.f" FM-REQ
   s" tools/signature-lint-core.f" FM-REQ
   s" tools/signature-lint.f" FM-REQ
   s" tools/signature-lint-test.f" FM-REQ
   s" tools/reserved-name-lint-core.f" FM-REQ
   s" tools/reserved-name-lint.f" FM-REQ
   s" tools/reserved-name-lint-test.f" FM-REQ
   s" tools/duplicate-definition-lint-core.f" FM-REQ
   s" tools/duplicate-definition-lint.f" FM-REQ
   s" tools/duplicate-definition-lint-test.f" FM-REQ
   s" tools/aot-lint-core.f" FM-REQ
   s" tools/aot-lint.f" FM-REQ
   s" tools/aot-lint-test.f" FM-REQ
   s" tools/diag-origin-core.f" FM-REQ
   s" tools/diag-origin.f" FM-REQ
   s" tools/diag-origin-test.f" FM-REQ
   s" tools/json-only-core.f" FM-REQ
   s" tools/json-only.f" FM-REQ
   s" tools/gate-json-assert-core.f" FM-REQ
   s" tools/gate-json-assert.f" FM-REQ
   s" tools/repair-schema-doc-test.f" FM-REQ
   s" tools/repair-packet-core.f" FM-REQ
   s" tools/repair-packet-test.f" FM-REQ
   s" tools/check-repair-hints-test.f" FM-REQ
   s" lib/process-cwd.f" FM-REQ
   s" lib/process-cwd-test.f" FM-REQ
   s" tools/host-lint.f" FM-REQ
   s" tools/check-all-errors-core.f" FM-REQ
   s" tools/check-all-errors.f" FM-REQ
   s" tools/check-all-errors-test.f" FM-REQ
   s" tools/checked-boundary-lint-core.f" FM-REQ
   s" tools/checked-boundary-lint.f" FM-REQ
   s" tools/checked-boundary-lint-test.f" FM-REQ
   s" tools/diag-to-sarif.f" FM-REQ
   s" tools/public-signatures-core.f" FM-REQ
   s" tools/public-signatures.f" FM-REQ
   s" tools/public-signatures-test.f" FM-REQ
   s" tools/stdlib-manifest-test.f" FM-REQ ;

: FM-CHECK-REQUIRED-TOOLS ( -- )
   s" tools/aot-call-report-lib.f" FM-REQ
   s" tools/aot-call-report.f" FM-REQ
   s" tools/aot-call-report-test.f" FM-REQ
   s" tools/bundle-lib-core.f" FM-REQ
   s" tools/bundle-lib.f" FM-REQ
   s" tools/bundle-lib-test.f" FM-REQ
   s" tools/examples-test.f" FM-REQ
   s" tools/filemap-lint.f" FM-REQ
   s" tools/repl-lint-core.f" FM-REQ
   s" tools/repl-lint.f" FM-REQ
   s" tools/repl-lint-test.f" FM-REQ
   s" tools/trust-lint-core.f" FM-REQ
   s" tools/trust-lint.f" FM-REQ
   s" tools/trust-lint-test.f" FM-REQ
   s" tools/stale-status-lint-core.f" FM-REQ
   s" tools/stale-status-lint.f" FM-REQ
   s" tools/stale-status-lint-test.f" FM-REQ
   s" tools/parallel-agent-lint.f" FM-REQ
   s" tools/dot-dep-lint-core.f" FM-REQ
   s" tools/dot-dep-lint.f" FM-REQ
   s" tools/dot-dep-lint-test.f" FM-REQ
   s" tools/spawn-emitter-test.f" FM-REQ
   s" tools/c-call-emitter-test.f" FM-REQ
   s" tools/signature-scan-emitter-test.f" FM-REQ
   s" tools/compiler-dispatch-test.f" FM-REQ
   s" tools/string.f" FM-REQ
   s" lib/string-test.f" FM-REQ
   s" tools/date.f" FM-REQ
   s" tools/date-test.f" FM-REQ
   s" lib/process-env.f" FM-REQ
   s" lib/process-env-test.f" FM-REQ
   s" lib/source.f" FM-REQ
   s" lib/source-test.f" FM-REQ
   s" test/process-env-child.f" FM-REQ
   s" test/gate-pool.f" FM-REQ
   s" test/run.f" FM-REQ ;

: FM-CHECK-REQUIRED ( -- )
   FM-CHECK-REQUIRED-ROOT
   FM-CHECK-REQUIRED-CORE
   FM-CHECK-REQUIRED-BUILD
   FM-CHECK-REQUIRED-DIAG
   FM-CHECK-REQUIRED-TOOLS ;

: FM-U. ( n -- )
   0 FM-NUM-L !
   dup 0= IF drop 48 emit exit THEN
   begin dup 0 > while
      dup 10 mod 48 + FM-NUM FM-NUM-L @ + c!
      10 /
      FM-NUM-L @ 1+ FM-NUM-L !
   repeat drop
   begin FM-NUM-L @ 0 > while
      FM-NUM-L @ 1- FM-NUM-L !
      FM-NUM FM-NUM-L @ + c@ emit
   repeat ;

: FILEMAP-LINT ( -- )
   0 FM-BAD !
   FM-SCAN-PATHS
   FM-CHECK-PATHS
   FM-CHECK-REQUIRED
   s" filemap-lint: " type INTERN# FM-U. s"  path(s), " type FM-BAD @ FM-U. s"  finding(s)" type FM-NL
   FM-BAD @ 0 > IF 1 throw THEN ;

FILEMAP-LINT
