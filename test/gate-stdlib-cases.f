require src/lib/test/suite.f

STDLIB-GATE:MAIN

\ Registry order is start order among ready rows. A row whose load closure needs
\ an unsettled image waits out of the pool and starts at the first start after
\ its images settle, ahead of every later unstarted row (src/lib/test/suite.f
\ PENDING). Long rows lead so they do not start late and set the gate's tail.

\ Measure with `/usr/bin/time bin/hb --load <file>`.

\ Each `false [if]` block holds tests that are off until their target's step:
\ Wasm, then x86-64.

TEST:SUITE lsp-boundary
   tools/lsp-test.f
   tools/lsp-entry-test.f
TEST:;SUITE

TEST:SUITE cross-engine-build
   test/cross-engine-build.f
TEST:;SUITE

\ RFC 6455 connections on the HTTP server, spoken to by a Habu client over
\ TCP4; writes its transcript to a file and compares it whole.
TEST:SUITE websocket
   src/lib/net/ws-test.f
TEST:;SUITE

\ The Wasm and browser modules, validated by wasm-tools and run by bun; both on
\ PATH are a suite requirement (docs/bootstrap.md "Requirements").
TEST:SUITE wasm-device
   test/wasm/device.f
TEST:;SUITE

TEST:SUITE xml-owner-consumer
   test/xml-owner-e2e.f
TEST:;SUITE

TEST:SUITE engine-stack-machine
   test/engine-stack-machine.f
TEST:;SUITE

TEST:SUITE main-routing
   test/main-routing.f
TEST:;SUITE

TEST:SUITE aot-shifted-bytes
   test/aot-shifted-bytes.f
TEST:;SUITE

TEST:SUITE diag-position
   test/diag-position-test.f
TEST:;SUITE

TEST:SUITE source-root-exe
   test/source-root-exe-test.f
TEST:;SUITE

TEST:SUITE require-check
   test/require-check.f
TEST:;SUITE

TEST:SUITE codesign
   src/lib/codesign-test.f
TEST:;SUITE

TEST:SUITE checker-redefinition-order
   test/checker-redefinition-order-suite.f
TEST:;SUITE

TEST:SUITE checker-rollback-signature
   test/checker-rollback-signature-suite.f
TEST:;SUITE

TEST:SUITE signature-nesting
   test/signature-nesting.f
TEST:;SUITE

TEST:SUITE memory-guarded-failure
   test/memory-guarded-failure.f
TEST:;SUITE

TEST:SUITE memory-fault
   test/memory-fault.f
TEST:;SUITE

TEST:SUITE byte-copy
   test/byte-copy-test.f
TEST:;SUITE

TEST:SUITE imagedisasm-tool
   tools/imagedisasm-test.f
TEST:;SUITE

TEST:SUITE streaming-sha256
   tools/sha256-file-test.f
TEST:;SUITE

TEST:SUITE content-key
   src/lib/content-key-test.f
TEST:;SUITE

\ These manifests execute the native schema and case checks without a prover.
TEST:SUITE compiler-checker-model-manifest
   test/compiler/checker-model-manifest.f
TEST:;SUITE

TEST:SUITE linker
   test/linker.f
TEST:;SUITE

HB-TARGET-LINUX-X86-64? [if]
TEST:SUITE x86-64-link-run
   test/compiler/x86-64-link-run.f
TEST:;SUITE

TEST:SUITE code-dump-x64
   test/code-dump-x64.f
TEST:;SUITE
[then]

\ The target/policy binding: src/compiler/digest.f, target.f, numeric-policy.f
\ and binding.f through their public words. A row of its own is reachable by
\ name in the registry, not only inside a fork list.
TEST:SUITE compiler-target-resolve
   test/compiler/target-resolve.f
TEST:;SUITE

TEST:SUITE compiler-target-policy
   test/compiler/target-policy.f
TEST:;SUITE

TEST:SUITE compiler-workspace
   test/compiler/workspace.f
TEST:;SUITE

\ The N01-N06 and W32 numeric rows of test/wasm/numeric-rows.f, each forked and
\ its printed bytes compared.
TEST:SUITE wasm-numeric
   test/wasm/numeric.f
TEST:;SUITE

\ WLEB, tier 0's Wasm LEB128: the shortest and padded forms at their
\ edges, the call and address fields rewritten in place, and each rejection.
TEST:SUITE wasm-leb
   test/wasm/leb.f
TEST:;SUITE

\ WLINK, tier 0's Wasm linker, src/arch/wasm/link.f: modules wasm-tools
\ assembles byte for byte, index and type order, patched call and address
\ fields, the table, data cells and memory pages, W03 and each rejection.
TEST:SUITE wasm-link
   test/wasm/link.f
TEST:;SUITE

\ Some of src/arch/wasm/kernel-words.f's words beside the engine's primitives, on the host.
TEST:SUITE wasm-kernel-words
   test/wasm/kernel.f
TEST:;SUITE

TEST:SUITE compiler-arm32-asm
   test/compiler/arm32-asm.f
TEST:;SUITE

TEST:SUITE compiler-x86-64-asm
   test/compiler/x86-64-asm.f
TEST:;SUITE

\ Tests that need an arm64 host live in test/gate-arm-cases.f; tests for every
\ host stay here.
package STDLIB-GATE
private
: HOST-ROWS ( -- )
   HB-TARGET-LINUX-X86-64? 0= if s" test/gate-arm-cases.f" included then ;
HOST-ROWS
;package

TEST:SUITE compiler-tic6x-asm
   test/compiler/tic6x-asm.f
   test/compiler/tic6x-facts.f
   test/compiler/tic6x-sim.f
   test/compiler/tic6x-eabi.f
TEST:;SUITE

TEST:SUITE compiler-native-eval-prefix
   test/compiler/native-eval-prefix.f
TEST:;SUITE

TEST:SUITE prim-parity
   test/prim-parity.f
TEST:;SUITE

\ The engine's own `.` and `u.` over MIN-N, read back through a genio device.
\ The parity gate cannot hold these: a printer's answer is text on a device, not
\ a cell a case column can state.
TEST:SUITE print-min-int
   test/print-min-int.f
TEST:;SUITE

TEST:SUITE genio
   src/lib/genio-test.f
TEST:;SUITE

TEST:SUITE loop-obligations
   test/loop-obligations.f
TEST:;SUITE

TEST:SUITE native-quote-forward
   test/native-quote-forward-e2e.f
TEST:;SUITE

TEST:SUITE compiler-native-hookless-reject
   test/compiler/native-hookless-reject.f
TEST:;SUITE

TEST:SUITE caught-text-keeps
   test/caught-text-keeps.f
TEST:;SUITE

TEST:SUITE compiler-native-plusloop
   test/compiler/native-plusloop.f
TEST:;SUITE

TEST:SUITE compiler-native-rstack-input
   test/compiler/native-rstack-input.f
TEST:;SUITE

TEST:SUITE finally
   test/finally.f
TEST:;SUITE

TEST:SUITE quotation-locals
   test/quotation-locals.f
TEST:;SUITE

TEST:SUITE compiler-native-lexical-recovery
   test/compiler/native-lexical-recovery.f
TEST:;SUITE

TEST:SUITE compiler-native-local-case
   test/compiler/native-local-case.f
TEST:;SUITE

TEST:SUITE compiler-native-local-ambiguity
   test/compiler/native-local-ambiguity.f
TEST:;SUITE

TEST:SUITE compiler-native-product-locals
   test/compiler/native-product-locals.f
TEST:;SUITE

TEST:SUITE compiler-native-word-binding
   test/compiler/native-word-binding.f
TEST:;SUITE

TEST:SUITE reopen-binding
   test/reopen-binding.f
TEST:;SUITE

TEST:SUITE undefine-binding
   test/undefine-binding.f
TEST:;SUITE

TEST:SUITE compiler-integer-literals
   test/compiler/integer-literals.f
TEST:;SUITE

TEST:SUITE compiler-divide-literal
   test/compiler/divide-literal.f
TEST:;SUITE

TEST:SUITE interpreter-number
   test/interpreter-number.f
TEST:;SUITE

TEST:SUITE compiler-native-eval
   test/compiler/native-eval.f
TEST:;SUITE

TEST:SUITE compiler-native-wide-mem
   test/compiler/native-wide-mem.f
TEST:;SUITE

TEST:SUITE compiler-native-vocab
   test/compiler/native-vocab.f
TEST:;SUITE

TEST:SUITE raw-storage-load-seal
   test/raw-storage-load-seal-test.f
TEST:;SUITE

TEST:SUITE raw-cell-pointer-rejections
   test/compiler/raw-cell-pointer-rejections.f
TEST:;SUITE

TEST:SUITE stored-type-param
   test/compiler/stored-type-param.f
TEST:;SUITE

TEST:SUITE width-type
   test/compiler/width-type.f
TEST:;SUITE

TEST:SUITE input-underflow-rejections
   test/compiler/input-underflow-rejections.f
TEST:;SUITE

TEST:SUITE zero-operand-rejections
   test/compiler/zero-operand-rejections.f
TEST:;SUITE

TEST:SUITE base-pointer-arith-rejections
   test/compiler/base-pointer-arith-rejections.f
TEST:;SUITE

TEST:SUITE proc-capture-under-signals
   test/proc-capture-signal.f
TEST:;SUITE

TEST:SUITE proc-pty-io-supervisor-smoke
   test/process-pty-io-smoke.f
TEST:;SUITE

TEST:SUITE proc-pty-tty-smoke
   test/process-pty-tty-smoke.f
TEST:;SUITE

\ CPU tasking over pthread, including shared storage and task-local FFI staging.
TEST:SUITE tasking-threads
   src/lib/task-test.f
TEST:;SUITE

TEST:SUITE task-signal-altstack
   test/task-signal-altstack.f
TEST:;SUITE

TEST:SUITE task-prepare-failure
   test/task-prepare-failure.f
TEST:;SUITE

\ Readiness, timers and cancellation on io_uring, over pipes only.
TEST:SUITE aio-uring
   src/lib/aio-test.f
TEST:;SUITE

\ Process signals on the engine's baked stub and a self-pipe.
TEST:SUITE process-signals
   src/lib/signal-test.f
TEST:;SUITE

\ The bounded queue: many producers and many consumers over one ring.
TEST:SUITE bounded-queue
   src/lib/queue-test.f
TEST:;SUITE

TEST:SUITE task-entry
   test/task-entry.f
TEST:;SUITE

TEST:SUITE string-helpers
   src/lib/string-test.f
TEST:;SUITE

TEST:SUITE utf8-scalar
   src/lib/utf8-scalar-test.f
TEST:;SUITE

TEST:SUITE utf8-lossy
   src/lib/utf8-lossy-test.f
TEST:;SUITE

TEST:SUITE base64
   src/lib/base64-test.f
TEST:;SUITE

TEST:SUITE base85
   src/lib/base85-test.f
TEST:;SUITE

TEST:SUITE utf16-units
   src/lib/utf16-test.f
TEST:;SUITE

TEST:SUITE encoding-decode
   src/lib/encoding-test.f
   src/lib/encoding/iso-2022-jp-test.f
TEST:;SUITE

TEST:SUITE be-integers
   src/lib/be-test.f
TEST:;SUITE

TEST:SUITE file-uri
   src/lib/uri-test.f
TEST:;SUITE

\ Exact reads and full writes over pipes; the write cases run under the
\ profiler's SIGALRM storm to force short writes.
TEST:SUITE fd
   src/lib/fd-test.f
TEST:;SUITE

TEST:SUITE content-length
   src/lib/content-length-test.f
TEST:;SUITE

TEST:SUITE json-rpc
   src/lib/json-rpc-test.f
TEST:;SUITE

TEST:SUITE sparql
   src/lib/sparql/syntax-test.f
   src/lib/sparql/store-test.f
TEST:;SUITE

TEST:SUITE regex
   src/lib/regex-test.f
TEST:;SUITE

TEST:SUITE json-schema
   src/lib/json-schema-test.f
TEST:;SUITE

TEST:SUITE diff
   src/lib/diff-test.f
TEST:;SUITE

TEST:SUITE ffi-abi
   src/lib/ffi-abi-test.f
TEST:;SUITE

TEST:SUITE svg-pdf
   test/svg-pdf-e2e.f
TEST:;SUITE

TEST:SUITE zip
   src/lib/zip-test.f
   src/lib/zip-lifecycle-test.f
   src/lib/zip-offset-test.f
   test/zip-remove-source.f
TEST:;SUITE

TEST:SUITE zip-temp
   test/zip-temp-e2e.f
TEST:;SUITE

TEST:SUITE ffi-cabi
   src/lib/ffi-test.f
TEST:;SUITE

\ Raw DEFLATE and CRC-32 through libz; keeps its 10 MiB original, compressed and
\ decoded payloads.
TEST:SUITE zlib
   test/zlib-source.f
TEST:;SUITE

\ FONT on the host's installed fonts; HarfBuzz and the fonts are a gate
\ requirement (docs/bootstrap.md "Requirements").
TEST:SUITE font-native
   test/font-native.f
TEST:;SUITE

\ C calling back into checked code through libc's qsort and pthread_create, on
\ the calling task and on foreign threads; writes
\ build/ffi-callback-transcript.txt and compares it whole.
TEST:SUITE ffi-callback
   src/lib/ffi-callback-test.f
TEST:;SUITE

\ More than 129 native C entries, stable pointers through growth, high-row
\ lifecycle and fallback.
TEST:SUITE ffi-callback-growth
   test/ffi-callback-growth.f
TEST:;SUITE

\ Package PG and the SPARQL term runtime against a private PostgreSQL cluster
\ (test/db/pg-cluster.f), then the harness killed by a pool, which must leave no
\ server process and no socket directory. initdb and postgres on PATH are a gate requirement
\ (docs/bootstrap.md "Requirements").
TEST:SUITE pg
   test/db/pg-cluster.f
   test/db/pg-kill-test.f
TEST:;SUITE

\ Package SQLITE through libsqlite3, on database files in its own temporary
\ directory.
TEST:SUITE sqlite
   src/lib/sqlite-test.f
TEST:;SUITE

\ Loopback HTTP in one process; the one HTTPS request is opt-in behind
\ HABU_NET_TESTS.
TEST:SUITE curl-http
   src/lib/net/curl-test.f
TEST:;SUITE

\ HTTPC's connection lifecycle against a scripted loopback HTTP/1.1 server, and
\ its https origins against openssl s_server on loopback, with certificates
\ made at test time. mbedTLS 4 and openssl are a gate requirement
\ (docs/bootstrap.md "Requirements").
HB-TARGET-LINUX? [if]
TEST:SUITE httpc
   src/lib/net/httpc-test.f
TEST:;SUITE
[then]

\ TLS client sessions over mbedTLS against openssl s_server on loopback, with
\ certificates made at test time. mbedTLS 4 and openssl are a gate requirement
\ (docs/bootstrap.md "Requirements").
HB-TARGET-LINUX? [if]
TEST:SUITE tls
   src/lib/net/tls-test.f
TEST:;SUITE
[then]

\ The HTTP/1.1 server on a loopback port, answered by CURL and by raw TCP4;
\ writes build/http-transcript.txt and compares it whole.
TEST:SUITE http
   src/lib/net/http-test.f
TEST:;SUITE

\ HTTP:BASIC-CREDENTIALS: RFC 7617's example, each `:` place, the rejections.
TEST:SUITE http-basic
   src/lib/net/http-basic-test.f
TEST:;SUITE

\ MCP:HANDLE on a loopback port: both eras' handshakes, tools/list and
\ tools/call, and the protocol's rejections, answered to CURL.
TEST:SUITE mcp
   src/lib/net/mcp-test.f
TEST:;SUITE

\ BEARER:ADMIT? on a loopback port that serves its own key set.
TEST:SUITE bearer
   src/lib/net/bearer-test.f
TEST:;SUITE

\ MCP:ROUTES on a loopback port: the Origin check, BEARER:CALLER? and the
\ metadata routes.
TEST:SUITE mcp-routes
   src/lib/net/mcp-routes-test.f
TEST:;SUITE

TEST:SUITE odata
   src/lib/odata-test.f
TEST:;SUITE

\ SECRET's words with no store bound and over a host store. Its own suite:
\ STORE! holds SECRET's store for the whole process.
TEST:SUITE secret
   src/lib/secret-test.f
TEST:;SUITE

\ SECRET:DIR-BIND over a directory under TMPDIR. Its own suite: the bind holds
\ SECRET's store for the whole process.
TEST:SUITE secret-dir
   src/lib/secret/dir-test.f
TEST:;SUITE

\ SECRET:AZURE-BIND against a loopback metadata service and vault. Its own
\ suite: the bind holds SECRET's store for the whole process.
TEST:SUITE secret-azure
   src/lib/secret/azure-test.f
TEST:;SUITE

\ SECRET:WRITE and SECRET:READ on Linux against a private bus and keyring, with
\ secret-tool. Its own suite: SECRET:LINUX-ITEM! and libsecret's connection
\ hold for the whole process.
HB-TARGET-LINUX? [if]
TEST:SUITE secret-linux
   src/lib/secret/linux-test.f
TEST:;SUITE
[then]

\ MODEL against an in-process OpenAI- and Ollama-shaped peer on a loopback port.
TEST:SUITE model
   src/lib/net/model-test.f
TEST:;SUITE

\ TEST-ISSUER's routes on a loopback port and tools/test-issuer.f as a child.
TEST:SUITE test-issuer
   src/lib/net/test-issuer-test.f
TEST:;SUITE

\ MODEL:CHAT against TEST-MODEL's replies on a loopback port.
TEST:SUITE test-model
   src/lib/net/test-model-test.f
TEST:;SUITE

\ The HTTP listener through an accept rejected for want of descriptors. Its own
\ row: it lowers RLIMIT_NOFILE and puts a pipe in place of stderr.
TEST:SUITE http-accept-starved
   test/http-accept-starved.f
TEST:;SUITE

\ BROWSER-HOST:ROUTES on a loopback port: CURL fetches the browser host's page,
\ each file's content type and bytes.
TEST:SUITE browser-host
   src/lib/browser/host-test.f
TEST:;SUITE

\ MCP-APP:PAGE+ and CODE+ of a small module: the kept prefix, the escaped
\ title, the page id and app.js's base64 in one UTF-8 page, and CODE+'s same
\ page id and the base64 of the view's files and the module; PAGE-ID$ as the
\ page's id, FILE+'s member, and the rejected title.
TEST:SUITE mcp-app
   src/lib/browser/app-test.f
TEST:;SUITE

\ STUDIO:KEY-FIT's ground record, planes and pixel for stated scenes, and
\ the matrices it rejects.
TEST:SUITE studio
   src/lib/browser/studio-test.f
TEST:;SUITE

TEST:SUITE crypto-evp
   src/lib/crypto/evp-test.f
TEST:;SUITE

TEST:SUITE crypto-sha1
   src/lib/crypto/sha1-test.f
TEST:;SUITE

TEST:SUITE float-parse
   src/lib/float-test.f
   src/lib/fmath-test.f
   src/lib/fmath-basis-test.f
TEST:;SUITE

TEST:SUITE finite-float-text
   src/lib/f64-text-test.f
TEST:;SUITE

TEST:SUITE ieee-float32
   src/lib/ieee754-test.f
   src/lib/float32-test.f
   src/lib/float32-buffer-test.f
TEST:;SUITE

TEST:SUITE fmt-numbers
   src/lib/fmt-test.f
TEST:;SUITE

\ State machines DEFINE builds from a table, their checker rejections and size.
TEST:SUITE fsm
   src/lib/fsm-test.f
TEST:;SUITE

\ A table in a source file rendered as its TLA+ model's Table module.
TEST:SUITE test-fsm
   src/lib/test-fsm-test.f
TEST:;SUITE

\ Rotations of a 3D affine transform about a line, and points mapped by one.
TEST:SUITE affine
   src/lib/affine-test.f
TEST:;SUITE

TEST:SUITE log
   src/lib/log-test.f
TEST:;SUITE

TEST:SUITE error
   src/lib/error-test.f
TEST:;SUITE

TEST:SUITE reason
   src/lib/reason-test.f
TEST:;SUITE

TEST:SUITE hashmap
   src/lib/hashmap-test.f
TEST:;SUITE

TEST:SUITE siphash
   src/lib/siphash-test.f
TEST:;SUITE

TEST:SUITE array-helpers
   src/lib/array-test.f
TEST:;SUITE

TEST:SUITE num-arithmetic
   src/lib/num-arithmetic-test.f
TEST:;SUITE

TEST:SUITE map-stdlib
   src/lib/map-test.f
TEST:;SUITE

TEST:SUITE codegen-stdlib
   src/lib/codegen-test.f
TEST:;SUITE

TEST:SUITE unicode-casefold
   src/lib/unicode-test.f
TEST:;SUITE

TEST:SUITE unicode-class-exhaustive
   tools/unicode/class-verify-main.f
TEST:;SUITE

TEST:SUITE unicode-script-exhaustive
   tools/unicode/script-verify-main.f
TEST:;SUITE

TEST:SUITE encoding-exhaustive
   tools/encoding/verify-main.f
TEST:;SUITE

TEST:SUITE argv-stdlib-mocks
   src/lib/argv-test.f
TEST:;SUITE

TEST:SUITE test-stdlib
   src/lib/test/assert-test.f
   src/lib/test/mapped-test.f
   src/lib/test/stderr-test.f
TEST:;SUITE

TEST:SUITE date-helpers
   tools/stdlib-date-test.f
TEST:;SUITE

TEST:SUITE memory
   src/lib/memory-test.f
   src/lib/arena-test.f
TEST:;SUITE

TEST:SUITE tail-pure-fixtures
   src/lib/json-write-test.f
   src/lib/json-read-test.f
   src/lib/json-pointer-test.f
   src/lib/jsonl-test.f
   src/lib/decimal-test.f
   src/lib/vector-test.f
   src/lib/elf32-test.f
   src/lib/fs-test.f
   tools/asm-src-test.f
   tools/image-bytes-test.f
TEST:;SUITE

TEST:SUITE json-value
   src/lib/json-value-test.f
TEST:;SUITE

TEST:SUITE byte-buffer
   src/lib/byte-buffer-test.f
TEST:;SUITE

\ The x86_64 seam is exercised from this aarch64 host: the mov r64, imm64
\ relocation site and the x86-64 target contract.
TEST:SUITE x86-64-seam
   test/x86-64-seam.f
TEST:;SUITE

\ The same seam's instruction emitters, whose bytes an aarch64 engine can produce
\ and read back. Its own suite because it loads src/os/linux-x86-64/sys.f, which
\ spells the host's syscall-number words.
TEST:SUITE x86-64-emit
   test/x86-64-emit.f
TEST:;SUITE

\ The x86-64 boot skeleton, src/habu/boot-x64.f, and its guard-page twin, for the
\ peer to run.
TEST:SUITE x86-64-skel-image
   test/x86-64-skel-image.f
TEST:;SUITE

\ Both captured entry cells in a real x86-64 boot: ARM emits the image and
\ the x86-64 host also runs it, requiring MAIN to invoke APP before stdin.
TEST:SUITE x86-64-main-entry
   test/x86-64-main-entry.f
TEST:;SUITE

\ The x86-64 kernel's rows (src/habu/kernel-x64.f) in the booted harness,
\ test/x86-64-boot-harness.f, for the peer to run. They load the x86-64 seam
\ globally, as skel-image does, so each is a suite.
TEST:SUITE x86-64-kernel-syscalls
   test/x86-64-kernel-syscalls.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-control
   test/x86-64-kernel-control.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-policy
   test/x86-64-kernel-policy.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-atomics
   test/x86-64-kernel-atomics.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-engine
   test/x86-64-kernel-engine.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-scope-find
   test/x86-64-kernel-scope-find.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-token
   test/x86-64-kernel-token.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-ffi
   test/x86-64-kernel-ffi.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-pure
   test/x86-64-kernel-pure.f
TEST:;SUITE

\ The sampling profiler's rows, src/habu/prof-x64.f, in the booted harness.
TEST:SUITE x86-64-kernel-prof
   test/x86-64-kernel-prof.f
TEST:;SUITE
\ The signal stub src/habu/boot-x64.f START, publishes, installed and raised in
\ the booted harness.
TEST:SUITE x86-64-boot-signal
   test/x86-64-boot-signal.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-task
   test/x86-64-kernel-task.f
TEST:;SUITE

TEST:SUITE x86-64-kernel-callback
   test/x86-64-kernel-callback.f
TEST:;SUITE

\ The crash handler the x86-64 boot installs, src/habu/boot-x64.f, in the
\ booted harness: each image's child faults with fd 2 on a pipe, and its parent
\ checks the dump or the guard page's line and the exit status.
TEST:SUITE x86-64-kernel-crash
   test/x86-64-kernel-crash.f
TEST:;SUITE

\ The definition writers of the x86-64 kernel in the booted harness.
TEST:SUITE x86-64-kernel-definition
   test/x86-64-kernel-definition.f
TEST:;SUITE

TEST:SUITE xml-byte-edits
   src/lib/xml-test.f
   src/lib/byte-edit-test.f
   src/lib/xml-roundtrip-test.f
   src/lib/xml-source-test.f
TEST:;SUITE

TEST:SUITE stdlib-source-default
   src/lib/source-test.f
TEST:;SUITE

TEST:SUITE source-lex
   src/lib/source-lex-test.f
   src/lib/word-docs-test.f
TEST:;SUITE

TEST:SUITE stdlib-process-fixtures
   tools/hb-cli-contracts-test.f
   src/lib/process-test.f
   src/lib/process-command-test.f
   src/lib/power-test.f
   src/lib/process-pty-handle-test.f
TEST:;SUITE

TEST:SUITE seal
   test/seal.f
TEST:;SUITE

TEST:SUITE pointer-storage
   test/pointer-storage-test.f
TEST:;SUITE

TEST:SUITE ptr-elem
   test/ptr-elem-test.f
TEST:;SUITE

TEST:SUITE typed-storage
   test/typed-storage-test.f
TEST:;SUITE

TEST:SUITE typed-storage-structural
   test/typed-storage-structural-test.f
TEST:;SUITE

TEST:SUITE storage-binding
   test/storage-binding-e2e.f
TEST:;SUITE

TEST:SUITE c2-records-source
   test/c2-records-program.f
   test/c2-records-rejections.f
   test/c2-records-lifecycle.f
   test/c2-records-scopes.f
TEST:;SUITE

TEST:SUITE c2-view-record-source
   test/c2-view-record-source.f
   test/c2-view-record-rejections.f
TEST:;SUITE

TEST:SUITE c2-bytes-rejections
   test/c2-bytes-rejections.f
TEST:;SUITE

TEST:SUITE c2-read-effects
   test/c2-read-effects.f
TEST:;SUITE

TEST:SUITE c2-mut-view
   test/c2-mut-view.f
TEST:;SUITE

TEST:SUITE c2-mut-records
   test/c2-mut-records.f
TEST:;SUITE

TEST:SUITE c2-loan-checker
   test/c2-loan-checker-positive.f
   test/c2-loan-checker-region.f
   test/c2-loan-checker-rejection.f
TEST:;SUITE

TEST:SUITE c2-memory-runtime
   test/c2-memory-runtime.f
TEST:;SUITE

TEST:SUITE c2-memory-source
   test/c2-memory-native-boundary.f
   test/c2-memory-program.f
   test/c2-init-program.f
   test/c2-memory-rejections.f
   test/c2-memory-scope-rejections.f
   test/c2-init-rejections.f
   test/c2-owner-producer-program.f
   test/c2-owner-producer-rejections.f
   test/c2-owner-dispose.f
   test/c2-forall-input-rejections.f
   test/c2-memory-entry.f
   test/c2-field-loan-kind.f
   test/c2-field-loan-program.f
   test/c2-field-loan-rejections.f
   test/c2-field-loan-lifecycle.f
TEST:;SUITE

TEST:SUITE c2-view-record-mut
   test/c2-view-record-mut.f
TEST:;SUITE

TEST:SUITE certify-does-definer
   test/certify-does-definer.f
TEST:;SUITE

TEST:SUITE underdepth-gate
   test/underdepth-gate.f
TEST:;SUITE

TEST:SUITE top-row-storage
   test/top-row-storage-test.f
TEST:;SUITE

TEST:SUITE xt-effect
   test/xt-effect-test.f
TEST:;SUITE

TEST:SUITE xt-cell
   test/xt-cell-test.f
TEST:;SUITE

TEST:SUITE catch-stale
   test/catch-stale-suite.f
TEST:;SUITE

TEST:SUITE create-axiom
   test/create-axiom-test.f
   test/prim-row-test.f
TEST:;SUITE

TEST:SUITE ndict-binding
   test/ndict-binding.f
TEST:;SUITE

TEST:SUITE engine-span-end
   test/engine-span-end.f
TEST:;SUITE

TEST:SUITE checker-soundness
   test/checker-soundness-suite.f
TEST:;SUITE

TEST:SUITE checker-verify-pkg-scope
   test/checker-verify-pkg-scope.f
TEST:;SUITE

TEST:SUITE checker-dup-record
   test/checker-dup-record.f
TEST:;SUITE

TEST:SUITE checker-completion
   test/checker-completion.f
TEST:;SUITE

TEST:SUITE checker-dead-path
   test/checker-dead-path-suite.f
TEST:;SUITE

TEST:SUITE local-spelling
   test/local-spelling-suite.f
TEST:;SUITE

TEST:SUITE stdlib-standalone-load
   test/stdlib-standalone-load.f
TEST:;SUITE

TEST:SUITE definition-name
   test/definition-name.f
TEST:;SUITE

TEST:SUITE address-cell-rollback
   test/address-cell-rollback.f
TEST:;SUITE

TEST:SUITE address-cell-cap-grown
   test/address-cell-cap-grown.f
TEST:;SUITE

TEST:SUITE address-cell-storage-oom
   test/address-cell-storage-oom.f
TEST:;SUITE

TEST:SUITE catch-frame
   test/catch-frame.f
TEST:;SUITE

TEST:SUITE engine-stack-wide
   test/engine-stack-wide.f
TEST:;SUITE

TEST:SUITE engine-stack-lifecycle
   test/engine-stack-lifecycle.f
TEST:;SUITE

TEST:SUITE engine-stack-debugger
   test/engine-stack-debugger.f
TEST:;SUITE

TEST:SUITE debugger-resume
   test/debugger-resume.f
TEST:;SUITE

TEST:SUITE stack-guard
   test/stack-guard.f
TEST:;SUITE

TEST:SUITE combinators
   test/combinators.f
TEST:;SUITE

TEST:SUITE int-convert
   test/int-convert.f
TEST:;SUITE

TEST:SUITE export-keyword-package
   test/export-package.f
TEST:;SUITE

TEST:SUITE postpone-define
   test/postpone-define.f
TEST:;SUITE

TEST:SUITE postpone-effect
   test/postpone-effect.f
TEST:;SUITE

\ Apart from postpone-define, whose file is also a build source: a definition
\ these cases reject gives data space back, which a build rejects.
TEST:SUITE postpone-reject
   test/postpone-reject.f
TEST:;SUITE

TEST:SUITE build-items
   test/build-items.f
TEST:;SUITE

\ The design seal: each test/policy case loaded sealed at tier 0, one child per
\ case; writes build/vocabulary-run.txt.
TEST:SUITE vocabulary
   src/lib/vocabulary-test.f
TEST:;SUITE

TEST:SUITE engine-private
   test/engine-private-test.f
TEST:;SUITE

TEST:SUITE shadowed-arity-rejection
   test/shadowed-arity-test.f
TEST:;SUITE

TEST:SUITE load-reject-diag
   test/load-reject-diag-test.f
TEST:;SUITE

TEST:SUITE dictionary-full
   test/dictionary-full-test.f
TEST:;SUITE

TEST:SUITE load-argv-contract
   tools/load-argv-test.f
TEST:;SUITE

TEST:SUITE tmp-path
   test/tmp-path-test.f
TEST:;SUITE

TEST:SUITE num-types
   src/lib/num-types-test.f
TEST:;SUITE

TEST:SUITE span
   src/lib/span-test.f
TEST:;SUITE

TEST:SUITE fs-mutate
   src/lib/fs-mutate-test.f
   src/lib/fs-rename-noreplace-test.f
TEST:;SUITE

\ Host failures come from an LD_PRELOAD shim of glibc's fsync and renameat2.
HB-TARGET-LINUX? [if]
TEST:SUITE fs-atomic-faults
   test/fs-atomic-faults.f
TEST:;SUITE
[then]

\ sd_notify(3) is Linux's: the cases bind unix datagram sockets, abstract ones too.
HB-TARGET-LINUX? [if]
TEST:SUITE systemd
   src/lib/systemd-test.f
TEST:;SUITE
[then]

\ The attribute calls are bound on the Linux kernel only, arm64 and x86-64.
HB-TARGET-LINUX? [if]
TEST:SUITE xattr
   src/lib/xattr-test.f
TEST:;SUITE
[then]

\ inotify is bound on the Linux kernel only.
HB-TARGET-LINUX? [if]
TEST:SUITE inotify
   src/lib/inotify-test.f
TEST:;SUITE
[then]

TEST:SUITE exit-hook
   test/exit-hook-test.f
TEST:;SUITE

TEST:SUITE fs-lock
   src/lib/fs-lock-test.f
TEST:;SUITE

TEST:SUITE os-cpu
   src/lib/os-cpu-test.f
TEST:;SUITE

\ statx(2) is Linux's: birth time and subvolume of a file under TMPDIR.
HB-TARGET-MACOS? 0= [if]
TEST:SUITE fs-statx
   src/lib/fs-statx-test.f
TEST:;SUITE
[then]

TEST:SUITE fs-list
   src/lib/fs-list-test.f
   src/lib/fs-cause-test.f
TEST:;SUITE

TEST:SUITE fs-anchored
   test/fs-anchored-consumer.f
TEST:;SUITE

\ Raw serial streams over a pty pair: the loop supplies every wait.
TEST:SUITE serial
   src/lib/serial-test.f
TEST:;SUITE

\ IPv4 stream sockets: a listener task and a client in one process.
TEST:SUITE tcp4
   src/lib/net/tcp4-test.f
TEST:;SUITE

\ IPv4 datagrams: two loopback sockets in one process.
TEST:SUITE udp4
   src/lib/net/udp4-test.f
TEST:;SUITE

\ Unix datagrams: sockets at paths and, on Linux, abstract names.
TEST:SUITE udp
   src/lib/net/udp-test.f
TEST:;SUITE

\ The RFC 6455 frame codec: bytes in and bytes out, no socket.
TEST:SUITE websocket-frame
   src/lib/net/ws-frame-test.f
TEST:;SUITE

\ SSE framing over arbitrary byte chunks, with no network dependency.
TEST:SUITE event-stream
   src/lib/net/event-stream-test.f
TEST:;SUITE

\ The process row is per task: a capturing task and a polling task at once.
TEST:SUITE process-tasks
   src/lib/process-task-test.f
TEST:;SUITE

TEST:SUITE process-argv
   src/lib/process-argv-test.f
TEST:;SUITE

TEST:SUITE process-cwd
   src/lib/process-cwd-test.f
TEST:;SUITE

TEST:SUITE process-env
   src/lib/process-env-test.f
TEST:;SUITE

\ A spawn request: arguments, environment, directory and descriptor map, spawned once.
TEST:SUITE spawn
   src/lib/spawn-test.f
TEST:;SUITE

\ A reaper kills what its pool spawned REAPED when the pool dies or stops it.
TEST:SUITE process-reaper
   src/lib/process-reaper-test.f
TEST:;SUITE

\ A tree walk the kernel rejects throws rather than reading the rejection as
\ nobody there, and one at a capture's early end leaves the capture its own
\ answer and names the walk's code.
TEST:SUITE process-tree
   src/lib/process-tree-test.f
TEST:;SUITE

TEST:SUITE hb-open-failure
   tools/hb-open-failure-test.f
TEST:;SUITE

TEST:SUITE realpath
   test/realpath-test.f
TEST:;SUITE

TEST:SUITE source-root
   test/source-root-test.f
TEST:;SUITE

TEST:SUITE deep-cwd
   test/deep-cwd-e2e.f
TEST:;SUITE

TEST:SUITE include-rejection
   test/include-rejection-e2e.f
TEST:;SUITE

TEST:SUITE whole-source-read
   test/whole-source-read-test.f
TEST:;SUITE

TEST:SUITE name-length
   test/name-length-test.f
TEST:;SUITE

TEST:SUITE boot-relocation
   test/boot-relocation-e2e.f
TEST:;SUITE

TEST:SUITE install-e2e
   test/install-e2e.f
TEST:;SUITE

TEST:SUITE source-discovery
   tools/source-discovery-test.f
TEST:;SUITE

TEST:SUITE cast-negative
   test/cast-negative-suite.f
TEST:;SUITE

TEST:SUITE nominal-pointer
   test/nominal-pointer.f
TEST:;SUITE

TEST:SUITE pointer-view
   test/pointer-view.f
TEST:;SUITE

TEST:SUITE linear
   test/linear-suite.f
TEST:;SUITE

TEST:SUITE deftype
   test/deftype-suite.f
TEST:;SUITE

TEST:SUITE split-str
   test/split-str-test.f
TEST:;SUITE

TEST:SUITE closed-source
   test/closed-source-suite.f
TEST:;SUITE

TEST:SUITE engine
   test/engine-suite.f
TEST:;SUITE

TEST:SUITE program-diagnostics
   test/program-diagnostics-test.f
TEST:;SUITE

TEST:SUITE diag-json-escape
   test/diag-json-escape.f
TEST:;SUITE

TEST:SUITE native-gate-debug
   test/gate-debug.f
TEST:;SUITE

TEST:SUITE profiler-index
   test/prof-index.f
TEST:;SUITE

TEST:SUITE profiler-rewind
   test/prof-rewind.f
TEST:;SUITE

TEST:SUITE perf-map-fixtures
   tools/perf-map-test.f
TEST:;SUITE

TEST:SUITE native-gate-dictionary
   test/gate-dictionary.f
TEST:;SUITE

TEST:SUITE type-layout-lower-pending
   test/type-layout-lower-pending.f
TEST:;SUITE

TEST:SUITE wide-typed-local-probe
   test/wide-typed-local-probe.f
TEST:;SUITE

TEST:SUITE layout-buffer
   test/layout-buffer.f
TEST:;SUITE

TEST:SUITE dynamic-buffer
   test/dynamic-buffer.f
TEST:;SUITE

TEST:SUITE arena
   test/arena.f
TEST:;SUITE

TEST:SUITE allocator
   src/lib/allocator-test.f
TEST:;SUITE

TEST:SUITE heap
   test/heap.f
   src/lib/heap-test.f
TEST:;SUITE

TEST:SUITE dynamic-buffer-tasks
   test/dynamic-buffer-tasks.f
TEST:;SUITE

TEST:SUITE rejection-tasks
   test/rejection-tasks.f
TEST:;SUITE

TEST:SUITE layout-defer
   test/layout-defer.f
TEST:;SUITE

TEST:SUITE protection-span
   test/protection-span.f
TEST:;SUITE

TEST:SUITE code-window
   test/code-window.f
TEST:;SUITE

TEST:SUITE enum-decl
   test/enum-decl-suite.f
TEST:;SUITE

TEST:SUITE extent-product
   test/extent-product-test.f
TEST:;SUITE

TEST:SUITE gate-pool-orphan
   test/gate-pool-orphan-test.f
TEST:;SUITE

\ A signalled gate root leaves no process and no scratch behind.
TEST:SUITE gate-signal
   test/gate-signal-test.f
TEST:;SUITE

\ A signalled check.f, and a check run past its deadline, leave no process and
\ no scratch behind.
TEST:SUITE check-signal
   test/check-signal-test.f
TEST:;SUITE

\ A capture an overflow ends early leaves nothing its child started.
TEST:SUITE capture-tree
   test/capture-tree-test.f
TEST:;SUITE

TEST:SUITE require-cap
   test/require-cap-test.f
TEST:;SUITE

TEST:SUITE rigid-region
   test/rigid-region-suite.f
   test/rigid-region-inferred-e2e.f
TEST:;SUITE

TEST:SUITE structure-certify
   test/structure-certify-suite.f
TEST:;SUITE

TEST:SUITE structure-quotation-rollback
   test/structure-quotation-rollback.f
TEST:;SUITE

TEST:SUITE structure-decl
   test/structure-decl-suite.f
TEST:;SUITE

TEST:SUITE structure-mutual
   test/structure-mutual.f
TEST:;SUITE

TEST:SUITE structure-opaque
   test/structure-opaque-e2e.f
TEST:;SUITE

TEST:SUITE type-ctor
   test/type-ctor-suite.f
TEST:;SUITE

TEST:SUITE type-decl
   test/type-decl-suite.f
TEST:;SUITE

TEST:SUITE c2-init-schema
   test/c2-init-schema.f
TEST:;SUITE

TEST:SUITE c2-init-accessors
   test/c2-init-accessor-rejections.f
TEST:;SUITE

TEST:SUITE c2-diagnostics
   test/c2-diagnostics.f
TEST:;SUITE

TEST:SUITE type-linear
   test/type-linear-suite.f
TEST:;SUITE

TEST:SUITE type-match
   test/type-match-suite.f
TEST:;SUITE

TEST:SUITE repl-address-cell-rollback
   test/repl-address-cell-rollback.f
TEST:;SUITE

\ The open-definition PTY fixture starts and reaps one engine child per case.
TEST:SUITE repl-open-definition
   test/repl-open-definition.f
TEST:;SUITE

TEST:SUITE dictionary-zero
   test/dictionary-zero.f
TEST:;SUITE

TEST:RUN

s" PASS: native tests" type cr
