\ aot-chain-preloaded.f - the capture tool run behind another file.
\
\ src/arch/arm64/asm.f is the file that makes this dangerous rather than merely
\ untidy: the compiler chain requires it too, so loading it first moves 178
\ records out of the capture window and under the prelude mark, where the audit
\ takes them for words the target engine already has. The tool refuses on the
\ count of files this process loaded before it, which is a question the engine's
\ own require registry answers (src/core/include.f REQUIRE-BOOT-N).
\
\ Used by test/aot-chain-capture-suite.f; its exit code and diagnostic are the
\ assertion.

require src/arch/arm64/asm.f
require tools/aot-chain-capture.f
