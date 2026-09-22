\ manifest-lint.f - CLI entrypoint for the engine manifest lint.
\ Run: bin/hb --load tools/manifest-lint.f
\ Enforcing: prints the manifest census and THROWS on any finding. A row in
\ src/habu/native-runtime.f that no entry point declares and that nothing in the
\ compiler, JIT or REPL closure requires fails the gate.

require tools/manifest-lint-core.f

package MANIFEST-LINT-CLI
private

: MAIN ( -- )
   [: MANIFEST-LINT:STRICT ;] catch {: code:n :}
   s" manifest-lint" code LINT-MAIN ;

MAIN

;package
