\ package-dag-lint.f - CLI entrypoint for the HBR2 package DAG lint.
\ Run: bin/hb --load tools/package-dag-lint.f
\ Enforcing: checks the layers of the tree in the working directory, prints each
\ finding and the census, and THROWS 1 on any finding.

require tools/package-dag-lint-core.f

package PACKAGE-DAG-LINT-CLI
private

: MAIN ( -- )
   [: PACKAGE-DAG-LINT:STRICT ;] catch {: code:n :}
   s" package-dag-lint" code LINT-MAIN ;

MAIN

;package
