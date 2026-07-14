\ suite-coverage-lint.f - CLI entrypoint for the stdlib gate suite-coverage lint.
\ Load after tools/suite-coverage-lint-core.f.

require tools/suite-coverage-lint-core.f

: SCL-MAIN ( -- )
   [: SUITE-COVERAGE-LINT ;] catch {: code:n :}
   s" suite-coverage-lint" code LINT-MAIN ;

SCL-MAIN
