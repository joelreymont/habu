\ gate-stdlib-inline-lib.f - in-process stdlib gate slices for warm runners.
\
\ Load after test/gate-stdlib-lib.f in the warm gate runner.

: GSI-INCLUDE ( ptr u8 n -- )
   included ;

: GSI-TOOL-BASE ( -- )
   s" tools/date.f" GSI-INCLUDE
   s" lib/test.f" GSI-INCLUDE
   s" tools/lint/text.f" GSI-INCLUDE
   s" tools/lint/intern.f" GSI-INCLUDE
   s" tools/lint/token.f" GSI-INCLUDE
   s" tools/lint/lib.f" GSI-INCLUDE
   s" tools/lint/json-writer.f" GSI-INCLUDE
   s" tools/lint/source-lex.f" GSI-INCLUDE
   s" tools/argv.f" GSI-INCLUDE
   s" tools/check-all-errors-core.f" GSI-INCLUDE
   s" tools/diag-origin-core.f" GSI-INCLUDE
   s" tools/json-only-core.f" GSI-INCLUDE
   s" tools/aot-lint-core.f" GSI-INCLUDE
   s" tools/signature-lint-core.f" GSI-INCLUDE
   s" tools/checked-boundary-lint-core.f" GSI-INCLUDE
   s" tools/reserved-name-lint-core.f" GSI-INCLUDE
   s" tools/duplicate-definition-lint-core.f" GSI-INCLUDE
   s" tools/bundle-lib-core.f" GSI-INCLUDE ;

: GSI-TOOL-LINTS ( -- )
   GSI-TOOL-BASE
   s" tools/repl-lint-core.f" GSI-INCLUDE
   s" tools/repl-lint-test.f" GSI-INCLUDE
   s" tools/diag-origin-test.f" GSI-INCLUDE
   s" tools/aot-lint-test.f" GSI-INCLUDE
   s" tools/signature-lint-test.f" GSI-INCLUDE
   s" tools/checked-boundary-lint-test.f" GSI-INCLUDE
   s" tools/reserved-name-lint-test.f" GSI-INCLUDE
   s" tools/duplicate-definition-lint-test.f" GSI-INCLUDE
   s" tools/bundle-lib-test.f" GSI-INCLUDE
   s" tools/json-only-test.f" GSI-INCLUDE ;

undefine SUITE-INLINE-WORK

: SUITE-INLINE-WORK ( -- )
   SUITE-SKIP-TOOL-LINTS @ 0= if exit then
   GSI-TOOL-LINTS ;
