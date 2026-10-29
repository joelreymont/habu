\ shadow-string-fixture.f - PART A negative fixture for shadow-lint, read as text
\ by tools/lint/shadow-lint-test.f (never loaded). At GLOBAL scope so the
\ package-scope skip cannot mask it: `variable or` inside the string literal is an
\ opaque span to the string-aware lexer, and the lint must find zero shadows here.

: SLF-SUGGEST ( -- ptr u8 n )
   s" declare it with variable or create instead" ;
