\ shadow-string-fixture.f - PART A negative fixture for shadow-lint.
\ Read as text by tools/lint/shadow-lint-test.f (never loaded). This word lives
\ at GLOBAL scope so the package-scope
\ skip cannot mask the case: the only reason it must not be flagged is that the
\ definer keyword `variable` and the prim name `or` sit inside a string literal,
\ which the string-aware lexer consumes as an opaque span. Before the fix the
\ plain tokenizer read the string body `variable or` as a definition of the prim
\ `or` and reported a false shadow; now the lint must find zero shadows here.

: SLF-SUGGEST ( -- ptr u8 n )
   s" declare it with variable or create instead" ;
