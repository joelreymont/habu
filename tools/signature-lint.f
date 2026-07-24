\ signature-lint.f - CLI wrapper for strict typed-signature lint.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f,
\ tools/lint/token.f, tools/lint/lib.f, tools/lint/json-writer.f,
\ tools/lint/source-lex.f, tools/signature-lint-core.f, and tools/argv.f.

package SIGNATURE-LINT-CLI
private

: ARGV-FILE ( n -- ) {: idx:n :}
   ARGV:LABEL? IF
      idx ARGV:POS$ ARGV:LABEL$ SIGNATURE-LINT:FILE-AS
   ELSE
      idx ARGV:POS$ SIGNATURE-LINT:FILE
   THEN ;

: RUN ( -- )
   s" tools/signature-lint.f [--json] [--label name] file ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   SIGNATURE-LINT:RESET
   ARGV:JSON? SIGNATURE-LINT:JSON!
   0 begin dup ARGV:POS# < while
      dup ARGV-FILE
      1+
   repeat drop
   SIGNATURE-LINT:FINISH ;

RUN

;package
