\ duplicate-definition-lint.f - CLI wrapper for flat duplicate-definition lint.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, tools/duplicate-definition-lint-core.f, and tools/argv.f.

: DUPLICATE-DEFINITION-LINT-ARGV-FILE ( n -- ) {: idx :}
   ARGV-LABEL? if
      idx ARGV-POS$ ARGV-LABEL$ DUPLICATE-DEFINITION-LINT-FILE-AS
   else
      idx ARGV-POS$ DUPLICATE-DEFINITION-LINT-FILE
   then ;

: DUPLICATE-DEFINITION-LINT ( -- )
   s" tools/duplicate-definition-lint.f [--json] [--label name] file ..." ARGV-USAGE!
   ARGV-PARSE
   1 -1 ARGV-EXPECT-POS
   DUPLICATE-DEFINITION-LINT-RESET
   ARGV-JSON? DDL-JSON!
   0 begin dup ARGV-POS# < while
      dup DUPLICATE-DEFINITION-LINT-ARGV-FILE
      1+
   repeat drop
   DUPLICATE-DEFINITION-LINT-FINISH ;

DUPLICATE-DEFINITION-LINT
