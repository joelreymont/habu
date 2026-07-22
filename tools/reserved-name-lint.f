\ reserved-name-lint.f - CLI wrapper for reserved definition-name lint.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, tools/lint/source-lex.f,
\ tools/reserved-name-lint-core.f, and tools/argv.f.

package RESERVED-NAME-LINT-CLI
private

: ARGV-FILE ( n -- ) {: idx:n :}
   ARGV:LABEL? if
      idx ARGV:POS$ ARGV:LABEL$ RESERVED-NAME-LINT-FILE-AS
   else
      idx ARGV:POS$ RESERVED-NAME-LINT-FILE
   then ;

: RUN ( -- )
   s" tools/reserved-name-lint.f [--json] [--label name] file ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   RESERVED-NAME-LINT-RESET
   ARGV:JSON? RNL-JSON!
   0 begin dup ARGV:POS# < while
      dup ARGV-FILE
      1+
   repeat drop
   RESERVED-NAME-LINT-FINISH ;

RUN

;package
