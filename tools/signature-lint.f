\ signature-lint.f - CLI wrapper for strict typed-signature lint.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f,
\ tools/lint/token.f, tools/lint/lib.f, tools/lint/json-writer.f,
\ tools/lint/source-lex.f, tools/signature-lint-core.f, and tools/argv.f.

require lib/source.f

: SIGNATURE-LINT-STDIN ( -- )
   SL-FILE-BUF SL-FILE-CAP >LEN READ-STDIN-ALL LEN>N {: srcu:n :}
   ARGV-LABEL? IF
      SL-FILE-BUF srcu ARGV-LABEL$ SIGNATURE-LINT-SOURCE-AS
   ELSE
      SL-FILE-BUF srcu s" <stdin>" SIGNATURE-LINT-SOURCE-AS
   THEN ;

: SIGNATURE-LINT-ARGV-FILE ( n -- ) {: idx :}
   ARGV-LABEL? IF
      idx ARGV-POS$ ARGV-LABEL$ SIGNATURE-LINT-FILE-AS
   ELSE
      idx ARGV-POS$ SIGNATURE-LINT-FILE
   THEN ;

: SIGNATURE-LINT ( -- )
   s" tools/signature-lint.f [--json] [--label name] [file ...]" ARGV-USAGE!
   ARGV-PARSE
   0 -1 ARGV-EXPECT-POS
   SIGNATURE-LINT-RESET
   ARGV-JSON? SL-JSON!
   ARGV-POS# 0= IF SIGNATURE-LINT-STDIN SIGNATURE-LINT-FINISH EXIT THEN
   0 begin dup ARGV-POS# < while
      dup SIGNATURE-LINT-ARGV-FILE
      1+
   repeat drop
   SIGNATURE-LINT-FINISH ;

SIGNATURE-LINT
