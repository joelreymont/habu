\ aot-lint.f - CLI wrapper for stripped-AOT source lint.
\ Load after tools/aot-lint-core.f and tools/argv.f.

require lib/source.f

: AOT-LINT-STDIN ( -- )
   AL-FILE-BUF AL-FILE-CAP >LEN READ-STDIN-ALL LEN>N {: srcu:n :}
   ARGV-LABEL? IF
      AL-FILE-BUF srcu ARGV-LABEL$ AOT-LINT-SOURCE-AS
   ELSE
      AL-FILE-BUF srcu s" <stdin>" AOT-LINT-SOURCE-AS
   THEN ;

: AOT-LINT-ARGV-FILE ( n -- ) {: idx:n :}
   ARGV-LABEL? IF
      idx ARGV-POS$ ARGV-LABEL$ AOT-LINT-FILE-AS
   ELSE
      idx ARGV-POS$ AOT-LINT-FILE
   THEN ;

: AOT-LINT ( -- )
   s" tools/aot-lint.f [--json] [--label name] [file ...]" ARGV-USAGE!
   ARGV-PARSE
   0 -1 ARGV-EXPECT-POS
   AOT-LINT-RESET
   ARGV-JSON? AL-JSON!
   ARGV-POS# 0= IF AOT-LINT-STDIN AOT-LINT-FINISH EXIT THEN
   0 begin dup ARGV-POS# < while
      dup AOT-LINT-ARGV-FILE
      1+
   repeat drop
   AOT-LINT-FINISH ;

AOT-LINT
