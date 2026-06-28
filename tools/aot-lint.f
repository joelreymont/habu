\ aot-lint.f - CLI wrapper for stripped-AOT source lint.
\ Load after tools/aot-lint-core.f and tools/argv.f.

: AOT-LINT-ARGV-FILE ( n -- ) {: idx:n :}
   ARGV-LABEL? IF
      idx ARGV-POS$ ARGV-LABEL$ AOT-LINT-FILE-AS
   ELSE
      idx ARGV-POS$ AOT-LINT-FILE
   THEN ;

: AOT-LINT ( -- )
   s" tools/aot-lint.f [--json] [--label name] file ..." ARGV-USAGE!
   ARGV-PARSE
   1 -1 ARGV-EXPECT-POS
   AOT-LINT-RESET
   ARGV-JSON? AL-JSON!
   0 begin dup ARGV-POS# < while
      dup AOT-LINT-ARGV-FILE
      1+
   repeat drop
   AOT-LINT-FINISH ;

AOT-LINT
