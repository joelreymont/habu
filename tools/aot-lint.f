\ aot-lint.f - CLI wrapper for stripped-AOT source lint.
\ Load after tools/aot-lint-core.f and lib/argv.f.

package AOT-LINT-CLI
private

: ARGV-FILE ( n -- ) {: idx:n :}
   ARGV:LABEL? IF
      idx ARGV:POS$ ARGV:LABEL$ AOT-LINT-FILE-AS
   ELSE
      idx ARGV:POS$ AOT-LINT-FILE
   THEN ;

: RUN ( -- )
   s" tools/aot-lint.f [--json] [--label name] file ..." ARGV:USAGE!
   ARGV:PARSE
   1 -1 ARGV:EXPECT-POS
   AOT-LINT-RESET
   ARGV:JSON? AL-JSON!
   0 begin dup ARGV:POS# < while
      dup ARGV-FILE
      1+
   repeat drop
   AOT-LINT-FINISH ;

RUN

;package
