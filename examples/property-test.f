\ property-test.f - checked stdlib property helper usage example.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/examples-test.f

17 constant PE-SEED
32 constant PE-COUNT
100 constant PE-BOUND
6 constant PE-DIGITS
4 constant PE-MIN-SHRUNK-LEN

: PE-SMALL ( -- n )
   PE-BOUND PROP-RND% ;

: PE-SQUARE-PROPERTY ( -- )
   PE-SEED PE-COUNT PROP-RUN-RESET
   0 begin dup PROP-COUNT@ < while
      PE-SMALL dup * 0 >= TTRUE
      1+
   repeat drop ;

: PE-DIGITS! ( -- )
   PROP-BUF-RESET
   PE-DIGITS 0 ?do
      10 PROP-RND% PROP-DIGIT+
   loop ;

: PE-DIGIT-PROPERTY ( -- )
   PE-SEED PE-COUNT PROP-RUN-RESET
   0 begin dup PROP-COUNT@ < while
      PE-DIGITS!
      PROP-BUF$ STR-DIGITS? TTRUE
      PROP-BUF$ nip PE-DIGITS T=
      1+
   repeat drop ;

: PE-KEEP-FIRST? ( -- bool )
   PROP-BUF$ nip PE-MIN-SHRUNK-LEN >= ;

: PE-SHRINK-EXAMPLE ( -- )
   PROP-BUF-RESET
   s" 123 456 789 " PROP-BUF+
   [: PE-KEEP-FIRST? ;] PROP-SHRINK
   PROP-BUF$ s" 123 " T$= ;

: PE-MAIN ( -- )
   T-RESET
   PE-SQUARE-PROPERTY
   PE-DIGIT-PROPERTY
   PE-SHRINK-EXAMPLE
   T-REPORT ;

PE-MAIN
