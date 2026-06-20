\ array.f - checked stdlib array usage example.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/examples-test.f

4 constant AE-LEN

create AE-DATA AE-LEN cells allot

: AE-LOAD ( -- )
   1 AE-DATA AE-LEN 0 A!
   2 AE-DATA AE-LEN 1 A!
   3 AE-DATA AE-LEN 2 A!
   4 AE-DATA AE-LEN 3 A! ;

: AE-SUM ( -- n )
   AE-DATA AE-LEN A-SUM ;

: AE-WEIGHTED ( -- n )
   AE-DATA AE-LEN 0 [: * + ;] A-FOLDI ;

: AE-DOUBLE! ( -- )
   AE-DATA AE-LEN [: 2 * ;] A-MAP! ;

: AE-FIRST-GT-SIX ( -- n )
   AE-DATA AE-LEN [: 6 > ;] A-FIND-INDEX ;

: AE-BIAS-BY-INDEX! ( -- )
   AE-DATA AE-LEN [: + ;] A-MAPI! ;

: AE-MAIN ( -- )
   T-RESET
   AE-LOAD
   AE-SUM 10 T=
   AE-WEIGHTED 20 T=
   AE-DATA AE-LEN A-MAX 4 T=
   AE-DATA AE-LEN A-MAX-INDEX 3 T=
   AE-DOUBLE!
   AE-SUM 20 T=
   AE-FIRST-GT-SIX 3 T=
   AE-BIAS-BY-INDEX!
   AE-DATA AE-LEN 0 A@ 2 T=
   AE-DATA AE-LEN 1 A@ 5 T=
   AE-DATA AE-LEN 2 A@ 8 T=
   AE-DATA AE-LEN 3 A@ 11 T=
   T-REPORT ;

AE-MAIN
