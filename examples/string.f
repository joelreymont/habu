\ string.f - checked stdlib string usage example.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/examples-test.f

$2D constant SE-DASH

: SE-SUFFIX-NUMBER ( ptr u8 n -- option<n> ) {: a:ptr u:n :}
   a u STR:LENGTH SE-DASH STR:INDEX-OF MATCH option
     none OF OPTION:NONE exit ENDOF
     some OF NUM:ORDINAL ENDOF        \ dash position drives a+ix+1 / u-ix-1
   ;MATCH {: ix:n :}
   a ix 1 + + u ix 1 + - STR>NUMBER? ;

: SE-CHECK-SUFFIX ( ptr u8 n n -- ) {: a:ptr u want :}
   a u SE-SUFFIX-NUMBER MATCH option
     none OF STR-FALSE TTRUE ENDOF
     some OF STR-TRUE TTRUE want T= ENDOF
   ;MATCH ;

: SE-MAIN ( -- )
   T-RESET
   s"   Habu-2026  " TRIM s" Habu-2026" T$=
   s" Habu-2026" s" Habu" STARTS-WITH? TTRUE
   s" Habu-2026" SE-DASH COUNT-CHAR 1 T=
   s" Habu-2026" 2026 SE-CHECK-SUFFIX
   s" Habu-20x6" SE-SUFFIX-NUMBER MATCH option
     none OF STR-TRUE ENDOF
     some OF drop STR-FALSE ENDOF
   ;MATCH TTRUE
   T-REPORT ;

SE-MAIN
