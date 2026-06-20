\ string-regex.f - checked stdlib string and regex usage example.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/examples-test.f

$2D constant SRE-DASH
$30 constant SRE-ZERO
$39 constant SRE-NINE
$61 constant SRE-A
$7A constant SRE-Z
64 constant SRE-RX-CAP
16 constant SRE-SLUG-RX-LEN

create SRE-RX SRE-RX-CAP allot

: SRE-SUFFIX-NUMBER ( ptr u8 n -- n bool ) {: a:ptr u :}
   a u SRE-DASH INDEX-OF {: ix :}
   ix 0 < if 0 STR-FALSE exit then
   a ix 1 + + u ix 1 + - STR>NUMBER? ;

: SRE-CHECK-SUFFIX ( ptr u8 n n -- ) {: a:ptr u want :}
   a u SRE-SUFFIX-NUMBER TTRUE
   want T= ;

: SRE-COMPILE-SLUG ( -- n )
   s" ^[a-z]+-[0-9]+$" SRE-RX SRE-RX-CAP RX-COMPILE ;

: SRE-RX@ ( n -- n )
   SRE-RX + c@ ;

: SRE-ASSERT-SLUG-RX ( -- )
   SRE-COMPILE-SLUG SRE-SLUG-RX-LEN T=
   0 SRE-RX@ RX-TOK-BOL T=
   1 SRE-RX@ RX-TOK-CLASS T=
   2 SRE-RX@ 3 T=
   3 SRE-RX@ SRE-A T=
   4 SRE-RX@ SRE-DASH T=
   5 SRE-RX@ SRE-Z T=
   6 SRE-RX@ RX-TOK-PLUS T=
   7 SRE-RX@ RX-TOK-LITERAL T=
   8 SRE-RX@ SRE-DASH T=
   9 SRE-RX@ RX-TOK-CLASS T=
   10 SRE-RX@ 3 T=
   11 SRE-RX@ SRE-ZERO T=
   12 SRE-RX@ SRE-DASH T=
   13 SRE-RX@ SRE-NINE T=
   14 SRE-RX@ RX-TOK-PLUS T=
   15 SRE-RX@ RX-TOK-EOL T= ;

: SRE-MAIN ( -- )
   T-RESET
   s"   Habu-2026  " TRIM s" Habu-2026" T$=
   s" Habu-2026" s" Habu" STARTS-WITH? TTRUE
   s" Habu-2026" SRE-DASH COUNT-CHAR 1 T=
   s" Habu-2026" 2026 SRE-CHECK-SUFFIX
   s" Habu-20x6" SRE-SUFFIX-NUMBER TFALSE 0 T=
   SRE-ASSERT-SLUG-RX
   T-REPORT ;

SRE-MAIN
