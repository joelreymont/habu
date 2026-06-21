\ gate-dictionary.f - checked runner for dictionary and checker contracts.
\
\ Load after test/gate-common.f.

$41 constant GD-A
$2E constant GD-DOT
$63 constant GD-C-LOWER
600 constant GD-LONG-NAME-LEN

: GD-OUT-LINE ( ptr u8 n -- )
   SB-APPEND
   GE-SB-LF ;

: GD-CHECK-LINE ( ptr u8 n -- )
   GE-SRC-S"
   s"  CHECK! ." GE-SRC-LINE ;

: GD-EMIT-LONG-NAME ( -- )
   GD-LONG-NAME-LEN GD-A GE-SRC-REPEAT-C ;

: GD-SRC-LONG-NAME-S" ( -- )
   s" s" GE-SRC+
   GE-DQ GE-SRC-C
   GE-SRC-SP
   GD-EMIT-LONG-NAME
   GE-DQ GE-SRC-C ;

: GD-SRC-DOTQ ( ptr u8 n -- ) {: a:ptr u :}
   GD-DOT GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GD-SRC-CQ ( ptr u8 n -- ) {: a:ptr u :}
   GD-C-LOWER GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GD-LONG-DICTIONARY-SOURCE ( -- )
   GE-SRC-RESET
   s" : LONG-DICTIONARY-NAME-ADDONE ( i64 -- i64 ) 1 + ;" GE-SRC-LINE
   s" 41 LONG-DICTIONARY-NAME-ADDONE ." GE-SRC-LINE
   s" 123 constant LONG-DICTIONARY-CONSTANT" GE-SRC-LINE
   s" LONG-DICTIONARY-CONSTANT ." GE-SRC-LINE
   s" variable LONG-DICTIONARY-VARIABLE" GE-SRC-LINE
   s" 77 LONG-DICTIONARY-VARIABLE !" GE-SRC-LINE
   s" LONG-DICTIONARY-VARIABLE @ ." GE-SRC-LINE
   s" LONG-DICTIONARY-NAME-ADDONE" GE-SRC-S"
   s"  get-current search-wl 0= ." GE-SRC-LINE
   s" long-dictionary-name-addone" GE-SRC-S"
   s"  get-current search-wl 0= ." GE-SRC-LINE
   s" : LONG-REDEFINE-NAME ( -- i64 ) 1 ;" GE-SRC-LINE
   s" : LONG-REDEFINE-NAME ( -- i64 ) 2 ;" GE-SRC-LINE
   s" LONG-REDEFINE-NAME ." GE-SRC-LINE
   s" TRUSTED: LONG-DICTIONARY-TRUSTED ( n -- n ) dup ;" GE-SRC-LINE
   s" USE ( n -- n ) LONG-DICTIONARY-TRUSTED" GD-CHECK-LINE
   s" BAD ( n -- n n ) LONG-DICTIONARY-TRUSTED" GD-CHECK-LINE
   s" 9 LONG-DICTIONARY-TRUSTED . ." GE-SRC-LINE ;

: GD-LONG-DICTIONARY ( -- )
   GE-HB-RESET
   GD-LONG-DICTIONARY-SOURCE
   s" hb long dictionary names" GE-HB-RUN-STDIN
   SB-RESET
   s" 42" GD-OUT-LINE
   s" 123" GD-OUT-LINE
   s" 77" GD-OUT-LINE
   s" 0" GD-OUT-LINE
   s" 0" GD-OUT-LINE
   s" 2" GD-OUT-LINE
   s" -1" GD-OUT-LINE
   s" 0" GD-OUT-LINE
   s" 9" GD-OUT-LINE
   s" 9" GD-OUT-LINE
   SB$ s" hb long dictionary names output" GE-EXPECT-OUT ;

: GD-WORDLIST-SOURCE ( -- )
   GE-SRC-RESET
   s" wordlist constant LONG-WL" GE-SRC-LINE
   s" LONG-WL set-current" GE-SRC-LINE
   s" : LONG-WORDLIST-ONLY-NAME ( -- i64 ) 8 ;" GE-SRC-LINE
   s" 0 set-current" GE-SRC-LINE
   s" LONG-WORDLIST-ONLY-NAME" GE-SRC-S"
   s"  0 search-wl 0= ." GE-SRC-LINE
   s" LONG-WORDLIST-ONLY-NAME" GE-SRC-S"
   s"  LONG-WL search-wl 0= ." GE-SRC-LINE ;

: GD-WORDLIST ( -- )
   GE-HB-RESET
   GD-WORDLIST-SOURCE
   s" hb long dictionary wordlist isolation" GE-HB-RUN-STDIN
   SB-RESET
   s" -1" GD-OUT-LINE
   s" 0" GD-OUT-LINE
   SB$ s" hb long dictionary wordlist isolation output" GE-EXPECT-OUT ;

: GD-LONG-NAME-SOURCE ( -- )
   GE-SRC-RESET
   s" : " GE-SRC+
   GD-EMIT-LONG-NAME
   s"  ( -- n ) 1 ;" GE-SRC-LINE
   GD-EMIT-LONG-NAME
   s"  ." GE-SRC-LINE
   GD-SRC-LONG-NAME-S"
   s"  get-current search-wl 0= ." GE-SRC-LINE ;

: GD-LONG-NAME ( -- )
   GE-HB-RESET
   GD-LONG-NAME-SOURCE
   s" hb dictionary name over 255 bytes" GE-HB-RUN-STDIN
   SB-RESET
   s" 1" GD-OUT-LINE
   s" 0" GD-OUT-LINE
   SB$ s" hb dictionary name over 255 bytes output" GE-EXPECT-OUT ;

: GD-TRUSTED-DOES-SOURCE ( -- )
   GE-SRC-RESET
   s" TRUSTED: ARR ( n -- ) CREATES ( n -- ptr a ) create cells allot does> swap 0 ?do cell+ loop ;" GE-SRC-LINE
   s" 4 ARR A4" GE-SRC-LINE
   s" USE ( n -- ptr a ) A4" GD-CHECK-LINE
   s" 7 2 A4 !" GE-SRC-LINE
   s" 2 A4 @ ." GE-SRC-LINE ;

: GD-TRUSTED-DOES ( -- )
   GE-HB-RESET
   GD-TRUSTED-DOES-SOURCE
   s" hb trusted CREATE...DOES> effect recording" GE-HB-RUN-STDIN
   SB-RESET
   s" -1" GD-OUT-LINE
   s" 7" GD-OUT-LINE
   SB$ s" hb trusted CREATE...DOES> effect recording output" GE-EXPECT-OUT ;

: GD-BAD-DOES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" TRUSTED: BADARR ( n -- ) CREATES ( n -- ptr a ) create cells allot does> drop ;" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb bad trusted DOES> rc" GE-EXPECT-RC
   s" does>" s" hb bad trusted DOES> diagnostic" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   GE-SRC-RESET
   s" TRUSTED: BADDEF ( n -- ) create cells allot does> drop ;" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   70 s" hb trusted DOES> without CREATES rc" GE-EXPECT-RC
   s" does>" s" hb trusted DOES> without CREATES diagnostic" GE-EXPECT-ERR-HAS ;

: GD-ROW-QUOT-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" V1 ( R -- R i64 ) 5" GD-CHECK-LINE
   s" V2 ( i64 [ i64 -- i64 ] -- i64 ) execute" GD-CHECK-LINE
   s" V3 ( R -- R i64 ) 5 5" GD-CHECK-LINE
   s" hb rows/quot sig verify" GE-HB-RUN-STDIN
   SB-RESET s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE s" 0" GD-OUT-LINE
   SB$ s" hb rows/quot sig verify output" GE-EXPECT-OUT ;

: GD-PRIMITIVE-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" P1 ( i64 i64 i64 i64 -- i64 i64 i64 i64 i64 i64 ) 2over" GD-CHECK-LINE
   s" P2 ( i64 i64 -- i64 i64 ) 2>r 2r>" GD-CHECK-LINE
   s" P3 ( i64 -- i64 ) abs" GD-CHECK-LINE
   s" P4 ( i64 i64 -- i64 i64 ) /mod" GD-CHECK-LINE
   s" P5 ( ptr u8 -- ptr u8 i64 ) count" GD-CHECK-LINE
   s" P6 ( i64 i64 -- i64 i64 i64 ) depth" GD-CHECK-LINE
   s" P7 ( -- n ) 0 4096 3 $1002 -1 0 mmap" GD-CHECK-LINE
   s" hb primitive checklist signatures" GE-HB-RUN-STDIN
   SB-RESET
   s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE
   s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE
   s" -1" GD-OUT-LINE
   SB$ s" hb primitive checklist signatures output" GE-EXPECT-OUT ;

: GD-RETURN-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" RBAD1 ( i64 i64 -- ) 2>r" GD-CHECK-LINE
   s" RBAD2 ( -- i64 i64 ) 2r>" GD-CHECK-LINE
   s" RPEEK ( i64 i64 -- i64 i64 i64 i64 ) 2>r 2r@ 2r>" GD-CHECK-LINE
   s" QD ( i64 -- i64 i64 ) ?dup" GD-CHECK-LINE
   s" hb return-stack/?dup primitive verdicts" GE-HB-RUN-STDIN
   SB-RESET s" 0" GD-OUT-LINE s" 0" GD-OUT-LINE s" -1" GD-OUT-LINE s" 1" GD-OUT-LINE
   SB$ s" hb return-stack/?dup primitive verdicts output" GE-EXPECT-OUT ;

: GD-COMBINATOR-CHECKS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" CDIP ( i64 i64 -- i64 i64 ) [: 1+ ;] DIP" GD-CHECK-LINE
   s" CKEEP ( i64 -- i64 i64 ) [: 1+ ;] KEEP" GD-CHECK-LINE
   s" CBI ( i64 -- i64 i64 ) [: 1+ ;] [: 2 * ;] BI" GD-CHECK-LINE
   s" CTRI ( i64 -- i64 i64 i64 ) [: 1+ ;] [: 2 * ;] [: 3 + ;] TRI" GD-CHECK-LINE
   s" CTIMES ( i64 -- i64 ) 5 [: 1+ ;] TIMES" GD-CHECK-LINE
   s" CEACH ( i64 ptr i64 i64 -- i64 ) [: + ;] EACH" GD-CHECK-LINE
   s" CMAP ( ptr i64 i64 -- ) [: 1+ ;] MAP" GD-CHECK-LINE
   s" CFOLD ( ptr i64 i64 i64 -- i64 ) [: + ;] FOLD" GD-CHECK-LINE
   s" hb combinator/iterator verdicts" GE-HB-RUN-STDIN
   SB-RESET
   s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE
   s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE s" -1" GD-OUT-LINE
   SB$ s" hb combinator/iterator verdicts output" GE-EXPECT-OUT ;

: GD-PARSING-RUNTIME-SOURCE ( -- )
   GE-SRC-RESET
   s" hi" GD-SRC-DOTQ s"  cr" GE-SRC-LINE
   s" ok" GD-SRC-CQ s"  count type cr" GE-SRC-LINE
   s" : DQ ( -- ) " GE-SRC+ s" bye" GD-SRC-DOTQ s"  ;" GE-SRC-LINE
   s" DQ cr" GE-SRC-LINE
   s" : CQ ( -- ptr u8 n ) " GE-SRC+ s" yo" GD-SRC-CQ s"  count ;" GE-SRC-LINE
   s" CQ type cr" GE-SRC-LINE ;

: GD-PARSING-RUNTIME ( -- )
   GE-HB-RESET
   GD-PARSING-RUNTIME-SOURCE
   s" hb parsing-word runtime surface" GE-HB-RUN-STDIN
   SB-RESET
   s" hi" GD-OUT-LINE s" ok" GD-OUT-LINE s" bye" GD-OUT-LINE s" yo" GD-OUT-LINE
   SB$ s" hb parsing-word runtime surface output" GE-EXPECT-OUT ;

: GD-PARSING-CHECK-SOURCE ( -- )
   GE-SRC-RESET
   s" : DQ ( -- ) " GE-SRC+ s" ok" GD-SRC-DOTQ s"  ;" GE-SRC-LINE
   s" : CQ ( -- ptr u8 n ) " GE-SRC+ s" ok" GD-SRC-CQ s"  count ;" GE-SRC-LINE ;

: GD-PARSING-CHECK ( -- )
   GD-PARSING-CHECK-SOURCE
   s" check.f parsing-word certification" GE-CHECK-RUN ;

: GD-DATA-OVERFLOW ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" $400000 allot" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   76 s" data-space overflow rc" GE-EXPECT-RC ;

: GD-NAMED-ROW-RUN ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : PSH ( R -- R i64 ) 5 ;" GE-SRC-LINE
   s" PSH ." GE-SRC-LINE
   s" hb named-row sig run" GE-HB-RUN-STDIN
   SB-RESET s" 5" GD-OUT-LINE
   SB$ s" hb named-row sig run output" GE-EXPECT-OUT ;

: GD-MAIN ( -- )
   GT-RESET
   GD-LONG-DICTIONARY
   GD-WORDLIST
   GD-LONG-NAME
   GD-TRUSTED-DOES
   GD-BAD-DOES
   GD-ROW-QUOT-CHECKS
   GD-PRIMITIVE-CHECKS
   GD-RETURN-CHECKS
   GD-COMBINATOR-CHECKS
   GD-PARSING-RUNTIME
   GD-PARSING-CHECK
   GD-DATA-OVERFLOW
   GD-NAMED-ROW-RUN
   s" PASS: native dictionary/checker gate phase" type cr ;

GD-MAIN
