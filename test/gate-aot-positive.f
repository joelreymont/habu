\ gate-aot-positive.f - checked runner for positive hb-build AOT checks.
\
\ Load after test/gate-build-common.f.

46 constant GAP-DOT
99 constant GAP-C-LOWER
2000 constant GAP-MACHO-STRIPPED-TEXT-MAX
$3000 constant GAP-ELF-STRIPPED-TEXT-MAX

: GAP-N= ( n n ptr u8 n -- ) {: got want label:ptr labelu :}
   got want <> if label labelu GE-FAIL then ;

: GAP-PH-TYPE ( n -- n )
   GB-ELF-PH-OFF GB-U32-OFF ;

: GAP-PH-FLAGS ( n -- n )
   GB-ELF-PH-OFF GB-ELF-PH-FLAGS-OFF + GB-U32-OFF ;

: GAP-PH-FILE-OFF ( n -- n )
   GB-ELF-PH-OFF GB-ELF-PH-FILE-OFF + GB-U64-OFF ;

: GAP-PH-VADDR ( n -- n )
   GB-ELF-PH-OFF GB-ELF-PH-VADDR-OFF + GB-U64-OFF ;

: GAP-PH-FILESZ ( n -- n )
   GB-ELF-PH-OFF GB-ELF-PH-FILESZ-OFF + GB-U64-OFF ;

: GAP-ASSERT-LINUX-DYNAMIC-ELF ( ptr u8 n -- ) {: label:ptr labelu :}
   HB-TARGET-LINUX? 0= if exit then
   GB-OUT$ GB-READ-EXEC
   GB-ELF-PHNUM-OFF GB-U16-OFF 4 label labelu GAP-N=
   0 GAP-PH-TYPE GB-ELF-PT-LOAD label labelu GAP-N=
   0 GAP-PH-FLAGS GB-ELF-PF-R GB-ELF-PF-X or label labelu GAP-N=
   1 GAP-PH-TYPE GB-ELF-PT-LOAD label labelu GAP-N=
   1 GAP-PH-FLAGS GB-ELF-PF-R GB-ELF-PF-W or label labelu GAP-N=
   1 GAP-PH-VADDR GB-ELF-RW-VA label labelu GAP-N=
   1 GAP-PH-FILESZ GB-ELF-RW-SZ label labelu GAP-N=
   2 GAP-PH-TYPE GB-ELF-PT-INTERP label labelu GAP-N=
   2 GAP-PH-FILE-OFF GB-ELF-INTERP-OFF label labelu GAP-N=
   2 GAP-PH-FILESZ GB-ELF-INTERP-SZ label labelu GAP-N=
   3 GAP-PH-TYPE GB-ELF-PT-DYNAMIC label labelu GAP-N=
   3 GAP-PH-FILE-OFF 1 GAP-PH-FILE-OFF label labelu GAP-N=
   3 GAP-PH-VADDR GB-ELF-RW-VA label labelu GAP-N=
   3 GAP-PH-FILESZ GB-ELF-DYNAMIC-SZ label labelu GAP-N=
   GB-ELF-RELA-OFF GB-U64-OFF GB-ELF-DLOPEN-SLOT label labelu GAP-N=
   GB-ELF-RELA-OFF 8 + GB-U64-OFF GB-ELF-DLOPEN-RINFO label labelu GAP-N=
   GB-ELF-RELA-OFF 24 + GB-U64-OFF GB-ELF-DLSYM-SLOT label labelu GAP-N=
   GB-ELF-RELA-OFF 32 + GB-U64-OFF GB-ELF-DLSYM-RINFO label labelu GAP-N= ;

: GAP-SRC-DOTQ ( ptr u8 n -- ) {: a:ptr u :}
   GAP-DOT GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GAP-SRC-CQ ( ptr u8 n -- ) {: a:ptr u :}
   GAP-C-LOWER GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GAP-PATHS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu out:ptr outu report:ptr reportu :}
   src srcu GB-SRC!
   out outu GB-OUT!
   report reportu GB-REPORT! ;

: GAP-BUILD ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-WRITE-SRC
   label labelu GB-HB-BUILD ;

: GAP-AOT-ASSERT ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu label:ptr labelu :}
   label labelu GB-AOT-REPORT
   mode modeu label labelu GB-GJA ;

: GAP-STRIPPED-TEXT-MAX ( -- n )
   HB-TARGET-LINUX? if GAP-ELF-STRIPPED-TEXT-MAX exit then
   HB-TARGET-MACOS? if GAP-MACHO-STRIPPED-TEXT-MAX exit then
   GB-TARGET-UNKNOWN ;

: GAP-FIB-SOURCE ( -- )
   GE-SRC-RESET
   s" : FIB ( n -- n ) DUP 2 < IF EXIT THEN DUP 1 - RECURSE SWAP 2 - RECURSE + ;" GE-SRC-LINE
   s" : MAIN ( -- ) 10 FIB . CR ;" GE-SRC-LINE ;

: GAP-FIB ( -- )
   s" hb-at.f" s" hb-at" s" hb-at-call-report.json" GAP-PATHS
   GAP-FIB-SOURCE
   s" hb-build AOT FIB" GAP-BUILD
   SB-RESET s" 55" GE-OUT-LINE GE-SB-LF
   SB$ s" hb-build AOT output" GB-RUN-EXPECT
   GB-OUT$ GB-EXEC-TEXT-SIZE {: textsz :}
   textsz GAP-STRIPPED-TEXT-MAX >= if s" hb-build AOT stripped text" GE-FAIL then
   s" hb-build AOT dynamic ELF shape" GAP-ASSERT-LINUX-DYNAMIC-ELF
   s" aot-stripped" s" aot-stripped call report" GAP-AOT-ASSERT
   s" PASS: hb-build AOT (engine stripped, text " type
   textsz GB-U.
   s"  B vs ~11800 embed)" type cr ;

: GAP-COMPACT-SOURCE ( -- )
   GE-SRC-RESET
   s" : BIG ( i64 -- i64 ) 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ ;" GE-SRC-LINE
   s" : WRAP ( i64 -- i64 ) DUP 0< IF NEGATE ELSE BIG THEN 1+ ;" GE-SRC-LINE
   s" : MAIN ( -- ) 1 WRAP . " GE-SRC+
   s" ok" GE-SRC-S"
   s"  type CR ;" GE-SRC-LINE ;

: GAP-COMPACT ( -- )
   s" hb-compact.f" s" hb-compact" s" hb-compact-call-report.json" GAP-PATHS
   GAP-COMPACT-SOURCE
   s" hb-build AOT compact calls" GAP-BUILD
   SB-RESET s" 22" GE-OUT-LINE s" ok" GE-OUT-LINE
   SB$ s" hb-build AOT compact call output" GB-RUN-EXPECT
   s" aot-compact" s" aot-compact call report" GAP-AOT-ASSERT
   s" PASS: hb-build AOT compact call layout" type cr ;

: GAP-CLOSURE-LINE ( n -- ) {: n :}
   s" : W" GE-SRC+
   n GE-SRC-U+
   s"  ( -- n ) W" GE-SRC+
   n 1+ GE-SRC-U+
   s"  1 + ;" GE-SRC-LINE ;

: GAP-CLOSURE-SOURCE ( -- )
   GE-SRC-RESET
   s" : W259 ( -- n ) 1 ;" GE-SRC-LINE
   258 begin dup -1 > while
      dup GAP-CLOSURE-LINE
      1-
   repeat drop
   s" : MAIN ( -- ) W0 . CR ;" GE-SRC-LINE ;

: GAP-CLOSURE ( -- )
   s" hb-cl.f" s" hb-cl" s" hb-cl-report.json" GAP-PATHS
   GAP-CLOSURE-SOURCE
   s" hb-build AOT closure stress" GAP-BUILD
   SB-RESET s" 260" GE-OUT-LINE GE-SB-LF
   SB$ s" hb-build AOT closure stress output" GB-RUN-EXPECT
   s" PASS: hb-build AOT closure stress (260 reachable words)" type cr ;

: GAP-LONG-SOURCE ( -- )
   GE-SRC-RESET
   s" : LONG-AOT-CALLED-WORD-NAME ( -- n ) 34 ;" GE-SRC-LINE
   s" : MAIN ( -- ) LONG-AOT-CALLED-WORD-NAME . CR ;" GE-SRC-LINE ;

: GAP-LONG ( -- )
   s" hb-aot-long.f" s" hb-aot-long" s" hb-aot-long-report.json" GAP-PATHS
   GAP-LONG-SOURCE
   s" hb-build AOT long names" GAP-BUILD
   SB-RESET s" 34" GE-OUT-LINE GE-SB-LF
   SB$ s" hb-build AOT long-name output" GB-RUN-EXPECT
   s" PASS: hb-build AOT long dictionary names" type cr ;

: GAP-SQUOTE-SOURCE ( -- )
   GE-SRC-RESET
   s" : MAIN ( -- ) " GE-SRC+
   s" hi" GE-SRC-S"
   s"  type CR ;" GE-SRC-LINE ;

: GAP-SQUOTE ( -- )
   s" hb-str.f" s" hb-str" s" hb-str-report.json" GAP-PATHS
   GAP-SQUOTE-SOURCE
   s" hb-build AOT S-quote build" GAP-BUILD
   SB-RESET s" hi" GE-OUT-LINE
   SB$ s" hb-build AOT S-quote output" GB-RUN-EXPECT
   s" PASS: hb-build AOT S-quote string literal (PC-relative, relocation-safe)" type cr ;

: GAP-PARSE-SOURCE ( -- )
   GE-SRC-RESET
   s" : MAIN ( -- ) " GE-SRC+
   s" hi" GAP-SRC-DOTQ
   s"  CR " GE-SRC+
   s" ok" GAP-SRC-CQ
   s"  count type CR ;" GE-SRC-LINE ;

: GAP-PARSE ( -- )
   s" hb-parse.f" s" hb-parse" s" hb-parse-report.json" GAP-PATHS
   GAP-PARSE-SOURCE
   s" hb-build AOT parsing words" GAP-BUILD
   SB-RESET s" hi" GE-OUT-LINE s" ok" GE-OUT-LINE
   SB$ s" hb-build AOT parsing-word output" GB-RUN-EXPECT
   s" PASS: hb-build AOT dot-quote/C-quote parsing words" type cr ;

: GAP-RUN ( -- )
   s" hb-gate-aot-positive" GT-START
   GAP-FIB
   GAP-COMPACT
   GAP-CLOSURE
   GAP-LONG
   GAP-SQUOTE
   GAP-PARSE
   GT-CLEANUP
   s" PASS: native hb-build AOT positive gate phase" type cr ;

GAP-RUN
