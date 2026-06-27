\ gate-aot-positive.f - checked runner for positive hb-build AOT checks.
\
\ Load after test/gate-build-common.f and test/gate-build-hbb.f.

46 constant GAP-DOT
99 constant GAP-C-LOWER
$10000 constant GAP-STRIPPED-TEXT-MAX

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

: GAP-BUILD-STRICT ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu GB-HBB-BUILD-STRICT ;

: GAP-AOT-ASSERT ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu label:ptr labelu :}
   mode modeu label labelu GB-GJA ;

: GAP-FIB-DEFS ( -- )
   s" : FIB ( n -- n ) DUP 2 < IF EXIT THEN DUP 1 - RECURSE SWAP 2 - RECURSE + ;" GE-SRC-LINE ;

: GAP-COMPACT-DEFS ( -- )
   s" : BIG ( i64 -- i64 ) 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ ;" GE-SRC-LINE
   s" : WRAP ( i64 -- i64 ) DUP 0< IF NEGATE ELSE BIG THEN 1+ ;" GE-SRC-LINE ;

: GAP-CLOSURE-LINE ( n -- ) {: n :}
   s" : W" GE-SRC+
   n GE-SRC-U+
   s"  ( -- n ) W" GE-SRC+
   n 1+ GE-SRC-U+
   s"  1 + ;" GE-SRC-LINE ;

: GAP-FEATURE-DEFS ( -- )
   s" : W259 ( -- n ) 1 ;" GE-SRC-LINE
   258 begin dup -1 > while
      dup GAP-CLOSURE-LINE
      1-
   repeat drop
   s" : LONG-AOT-CALLED-WORD-NAME ( -- n ) 34 ;" GE-SRC-LINE ;

: GAP-BUNDLE-MAIN ( -- )
   s" : MAIN ( -- ) 10 FIB . CR 1 WRAP . " GE-SRC+
   s" ok" GE-SRC-S"
   s"  type CR W0 . CR LONG-AOT-CALLED-WORD-NAME . CR " GE-SRC+
   s" hi" GAP-SRC-DOTQ
   s"  CR " GE-SRC+
   s" ok" GAP-SRC-CQ
   s"  count type CR ;" GE-SRC-LINE ;

: GAP-BUNDLE-SOURCE ( -- )
   GE-SRC-RESET
   GAP-FIB-DEFS
   GAP-COMPACT-DEFS
   GAP-FEATURE-DEFS
   GAP-BUNDLE-MAIN ;

: GAP-BUNDLE-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 55" GE-OUT-LINE GE-SB-LF
   s" 22" GE-OUT-LINE s" ok" GE-OUT-LINE
   s" 260" GE-OUT-LINE GE-SB-LF s" 34" GE-OUT-LINE GE-SB-LF
   s" hi" GE-OUT-LINE s" ok" GE-OUT-LINE
   SB$ ;

: GAP-BUNDLE ( -- )
   s" hb-aot-bundle.f" s" hb-aot-bundle" s" hb-aot-bundle-report.json" GAP-PATHS
   GAP-BUNDLE-SOURCE
   s" hb-build AOT strict compact/features" GAP-BUILD-STRICT
   GAP-BUNDLE-EXPECT s" hb-build AOT compact/features output" GB-RUN-EXPECT
   GB-OUT$ GB-EXEC-TEXT-SIZE {: textsz :}
   textsz GAP-STRIPPED-TEXT-MAX >= if s" hb-build AOT stripped text" GE-FAIL then
   s" hb-build AOT dynamic ELF shape" GAP-ASSERT-LINUX-DYNAMIC-ELF
   s" hb-build AOT call report" GB-AOT-REPORT
   s" aot-stripped" s" aot-stripped call report" GAP-AOT-ASSERT
   s" aot-compact" s" aot-compact call report" GAP-AOT-ASSERT
   s" PASS: hb-build AOT strict compact/feature coverage (text " type
   textsz GB-U.
   s"  B)" type cr ;

: GAP-RUN ( -- )
   s" hb-gate-aot-positive" GT-START
   GAP-BUNDLE
   GT-CLEANUP
   s" PASS: native hb-build AOT positive gate phase" type cr ;

GAP-RUN
