\ gate-aot-positive.f - checked runner for positive hb-build AOT checks.
\
\ Load after test/gate-build-common.f and test/gate-build-hbb.f.

46 constant GAP-DOT
99 constant GAP-C-LOWER
$10000 constant GAP-STRIPPED-TEXT-MAX

: GAP-N= ( n n ptr u8 n -- ) {: got:n want:n label:ptr labelu:n :}
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

: GAP-ELF-TEXT-SZ ( -- n )
   0 GAP-PH-FILESZ ;

: GAP-ELF-RW-VA ( -- n )
   GB-ELF-VMBASE GAP-ELF-TEXT-SZ + ;

: GAP-ELF-DLOPEN-SLOT ( -- n )
   GAP-ELF-RW-VA GB-ELF-DLOPEN-SLOT-OFF + ;

: GAP-ELF-DLSYM-SLOT ( -- n )
   GAP-ELF-RW-VA GB-ELF-DLSYM-SLOT-OFF + ;

: GAP-ASSERT-LINUX-DYNAMIC-ELF ( ptr u8 n -- ) {: label:ptr labelu:n :}
   HB-TARGET-LINUX? 0= if exit then
   GB-OUT$ GB-READ-EXEC
   GB-ELF-PHNUM-OFF GB-U16-OFF 4 label labelu GAP-N=
   0 GAP-PH-TYPE GB-ELF-PT-LOAD label labelu GAP-N=
   0 GAP-PH-FLAGS GB-ELF-PF-R GB-ELF-PF-X or label labelu GAP-N=
   1 GAP-PH-TYPE GB-ELF-PT-LOAD label labelu GAP-N=
   1 GAP-PH-FLAGS GB-ELF-PF-R GB-ELF-PF-W or label labelu GAP-N=
   1 GAP-PH-VADDR GAP-ELF-RW-VA label labelu GAP-N=
   1 GAP-PH-FILESZ GB-ELF-RW-SZ label labelu GAP-N=
   2 GAP-PH-TYPE GB-ELF-PT-INTERP label labelu GAP-N=
   2 GAP-PH-FILE-OFF GB-ELF-INTERP-OFF label labelu GAP-N=
   2 GAP-PH-FILESZ GB-ELF-INTERP-SZ label labelu GAP-N=
   3 GAP-PH-TYPE GB-ELF-PT-DYNAMIC label labelu GAP-N=
   3 GAP-PH-FILE-OFF 1 GAP-PH-FILE-OFF label labelu GAP-N=
   3 GAP-PH-VADDR GAP-ELF-RW-VA label labelu GAP-N=
   3 GAP-PH-FILESZ GB-ELF-DYNAMIC-SZ label labelu GAP-N=
   GB-ELF-RELA-OFF GB-U64-OFF GAP-ELF-DLOPEN-SLOT label labelu GAP-N=
   GB-ELF-RELA-OFF 8 + GB-U64-OFF GB-ELF-DLOPEN-RINFO label labelu GAP-N=
   GB-ELF-RELA-OFF 24 + GB-U64-OFF GAP-ELF-DLSYM-SLOT label labelu GAP-N=
   GB-ELF-RELA-OFF 32 + GB-U64-OFF GB-ELF-DLSYM-RINFO label labelu GAP-N= ;

: GAP-SRC-DOTQ ( ptr u8 n -- ) {: a:ptr u:n :}
   GAP-DOT GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GAP-SRC-CQ ( ptr u8 n -- ) {: a:ptr u:n :}
   GAP-C-LOWER GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: GAP-PATHS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n report:ptr reportu:n :}
   src srcu GB-SRC!
   out outu GB-OUT!
   report reportu GB-REPORT! ;

: GAP-BUILD-STRICT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GB-HBB-BUILD-STRICT ;

: GAP-AOT-ASSERT ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n label:ptr labelu:n :}
   mode modeu label labelu GB-GJA ;

: GAP-FIB-DEFS ( -- )
   s" : FIB ( n -- n ) DUP 2 < IF EXIT THEN DUP 1 - RECURSE SWAP 2 - RECURSE + ;" GE-SRC-LINE ;

: GAP-COMPACT-DEFS ( -- )
   s" : BIG ( i64 -- i64 ) 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ ;" GE-SRC-LINE
   s" : WRAP ( i64 -- i64 ) DUP 0< IF NEGATE ELSE BIG THEN 1+ ;" GE-SRC-LINE ;

: GAP-CLOSURE-LINE ( n -- ) {: n:n :}
   s" : GAPW" GE-SRC+
   n GE-SRC-U+
   s"  ( -- n ) GAPW" GE-SRC+
   n 1+ GE-SRC-U+
   s"  1 + ;" GE-SRC-LINE ;

: GAP-FEATURE-DEFS ( -- )
   s" : GAPW259 ( -- n ) 1 ;" GE-SRC-LINE
   258 begin dup -1 > while
      dup GAP-CLOSURE-LINE
      1-
   repeat drop
   s" : LONG-AOT-CALLED-WORD-NAME ( -- n ) 34 ;" GE-SRC-LINE ;

: GAP-BUNDLE-MAIN ( -- )
   s" : MAIN ( -- ) 10 FIB . CR 1 WRAP . " GE-SRC+
   s" ok" GE-SRC-S"
   s"  type CR GAPW0 . CR LONG-AOT-CALLED-WORD-NAME . CR " GE-SRC+
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
   GB-OUT$ GB-EXEC-TEXT-SIZE {: textsz:n :}
   textsz GAP-STRIPPED-TEXT-MAX >= if s" hb-build AOT stripped text" GE-FAIL then
   s" hb-build AOT dynamic ELF shape" GAP-ASSERT-LINUX-DYNAMIC-ELF
   s" hb-build AOT call report" GB-AOT-REPORT
   s" aot-stripped" s" aot-stripped call report" GAP-AOT-ASSERT
   s" aot-compact" s" aot-compact call report" GAP-AOT-ASSERT
   s" PASS: hb-build AOT strict compact/feature coverage (text " type
   textsz GB-U.
   s"  B)" type cr ;

\ Persistent data region: a program that builds a compile-time table with
\ create/comma, reads it in a runtime ?do/loop, and accumulates into a
\ variable via @/!/+!. Proves the AOT entry maps DATA-VA, restores the
\ persistent content, and sets up the return/loop stack.
: GAP-DATA-SOURCE ( -- )
   GE-SRC-RESET
   s" create TABLE 10 , 20 , 30 ," GE-SRC-LINE
   s" variable SUM" GE-SRC-LINE
   s" : MAIN ( -- ) 0 SUM ! 3 0 ?do TABLE i 8 * + @ SUM +! loop SUM @ . ;" GE-SRC-LINE ;

: GAP-DATA-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 60" GE-OUT-LINE
   SB$ ;

: GAP-DATA ( -- )
   s" hb-aot-data.f" s" hb-aot-data" s" hb-aot-data-report.json" GAP-PATHS
   GAP-DATA-SOURCE
   s" hb-build AOT data region build" GAP-BUILD-STRICT
   GAP-DATA-EXPECT s" hb-build AOT data region output" GB-RUN-EXPECT
   s" PASS: hb-build AOT persistent data region (create/,/variable/@/!/+!/loop)" type cr ;

\ item 10 slice 5: a preseeded bad-tag object/AOT test entry. A source declaring a
\ matched family + helper is AOT-built with a SELECTED non-MAIN entry (the helper)
\ and a forged value-stack seed (payload slots + an out-of-range tag), so the
\ stripped image starts at the helper and reaches its inline invalid-tag die
\ (rc E-BAD-TAG 85 + "hb: bad gemt tag"). The SAME source built normally (entry
\ MAIN) exits 0, and the entry/seed/mode axis is folded into every cache layer
\ (artifact key + source-index key + object bytes) so the two are distinct
\ artifacts with no cross-restore in either direction, and the die survives an
\ object-cache relink. docs/census-tfam-10.md.
: GAP-PRESEED-SRC ( -- )                        \ matched family + helper + trivial MAIN
   GE-SRC-RESET
   s" SUMTYPE gemt 0" GE-SRC-LINE
   s"   VARIANT one n ;VARIANT" GE-SRC-LINE
   s"   VARIANT two n n ;VARIANT" GE-SRC-LINE
   s"   VARIANT nil ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" : HLP ( gemt -- n ) MATCH gemt one OF ENDOF two OF + ENDOF nil OF 999 ENDOF ;MATCH ;" GE-SRC-LINE
   s" : MAIN ( -- ) ;" GE-SRC-LINE ;

\ Bundle width M+1 = 3 cells: pad, pad, out-of-range tag 5 (gemt tags 0..2 valid),
\ each cell a big-endian u64 (16 hex chars), bottom-of-stack first / tag last.
: GAP-PRESEED-SEED$ ( -- ptr u8 n )
   s" 000000000000000000000000000000000000000000000005" ;

: GAP-PRESEED-ARM ( -- )                        \ select the non-MAIN entry + forged seed
   s" HLP" HBB-PRESEED-ENTRY!
   GAP-PRESEED-SEED$ HBB-PRESEED-SEED! ;

: GAP-PRESEED-BUILD ( -- )
   GB-WRITE-SRC
   GB-HBB-PREPARE
   GAP-PRESEED-ARM
   s" hb-build AOT preseed bad-tag entry build" GB-HBB-BUILD-OUT ;

: GAP-PRESEED-BUILD-JSON ( -- )                 \ --json flips the artifact key only -> object-cache relink
   GB-WRITE-SRC
   GB-HBB-PREPARE
   -1 HBB-JSON !
   GAP-PRESEED-ARM
   s" hb-build AOT preseed object-cache relink build" GB-HBB-BUILD-OUT ;

: GAP-PRESEED-RUN-BAD ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GE-HB-RESET
   GB-OUT$ GE-TIMEOUT-MS GE-RUN-ENV
   85 label labelu GE-EXPECT-RC
   s" hb: bad gemt tag" label labelu GE-EXPECT-ERR-HAS ;

: GAP-PRESEED ( -- )
   \ This assertion owns a fresh cache: without one, HBB-OBJECT-HIT is
   \ structurally impossible; a shared warm artifact can also bypass production.
   GT-ROOT HBB-CACHE-ROOT!
   s" hb-aot-preseed.f" s" hb-aot-preseed" s" hb-aot-preseed-report.json" GAP-PATHS
   GAP-PRESEED-SRC
   s" hb-build AOT preseed normal-MAIN control" GB-HBB-BUILD
   s" hb-build AOT preseed normal-MAIN exits 0" GB-RUN-OUT
   GAP-PRESEED-BUILD
   s" hb-build AOT preseed bad-tag entry run" GAP-PRESEED-RUN-BAD
   GAP-PRESEED-BUILD
   s" hb-build AOT preseed restore" GAP-PRESEED-RUN-BAD
   GAP-PRESEED-BUILD-JSON
   HBB-OBJECT-HIT @ 0= if s" hb-build AOT preseed object-cache hit" GE-FAIL then
   s" hb-build AOT preseed object-cache relink run" GAP-PRESEED-RUN-BAD
   s" hb-build AOT preseed normal-MAIN control (bis)" GB-HBB-BUILD
   s" hb-build AOT preseed normal-MAIN still exits 0" GB-RUN-OUT
   s" PASS: hb-build AOT preseeded bad-tag entry (rc 85 hb: bad gemt tag; three-key lockstep; object relink)" type cr ;

: GAP-RUN ( -- )
   s" hb-gate-aot-positive" GT-START
   GAP-BUNDLE
   GAP-DATA
   GAP-PRESEED
   GT-CLEANUP
   s" PASS: native hb-build AOT positive gate phase" type cr ;
