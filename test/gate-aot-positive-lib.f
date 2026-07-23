\ gate-aot-positive.f - checked runner for positive hb-build AOT checks.
\
\ Load after test/gate-build-common.f and test/gate-build-hbb.f.

require test/gate-pool.f

package AOT-POSITIVE

46 constant DOT
99 constant C-LOWER
$10000 constant STRIPPED-TEXT-MAX
$D63F0200 constant BLR-X16     \ arm64 `blr x16`: an indirect engine-address call the linker never emits

variable BLR-CNT
\ Count `blr x16` words in the built image's executable text region. A correctly
\ linked stripped image has none: under the direct-BL-only contract every native call
\ is a direct BL that the linker relocates in place to a PC-relative branch into the
\ copied blobs (aot-lib.f COPY-COMPACT-BLOB / RELOC-W32), so a surviving blr x16 is an
\ un-relocated build-time engine address that would crash at load.
: COUNT-BLR-X16 ( n n -- n ) {: foff:n fsize:n :}      \ text-file-offset text-size -- count
   0 BLR-CNT !
   foff begin dup foff fsize + < while
      dup GB-U32-OFF BLR-X16 = if 1 BLR-CNT +! then
      4 +
   repeat drop
   BLR-CNT @ ;

: ASSERT-BLR-ABSENT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GB-OUT$ GB-EXEC-TEXT-RANGE COUNT-BLR-X16
   0 <> if label labelu GE-FAIL then ;

: N= ( n n ptr u8 n -- ) {: got:n want:n label:ptr labelu:n :}
   got want <> if label labelu GE-FAIL then ;

: PH-TYPE ( n -- n )
   GB-ELF-PH-OFF GB-U32-OFF ;

: PH-FLAGS ( n -- n )
   GB-ELF-PH-OFF GB-ELF-PH-FLAGS-OFF + GB-U32-OFF ;

: PH-FILE-OFF ( n -- n )
   GB-ELF-PH-OFF GB-ELF-PH-FILE-OFF + GB-U64-OFF ;

: PH-VADDR ( n -- n )
   GB-ELF-PH-OFF GB-ELF-PH-VADDR-OFF + GB-U64-OFF ;

: PH-FILESZ ( n -- n )
   GB-ELF-PH-OFF GB-ELF-PH-FILESZ-OFF + GB-U64-OFF ;

: ELF-TEXT-SZ ( -- n )
   0 PH-FILESZ ;

: ELF-RW-VA ( -- n )
   GB-ELF-VMBASE ELF-TEXT-SZ + ;

: ELF-DLOPEN-SLOT ( -- n )
   ELF-RW-VA GB-ELF-DLOPEN-SLOT-OFF + ;

: ELF-DLSYM-SLOT ( -- n )
   ELF-RW-VA GB-ELF-DLSYM-SLOT-OFF + ;

: ASSERT-DYNAMIC-ELF ( ptr u8 n -- ) {: label:ptr labelu:n :}
   HB-TARGET-LINUX? 0= if exit then
   GB-OUT$ GB-READ-EXEC
   GB-ELF-PHNUM-OFF GB-U16-OFF 4 label labelu N=
   0 PH-TYPE GB-ELF-PT-LOAD label labelu N=
   0 PH-FLAGS GB-ELF-PF-R GB-ELF-PF-X or label labelu N=
   1 PH-TYPE GB-ELF-PT-LOAD label labelu N=
   1 PH-FLAGS GB-ELF-PF-R GB-ELF-PF-W or label labelu N=
   1 PH-VADDR ELF-RW-VA label labelu N=
   1 PH-FILESZ GB-ELF-RW-SZ label labelu N=
   2 PH-TYPE GB-ELF-PT-INTERP label labelu N=
   2 PH-FILE-OFF GB-ELF-INTERP-OFF label labelu N=
   2 PH-FILESZ GB-ELF-INTERP-SZ label labelu N=
   3 PH-TYPE GB-ELF-PT-DYNAMIC label labelu N=
   3 PH-FILE-OFF 1 PH-FILE-OFF label labelu N=
   3 PH-VADDR ELF-RW-VA label labelu N=
   3 PH-FILESZ GB-ELF-DYNAMIC-SZ label labelu N=
   GB-ELF-RELA-OFF GB-U64-OFF ELF-DLOPEN-SLOT label labelu N=
   GB-ELF-RELA-OFF 8 + GB-U64-OFF GB-ELF-DLOPEN-RINFO label labelu N=
   GB-ELF-RELA-OFF 24 + GB-U64-OFF ELF-DLSYM-SLOT label labelu N=
   GB-ELF-RELA-OFF 32 + GB-U64-OFF GB-ELF-DLSYM-RINFO label labelu N= ;

: SRC-DOTQ ( ptr u8 n -- ) {: a:ptr u:n :}
   DOT GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: SRC-CQ ( ptr u8 n -- ) {: a:ptr u:n :}
   C-LOWER GE-SRC-C
   GE-DQ GE-SRC-C
   GE-SRC-SP
   a u GE-SRC+
   GE-DQ GE-SRC-C ;

: PATHS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n report:ptr reportu:n :}
   src srcu GB-SRC!
   out outu GB-OUT!
   report reportu GB-REPORT! ;

: BUILD-STRICT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GB-HBB-BUILD-STRICT ;

: AOT-ASSERT ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n label:ptr labelu:n :}
   mode modeu label labelu GB-GJA ;

: FIB-DEFS ( -- )
   s" : FIB ( n -- n ) DUP 2 < IF EXIT THEN DUP 1 - RECURSE SWAP 2 - RECURSE + ;" GE-SRC-LINE ;

: COMPACT-DEFS ( -- )
   s" : BIG ( i64 -- i64 ) 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ 1+ ;" GE-SRC-LINE
   s" : WRAP ( i64 -- i64 ) DUP 0< IF NEGATE ELSE BIG THEN 1+ ;" GE-SRC-LINE ;

: CLOSURE-LINE ( n -- ) {: n:n :}
   s" : GAPW" GE-SRC+
   n GE-SRC-U+
   s"  ( -- n ) GAPW" GE-SRC+
   n 1+ GE-SRC-U+
   s"  1 + ;" GE-SRC-LINE ;

: FEATURE-DEFS ( -- )
   s" : GAPW259 ( -- n ) 1 ;" GE-SRC-LINE
   258 begin dup -1 > while
      dup CLOSURE-LINE
      1-
   repeat drop
   s" : LONG-AOT-CALLED-WORD-NAME ( -- n ) 34 ;" GE-SRC-LINE ;

: BUNDLE-MAIN ( -- )
   s" : MAIN ( -- ) 10 FIB . CR 1 WRAP . " GE-SRC+
   s" ok" GE-SRC-S"
   s"  type CR GAPW0 . CR LONG-AOT-CALLED-WORD-NAME . CR " GE-SRC+
   s" hi" SRC-DOTQ
   s"  CR " GE-SRC+
   s" ok" SRC-CQ
   s"  count type CR ;" GE-SRC-LINE ;

: SURFACE-BASE-DEFS ( -- )
   s" package AOT-SURFACE-HOSTILE" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : SENTSET ( -- ) ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package AOT-SURFACE-TEST" GE-SRC-LINE
   s\" : AS-FAIL ( bool -- ) 0= if s\" AOT-LINK surface mismatch\" 74 die then ;" GE-SRC-LINE
   s\" : AS-NS ( -- ptr a ) s\" AOT-LINK\" XREF-NAMESPACE-WL XREF-FIND-WL ;" GE-SRC-LINE
   s" : AS-PUB ( -- n ) AS-NS XREF-START ;" GE-SRC-LINE
   s" : AS-PRI ( -- n ) AS-NS XREF-LEN ;" GE-SRC-LINE
   s\" : AS-HOST-PUB ( -- n ) s\" AOT-SURFACE-HOSTILE\" XREF-NAMESPACE-WL XREF-FIND-WL XREF-START ;" GE-SRC-LINE ;

: SURFACE-PUB-DEFS ( -- )
   s\" : AS-PUB-NAME? ( ptr u8 n -- bool ) 2dup s\" LINK\" XREF-STR=CI >r s\" RUN\" XREF-STR=CI r> or ;" GE-SRC-LINE
   s" : AS-PUB-CHECK ( -- ) AS-NS XREF-FOUND? AS-FAIL" GE-SRC+
   s"  0 ndict@ 0 ?do i XREF-REC dup XREF-WORDLIST AS-PUB = if" GE-SRC+
   s"  dup XREF-NAME$ AS-PUB-NAME? AS-FAIL drop 1+" GE-SRC+
   s"  else drop then loop 2 = AS-FAIL ;" GE-SRC-LINE ;

: SURFACE-ID-1 ( -- )
   s\"  s\" AOT-DBASE@\" AS-ID s\" AOT-DBASE-N\" AS-ID s\" AOT-CP-N\" AS-ID" GE-SRC-LINE
   s\"  s\" AOT-PTR@\" AS-ID s\" AOT-W32@\" AS-ID s\" MASK\" AS-ID" GE-SRC-LINE
   s\"  s\" OPCODE\" AS-ID s\" DELTA-MASK\" AS-ID s\" SIGN\" AS-ID" GE-SRC-LINE
   s\"  s\" CALL-MASK\" AS-ID s\" CALL-OP\" AS-ID s\" SIGNED\" AS-ID" GE-SRC-LINE ;

: SURFACE-ID-2 ( -- )
   s\"  s\" DIRECT?\" AS-ID s\" CALL?\" AS-ID s\" TARGET\" AS-ID" GE-SRC-LINE
   s\"  s\" REC\" AS-ID s\" AOT-FOLD\" AS-ID s\" REC-NAME-LEN\" AS-ID" GE-SRC-LINE
   s\"  s\" REC-NAME-PTR\" AS-ID s\" REC-NAME@\" AS-ID s\" REC-NAME-C@\" AS-ID" GE-SRC-LINE
   s\"  s\" REC-NAME=\" AS-ID s\" ENTRY-NAME-BUF\" AS-ID s\" ENTRY-NAME-U\" AS-ID" GE-SRC-LINE ;

: SURFACE-ID-3 ( -- )
   s\"  s\" ENTRY-NAME$\" AS-ID s\" ENTRY-NAME!\" AS-ID s\" MAIN?\" AS-ID" GE-SRC-LINE
   s\"  s\" AOT-UNSAFE?\" AS-ID s\" AECH\" AS-ID s\" AE1\" AS-ID" GE-SRC-LINE
   s\"  s\" AETXT\" AS-ID s\" AEREC-TXT\" AS-ID s\" AEJCHAR\" AS-ID" GE-SRC-LINE
   s\"  s\" AEJSTR\" AS-ID s\" AEJKEY\" AS-ID s\" AEJREC\" AS-ID" GE-SRC-LINE ;

: SURFACE-ID-4 ( -- )
   s\"  s\" AENB\" AS-ID s\" AENV\" AS-ID s\" AENN\" AS-ID" GE-SRC-LINE
   s\"  s\" AEJNUM\" AS-ID s\" AOT-UNSAFE-JSON\" AS-ID s\" AOT-UNSAFE-PROSE\" AS-ID" GE-SRC-LINE
   s\"  s\" AOT-UNSAFE-DIE\" AS-ID s\" FX\" AS-ID s\" REC-CODE-PTR\" AS-ID" GE-SRC-LINE
   s\"  s\" REC-CODE-PTR@\" AS-ID s\" REC-WID@\" AS-ID s\" FINDMAIN\" AS-ID" GE-SRC-LINE ;

: SURFACE-ID-5 ( -- )
   s\"  s\" MAX-CLO\" AS-ID s\" CLO\" AS-ID s\" NCLO\" AS-ID" GE-SRC-LINE
   s\"  s\" CLO-CX\" AS-ID s\" ROOTREC\" AS-ID s\" CLO-LIMIT\" AS-ID" GE-SRC-LINE
   s\"  s\" CLO-LIMIT!\" AS-ID s\" IN-CLO?\" AS-ID s\" CLO-OVERFLOW-JSON\" AS-ID" GE-SRC-LINE
   s\"  s\" CLO-OVERFLOW-PROSE\" AS-ID s\" CLO-OVERFLOW-DIE\" AS-ID s\" ADD-CLO\" AS-ID" GE-SRC-LINE ;

: SURFACE-ID-6 ( -- )
   s\"  s\" SP2\" AS-ID s\" SEND\" AS-ID s\" SCAN-CALLEE\" AS-ID" GE-SRC-LINE
   s\"  s\" FINDADDR-PTR\" AS-ID s\" SCAN-DIRECT\" AS-ID s\" SCAN-REC\" AS-ID" GE-SRC-LINE
   s\"  s\" WI\" AS-ID s\" NO-ENTRY-DIE\" AS-ID s\" CLOSURE\" AS-ID" GE-SRC-LINE ;

: SURFACE-ID-ROWS ( -- )
   SURFACE-ID-1 SURFACE-ID-2 SURFACE-ID-3
   SURFACE-ID-4 SURFACE-ID-5 SURFACE-ID-6 ;

: SURFACE-PRI-DEFS ( -- )
   s" : AS-NOT-EXPOSED-WL? ( ptr a n -- bool ) over XREF-NAME$ rot XREF-FIND-WL <> ;" GE-SRC-LINE
   s" : AS-BARE-IDENTITY? ( ptr a -- bool ) 0 AS-NOT-EXPOSED-WL? ;" GE-SRC-LINE
   s" : AS-QUAL-IDENTITY? ( ptr a -- bool ) AS-PUB AS-NOT-EXPOSED-WL? ;" GE-SRC-LINE
   s" : AS-NAME-ABSENT? ( ptr u8 n n -- bool ) XREF-FIND-WL XREF-FOUND? 0= ;" GE-SRC-LINE
   s" : AS-BARE-NAME ( ptr u8 n -- ) 0 AS-NAME-ABSENT? AS-FAIL ;" GE-SRC-LINE
   s" : AS-QUAL-NAME ( ptr u8 n -- ) AS-PUB AS-NAME-ABSENT? AS-FAIL ;" GE-SRC-LINE
   s" : AS-NAMED ( ptr u8 n -- ) 2dup AS-BARE-NAME AS-QUAL-NAME ;" GE-SRC-LINE
   s" : AS-PRIVATE ( ptr u8 n -- ptr a ) AS-PRI XREF-FIND-WL dup XREF-FOUND? AS-FAIL ;" GE-SRC-LINE
   s\" : AS-HOSTILE-CHECK ( -- ) s\" SENTSET\" AS-PRIVATE" GE-SRC+
   s"  dup AS-HOST-PUB AS-NOT-EXPOSED-WL? AS-FAIL drop" GE-SRC+
   s\"  s\" SENTSET\" AS-HOST-PUB AS-NAME-ABSENT? 0= AS-FAIL ;" GE-SRC-LINE
   s\" : AS-MAIN-CHECK ( -- ) s\" MAIN\" 0 XREF-FIND-WL XREF-FOUND? AS-FAIL" GE-SRC+
   s\"  s\" MAIN\" AS-PRI XREF-FIND-WL XREF-FOUND? 0= AS-FAIL ;" GE-SRC-LINE
   s" : AS-NAMED-CHECK ( -- )" GE-SRC+
   s\"  s\" SENTSET\" AS-NAMED s\" READ-PROG\" AS-NAMED" GE-SRC+
   s\"  s\" MAP-IN-BLOB\" AS-NAMED s\" COPY-COMPACT-BLOB\" AS-NAMED" GE-SRC+
   s\"  s\" SEED+\" AS-NAMED s\" CELL-TEXTPTR?\" AS-NAMED ;" GE-SRC-LINE
   s" variable AS-ID-N" GE-SRC-LINE
   s" 69 constant AS-ID-TOTAL" GE-SRC-LINE
   s" : AS-ID ( ptr u8 n -- ) 2dup AS-PRIVATE drop AS-NAMED 1 AS-ID-N +! ;" GE-SRC-LINE
   s" : AS-ID-CHECK ( -- ) 0 AS-ID-N !" GE-SRC-LINE
   SURFACE-ID-ROWS
   s"  AS-ID-N @ AS-ID-TOTAL = AS-FAIL ;" GE-SRC-LINE
   s\" : AS-NO-BRANCH-NS ( -- ) s\" AOT-BRANCH\" XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? 0= AS-FAIL ;" GE-SRC-LINE
   s" : AS-PRI-CHECK ( -- ) 0 ndict@ 0 ?do" GE-SRC+
   s"  i XREF-REC dup XREF-WORDLIST AS-PRI = if" GE-SRC+
   s"  dup AS-BARE-IDENTITY? AS-FAIL dup AS-QUAL-IDENTITY? AS-FAIL" GE-SRC+
   s"  drop 1+ else drop then loop 0 > AS-FAIL ;" GE-SRC-LINE ;

: SURFACE-DEFS ( -- )
   SURFACE-BASE-DEFS
   SURFACE-PUB-DEFS
   SURFACE-PRI-DEFS
   s\" : AS-RUN ( -- ) s\" AOT-LINK\" 0 XREF-FIND-WL XREF-FOUND? 0= AS-FAIL" GE-SRC+
   s"  AS-HOSTILE-CHECK AS-MAIN-CHECK AS-NAMED-CHECK AS-ID-CHECK AS-NO-BRANCH-NS AS-PUB-CHECK AS-PRI-CHECK ;" GE-SRC-LINE
   s" AS-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: MAIN-OWNER-CHECK ( -- )
   s\" : AMC ( -- ) s\" MAIN\" 0 XREF-FIND-WL XREF-FOUND? 0= if" GE-SRC+
   s\"  s\" AOT global MAIN unavailable\" 74 die then ;" GE-SRC-LINE
   s" AMC" GE-SRC-LINE ;

: BUNDLE-SOURCE ( -- )
   GE-SRC-RESET
   FIB-DEFS
   COMPACT-DEFS
   FEATURE-DEFS
   BUNDLE-MAIN
   MAIN-OWNER-CHECK
   SURFACE-DEFS ;

: BUNDLE-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 55" GE-OUT-LINE GE-SB-LF
   s" 22" GE-OUT-LINE s" ok" GE-OUT-LINE
   s" 260" GE-OUT-LINE GE-SB-LF s" 34" GE-OUT-LINE GE-SB-LF
   s" hi" GE-OUT-LINE s" ok" GE-OUT-LINE
   SB$ ;

: BUNDLE ( -- )
   s" hb-aot-bundle.f" s" hb-aot-bundle" s" hb-aot-bundle-report.json" PATHS
   BUNDLE-SOURCE
   s" hb-build AOT strict compact/features" BUILD-STRICT
   BUNDLE-EXPECT s" hb-build AOT compact/features output" GB-RUN-EXPECT
   GB-OUT$ GB-EXEC-TEXT-SIZE {: textsz:n :}
   textsz STRIPPED-TEXT-MAX >= if s" hb-build AOT stripped text" GE-FAIL then
   s" hb-build AOT dynamic ELF shape" ASSERT-DYNAMIC-ELF
   s" hb-build AOT call report" GB-AOT-REPORT
   s" aot-stripped" s" aot-stripped call report" AOT-ASSERT
   s" aot-compact" s" aot-compact call report" AOT-ASSERT
   s" PASS: hb-build AOT strict compact/feature coverage (text " type
   textsz GB-U.
   s"  B)" type cr ;

\ Persistent data region: a program that builds a compile-time table with
\ create/comma, reads it in a runtime ?do/loop, and accumulates into a
\ variable via @/!/+!. Proves the AOT entry maps DATA-VA, restores the
\ persistent content, and sets up the return/loop stack.
\ Load-time self-test of the relocation math the direct-branch capability adds:
\ two adjacent synthetic records prove MAP-IN-BLOB treats a target at a record's
\ end as the NEXT record's start (the >= boundary), and MAP-TARGET relocates that
\ adjacent target to the next record's new offset. Runs while the AOT program is
\ compiled (not reachable from MAIN), so it validates the linker without bloating
\ the built image.
\ Safe to scribble on the live CLO/NEWOFF/NCLO here: this runs at program
\ load/compile time, before this program's own closure walk, and the real build
\ recomputes the closure from scratch (aot-closure.f CLOSURE resets NCLO to 0 and
\ refills CLO/NEWOFF), so the synthetic values cannot leak into the shipped image.
: DATA-SOURCE ( -- )
   GE-SRC-RESET
   s" package AOT-LINK" GE-SRC-LINE
   s" create AMAP-CODE 16 allot" GE-SRC-LINE
   s" create AMAP-R1 DREC allot" GE-SRC-LINE
   s" create AMAP-R2 DREC allot" GE-SRC-LINE
   s" 4 constant AMAP-BODY-LEN" GE-SRC-LINE
   s" 8 constant AMAP-CODE-ROW" GE-SRC-LINE
   s" $40 constant AMAP-R2-OFF" GE-SRC-LINE
   s" : AMAP-REC! ( ptr a ptr u8 n -- ) {: r:ptr code:ptr len:n :} code r 0 ptr-field ! len r 8 + ! ;" GE-SRC-LINE
   s" : AMAP-RECS! ( -- ) AMAP-R1 AMAP-CODE AMAP-BODY-LEN AMAP-REC! AMAP-R2 AMAP-CODE AMAP-CODE-ROW + AMAP-BODY-LEN AMAP-REC! ;" GE-SRC-LINE
   s" : AMAP-CLOSURE! ( -- ) AMAP-R1 CLO 0 ptr-field ! AMAP-R2 CLO 1 ptr-field ! 0 NEWOFF ! AMAP-R2-OFF NEWOFF cell+ ! 2 NCLO ! ;" GE-SRC-LINE
   s" : AMAP-EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :} ok 0= if label labelu 74 die then ;" GE-SRC-LINE
   s" : AMAP-RUN ( -- ) AMAP-RECS! AMAP-CLOSURE!" GE-SRC+
   s"  AMAP-R1 AMAP-CODE AMAP-CODE-ROW + MAP-IN-BLOB -1 =" GE-SRC+
   s\"  s\" AOT closed record range\" AMAP-EXPECT" GE-SRC+
   s"  AMAP-R1 AMAP-CODE AMAP-CODE-ROW + MAP-TARGET AMAP-R2-OFF =" GE-SRC+
   s\"  s\" AOT adjacent record relocation\" AMAP-EXPECT ;" GE-SRC-LINE
   s" AMAP-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" create TABLE 10 , 20 , 30 ," GE-SRC-LINE
   s" variable SUM" GE-SRC-LINE
   s" : MAIN ( -- ) 0 SUM ! 3 0 ?do TABLE i 8 * + @ SUM +! loop SUM @ . ;" GE-SRC-LINE ;

: DATA-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 60" GE-OUT-LINE
   SB$ ;

: DATA ( -- )
   s" hb-aot-data.f" s" hb-aot-data" s" hb-aot-data-report.json" PATHS
   DATA-SOURCE
   s" hb-build AOT data region build" BUILD-STRICT
   DATA-EXPECT s" hb-build AOT data region output" GB-RUN-EXPECT
   s" PASS: hb-build AOT persistent data region (create/,/variable/@/!/+!/loop)" type cr ;

\ Persistent-data code-window regression (dot habu-identify-code-pointers-b973e6cc,
\ red-first). A datum whose VALUE lands in the former [RBASE-VA, RBASE-VA+REGION)
\ magnitude window -- here RBASE-VA+REGION-8, the top cell of the JIT region, free
\ space far above the code high-water -- is NOT a pointer. The old CELL-TEXTPTR?
\ magnitude window MISclassified it and hb-build rejected the program (exit 70); the
\ live-extents test correctly classifies it as data, so the program builds and its
\ MAIN reads the datum back unchanged. The embedded AOT-TEXTPTR-TEST self-test runs in
\ the maker (where CELL-TEXTPTR? lives, not reachable from MAIN so it never bloats the
\ image) and pins BOTH directions: the free-region value is data, a live dict-record
\ address and a live code entry (MAIN's) are pointers. The RBASE-VA/REGION expression is
\ evaluated by the maker, so the case tracks the constants if a later dot moves the region.
: DATA-WINDOW-SOURCE ( -- )
   GE-SRC-RESET
   s" create X RBASE-VA REGION + 8 - ," GE-SRC-LINE
   s\" : MAIN ( -- ) X @ RBASE-VA REGION + 8 - = IF s\" ok\" ELSE s\" bad\" THEN type cr ;" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" : ATP-EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :} ok 0= if label labelu 74 die then ;" GE-SRC-LINE
   s" : ATP-RUN ( -- ) RBASE-VA REGION + 8 - CELL-TEXTPTR? 0=" GE-SRC+
   s\"  s\" free-region value is data\" ATP-EXPECT" GE-SRC+
   s"  0 XREF-REC-ADDR CELL-TEXTPTR?" GE-SRC+
   s\"  s\" live dict-record is a pointer\" ATP-EXPECT" GE-SRC+
   s"  FINDMAIN XREF-START CELL-TEXTPTR?" GE-SRC+
   s\"  s\" live code entry is a pointer\" ATP-EXPECT ;" GE-SRC-LINE
   s" ATP-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: DATA-WINDOW-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" ok" GE-OUT-LINE
   SB$ ;

: DATA-WINDOW ( -- )
   s" hb-aot-window.f" s" hb-aot-window" s" hb-aot-window-report.json" PATHS
   DATA-WINDOW-SOURCE
   s" hb-build AOT code-window datum build" BUILD-STRICT
   DATA-WINDOW-EXPECT s" hb-build AOT code-window datum output" GB-RUN-EXPECT
   s" PASS: hb-build AOT code-window datum (metadata classification, not magnitude)" type cr ;

\ Layout-bundle store: a program whose MAIN stores a wide (multi-cell) layout
\ value through `!`. The pass-2 wide-store lowering (LP2STORE) emits a runtime
\ call to the engine-resident (PROT-SPAN) span guard before the mutation. In a
\ stripped AOT image that runtime call is a direct BL whose target
\ is the (PROT-SPAN) helper; unless the linker rewrites it to a PC-relative
\ branch into the copied helper, the built-time engine address ships and the
\ store SIGSEGVs at load (dot habu-relocate-absolute-helper-dbb53aef). Because
\ (PROT-SPAN) is a registered engine helper, the closure walk resolves the call
\ by record address and collapses it to an in-image branch, so this MAIN runs.
\ Reaching the trailing `42 .` proves the guarded store completed.
: LAYOUT-STORE-SOURCE ( -- )
   GE-SRC-RESET
   s" package AOT-LAYOUT-STORE" GE-SRC-LINE
   s" SUMTYPE res 2" GE-SRC-LINE
   s"   VARIANT ok a ;VARIANT" GE-SRC-LINE
   s"   VARIANT err b ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" 1 LAYOUT-BUFFER MEM res<n,n>" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : STORE-IT ( -- ) 37 construct res ok 0 MEM ! ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : MAIN ( -- ) AOT-LAYOUT-STORE:STORE-IT 42 . ;" GE-SRC-LINE ;

: LAYOUT-STORE-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 42" GE-OUT-LINE
   SB$ ;

: LAYOUT-STORE ( -- )
   s" hb-aot-layout-store.f" s" hb-aot-layout-store" s" hb-aot-layout-store-report.json" PATHS
   LAYOUT-STORE-SOURCE
   s" hb-build AOT layout-bundle store build" BUILD-STRICT
   LAYOUT-STORE-EXPECT s" hb-build AOT layout-bundle store output" GB-RUN-EXPECT
   s" PASS: hb-build AOT layout-bundle store (LP2STORE reaches (PROT-SPAN) via a relocated call)" type cr ;

\ Layout-bundle fetch: a program whose MAIN constructs a wide (multi-cell) layout
\ value, stores it through `!`, then reads it back through `@` and destructures it
\ with MATCH, printing the recovered payload. The pass-2 wide-fetch lowering
\ (LP2VEMIT) emits a runtime call to the engine-resident LP2VEXEC tag validator
\ before the value is used. In a stripped AOT image that call is an absolute
\ direct BL whose target is LP2VEXEC; unless the linker rewrites it to a
\ PC-relative branch into the copied helper, the build-time engine address ships
\ and the fetch SIGSEGVs at load (dot habu-relocate-lp2vexec-fetch-b5472dc1).
\ Because LP2VEXEC is now a registered engine helper the closure walk resolves the
\ call by record address and collapses it in-image, so this MAIN runs and prints
\ the stored payload (37). ASSERT-BLR-ABSENT then proves the collapse by
\ construction: the stripped __text contains zero un-collapsed blr x16.
: LAYOUT-FETCH-SOURCE ( -- )
   GE-SRC-RESET
   s" package AOT-LAYOUT-FETCH" GE-SRC-LINE
   s" SUMTYPE res 2" GE-SRC-LINE
   s"   VARIANT ok a ;VARIANT" GE-SRC-LINE
   s"   VARIANT err b ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" 1 LAYOUT-BUFFER MEM res<n,n>" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : ROUND-TRIP ( -- n ) 37 construct res ok 0 MEM ! 0 MEM @ MATCH res ok OF ENDOF err OF ENDOF ;MATCH ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : MAIN ( -- ) AOT-LAYOUT-FETCH:ROUND-TRIP . ;" GE-SRC-LINE ;

: LAYOUT-FETCH-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 37" GE-OUT-LINE
   SB$ ;

: LAYOUT-FETCH ( -- )
   s" hb-aot-layout-fetch.f" s" hb-aot-layout-fetch" s" hb-aot-layout-fetch-report.json" PATHS
   LAYOUT-FETCH-SOURCE
   s" hb-build AOT layout-bundle fetch build" BUILD-STRICT
   LAYOUT-FETCH-EXPECT s" hb-build AOT layout-bundle fetch output" GB-RUN-EXPECT
   s" hb-build AOT layout-bundle fetch zero un-collapsed blr x16" ASSERT-BLR-ABSENT
   s" PASS: hb-build AOT layout-bundle fetch (LP2VEXEC reaches via a relocated call; zero blr x16)" type cr ;

\ Fail-closed abs-chain reject (red-first negative case). The AOT linker contract is
\ DIRECT-BL-ONLY: no native emitter produces the absolute movz/movk/movk x16 + blr x16 call
\ form, so the copier and relocator (aot-lib.f COPY-COMPACT-BLOB / RELOCATE) die with
\ E-AOT-ABS-CHAIN if one is ever encountered. This hand-builds one full chain in a synthetic
\ blob and drives the copier over it in a real hb-build. The maker's rejection is a die (not
\ a catchable result), so per the gate boundary rule it runs as a subprocess sentinel; it
\ lives here rather than in the in-process negative gate because the copier is in the
\ maker-only aot-lib.f. Red-first: before the retirement the copier silently collapsed/copied
\ the chain and hb-build exited 0; now it rejects with the named error (exit 74).
: ABS-CHAIN-SOURCE ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" create ABT-CHAIN 16 allot" GE-SRC-LINE
   s" create ABT-REC DREC allot" GE-SRC-LINE
   s" : ABT-W! ( n ptr u8 -- ) {: w:n a:ptr :} w a c! w 8 rshift a 1+ c! w 16 rshift a 2 + c! w 24 rshift a 3 + c! ;" GE-SRC-LINE
   s" : ABT-BUILD ( -- ) $D2800010 ABT-CHAIN ABT-W! $F2A00010 ABT-CHAIN 4 + ABT-W! $F2C00010 ABT-CHAIN 8 + ABT-W! $D63F0200 ABT-CHAIN 12 + ABT-W! ;" GE-SRC-LINE
   s" : ABT-RUN ( -- ) ABT-BUILD ABT-CHAIN ABT-REC 0 ptr-field ! 12 ABT-REC 8 + ! ABT-REC CLO 0 ptr-field ! 0 NEWOFF ! 1 NCLO ! ABT-REC COPY-COMPACT-BLOB ;" GE-SRC-LINE
   s" ABT-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : MAIN ( -- ) ;" GE-SRC-LINE ;

: ABS-CHAIN-ARGV ( -- )                      \ --load <hb-build layers> -- SRC -o OUT
   GE-HB-RESET
   s" --load" GE-ARG+
   s" lib/errors.f" GE-ARG+  s" lib/string.f" GE-ARG+  s" lib/memory.f" GE-ARG+
   s" lib/fs.f" GE-ARG+  s" lib/fs-mutate.f" GE-ARG+  s" lib/process.f" GE-ARG+
   s" lib/process-argv.f" GE-ARG+  s" lib/process-env.f" GE-ARG+  s" lib/source.f" GE-ARG+
   s" lib/build.f" GE-ARG+  s" lib/codesign.f" GE-ARG+  s" lib/content-key.f" GE-ARG+
   s" tools/build-fixpoint.f" GE-ARG+  s" tools/cli-run.f" GE-ARG+
   s" tools/hb-build-lib.f" GE-ARG+  s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+  GB-SRC$ GE-ARG+  s" -o" GE-ARG+  GB-OUT$ GE-ARG+ ;

: ABS-CHAIN ( -- )
   s" hb-aot-abschain.f" s" hb-aot-abschain" s" hb-aot-abschain-report.json" PATHS
   ABS-CHAIN-SOURCE
   GB-WRITE-SRC
   ABS-CHAIN-ARGV
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   74 s" hb-build AOT abs-chain reject rc" GE-EXPECT-RC
   s" E-AOT-ABS-CHAIN" s" hb-build AOT abs-chain reject code" GE-EXPECT-ERR-HAS
   s" PASS: hb-build AOT abs-chain reject (E-AOT-ABS-CHAIN; copier fails closed on a direct-BL-only violation)" type cr ;

\ item 10 slice 5: a preseeded bad-tag object/AOT test entry. A source declaring a
\ matched family + helper is AOT-built with a SELECTED non-MAIN entry (the helper)
\ and a forged value-stack seed (payload slots + an out-of-range tag), so the
\ stripped image starts at the helper and reaches its inline invalid-tag die
\ (rc ENGINE-ERROR:BAD-TAG 85 + "hb: bad gemt tag"). The SAME source built normally (entry
\ MAIN) exits 0, and the entry/seed/mode axis is folded into every cache layer
\ (artifact key + source-index key + object bytes) so the two are distinct
\ artifacts with no cross-restore in either direction, and the die survives an
\ object-cache relink. docs/census-tfam-10.md.
: PRESEED-SRC ( -- )                        \ matched family + helper + trivial MAIN
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
: PRESEED-SEED$ ( -- ptr u8 n )
   s" 000000000000000000000000000000000000000000000005" ;

: PRESEED-ARM ( -- )                        \ select the non-MAIN entry + forged seed
   s" HLP" HBB-PRESEED-ENTRY!
   PRESEED-SEED$ HBB-PRESEED-SEED! ;

: PRESEED-BUILD ( -- )
   GB-WRITE-SRC
   GB-HBB-PREPARE
   PRESEED-ARM
   s" hb-build AOT preseed bad-tag entry build" GB-HBB-BUILD-OUT ;

: PRESEED-BUILD-JSON ( -- )                 \ --json flips the artifact key only -> object-cache relink
   GB-WRITE-SRC
   GB-HBB-PREPARE
   -1 HBB-JSON !
   PRESEED-ARM
   s" hb-build AOT preseed object-cache relink build" GB-HBB-BUILD-OUT ;

: PRESEED-RUN-BAD ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GE-HB-RESET
   GB-OUT$ GE-TIMEOUT-MS GE-RUN-ENV
   85 label labelu GE-EXPECT-RC
   s" hb: bad gemt tag" label labelu GE-EXPECT-ERR-HAS ;

: PRESEED ( -- )
   \ This assertion owns a fresh cache: without one, HB-BUILD:OBJECT-HIT? is
   \ structurally impossible; a shared warm artifact can also bypass production.
   GT-ROOT BUILD-CACHE:ROOT!
   s" hb-aot-preseed.f" s" hb-aot-preseed" s" hb-aot-preseed-report.json" PATHS
   PRESEED-SRC
   s" hb-build AOT preseed normal-MAIN control" GB-HBB-BUILD
   s" hb-build AOT preseed normal-MAIN exits 0" GB-RUN-OUT
   PRESEED-BUILD
   s" hb-build AOT preseed bad-tag entry run" PRESEED-RUN-BAD
   PRESEED-BUILD
   s" hb-build AOT preseed restore" PRESEED-RUN-BAD
   PRESEED-BUILD-JSON
   HB-BUILD:OBJECT-HIT? 0= if s" hb-build AOT preseed object-cache hit" GE-FAIL then
   s" hb-build AOT preseed object-cache relink run" PRESEED-RUN-BAD
   s" hb-build AOT preseed normal-MAIN control (bis)" GB-HBB-BUILD
   s" hb-build AOT preseed normal-MAIN still exits 0" GB-RUN-OUT
   s" PASS: hb-build AOT preseeded bad-tag entry (rc 85 hb: bad gemt tag; three-key lockstep; object relink)" type cr ;

\ Preseeded bad-tag FETCH: proves LP2VEXEC's own invalid-tag diagnostic fires
\ correctly in a stripped image after the relocation fix. HLP stores a preseeded
\ layout value then reads it back through `@`; the forged seed carries an
\ out-of-range tag (res tags are 0..1, seed tag 5), so the wide fetch reaches
\ LP2VEXEC's invalid path, which writes "hb: bad layout tag\n" and exits
\ ENGINE-ERROR:BAD-TAG (85). Because the message is inlined inside the registered
\ LP2VEXEC record, its ADR is relocated with the copied helper and the diagnostic
\ is byte-identical to the engine's in the stripped image. The SAME source built
\ normally (entry MAIN) exits 0.
: PRESEED-FETCH-SRC ( -- )                 \ matched family + fetch helper + trivial MAIN
   GE-SRC-RESET
   s" package AOT-LAYOUT-FETCH-BAD" GE-SRC-LINE
   s" SUMTYPE res 2" GE-SRC-LINE
   s"   VARIANT ok a ;VARIANT" GE-SRC-LINE
   s"   VARIANT err b ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" 1 LAYOUT-BUFFER MEM res<n,n>" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : HLP ( res<n,n> -- n ) 0 MEM ! 0 MEM @ MATCH res ok OF ENDOF err OF ENDOF ;MATCH ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : MAIN ( -- ) ;" GE-SRC-LINE ;

\ Bundle width M+1 = 2 cells for res<n,n> (one payload + tag), each a big-endian
\ u64, bottom-of-stack first / tag last. Forge tag 5 (res tags 0..1 valid).
: PRESEED-FETCH-SEED$ ( -- ptr u8 n )
   s" 00000000000000000000000000000005" ;

: PRESEED-FETCH-ARM ( -- )                 \ select the fetch helper entry + forged seed
   s" HLP" HBB-PRESEED-ENTRY!
   PRESEED-FETCH-SEED$ HBB-PRESEED-SEED! ;

: PRESEED-FETCH-BUILD ( -- )
   GB-WRITE-SRC
   GB-HBB-PREPARE
   PRESEED-FETCH-ARM
   s" hb-build AOT preseed bad-tag fetch build" GB-HBB-BUILD-OUT ;

: FETCH-RUN-BAD ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GE-HB-RESET
   GB-OUT$ GE-TIMEOUT-MS GE-RUN-ENV
   85 label labelu GE-EXPECT-RC
   s" hb: bad layout tag" label labelu GE-EXPECT-ERR-HAS ;

: PRESEED-FETCH ( -- )
   s" hb-aot-preseed-fetch.f" s" hb-aot-preseed-fetch" s" hb-aot-preseed-fetch-report.json" PATHS
   PRESEED-FETCH-SRC
   s" hb-build AOT preseed fetch normal-MAIN control" GB-HBB-BUILD
   s" hb-build AOT preseed fetch normal-MAIN exits 0" GB-RUN-OUT
   PRESEED-FETCH-BUILD
   s" hb-build AOT preseed bad-tag fetch run" FETCH-RUN-BAD
   s" hb-build AOT preseed bad-tag fetch zero un-collapsed blr x16" ASSERT-BLR-ABSENT
   s" PASS: hb-build AOT preseeded bad-tag fetch (rc 85 hb: bad layout tag via LP2VEXEC in a stripped image)" type cr ;

: RUN-BUNDLE-DATA ( -- )
   s" hb-gate-aot-bundle-data" GT-START
   BUNDLE
   DATA
   DATA-WINDOW
   LAYOUT-STORE
   LAYOUT-FETCH
   ABS-CHAIN
   GT-CLEANUP ;

: RUN-PRESEED ( -- )
   s" hb-gate-aot-preseed" GT-START
   PRESEED
   PRESEED-FETCH
   GT-CLEANUP ;

: START-BUNDLE-DATA ( -- )
   s" fork hb-build AOT bundle/data" GE-TIMEOUT-MS [: RUN-BUNDLE-DATA ;] GT-POOL-START-FORK ;

: START-PRESEED ( -- )
   s" fork hb-build AOT preseed" GE-TIMEOUT-MS [: RUN-PRESEED ;] GT-POOL-START-FORK ;

public

: RUN ( -- )
   s" hb-gate-aot-positive" GT-START
   GT-POOL-RESET
   START-BUNDLE-DATA
   START-PRESEED
   GT-POOL-DRAIN
   GT-CLEANUP
   s" PASS: native hb-build AOT positive gate phase" type cr ;

;package
