\ gate-aot-positive.f - checked runner for positive hb-build AOT checks.
\
\ Load after test/gate-build-common.f and test/gate-build-hbb.f.

require test/gate-pool.f

46 constant GAP-DOT
99 constant GAP-C-LOWER
$10000 constant GAP-STRIPPED-TEXT-MAX
$D63F0200 constant GAP-BLR-X16     \ arm64 `blr x16`: the tail of an un-collapsed absolute movz/movk+blr call

variable GAP-BLR-CNT
\ Count `blr x16` words in the built image's executable text region. A correctly
\ linked stripped image has none: every in-closure absolute call is collapsed to a
\ PC-relative branch (aot-lib.f COPY-COMPACT-BLOB / RELOCATE), so a surviving
\ blr x16 is an un-relocated build-time engine address that would crash at load.
: GAP-COUNT-BLR-X16 ( n n -- n ) {: foff:n fsize:n :}      \ text-file-offset text-size -- count
   0 GAP-BLR-CNT !
   foff begin dup foff fsize + < while
      dup GB-U32-OFF GAP-BLR-X16 = if 1 GAP-BLR-CNT +! then
      4 +
   repeat drop
   GAP-BLR-CNT @ ;

: GAP-ASSERT-NO-BLR-X16 ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GB-OUT$ GB-EXEC-TEXT-RANGE GAP-COUNT-BLR-X16
   0 <> if label labelu GE-FAIL then ;

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
: GAP-DATA-SOURCE ( -- )
   GE-SRC-RESET
   s" package AOT-MAP-TEST" GE-SRC-LINE
   s" create CODE 16 allot" GE-SRC-LINE
   s" create REC1 DREC allot" GE-SRC-LINE
   s" create REC2 DREC allot" GE-SRC-LINE
   s" 4 constant BODY-LEN" GE-SRC-LINE
   s" 8 constant CODE-ROW" GE-SRC-LINE
   s" $40 constant REC2-OFF" GE-SRC-LINE
   s" : REC! ( ptr a ptr u8 n -- ) {: r:ptr code:ptr len:n :} code r 0 ptr-field ! len r 8 + ! ;" GE-SRC-LINE
   s" : RECORDS! ( -- ) REC1 CODE BODY-LEN REC! REC2 CODE CODE-ROW + BODY-LEN REC! ;" GE-SRC-LINE
   s" : CLOSURE! ( -- ) REC1 CLO 0 ptr-field ! REC2 CLO 1 ptr-field ! 0 NEWOFF ! REC2-OFF NEWOFF cell+ ! 2 NCLO ! ;" GE-SRC-LINE
   s" : EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :} ok 0= if label labelu 74 die then ;" GE-SRC-LINE
   s\" : RUN ( -- ) RECORDS! CLOSURE! REC1 CODE CODE-ROW + MAP-IN-BLOB -1 = s\" AOT closed record range\" EXPECT REC1 CODE CODE-ROW + MAP-TARGET REC2-OFF = s\" AOT adjacent record relocation\" EXPECT ;" GE-SRC-LINE
   s" RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
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

\ Layout-bundle store: a program whose MAIN stores a wide (multi-cell) layout
\ value through `!`. The pass-2 wide-store lowering (LP2STORE) emits a runtime
\ call to the engine-resident (PROT-SPAN) span guard before the mutation. In a
\ stripped AOT image that runtime call is an absolute movz/movk+blr whose target
\ is the (PROT-SPAN) helper; unless the linker rewrites it to a PC-relative
\ branch into the copied helper, the built-time engine address ships and the
\ store SIGSEGVs at load (dot habu-relocate-absolute-helper-dbb53aef). Because
\ (PROT-SPAN) is a registered engine helper, the closure walk resolves the call
\ by record address and collapses it to an in-image branch, so this MAIN runs.
\ Reaching the trailing `42 .` proves the guarded store completed.
: GAP-LAYOUT-STORE-SOURCE ( -- )
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

: GAP-LAYOUT-STORE-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 42" GE-OUT-LINE
   SB$ ;

: GAP-LAYOUT-STORE ( -- )
   s" hb-aot-layout-store.f" s" hb-aot-layout-store" s" hb-aot-layout-store-report.json" GAP-PATHS
   GAP-LAYOUT-STORE-SOURCE
   s" hb-build AOT layout-bundle store build" GAP-BUILD-STRICT
   GAP-LAYOUT-STORE-EXPECT s" hb-build AOT layout-bundle store output" GB-RUN-EXPECT
   s" PASS: hb-build AOT layout-bundle store (LP2STORE reaches (PROT-SPAN) via a relocated call)" type cr ;

\ Layout-bundle fetch: a program whose MAIN constructs a wide (multi-cell) layout
\ value, stores it through `!`, then reads it back through `@` and destructures it
\ with MATCH, printing the recovered payload. The pass-2 wide-fetch lowering
\ (LP2VEMIT) emits a runtime call to the engine-resident LP2VEXEC tag validator
\ before the value is used. In a stripped AOT image that call is an absolute
\ movz/movk+blr whose target is LP2VEXEC; unless the linker rewrites it to a
\ PC-relative branch into the copied helper, the build-time engine address ships
\ and the fetch SIGSEGVs at load (dot habu-relocate-lp2vexec-fetch-b5472dc1).
\ Because LP2VEXEC is now a registered engine helper the closure walk resolves the
\ call by record address and collapses it in-image, so this MAIN runs and prints
\ the stored payload (37). GAP-ASSERT-NO-BLR-X16 then proves the collapse by
\ construction: the stripped __text contains zero un-collapsed blr x16.
: GAP-LAYOUT-FETCH-SOURCE ( -- )
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

: GAP-LAYOUT-FETCH-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 37" GE-OUT-LINE
   SB$ ;

: GAP-LAYOUT-FETCH ( -- )
   s" hb-aot-layout-fetch.f" s" hb-aot-layout-fetch" s" hb-aot-layout-fetch-report.json" GAP-PATHS
   GAP-LAYOUT-FETCH-SOURCE
   s" hb-build AOT layout-bundle fetch build" GAP-BUILD-STRICT
   GAP-LAYOUT-FETCH-EXPECT s" hb-build AOT layout-bundle fetch output" GB-RUN-EXPECT
   s" hb-build AOT layout-bundle fetch zero un-collapsed blr x16" GAP-ASSERT-NO-BLR-X16
   s" PASS: hb-build AOT layout-bundle fetch (LP2VEXEC reaches via a relocated call; zero blr x16)" type cr ;

\ item 10 slice 5: a preseeded bad-tag object/AOT test entry. A source declaring a
\ matched family + helper is AOT-built with a SELECTED non-MAIN entry (the helper)
\ and a forged value-stack seed (payload slots + an out-of-range tag), so the
\ stripped image starts at the helper and reaches its inline invalid-tag die
\ (rc ENGINE-ERROR:BAD-TAG 85 + "hb: bad gemt tag"). The SAME source built normally (entry
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
   \ This assertion owns a fresh cache: without one, HB-BUILD:OBJECT-HIT? is
   \ structurally impossible; a shared warm artifact can also bypass production.
   GT-ROOT BUILD-CACHE:ROOT!
   s" hb-aot-preseed.f" s" hb-aot-preseed" s" hb-aot-preseed-report.json" GAP-PATHS
   GAP-PRESEED-SRC
   s" hb-build AOT preseed normal-MAIN control" GB-HBB-BUILD
   s" hb-build AOT preseed normal-MAIN exits 0" GB-RUN-OUT
   GAP-PRESEED-BUILD
   s" hb-build AOT preseed bad-tag entry run" GAP-PRESEED-RUN-BAD
   GAP-PRESEED-BUILD
   s" hb-build AOT preseed restore" GAP-PRESEED-RUN-BAD
   GAP-PRESEED-BUILD-JSON
   HB-BUILD:OBJECT-HIT? 0= if s" hb-build AOT preseed object-cache hit" GE-FAIL then
   s" hb-build AOT preseed object-cache relink run" GAP-PRESEED-RUN-BAD
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
: GAP-PRESEED-FETCH-SRC ( -- )                 \ matched family + fetch helper + trivial MAIN
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
: GAP-PRESEED-FETCH-SEED$ ( -- ptr u8 n )
   s" 00000000000000000000000000000005" ;

: GAP-PRESEED-FETCH-ARM ( -- )                 \ select the fetch helper entry + forged seed
   s" HLP" HBB-PRESEED-ENTRY!
   GAP-PRESEED-FETCH-SEED$ HBB-PRESEED-SEED! ;

: GAP-PRESEED-FETCH-BUILD ( -- )
   GB-WRITE-SRC
   GB-HBB-PREPARE
   GAP-PRESEED-FETCH-ARM
   s" hb-build AOT preseed bad-tag fetch build" GB-HBB-BUILD-OUT ;

: GAP-PRESEED-FETCH-RUN-BAD ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GE-HB-RESET
   GB-OUT$ GE-TIMEOUT-MS GE-RUN-ENV
   85 label labelu GE-EXPECT-RC
   s" hb: bad layout tag" label labelu GE-EXPECT-ERR-HAS ;

: GAP-PRESEED-FETCH ( -- )
   s" hb-aot-preseed-fetch.f" s" hb-aot-preseed-fetch" s" hb-aot-preseed-fetch-report.json" GAP-PATHS
   GAP-PRESEED-FETCH-SRC
   s" hb-build AOT preseed fetch normal-MAIN control" GB-HBB-BUILD
   s" hb-build AOT preseed fetch normal-MAIN exits 0" GB-RUN-OUT
   GAP-PRESEED-FETCH-BUILD
   s" hb-build AOT preseed bad-tag fetch run" GAP-PRESEED-FETCH-RUN-BAD
   s" hb-build AOT preseed bad-tag fetch zero un-collapsed blr x16" GAP-ASSERT-NO-BLR-X16
   s" PASS: hb-build AOT preseeded bad-tag fetch (rc 85 hb: bad layout tag via LP2VEXEC in a stripped image)" type cr ;

: GAP-RUN-BUNDLE-DATA ( -- )
   s" hb-gate-aot-bundle-data" GT-START
   GAP-BUNDLE
   GAP-DATA
   GAP-LAYOUT-STORE
   GAP-LAYOUT-FETCH
   GT-CLEANUP ;

: GAP-RUN-PRESEED ( -- )
   s" hb-gate-aot-preseed" GT-START
   GAP-PRESEED
   GAP-PRESEED-FETCH
   GT-CLEANUP ;

: GAP-START-BUNDLE-DATA ( -- )
   s" fork hb-build AOT bundle/data" GE-TIMEOUT-MS [: GAP-RUN-BUNDLE-DATA ;] GT-POOL-START-FORK ;

: GAP-START-PRESEED ( -- )
   s" fork hb-build AOT preseed" GE-TIMEOUT-MS [: GAP-RUN-PRESEED ;] GT-POOL-START-FORK ;

: GAP-RUN ( -- )
   s" hb-gate-aot-positive" GT-START
   GT-POOL-RESET
   GAP-START-BUNDLE-DATA
   GAP-START-PRESEED
   GT-POOL-DRAIN
   GT-CLEANUP
   s" PASS: native hb-build AOT positive gate phase" type cr ;
