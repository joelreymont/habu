\ gate-aot-positive-lib.f - the positive hb-build AOT checks the gate row
\ test/gate-aot-positive-bundle.f runs.

require src/lib/string.f
require src/lib/source.f
require src/lib/build.f
require src/lib/codesign.f
require tools/build-fixpoint.f
require tools/gate-json-assert-core.f
require test/gate-common.f
require test/gate-build-common.f
require test/gate-build-hbb.f
require test/gate-aot-image.f
require test/aot-builder-engine.f

package AOT-POSITIVE
using AOT-IMAGE

46 constant DOT
99 constant C-LOWER
$10000 constant STRIPPED-CODE-MACOS-MAX
48 constant PH-ALIGN-OFF       \ p_align inside an Elf64_Phdr; the rest are GB-ELF-PH-*-OFF
$D63F0200 constant BLR-X16     \ arm64 `blr x16`: an indirect engine-address call the linker never emits

variable BLR-CNT

\ Preserve the original code budget: Linux code plus headers fits one maximum
\ page; macOS code stays below 64 KiB. Restored DATA is not generated code.
: CODE-TOO-LARGE? ( n -- bool )
   HB-TARGET-LINUX? if CODE-OFF + PROT-PAGE-MAX > exit then
   STRIPPED-CODE-MACOS-MAX >= ;

\ A correctly linked stripped image has no `blr x16` in its validated code span:
\ every native call is a direct BL the linker relocates to a PC-relative branch
\ into the copied blobs (aot-lib.f COPY-COMPACT-BLOB / RELOC-W32), so a
\ surviving one is an un-relocated build-time engine address that crashes at
\ load.
: COUNT-BLR-X16 ( n n -- n ) {: foff:n fsize:n :}      \ text-file-offset text-size -- count
   0 BLR-CNT !
   foff begin dup 4 + foff fsize + <= while
      dup GB-U32-OFF BLR-X16 = if 1 BLR-CNT +! then
      4 +
   repeat drop
   BLR-CNT @ ;

: ASSERT-BLR-ABSENT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GB-OUT$ CODE-RANGE COUNT-BLR-X16
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

: PH-ALIGN ( n -- n )
   GB-ELF-PH-OFF PH-ALIGN-OFF + GB-U64-OFF ;

\ Verify the real ELF headers keep RX and RW on maximum-page boundaries.
: PH-LOAD-ALIGNED ( n ptr u8 n -- ) {: idx:n label:ptr labelu:n :}
   idx PH-ALIGN PROT-PAGE-MAX < if label labelu GE-FAIL then
   idx PH-FILE-OFF idx PH-ALIGN mod 0 <> if label labelu GE-FAIL then
   idx PH-VADDR    idx PH-ALIGN mod 0 <> if label labelu GE-FAIL then ;

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
   0 label labelu PH-LOAD-ALIGNED
   1 label labelu PH-LOAD-ALIGNED
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
   s"  count type CR DATA-SUM WINDOW-DATUM ;" GE-SRC-LINE ;

\ DATA-SUM proves the entry maps DATA-VA, restores it and sets up loop stacks.
\ X holds dbase@+REGION-8, data above the code, not RBASE-VA (ASLR moves it).
: DATA-DEFS ( -- )
   s" create TABLE 10 , 20 , 30 ," GE-SRC-LINE
   s" variable SUM" GE-SRC-LINE
   s" : DATA-SUM ( -- ) 0 SUM ! 3 0 ?do TABLE i 8 * + @ SUM +! loop SUM @ . ;" GE-SRC-LINE
   s" dbase@ REGION + 8 - dup constant EXPECTED create X ," GE-SRC-LINE
   s\" : WINDOW-DATUM ( -- ) X @ EXPECTED = IF s\" ok\" ELSE s\" bad\" THEN type cr ;" GE-SRC-LINE ;

: MAIN-OWNER-CHECK ( -- )
   s\" : AMC ( -- ) s\" MAIN\" 0 XREF-FIND-WL XREF-FOUND? 0= if" GE-SRC+
   s\"  s\" AOT global MAIN unavailable\" 74 die then ;" GE-SRC-LINE
   s" AMC" GE-SRC-LINE ;

: BUNDLE-SOURCE ( -- )
   GE-SRC-RESET
   \ Aligned DATA carries BLR x16 and NOP/NOP/NOP/BL bytes: code-only scans must
   \ not see them. The sparse image stores DATA as unaligned non-zero runs
   \ (src/habu/aot-lib.f), so only the code-only exclusion is checked below.
   s" align create INSTRUCTION-DATA $00 c, $02 c, $3F c, $D6 c," GE-SRC-LINE
   s" $1F c, $20 c, $03 c, $D5 c, $1F c, $20 c, $03 c, $D5 c," GE-SRC-LINE
   s" $1F c, $20 c, $03 c, $D5 c, $00 c, $00 c, $00 c, $94 c," GE-SRC-LINE
   FIB-DEFS
   COMPACT-DEFS
   FEATURE-DEFS
   DATA-DEFS
   \ An unused code writer is absent from MAIN's closure and must not reject
   \ this build merely because its spelling appears in the source.
   s" TRUSTED: UNUSED-PATCH ( -- ) 0 0 patch32 ;" GE-SRC-LINE
   BUNDLE-MAIN
   MAIN-OWNER-CHECK ;

: BUNDLE-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 55" GE-OUT-LINE GE-SB-LF
   s" 22" GE-OUT-LINE s" ok" GE-OUT-LINE
   s" 260" GE-OUT-LINE GE-SB-LF s" 34" GE-OUT-LINE GE-SB-LF
   s" hi" GE-OUT-LINE s" ok" GE-OUT-LINE
   s" 60" GE-OUT-LINE s" ok" GE-OUT-LINE
   SB$ ;

: BUNDLE ( -- )
   s" hb-aot-bundle.f" s" hb-aot-bundle" s" hb-aot-bundle-report.json" PATHS
   BUNDLE-SOURCE
   s" hb-build AOT compact/features/data" GB-HBB-BUILD
   BUNDLE-EXPECT s" hb-build AOT compact/features/data output" GB-RUN-EXPECT
   GB-OUT$ CODE-RANGE nip {: codesz:n :}
   HB-TARGET-LINUX-X86-64? 0= if
      codesz CODE-TOO-LARGE? if s" hb-build AOT stripped code" GE-FAIL then
      s" hb-build AOT dynamic ELF shape" ASSERT-DYNAMIC-ELF
      \ ARM's direct-BL report and BLR scan inspect ARM instructions. The
      \ x86-64 case above ran the linked calls in the stripped image.
      s" hb-build AOT code excludes DATA blr x16" ASSERT-BLR-ABSENT
      CODE-REPORT
      s" aot-stripped" s" aot-stripped call report" AOT-ASSERT
      s" aot-compact" s" aot-compact call report" AOT-ASSERT
   then
   s" PASS: hb-build AOT compact/feature/data coverage (persistent data region, code-window datum; executable span " type
   codesz GB-U.
   s"  B)" type cr ;

\ The stores call the engine's (PROT-SPAN) guard and the fetch NFETCH-CHECK:TAGS
\ (elaborate.f VALIDATE-FETCH): printing 37 proves the linker relocated both.
: LAYOUT-FETCH-LINES ( -- )
   s" package AOT-LAYOUT-FETCH" GE-SRC-LINE
   s" SUMTYPE res<a,b>" GE-SRC-LINE
   s"   VARIANT ok a ;VARIANT" GE-SRC-LINE
   s"   VARIANT err b ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" 1 LAYOUT-BUFFER MEM res<n,n>" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : ROUND-TRIP ( -- n ) 37 construct res ok 0 MEM ! 0 MEM @ MATCH res ok OF ENDOF err OF ENDOF ;MATCH ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: LAYOUT-FETCH-SOURCE ( -- )
   GE-SRC-RESET
   LAYOUT-FETCH-LINES
   s" : MAIN ( -- ) AOT-LAYOUT-FETCH:ROUND-TRIP . ;" GE-SRC-LINE ;

: LAYOUT-FETCH-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 37" GE-OUT-LINE
   SB$ ;

: LAYOUT-FETCH ( -- )
   s" hb-aot-layout-fetch.f" s" hb-aot-layout-fetch" s" hb-aot-layout-fetch-report.json" PATHS
   LAYOUT-FETCH-SOURCE
   s" hb-build AOT layout-bundle fetch build" GB-HBB-BUILD
   LAYOUT-FETCH-EXPECT s" hb-build AOT layout-bundle fetch output" GB-RUN-EXPECT
   HB-TARGET-LINUX-X86-64? 0= if
      s" hb-build AOT layout-bundle fetch zero un-collapsed blr x16" ASSERT-BLR-ABSENT
   then
   HB-TARGET-LINUX-X86-64? if
      s" PASS: hb-build AOT layout-bundle store and fetch via relocated calls" type cr
   else
      s" PASS: hb-build AOT layout-bundle store and fetch ((PROT-SPAN) and NFETCH-CHECK:TAGS reached via relocated calls; zero blr x16)" type cr
   then ;

public

: RUN-BUNDLE-DATA ( -- )
   s" hb-gate-aot-bundle-data" GT-START
   AOT-BUILDER-ENGINE:ENSURE
   BUNDLE
   LAYOUT-FETCH
   GT-CLEANUP ;

;using                                   \ AOT-IMAGE
;package
