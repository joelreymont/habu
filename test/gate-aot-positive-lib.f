\ gate-aot-positive.f - checked runner for positive hb-build AOT checks.

require test/gate-pool.f
require test/gate-build-common.f
require test/gate-build-hbb.f
require test/gate-aot-image.f

package AOT-POSITIVE
using HB-BUILD-CLI                       \ the preseed knobs and the json flag
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

\ Count `blr x16` words in the built image's validated code span. A correctly
\ linked stripped image has none: under the direct-BL-only contract every native call
\ is a direct BL that the linker relocates in place to a PC-relative branch into the
\ copied blobs (aot-lib.f COPY-COMPACT-BLOB / RELOC-W32), so a surviving blr x16 is an
\ un-relocated build-time engine address that would crash at load.
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
   s"  count type CR ;" GE-SRC-LINE ;

: SURFACE-BASE-DEFS ( -- )
   s" package AOT-SURFACE-HOSTILE" GE-SRC-LINE
   s" public" GE-SRC-LINE
   s" : NATIVE-BUILD ( -- ) ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" package AOT-SURFACE-TEST" GE-SRC-LINE
   s\" : AS-FAIL ( bool -- ) 0= if s\" AOT-LINK surface mismatch\" 74 die then ;" GE-SRC-LINE
   s\" : AS-NS ( -- ptr n ) s\" AOT-LINK\" XREF-NAMESPACE-WL XREF-FIND-WL ;" GE-SRC-LINE
   s" : AS-PUB ( -- n ) AS-NS XREF-START ;" GE-SRC-LINE
   s" : AS-PRI ( -- n ) AS-NS XREF-LEN ;" GE-SRC-LINE
   s\" : AS-HOST-PUB ( -- n ) s\" AOT-SURFACE-HOSTILE\" XREF-NAMESPACE-WL XREF-FIND-WL XREF-START ;" GE-SRC-LINE ;

: SURFACE-PUB-DEFS ( -- )
   s\" : AS-PUB-NAME? ( ptr u8 n -- bool ) 2dup s\" LINK\" XREF-STR=CI >r s\" BUILD-NATIVE\" XREF-STR=CI r> or ;" GE-SRC-LINE
   s" : AS-PUB-CHECK ( -- ) AS-NS XREF-FOUND? AS-FAIL" GE-SRC+
   s"  0 ndict@ 0 ?do i XREF-REC dup XREF-WORDLIST AS-PUB = if" GE-SRC+
   s"  dup XREF-NAME$ AS-PUB-NAME? AS-FAIL drop 1+" GE-SRC+
   s"  else drop then loop 2 = AS-FAIL ;" GE-SRC-LINE ;

: SURFACE-PRI-DEFS ( -- )
   s" : AS-NOT-EXPOSED-WL? ( ptr n n -- bool ) over XREF-NAME$ rot XREF-FIND-WL <> ;" GE-SRC-LINE
   s" : AS-NAME-ABSENT? ( ptr u8 n n -- bool ) XREF-FIND-WL XREF-FOUND? 0= ;" GE-SRC-LINE
   s" : AS-BARE-NAME ( ptr u8 n -- ) 0 AS-NAME-ABSENT? AS-FAIL ;" GE-SRC-LINE
   s" : AS-QUAL-NAME ( ptr u8 n -- ) AS-PUB AS-NAME-ABSENT? AS-FAIL ;" GE-SRC-LINE
   s" : AS-PRIVATE ( ptr u8 n -- ptr n ) AS-PRI XREF-FIND-WL dup XREF-FOUND? AS-FAIL ;" GE-SRC-LINE
   s" : AS-NAMED ( ptr u8 n -- ) 2dup AS-PRIVATE drop" GE-SRC+
   s"  2dup AS-BARE-NAME AS-QUAL-NAME ;" GE-SRC-LINE
   s\" : AS-HOSTILE-CHECK ( -- ) s\" NATIVE-BUILD\" AS-PRIVATE" GE-SRC+
   s"  dup AS-HOST-PUB AS-NOT-EXPOSED-WL? AS-FAIL drop" GE-SRC+
   s\"  s\" NATIVE-BUILD\" AS-HOST-PUB AS-NAME-ABSENT? 0= AS-FAIL ;" GE-SRC-LINE
   s\" : AS-MAIN-CHECK ( -- ) s\" MAIN\" 0 XREF-FIND-WL XREF-FOUND? AS-FAIL" GE-SRC+
   s\"  s\" MAIN\" AS-PRI XREF-FIND-WL XREF-FOUND? 0= AS-FAIL ;" GE-SRC-LINE
   \ Representative words, one per way the packaging could regress: REC and
   \ CLOSURE are ordinary closure-walk code, FINDADDR-PTR is the record
   \ resolver the linker calls, DIRECT? and TARGET are the branch decoders
   \ absorbed out of the retired AOT-BRANCH package, CELL-TEXTPTR? is the
   \ pointer classifier, and RELOC-W32 is the aot-lib caller that used to
   \ reach the branch decoders through the AOT-BRANCH: prefix and now calls
   \ them bare. Each must live in AOT-LINK's private wordlist and be reachable
   \ under no other name.
   s" : AS-NAMED-CHECK ( -- )" GE-SRC+
   s\"  s\" REC\" AS-NAMED s\" REC-NAME-PTR\" AS-NAMED" GE-SRC+
   s\"  s\" CLOSURE\" AS-NAMED s\" FINDADDR-PTR\" AS-NAMED" GE-SRC+
   s\"  s\" DIRECT?\" AS-NAMED s\" TARGET\" AS-NAMED" GE-SRC+
   s\"  s\" CELL-TEXTPTR?\" AS-NAMED s\" RELOC-W32\" AS-NAMED ;" GE-SRC-LINE
   s\" : AS-NO-RETIRED-NS ( -- ) s\" AOT-BRANCH\" XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? 0= AS-FAIL" GE-SRC+
   s\"  s\" AOT-REC\" XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? 0= AS-FAIL ;" GE-SRC-LINE ;

: SURFACE-DEFS ( -- )
   SURFACE-BASE-DEFS
   SURFACE-PUB-DEFS
   SURFACE-PRI-DEFS
   s\" : AS-RUN ( -- ) s\" AOT-LINK\" 0 XREF-FIND-WL XREF-FOUND? 0= AS-FAIL" GE-SRC+
   s"  AS-HOSTILE-CHECK AS-MAIN-CHECK AS-NAMED-CHECK AS-NO-RETIRED-NS AS-PUB-CHECK ;" GE-SRC-LINE
   s" AS-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

\ ---- the maker-side linker self-tests ----------------------------------------
\
\ SURFACE-DEFS above and the three blocks below check the LINKER: what AOT-LINK
\ publishes, its relocation math, the span table the seed published, and its
\ pointer classifier. All four need the linker in the dictionary, and they used
\ to be spliced into the APPLICATION source, which worked only while the maker
\ loaded the linker before the application. The capture window forbids that -
\ nothing the application can require may be loaded when the window opens
\ (docs/native-applications.md) - so an application naming a linker word now
\ dies with `E-UNDEFINED ... undefined word 'CLO'`, and these run where the
\ application and the linker first coexist instead: a file the maker's stdin
\ requires after tools/aot-build.f, one line before AOT-LINK:BUILD-NATIVE.
create SELF-SRC FS-PATH-CAP allot
variable SELF-SRC-U

: SELF-SRC$ ( -- ptr u8 n )
   SELF-SRC SELF-SRC-U @ ;

: SELF-SRC! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu SELF-SRC GT-PATH SELF-SRC-U ! ;

: WRITE-SELF-SRC ( -- )
   SELF-SRC$ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

\ The production maker script (tools/hb-build-lib.f HBB-RUN-MAKER-CMD) with one
\ line added: the self-test, required after the linker and before the link.
: SELF-STDIN$ ( -- ptr u8 n )
   SB-RESET
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\ns\" " SB-APPEND
   SELF-SRC$ SB-APPEND
   S\" \" required\nAOT-LINK:BUILD-NATIVE\n" SB-APPEND
   SB$ ;

\ Drive the maker child the way tools/hb-build.f does - the application as the
\ build's argv[0], the JSON flag as argv[1], HB_TMP as the image's directory -
\ and hand it the script above on stdin. A build that reaches the end writes its
\ image to HB_TMP/hb-aot-got, which is the name GB-OUT$ carries in these cases;
\ the stale one goes first, so "an image exists" and "none was emitted" are both
\ statements about THIS run.
: MAKER-RUN ( -- )
   GB-OUT$ EXISTS? if GB-OUT$ REMOVE-FILE then
   GE-HB-RESET
   GE-HB$ GE-ARGV+
   s" --" GE-ARG+ GB-SRC$ GE-ARG+ s" 0" GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   GE-HB$ SELF-STDIN$ GE-TIMEOUT-MS GE-RUN-STDIN ;

\ The relocation math the direct-branch capability adds: two adjacent synthetic
\ closure MEMBERS prove MAP-IN-MEMBER treats a target at a member's end as the
\ NEXT member's start (the >= boundary), and MAP-TARGET relocates that adjacent
\ target to the next member's new offset. The members carry XREF-NULL for their
\ record, which is what a word the image ships no record for looks like to this
\ walk. Safe to scribble on the live CLO/CLO-LEN/CLO-REC/NEWOFF/NCLO: this runs
\ before LINK and so before the program's own closure walk, and the real build
\ recomputes the closure from scratch (aot-closure.f CLOSURE sizes the tables for
\ the program, resets NCLO to 0 and refills them), so the synthetic values cannot
\ leak into the image - which is why MAKER-SELFTEST runs the image it built and
\ reads its output back. The two synthetic rows are allocated the same way the
\ walk's are, by asking for the rows about to be written (CLO-TABLES, PLAN-TABLES),
\ and the entry order MAP-TARGET binary-searches is built here the way PLAN-BLOBS
\ builds it, from the rows this fixture just wrote (aot-lib.f MEMBER-ORDER).
\ THE ADR CASE re-files member 0 at the assembler cursor the way PLAN-BLOBS does
\ (its first member starts at ASM-LEN), so the site's new address and its target's
\ differ by exactly the delta the compiler emitted: `ADR x0, .+4` aimed inside
\ member 0 comes back from RELOC-W32 as the SAME instruction word, and TNEW is
\ member 0's new offset plus the target's 4. Both assertions are exact - the
\ harness recomputes neither the encoding nor the map.
: SELF-AMAP-DEFS ( -- )
   s" package AOT-LINK" GE-SRC-LINE
   s" create AMAP-CODE 16 allot" GE-SRC-LINE
   s" 8 constant AMAP-SPAN-BYTES" GE-SRC-LINE
   s" 8 constant AMAP-CODE-ROW" GE-SRC-LINE
   s" $40 constant AMAP-M2-OFF" GE-SRC-LINE
   s" $10000020 constant AMAP-ADR" GE-SRC-LINE        \ ADR x0, .+4
   s" 4 constant AMAP-ADR-DELTA" GE-SRC-LINE
   s" : AMAP-MEMBER! ( n ptr u8 n -- ) {: i:n code:ptr len:n :}" GE-SRC+
   s"  code i CLO ! len i CLO-LEN ! XREF-NULL i CLO-REC ! ;" GE-SRC-LINE
   s" : AMAP-CLOSURE! ( -- ) 2 CLO-TABLES 2 PLAN-TABLES" GE-SRC+
   s"  0 AMAP-CODE AMAP-SPAN-BYTES AMAP-MEMBER!" GE-SRC+
   s"  1 AMAP-CODE AMAP-CODE-ROW + AMAP-SPAN-BYTES AMAP-MEMBER!" GE-SRC+
   s"  0 0 NEWOFF ! AMAP-M2-OFF 1 NEWOFF ! 2 NCLO ! MEMBER-ORDER ;" GE-SRC-LINE
   s" : AMAP-EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :} ok 0= if label labelu 74 die then ;" GE-SRC-LINE
   s" : AMAP-RUN ( -- ) AMAP-CLOSURE!" GE-SRC+
   s"  0 AMAP-CODE AMAP-CODE-ROW + MAP-IN-MEMBER -1 =" GE-SRC+
   s\"  s\" AOT closed member range\" AMAP-EXPECT" GE-SRC+
   s"  0 AMAP-CODE AMAP-CODE-ROW + MAP-TARGET AMAP-M2-OFF =" GE-SRC+
   s\"  s\" AOT adjacent member relocation\" AMAP-EXPECT" GE-SRC+
   s"  ASM-LEN 0 NEWOFF !" GE-SRC+
   s"  0 AMAP-CODE AMAP-ADR RELOC-W32 AMAP-ADR =" GE-SRC+
   s\"  s\" AOT in-member ADR keeps its delta\" AMAP-EXPECT" GE-SRC+
   s"  TNEW @ ASM-LEN AMAP-ADR-DELTA + =" GE-SRC+
   s\"  s\" AOT in-member ADR target\" AMAP-EXPECT ;" GE-SRC-LINE
   s" AMAP-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

\ The span table the seed published, read in the shipped engine's own linker
\ through the production reader, inside a real stripped build.
\ THE FIRST TWO ASSERTIONS ARE THE WHOLE POINT and they are two-sided: a row's
\ code has NO dictionary record (if a stripped word's row came back, the record
\ lookup would answer and this fails) and the span table answers for it instead
\ (if the table stopped being published, this fails). The other two pin the
\ reader's boundaries - a row's own entry answers its row, and so does an
\ address inside it - which is what the closure walk asks of every branch.
: SELF-SPAN-DEFS ( -- )
   s" package AOT-LINK" GE-SRC-LINE
   s" : ASPAN-RUN ( -- ) SPAN-N 0 >" GE-SRC+
   s\"  s\" AOT span table is empty\" AMAP-EXPECT" GE-SRC+
   s"  0 SPAN-START FINDADDR-PTR XREF-FOUND? 0=" GE-SRC+
   s\"  s\" AOT span row still has a record\" AMAP-EXPECT" GE-SRC+
   s"  0 SPAN-START SPAN-AT-ENTRY 0 =" GE-SRC+
   s\"  s\" AOT span entry lookup\" AMAP-EXPECT" GE-SRC+
   s"  0 SPAN-START-N 0 SPAN-BYTES 1- + SPAN-OWNER 0 =" GE-SRC+
   s\"  s\" AOT span interior lookup\" AMAP-EXPECT ;" GE-SRC-LINE
   s" ASPAN-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

\ CELL-TEXTPTR? pins BOTH directions: a value in the former [RBASE-VA,
\ RBASE-VA+REGION) magnitude window - the top cell of the JIT region, free space
\ far above the code high-water - is data, while a live dict-record address and a
\ live code entry (MAIN's) are pointers. DATA-WINDOW builds and runs the program
\ that carries such a datum; this is the classifier's own two-sided check, and
\ the RBASE-VA/REGION expression is evaluated in the maker, so the case tracks
\ the constants if a later dot moves the region.
: SELF-TEXTPTR-DEFS ( -- )
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

: SELF-SOURCE ( -- )
   GE-SRC-RESET
   SURFACE-DEFS
   SELF-AMAP-DEFS
   SELF-SPAN-DEFS
   SELF-TEXTPTR-DEFS ;

\ Two words, so the closure the linker walks has members to relocate after
\ AMAP-RUN scribbled on the live arrays. MAIN also gives the surface check its
\ subject (AS-MAIN-CHECK: MAIN is global, and not AOT-LINK's) and the classifier
\ check a live code entry.
: SELF-APP-SOURCE ( -- )
   GE-SRC-RESET
   s" : SELF-BUMP ( n -- n ) 1+ ;" GE-SRC-LINE
   s" : MAIN ( -- ) 6 SELF-BUMP . ;" GE-SRC-LINE ;

: SELF-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" 7" GE-OUT-LINE
   SB$ ;

: MAKER-SELFTEST ( -- )
   s" hb-aot-selftest.f" s" hb-aot-got" s" hb-aot-selftest-report.json" PATHS
   SELF-APP-SOURCE GB-WRITE-SRC
   s" hb-aot-selftest-maker.f" SELF-SRC!
   SELF-SOURCE WRITE-SELF-SRC
   MAKER-RUN
   0 s" maker self-test build rc" GE-EXPECT-RC
   GB-OUT$ FILE? 0= if s" maker self-test image" GE-FAIL then
   SELF-EXPECT s" maker self-test image output" GB-RUN-EXPECT
   s" PASS: maker-side linker self-tests (AOT-LINK surface, relocation math, span table, pointer classifier)" type cr ;

: MAIN-OWNER-CHECK ( -- )
   s\" : AMC ( -- ) s\" MAIN\" 0 XREF-FIND-WL XREF-FOUND? 0= if" GE-SRC+
   s\"  s\" AOT global MAIN unavailable\" 74 die then ;" GE-SRC-LINE
   s" AMC" GE-SRC-LINE ;

: BUNDLE-SOURCE ( -- )
   GE-SRC-RESET
   \ Legitimate aligned DATA carries BLR x16 and NOP/NOP/NOP/BL bytes: code-only
   \ scans must not see them. The sparse image stores DATA as unaligned non-zero
   \ runs (src/habu/aot-lib.f), so a whole-range aligned scan no longer sees
   \ these words verbatim either; only the code-only exclusion is checked below.
   s" align create INSTRUCTION-DATA $00 c, $02 c, $3F c, $D6 c," GE-SRC-LINE
   s" $1F c, $20 c, $03 c, $D5 c, $1F c, $20 c, $03 c, $D5 c," GE-SRC-LINE
   s" $1F c, $20 c, $03 c, $D5 c, $00 c, $00 c, $00 c, $94 c," GE-SRC-LINE
   FIB-DEFS
   COMPACT-DEFS
   FEATURE-DEFS
   BUNDLE-MAIN
   \ MAIN-OWNER-CHECK is engine reflection and stays in the application; the
   \ AOT-LINK surface check moved to MAKER-SELFTEST, where the linker exists.
   MAIN-OWNER-CHECK ;

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
   s" hb-build AOT compact/features" GB-HBB-BUILD
   BUNDLE-EXPECT s" hb-build AOT compact/features output" GB-RUN-EXPECT
   GB-OUT$ CODE-RANGE nip {: codesz:n :}
   codesz CODE-TOO-LARGE? if s" hb-build AOT stripped code" GE-FAIL then
   s" hb-build AOT dynamic ELF shape" ASSERT-DYNAMIC-ELF
   \ No whole-range instruction/stencil control here: the sparse image stores
   \ DATA as unaligned non-zero runs, so a whole-file aligned scan can no
   \ longer find INSTRUCTION-DATA's planted blr x16 or NOP/NOP/NOP/BL words
   \ verbatim. Only the code-only exclusion below remains meaningful.
   s" hb-build AOT code excludes DATA blr x16" ASSERT-BLR-ABSENT
   GB-OUT$ REPORT-FILE! REPORT-COUNT
   CODE-REPORT
   s" aot-stripped" s" aot-stripped call report" AOT-ASSERT
   s" aot-compact" s" aot-compact call report" AOT-ASSERT
   s" PASS: hb-build AOT compact/feature coverage (code " type
   codesz GB-U.
   s"  B)" type cr ;

\ Persistent data region: a program that builds a compile-time table with
\ create/comma, reads it in a runtime ?do/loop, and accumulates into a
\ variable via @/!/+!. Proves the AOT entry maps DATA-VA, restores the
\ persistent content, and sets up the return/loop stack. The relocation-math and
\ span-table self-tests this source used to carry are MAKER-SELFTEST's now: an
\ application cannot name a linker word, because the linker is loaded after it.
: DATA-SOURCE ( -- )
   GE-SRC-RESET
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
   s" hb-build AOT data region build" GB-HBB-BUILD
   DATA-EXPECT s" hb-build AOT data region output" GB-RUN-EXPECT
   s" PASS: hb-build AOT persistent data region (create/,/variable/@/!/+!/loop)" type cr ;

\ Persistent-data code-window regression (dot habu-identify-code-pointers-b973e6cc,
\ red-first). A datum whose VALUE lands in the former [RBASE-VA, RBASE-VA+REGION)
\ magnitude window -- here RBASE-VA+REGION-8, the top cell of the JIT region, free
\ space far above the code high-water -- is NOT a pointer. The old CELL-TEXTPTR?
\ magnitude window MISclassified it and hb-build rejected the program (exit 70); the
\ live-extents test correctly classifies it as data, so the program builds and its
\ MAIN reads the datum back unchanged. The classifier's own two-sided check is
\ SELF-TEXTPTR-DEFS, in MAKER-SELFTEST: CELL-TEXTPTR? is a linker word and this
\ application is compiled before the linker is loaded. The RBASE-VA/REGION
\ expression is evaluated by the maker, so the case tracks the constants if a
\ later dot moves the region.
: DATA-WINDOW-SOURCE ( -- )
   GE-SRC-RESET
   s" create X RBASE-VA REGION + 8 - ," GE-SRC-LINE
   s\" : MAIN ( -- ) X @ RBASE-VA REGION + 8 - = IF s\" ok\" ELSE s\" bad\" THEN type cr ;" GE-SRC-LINE ;

: DATA-WINDOW-EXPECT ( -- ptr u8 n )
   SB-RESET
   s" ok" GE-OUT-LINE
   SB$ ;

: DATA-WINDOW ( -- )
   s" hb-aot-window.f" s" hb-aot-window" s" hb-aot-window-report.json" PATHS
   DATA-WINDOW-SOURCE
   s" hb-build AOT code-window datum build" GB-HBB-BUILD
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
   s" hb-build AOT layout-bundle store build" GB-HBB-BUILD
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
\ construction: the validated code span contains zero un-collapsed blr x16.
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
   s" hb-build AOT layout-bundle fetch build" GB-HBB-BUILD
   LAYOUT-FETCH-EXPECT s" hb-build AOT layout-bundle fetch output" GB-RUN-EXPECT
   s" hb-build AOT layout-bundle fetch zero un-collapsed blr x16" ASSERT-BLR-ABSENT
   s" PASS: hb-build AOT layout-bundle fetch (LP2VEXEC reaches via a relocated call; zero blr x16)" type cr ;

\ Fail-closed abs-chain reject (red-first negative case). The AOT linker contract is
\ DIRECT-BL-ONLY: no native emitter produces the absolute movz/movk/movk x16 + blr x16 call
\ form, so the copier and relocator (aot-lib.f COPY-COMPACT-BLOB / RELOCATE) die with
\ E-AOT-ABS-CHAIN if one is ever encountered. This hand-builds one full chain in a synthetic
\ blob and drives the copier over it inside a real stripped build. The maker's rejection is
\ a die (not a catchable result), so per the gate boundary rule it runs as a subprocess
\ sentinel; it lives here rather than in the in-process negative gate because the copier is
\ in the maker-only aot-lib.f. Red-first: before the retirement the copier silently
\ collapsed/copied the chain and the build exited 0; now it rejects with the named error
\ (exit 74).
\
\ IT LEFT THE hb-build WRAPPER because the driver names COPY-COMPACT-BLOB, and an
\ application is now compiled BEFORE the linker is loaded - the capture window opens first -
\ so it has to run where MAKER-SELFTEST's checks run: a file the maker's stdin requires
\ after tools/aot-build.f. hb-build's own propagation of a maker die, non-zero rc with the
\ diagnostic on stderr, is tools/hb-build-test.f HBT-STRIPPED-BELOW-WINDOW's assertion.
: ABS-CHAIN-SOURCE ( -- )
   GE-SRC-RESET
   s" : MAIN ( -- ) ;" GE-SRC-LINE ;

: ABS-CHAIN-SELF-SOURCE ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" create ABT-CHAIN 16 allot" GE-SRC-LINE
   s" : ABT-W! ( n ptr u8 -- ) {: w:n a:ptr :} w a c! w 8 rshift a 1+ c! w 16 rshift a 2 + c! w 24 rshift a 3 + c! ;" GE-SRC-LINE
   s" : ABT-BUILD ( -- ) $D2800010 ABT-CHAIN ABT-W! $F2A00010 ABT-CHAIN 4 + ABT-W! $F2C00010 ABT-CHAIN 8 + ABT-W! $D63F0200 ABT-CHAIN 12 + ABT-W! ;" GE-SRC-LINE
   s" : ABT-RUN ( -- ) 1 CLO-TABLES 1 PLAN-TABLES ABT-BUILD" GE-SRC+
   s"  ABT-CHAIN 0 CLO ! 16 0 CLO-LEN ! XREF-NULL 0 CLO-REC !" GE-SRC+
   s"  0 0 NEWOFF ! 1 NCLO ! 0 COPY-COMPACT-BLOB ;" GE-SRC-LINE
   s" ABT-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: ABS-CHAIN ( -- )
   s" hb-aot-abschain.f" s" hb-aot-got" s" hb-aot-abschain-report.json" PATHS
   ABS-CHAIN-SOURCE GB-WRITE-SRC
   s" hb-aot-abschain-maker.f" SELF-SRC!
   ABS-CHAIN-SELF-SOURCE WRITE-SELF-SRC
   MAKER-RUN
   74 s" hb-build AOT abs-chain reject rc" GE-EXPECT-RC
   s" E-AOT-ABS-CHAIN" s" hb-build AOT abs-chain reject code" GE-EXPECT-ERR-HAS
   GB-OUT$ EXISTS? if s" hb-build AOT abs-chain emitted an image" GE-FAIL then
   s" PASS: hb-build AOT abs-chain reject (E-AOT-ABS-CHAIN; copier fails closed on a direct-BL-only violation)" type cr ;

\ An ADR may not reach out of the member its site is in: the only ADR a compiled
\ body carries is a quotation's address, whose target is a later function of the
\ same emission and so of the same member (src/habu/aot-lib.f ADR-TARGET!). This
\ hand-builds the violation - `ADR x0, .+8` in member 0, aimed at member 1's
\ first byte - and drives the relocator over it inside a real stripped build.
\ Both synthetic members carry XREF-NULL, the record AEREC-TXT spells
\ `<unknown>`, so the site and the target word print that name.
: ADR-MEMBER-SOURCE ( -- )
   GE-SRC-RESET
   s" : MAIN ( -- ) ;" GE-SRC-LINE ;

: ADR-MEMBER-SELF-SOURCE ( -- )
   GE-SRC-RESET
   s" package AOT-LINK" GE-SRC-LINE
   s" create AMT-CODE 16 allot" GE-SRC-LINE
   s" : AMT-RUN ( -- ) 2 CLO-TABLES 2 PLAN-TABLES" GE-SRC+
   s"  AMT-CODE 0 CLO ! 8 0 CLO-LEN ! XREF-NULL 0 CLO-REC !" GE-SRC+
   s"  AMT-CODE 8 + 1 CLO ! 8 1 CLO-LEN ! XREF-NULL 1 CLO-REC !" GE-SRC+
   s"  ASM-LEN 0 NEWOFF ! ASM-LEN 8 + 1 NEWOFF ! 2 NCLO !" GE-SRC+
   s"  0 AMT-CODE $10000040 RELOC-W32 drop ;" GE-SRC-LINE
   s" AMT-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: ADR-MEMBER ( -- )
   s" hb-aot-adrmember.f" s" hb-aot-got" s" hb-aot-adrmember-report.json" PATHS
   ADR-MEMBER-SOURCE GB-WRITE-SRC
   s" hb-aot-adrmember-maker.f" SELF-SRC!
   ADR-MEMBER-SELF-SOURCE WRITE-SELF-SRC
   MAKER-RUN
   74 s" hb-build AOT cross-member ADR reject rc" GE-EXPECT-RC
   s" aot: ADR target outside its member site=<unknown> target="
      s" hb-build AOT cross-member ADR reject site" GE-EXPECT-ERR-HAS
   s" target-word=<unknown>"
      s" hb-build AOT cross-member ADR reject target word" GE-EXPECT-ERR-HAS
   GB-OUT$ EXISTS? if s" hb-build AOT cross-member ADR emitted an image" GE-FAIL then
   s" PASS: hb-build AOT cross-member ADR reject (named site and target word; relocation fails closed)" type cr ;

\ item 10 slice 5: a preseeded bad-tag object/AOT test entry. A source declaring a
\ matched family + helper is AOT-built with a SELECTED non-MAIN entry (the helper)
\ and a forged value-stack seed (payload slots + an out-of-range tag), so the
\ stripped image starts at the helper and reaches its inline invalid-tag die
\ (rc ENGINE-ERROR:BAD-TAG 85 + "hb: bad gemt tag"). The SAME source built normally (entry
\ MAIN) exits 0, and the entry/seed/mode axis is folded into every cache layer
\ (artifact key + source-index key + object bytes) so the two are distinct
\ artifacts with no cross-restore in either direction, and the die survives an
\ object-cache relink.
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
   s" AOT-LAYOUT-FETCH-BAD:HLP" HBB-PRESEED-ENTRY!
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
   MAKER-SELFTEST
   BUNDLE
   DATA
   DATA-WINDOW
   LAYOUT-STORE
   LAYOUT-FETCH
   ABS-CHAIN
   ADR-MEMBER
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
   s" PASS: native hb-build AOT positive tests" type cr ;

;using                                   \ AOT-IMAGE
;using                                   \ HB-BUILD-CLI
;package
