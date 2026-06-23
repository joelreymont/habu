\ gate-build-common.f - checked helpers for native hb-build gate slices.
\
\ Load after test/gate-common.f.

$40000 constant GB-MACHO-CAP
$FEEDFACF constant GB-MH-MAGIC64
$19 constant GB-LC-SEGMENT-64
32 constant GB-MH-SIZE
16 constant GB-MH-NCMDS-OFF
4 constant GB-LC-CMDSIZE-OFF
64 constant GB-SEG-NSECTS-OFF
72 constant GB-SEG-SECTIONS-OFF
40 constant GB-SECT-SIZE-OFF
80 constant GB-SECT-SIZE
34 constant GB-DQ

create GB-SRC-PATH FS-PATH-CAP allot
create GB-OUT-PATH FS-PATH-CAP allot
create GB-REPORT-PATH FS-PATH-CAP allot
create GB-MACHO-BUF GB-MACHO-CAP allot

variable GB-SRC-U
variable GB-OUT-U
variable GB-REPORT-U
variable GB-MACHO-U
variable GB-TEXT-SIZE-V
variable GB-TEXT-FOUND
variable GB-LC-OFF

: GB-SRC$ ( -- ptr u8 n )
   GB-SRC-PATH GB-SRC-U @ ;

: GB-OUT$ ( -- ptr u8 n )
   GB-OUT-PATH GB-OUT-U @ ;

: GB-REPORT$ ( -- ptr u8 n )
   GB-REPORT-PATH GB-REPORT-U @ ;

: GB-SRC! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GB-SRC-PATH GT-PATH GB-SRC-U ! ;

: GB-OUT! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GB-OUT-PATH GT-PATH GB-OUT-U ! ;

: GB-REPORT! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GB-REPORT-PATH GT-PATH GB-REPORT-U ! ;

: GB-WRITE-SRC ( -- )
   GB-SRC$ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

: GB-BUILD-ARGV ( -- )
   GE-HB-RESET
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: GB-HB-BUILD-ARGS ( -- )
   GB-SRC$  >LEN PROC-ARGV+
   s" -o"  >LEN PROC-ARGV+
   GB-OUT$  >LEN PROC-ARGV+ ;

: GB-HB-BUILD-CAPTURE ( -- )
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   ;

: GB-HB-BUILD ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-BUILD-ARGV
   GB-HB-BUILD-ARGS
   GB-HB-BUILD-CAPTURE
   label labelu GE-EXPECT-OK
   GB-OUT$ FILE? 0= if label labelu GE-FAIL then ;

: GB-RUN-OUT ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-HB-RESET
   GB-OUT$ GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK ;

: GB-RUN-EXPECT ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu label:ptr labelu :}
   label labelu GB-RUN-OUT
   want wantu label labelu GE-EXPECT-OUT ;

: GB-AOT-REPORT ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-HB-RESET
   s" tools/aot-call-report.f"  >LEN PROC-ARGV+
   GB-OUT$  >LEN PROC-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   GB-REPORT$ GT-OUT$ WRITE-ALL ;

: GB-GJA-ARGV ( -- )
   GE-HB-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/gate-json-assert.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: GB-GJA ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu label:ptr labelu :}
   GB-GJA-ARGV
   mode modeu  >LEN PROC-ARGV+
   GB-REPORT$  >LEN PROC-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK ;

: GB-J-DQ ( -- )
   GB-DQ SB-APPEND-C ;

: GB-J-COLON ( -- )
   s" :" SB-APPEND ;

: GB-JKEY ( ptr u8 n -- )
   GB-J-DQ
   SB-APPEND
   GB-J-DQ ;

: GB-EXPECT-ERR-FIELD ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu label:ptr labelu :}
   SB-RESET
   key keyu GB-JKEY
   GB-J-COLON
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GB-EXPECT-ERR-RAW-FIELD ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key:ptr keyu raw:ptr rawu label:ptr labelu :}
   SB-RESET
   key keyu GB-JKEY
   GB-J-COLON
   raw rawu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GB-EXPECT-ERR-STR-FIELD ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key:ptr keyu val:ptr valu label:ptr labelu :}
   SB-RESET
   key keyu GB-JKEY
   GB-J-COLON
   GB-J-DQ
   val valu SB-APPEND
   GB-J-DQ
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GB-U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

: GB-RANGE ( n n -- ) {: off u :}
   off 0 < if E-BUILD-SOURCE throw then
   u 0 < if E-BUILD-SOURCE throw then
   off u + GB-MACHO-U @ > if E-BUILD-SOURCE throw then ;

: GB-ADDR ( n -- ptr u8 ) {: off :}
   off 1 GB-RANGE
   GB-MACHO-BUF off + ;

: GB-U32-OFF ( n -- n ) {: off :}
   off 4 GB-RANGE
   off GB-ADDR GB-U32@ ;

: GB-U64-OFF ( n -- n ) {: off :}
   off 8 GB-RANGE
   off GB-ADDR FS-U64@ ;

: GB-SECTION-TEXT? ( n -- bool )
   GB-ADDR 6 s" __text" STR= ;

: GB-SCAN-SECTION ( n -- ) {: off :}
   off GB-SECTION-TEXT? if
      off GB-SECT-SIZE-OFF + GB-U64-OFF GB-TEXT-SIZE-V !
      -1 GB-TEXT-FOUND !
   then ;

: GB-SCAN-SECTIONS ( n n -- ) {: seg nsects :}
   0 begin dup nsects < GB-TEXT-FOUND @ 0= and while
      seg GB-SEG-SECTIONS-OFF + over GB-SECT-SIZE * + GB-SCAN-SECTION
      1+
   repeat drop ;

: GB-SCAN-LOAD ( n -- ) {: off :}
   off GB-U32-OFF GB-LC-SEGMENT-64 = if
      off off GB-SEG-NSECTS-OFF + GB-U32-OFF GB-SCAN-SECTIONS
   then ;

: GB-SCAN-LOADS ( -- )
   0 GB-TEXT-FOUND !
   GB-MH-SIZE GB-LC-OFF !
   0 begin dup GB-MH-NCMDS-OFF GB-U32-OFF < GB-TEXT-FOUND @ 0= and while
      GB-LC-OFF @ GB-SCAN-LOAD
      GB-LC-OFF @ GB-LC-CMDSIZE-OFF + GB-U32-OFF GB-LC-OFF @ + GB-LC-OFF !
      1+
   repeat drop ;

: GB-MACHO-TEXT-SIZE ( ptr u8 n -- n ) {: path:ptr pathu :}
   path pathu GB-MACHO-BUF GB-MACHO-CAP READ-ALL GB-MACHO-U !
   0 GB-U32-OFF GB-MH-MAGIC64 <> if E-BUILD-SOURCE throw then
   GB-SCAN-LOADS
   GB-TEXT-FOUND @ 0= if E-BUILD-SOURCE throw then
   GB-TEXT-SIZE-V @ ;

: GB-U. ( n -- ) {: n :}
   n 0 < if [char] - emit n negate recurse exit then
   n 10 >= if n 10 / recurse then
   n 10 mod STR-ZERO + emit ;
