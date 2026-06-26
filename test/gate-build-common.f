\ gate-build-common.f - checked helpers for native hb-build gate slices.
\
\ Load after test/gate-common.f and lib/memory.f.

$FEEDFACF constant GB-MH-MAGIC64
$19 constant GB-LC-SEGMENT-64
32 constant GB-MH-SIZE
16 constant GB-MH-NCMDS-OFF
4 constant GB-LC-CMDSIZE-OFF
64 constant GB-SEG-NSECTS-OFF
72 constant GB-SEG-SECTIONS-OFF
40 constant GB-SECT-SIZE-OFF
80 constant GB-SECT-SIZE
$464C457F constant GB-ELF-MAGIC
$B7 constant GB-ELF-MACHINE-AARCH64
1 constant GB-ELF-PT-LOAD
2 constant GB-ELF-PT-DYNAMIC
3 constant GB-ELF-PT-INTERP
1 constant GB-ELF-PF-X
2 constant GB-ELF-PF-W
4 constant GB-ELF-PF-R
18 constant GB-ELF-MACHINE-OFF
32 constant GB-ELF-PHOFF-OFF
54 constant GB-ELF-PHENTSIZE-OFF
56 constant GB-ELF-PHNUM-OFF
0 constant GB-ELF-PH-TYPE-OFF
4 constant GB-ELF-PH-FLAGS-OFF
8 constant GB-ELF-PH-FILE-OFF
16 constant GB-ELF-PH-VADDR-OFF
32 constant GB-ELF-PH-FILESZ-OFF
$120 constant GB-ELF-INTERP-OFF
27 constant GB-ELF-INTERP-SZ
$1B8 constant GB-ELF-RELA-OFF
$500000 constant GB-ELF-RW-VA
$5000B0 constant GB-ELF-DLOPEN-SLOT
$5000B8 constant GB-ELF-DLSYM-SLOT
$C0 constant GB-ELF-RW-SZ
$B0 constant GB-ELF-DYNAMIC-SZ
$100000401 constant GB-ELF-DLOPEN-RINFO
$200000401 constant GB-ELF-DLSYM-RINFO
34 constant GB-DQ

create GB-SRC-PATH FS-PATH-CAP allot
create GB-OUT-PATH FS-PATH-CAP allot
create GB-REPORT-PATH FS-PATH-CAP allot

variable GB-SRC-U
variable GB-OUT-U
variable GB-REPORT-U
variable GB-EXEC-A
variable GB-EXEC-CAP-U
variable GB-EXEC-U
variable GB-TEXT-SIZE-V
variable GB-TEXT-FOUND
variable GB-LC-OFF

: GB-SRC$ ( -- ptr u8 n )
   GB-SRC-PATH GB-SRC-U @ ;

: GB-OUT$ ( -- ptr u8 n )
   GB-OUT-PATH GB-OUT-U @ ;

: GB-REPORT$ ( -- ptr u8 n )
   GB-REPORT-PATH GB-REPORT-U @ ;

: GB-EXEC-BUF ( -- ptr u8 )
   GB-EXEC-A @ ;

: GB-EXEC-CAPACITY ( -- n )
   GB-EXEC-CAP-U @ ;

: GB-SRC! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GB-SRC-PATH GT-PATH GB-SRC-U ! ;

: GB-OUT! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GB-OUT-PATH GT-PATH GB-OUT-U ! ;

: GB-REPORT! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GB-REPORT-PATH GT-PATH GB-REPORT-U ! ;

: GB-WRITE-SRC ( -- )
   GB-SRC$ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

: GB-ARGV+ ( ptr u8 n -- )
   GE-ARG+ ;

: GB-TARGET-UNKNOWN ( -- )
   E-BUILD-SOURCE throw ;

: GB-TARGET-LAYOUT ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/layout.f" GB-ARGV+
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/layout.f" GB-ARGV+
      exit
   then
   GB-TARGET-UNKNOWN ;

: GB-BUILD-LOADS ( -- )
   s" --load" GB-ARGV+
   s" lib/errors.f" GB-ARGV+
   s" lib/string.f" GB-ARGV+
   s" lib/fs.f" GB-ARGV+
   s" lib/fs-mutate.f" GB-ARGV+
   s" lib/process.f" GB-ARGV+
   s" lib/process-argv.f" GB-ARGV+
   s" lib/process-env.f" GB-ARGV+
   s" lib/source.f" GB-ARGV+
   s" lib/build.f" GB-ARGV+
   s" lib/codesign.f" GB-ARGV+
   s" tools/build-fixpoint.f" GB-ARGV+
   s" tools/warm-run.f" GB-ARGV+
   s" tools/hb-build-lib.f" GB-ARGV+
   s" tools/hb-build.f" GB-ARGV+
   s" --" GB-ARGV+ ;

: GB-BUILD-ARGV-TMP ( ptr u8 n -- ) {: tmp:ptr tmpu :}
   GE-HB-RESET
   s" HB_TMP" >LEN tmp tmpu >LEN PROC-ENV+
   GB-BUILD-LOADS ;

: GB-BUILD-ARGV ( -- )
   GT-ROOT GB-BUILD-ARGV-TMP ;

: GB-HB-BUILD-ARGS ( -- )
   GB-SRC$ GB-ARGV+
   s" -o" GB-ARGV+
   GB-OUT$ GB-ARGV+ ;

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
   s" tools/aot-call-report.f" GB-ARGV+
   GB-OUT$ GB-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK
   GB-REPORT$ GT-OUT$ WRITE-ALL ;

: GB-GJA-ARGV ( -- )
   GE-HB-RESET
   s" --load" GB-ARGV+
   s" lib/errors.f" GB-ARGV+
   s" lib/memory.f" GB-ARGV+
   s" tools/json.f" GB-ARGV+
   s" tools/gate-json-assert-core.f" GB-ARGV+
   s" tools/gate-json-assert.f" GB-ARGV+
   s" --" GB-ARGV+ ;

: GB-GJA ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu label:ptr labelu :}
   GB-GJA-ARGV
   mode modeu GB-ARGV+
   GB-REPORT$ GB-ARGV+
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

: GB-U16@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1 + c@ 8 lshift or ;

: GB-EXEC-ALLOC ( n -- ) {: need :}
   need MEM-ALLOC-64K-SPAN {: buf:ptr cap :}
   buf GB-EXEC-A !
   cap GB-EXEC-CAP-U ! ;

: GB-EXEC-ENSURE ( n -- ) {: need :}
   need GB-EXEC-CAPACITY <= if exit then
   need GB-EXEC-ALLOC ;

: GB-READ-EXEC ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE-SIZE {: need :}
   need GB-EXEC-ENSURE
   path pathu GB-EXEC-BUF GB-EXEC-CAPACITY READ-ALL GB-EXEC-U !
   GB-EXEC-U @ need <> if E-BUILD-SOURCE throw then ;

: GB-RANGE ( n n -- ) {: off u :}
   off 0 < if E-BUILD-SOURCE throw then
   u 0 < if E-BUILD-SOURCE throw then
   off u + GB-EXEC-U @ > if E-BUILD-SOURCE throw then ;

: GB-ADDR ( n -- ptr u8 ) {: off :}
   off 1 GB-RANGE
   GB-EXEC-BUF off + ;

: GB-U16-OFF ( n -- n ) {: off :}
   off 2 GB-RANGE
   off GB-ADDR GB-U16@ ;

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

: GB-MACHO-TEXT-SIZE-LOADED ( -- n )
   0 GB-U32-OFF GB-MH-MAGIC64 <> if E-BUILD-SOURCE throw then
   GB-SCAN-LOADS
   GB-TEXT-FOUND @ 0= if E-BUILD-SOURCE throw then
   GB-TEXT-SIZE-V @ ;

: GB-MACHO-TEXT-SIZE ( ptr u8 n -- n )
   GB-READ-EXEC
   GB-MACHO-TEXT-SIZE-LOADED ;

: GB-ELF-PH-OFF ( n -- n ) {: idx :}
   GB-ELF-PHOFF-OFF GB-U64-OFF
   idx GB-ELF-PHENTSIZE-OFF GB-U16-OFF * + ;

: GB-ELF-RX-FLAGS? ( n -- bool ) {: flags :}
   flags GB-ELF-PF-R and 0= 0= 
   flags GB-ELF-PF-X and 0= 0= and
   flags GB-ELF-PF-W and 0= and ;

: GB-ELF-RX-LOAD? ( n -- bool ) {: off :}
   off GB-ELF-PH-TYPE-OFF + GB-U32-OFF GB-ELF-PT-LOAD =
   off GB-ELF-PH-FLAGS-OFF + GB-U32-OFF GB-ELF-RX-FLAGS? and ;

: GB-SCAN-PHDR ( n -- ) {: idx :}
   idx GB-ELF-PH-OFF {: off :}
   off GB-ELF-RX-LOAD? if
      off GB-ELF-PH-FILESZ-OFF + GB-U64-OFF GB-TEXT-SIZE-V !
      -1 GB-TEXT-FOUND !
   then ;

: GB-SCAN-PHDRS ( -- )
   0 GB-TEXT-FOUND !
   0 begin dup GB-ELF-PHNUM-OFF GB-U16-OFF < GB-TEXT-FOUND @ 0= and while
      dup GB-SCAN-PHDR
      1+
   repeat drop ;

: GB-ELF-TEXT-SIZE-LOADED ( -- n )
   0 GB-U32-OFF GB-ELF-MAGIC <> if E-BUILD-SOURCE throw then
   GB-ELF-MACHINE-OFF GB-U16-OFF GB-ELF-MACHINE-AARCH64 <> if E-BUILD-SOURCE throw then
   GB-SCAN-PHDRS
   GB-TEXT-FOUND @ 0= if E-BUILD-SOURCE throw then
   GB-TEXT-SIZE-V @ ;

: GB-ELF-TEXT-SIZE ( ptr u8 n -- n )
   GB-READ-EXEC
   GB-ELF-TEXT-SIZE-LOADED ;

: GB-EXEC-TEXT-SIZE ( ptr u8 n -- n )
   GB-READ-EXEC
   HB-TARGET-LINUX? if
      GB-ELF-TEXT-SIZE-LOADED
      exit
   then
   HB-TARGET-MACOS? if
      GB-MACHO-TEXT-SIZE-LOADED
      exit
   then
   GB-TARGET-UNKNOWN ;

: GB-U. ( n -- ) {: n :}
   n 0 < if [char] - emit n negate recurse exit then
   n 10 >= if n 10 / recurse then
   n 10 mod STR-ZERO + emit ;
