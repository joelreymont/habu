\ fs-primitive-parity-test.f - target and recovery filesystem primitive parity.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f

package FS-PRIMITIVE-PARITY
private

$78563412 constant U32-VALUE
448 constant MODE-0700
$200 constant LINUX-REMOVE-DIR
$80 constant MACOS-REMOVE-DIR

create U32-BYTES
   $12 c, $34 c, $56 c, $78 c,

create NUL-PATH
   $61 c, 0 c, $62 c,

create ROOT-BUF FS-PATH-CAP allot
create A-BUF FS-PATH-CAP allot
create B-BUF FS-PATH-CAP allot
create HARD-BUF FS-PATH-CAP allot
create LINK-BUF FS-PATH-CAP allot
create ROOT-Z FS-PATHZ-CAP allot
create A-Z FS-PATHZ-CAP allot
create HARD-Z FS-PATHZ-CAP allot
create REL-A $61 c, 0 c,
create REL-B $62 c, 0 c,
create REL-DIR $64 c, $69 c, $72 c, 0 c,
create REL-LINK $6C c, $69 c, $6E c, $6B c, 0 c,
create IO-BUF 16 allot
create STAT-BUF FS-STAT-CAP allot

variable ROOT-U
variable A-U
variable B-U
variable HARD-U
variable LINK-U
variable IO-OFF

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: A$ ( -- ptr u8 n )
   A-BUF A-U @ ;

: B$ ( -- ptr u8 n )
   B-BUF B-U @ ;

: HARD$ ( -- ptr u8 n )
   HARD-BUF HARD-U @ ;

: LINK$ ( -- ptr u8 n )
   LINK-BUF LINK-U @ ;

: PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: JOIN! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr up:ptr :}
   pa pu na nu dst JOIN-PATH up ! ;

: PATHS! ( -- )
   s" /tmp" s" hb-fs-primitive" MAKE-TEMP-DIR ROOT-BUF ROOT-U PATH!
   ROOT$ s" a" A-BUF A-U JOIN!
   ROOT$ s" b" B-BUF B-U JOIN!
   ROOT$ s" hard" HARD-BUF HARD-U JOIN!
   ROOT$ s" link" LINK-BUF LINK-U JOIN!
   ROOT$ ROOT-Z FS-PATHZ-INTO drop
   A$ A-Z FS-PATHZ-INTO drop
   HARD$ HARD-Z FS-PATHZ-INTO drop ;

: N>FD ( n -- fd )
   dup 0 < if throw then >FD ;

: MUST-RC ( rc -- )
   RC>N dup 0 <> if throw then drop ;

: WRITE-SPAN ( fd ptr u8 n -- ) {: fd:fd a:ptr u:n :}
   0 IO-OFF !
   begin IO-OFF @ u < while
      fd a IO-OFF @ + u IO-OFF @ - write-fd
      dup 0 <= if dup 0= if drop E-FS-IO then throw then
      IO-OFF +!
   repeat ;

: READ-SPAN ( fd ptr u8 n -- n ) {: fd:fd a:ptr u:n :}
   0 IO-OFF !
   begin IO-OFF @ u < while
      fd a IO-OFF @ + u IO-OFF @ - read-fd dup 0 < if throw then
      dup 0= if drop IO-OFF @ exit then
      IO-OFF +!
   repeat
   IO-OFF @ ;

: MODE@ ( ptr u8 -- n )
   4 + FS-U16@ ;

: CLOSE-MUST ( fd -- )
   close-rc MUST-RC ;

: REMOVE-DIR-FLAG ( -- n )
   HB-TARGET-LINUX? if LINUX-REMOVE-DIR exit then
   HB-TARGET-MACOS? if MACOS-REMOVE-DIR exit then
   FS-TARGET-UNKNOWN ;

: HAS ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n text:ptr textu:n :}
   path pathu LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT text textu LINT-CONTAINS? TTRUE ;

: LINUX-NUMBERS ( -- )
   s" src/os/linux/sys.f" s" 56  constant NR-OPENAT" HAS
   s" src/os/linux/sys.f" s" 34  constant NR-MKDIRAT" HAS
   s" src/os/linux/sys.f" s" 35  constant NR-UNLINKAT" HAS
   s" src/os/linux/sys.f" s" 37  constant NR-LINKAT" HAS
   s" src/os/linux/sys.f" s" 52  constant NR-FCHMOD" HAS
   s" src/os/linux/sys.f" s" 38  constant NR-RENAMEAT" HAS
   s" src/os/linux/sys.f" s" 80  constant NR-FSTAT64" HAS
   s" src/os/linux/sys.f" s" 79  constant NR-FSTATAT64" HAS
   s" src/os/linux/sys.f" s" $100 constant AT-SYMLINK-NOFOLLOW" HAS
   s" src/os/linux/sys.f" s" 82  constant NR-FSYNC" HAS
   s" src/os/linux/sys.f" s" 278 constant NR-GETRANDOM" HAS
   s" src/os/linux/sys.f" s" -100 constant AT-FDCWD" HAS ;

: RECOVERY-LINUX-NUMBERS ( -- )
   s" bootstrap/cg/sys.fs" s" 56  constant NR-OPENAT" HAS
   s" bootstrap/cg/sys.fs" s" 34  constant NR-MKDIRAT" HAS
   s" bootstrap/cg/sys.fs" s" 35  constant NR-UNLINKAT" HAS
   s" bootstrap/cg/sys.fs" s" 37  constant NR-LINKAT" HAS
   s" bootstrap/cg/sys.fs" s" 52  constant NR-FCHMOD" HAS
   s" bootstrap/cg/sys.fs" s" 38  constant NR-RENAMEAT" HAS
   s" bootstrap/cg/sys.fs" s" 80  constant NR-FSTAT64" HAS
   s" bootstrap/cg/sys.fs" s" 79  constant NR-FSTATAT64" HAS
   s" bootstrap/cg/sys.fs" s" $100 constant AT-SYMLINK-NOFOLLOW" HAS
   s" bootstrap/cg/sys.fs" s" 82  constant NR-FSYNC" HAS
   s" bootstrap/cg/sys.fs" s" 278 constant NR-GETRANDOM" HAS
   s" bootstrap/cg/sys.fs" s" -100 constant AT-FDCWD" HAS ;

: MACOS-NUMBERS ( -- )
   s" src/os/macos/sys.f" s" $1CF constant NR-OPENAT" HAS
   s" src/os/macos/sys.f" s" $1DB constant NR-MKDIRAT" HAS
   s" src/os/macos/sys.f" s" $1D8 constant NR-UNLINKAT" HAS
   s" src/os/macos/sys.f" s" $1D7 constant NR-LINKAT" HAS
   s" src/os/macos/sys.f" s" $7C  constant NR-FCHMOD" HAS
   s" src/os/macos/sys.f" s" $1D1 constant NR-RENAMEAT" HAS
   s" src/os/macos/sys.f" s" $153 constant NR-FSTAT64" HAS
   s" src/os/macos/sys.f" s" $1D6 constant NR-FSTATAT64" HAS
   s" src/os/macos/sys.f" s" $20 constant AT-SYMLINK-NOFOLLOW" HAS
   s" src/os/macos/sys.f" s" $5F  constant NR-FSYNC" HAS
   s" src/os/macos/sys.f" s" $1F4 constant NR-GETENTROPY" HAS
   s" src/os/macos/sys.f" s" -2 constant AT-FDCWD" HAS ;

: RECOVERY-MACOS-NUMBERS ( -- )
   s" bootstrap/cg/sys.fs" s" 463 constant NR-OPENAT" HAS
   s" bootstrap/cg/sys.fs" s" 475 constant NR-MKDIRAT" HAS
   s" bootstrap/cg/sys.fs" s" 472 constant NR-UNLINKAT" HAS
   s" bootstrap/cg/sys.fs" s" 471 constant NR-LINKAT" HAS
   s" bootstrap/cg/sys.fs" s" 124 constant NR-FCHMOD" HAS
   s" bootstrap/cg/sys.fs" s" 465 constant NR-RENAMEAT" HAS
   s" bootstrap/cg/sys.fs" s" 339 constant NR-FSTAT64" HAS
   s" bootstrap/cg/sys.fs" s" 470 constant NR-FSTATAT64" HAS
   s" bootstrap/cg/sys.fs" s" $20 constant AT-SYMLINK-NOFOLLOW" HAS
   s" bootstrap/cg/sys.fs" s" 95  constant NR-FSYNC" HAS
   s" bootstrap/cg/sys.fs" s" 500 constant NR-GETENTROPY" HAS
   s" bootstrap/cg/sys.fs" s" -2 constant AT-FDCWD" HAS ;

: FLAGS ( -- )
   s" lib/fs.f" s" $800 constant O-EXCL" HAS
   s" lib/fs.f" s" $100000 constant O-DIRECTORY" HAS
   s" lib/fs.f" s" $100 constant O-NOFOLLOW" HAS
   s" lib/fs.f" s" 420 constant MODE-0644" HAS
   s" lib/fs.f" s" 493 constant MODE-0755" HAS
   s" src/os/linux/sys.f" s" 7 $8000 MOVZ" HAS
   s" src/os/linux/sys.f" s" 7 $80 MOVZ" HAS
   s" src/os/linux/sys.f" s" 7 $4000 MOVZ" HAS
   s" bootstrap/cg/sys.fs" s" 7 $8000 MOVZ" HAS
   s" bootstrap/cg/sys.fs" s" 7 $80 MOVZ" HAS
   s" bootstrap/cg/sys.fs" s" 7 $4000 MOVZ" HAS ;

: PRIMITIVES ( -- )
   s" src/habu/habu1.f" S\" s\" openat\" ['] BOPENAT FPRIM-L" HAS
   s" src/habu/habu1.f" S\" s\" mkdirat\" ['] BMKDIRAT FPRIM-L" HAS
   s" src/habu/habu1.f" S\" s\" open-errno\" ['] BOPEN-ERRNO FPRIM-L" HAS
   s" src/habu/habu1.f" S\" s\" link\" ['] BLINK FPRIM-L" HAS
   s" src/habu/habu1.f" S\" s\" fchmod\" ['] BFCHMOD FPRIM-L" HAS
   s" src/habu/habu1.f" S\" s\" fstat64\" ['] BFSTAT64 FPRIM-L" HAS
   s" src/habu/habu1.f" S\" s\" fstatat-nofollow\" ['] BFSTATAT-NOFOLLOW FPRIM-L" HAS
   s" src/habu/habu1.f" S\" s\" close-rc\" ['] BCLOSE-RC FPRIM-L" HAS
   s" bootstrap/cg/forth.fs" S\" s\" openat\" ['] BOPENAT FPRIM-L" HAS
   s" bootstrap/cg/forth.fs" S\" s\" mkdirat\" ['] BMKDIRAT FPRIM-L" HAS
   s" bootstrap/cg/forth.fs" S\" s\" open-errno\" ['] BOPEN-ERRNO FPRIM-L" HAS
   s" bootstrap/cg/forth.fs" S\" s\" link\" ['] BLINK FPRIM-L" HAS
   s" bootstrap/cg/forth.fs" S\" s\" fchmod\" ['] BFCHMOD FPRIM-L" HAS
   s" bootstrap/cg/forth.fs" S\" s\" fstat64\" ['] BFSTAT64 FPRIM-L" HAS
   s" bootstrap/cg/forth.fs" S\" s\" fstatat-nofollow\" ['] BFSTATAT-NOFOLLOW FPRIM-L" HAS
   s" bootstrap/cg/forth.fs" S\" s\" close-rc\" ['] BCLOSE-RC FPRIM-L" HAS
   s" src/core/checker.f" s" PRIM: link" HAS
   s" src/core/checker.f" s" PRIM: mkdirat" HAS
   s" src/core/checker.f" s" PRIM: open-errno" HAS
   s" src/core/checker.f" s" PRIM: fchmod" HAS
   s" src/core/checker.f" s" PRIM: fstatat-nofollow" HAS ;

: BAD-PATH ( -- )
   NUL-PATH 3 FS:CHECK-PATH-BYTES ;

: HELPERS ( -- )
   U32-BYTES FS:U32@ U32-VALUE T=
   s" lib/fs.f" FS:CHECK-PATH-BYTES
   [: BAD-PATH ;] E-FS-PATH-UNSAFE TTHROWSQ
   s" lib/fs.f" FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then >FD
   {: fd:fd :}
   fd FS:TRY-FSTAT TTRUE
   FS:STAT-INO@ 0 > TTRUE
   FS:STAT-NLINK@ 0 > TTRUE
   fd close-rc RC>N 0 T= ;

: RUNTIME ( -- )
   PATHS!
   ROOT-Z FS:O-DIRECTORY FS:O-NOFOLLOW or 0 open N>FD {: dir:fd :}
   dir REL-A FS-O-RDWR FS-O-CREAT or FS:O-EXCL or FS:O-NOFOLLOW or
   $180 openat N>FD {: file:fd :}
   dir REL-DIR MODE-0700 mkdirat MUST-RC
   dir REL-DIR REMOVE-DIR-FLAG unlinkat MUST-RC
   file s" abc" WRITE-SPAN
   file fsync MUST-RC
   file FS:MODE-0755 fchmod MUST-RC
   file STAT-BUF fstat64 MUST-RC
   STAT-BUF MODE@ $1FF and FS:MODE-0755 T=
   file CLOSE-MUST
   dir REL-A FS:O-NOFOLLOW 0 openat N>FD {: input:fd :}
   input IO-BUF 3 READ-SPAN 3 T=
   IO-BUF 3 s" abc" T$=
   input CLOSE-MUST
   A-Z HARD-Z link MUST-RC
   dir REL-A dir REL-B renameat MUST-RC
   A$ EXISTS? TFALSE
   A-Z FS:O-NOFOLLOW 0 open-errno -2 T=
   B$ FILE? TTRUE
   HARD$ FILE? TTRUE
   B$ LINK$ MAKE-SYMLINK
   dir REL-LINK STAT-BUF fstatat-nofollow MUST-RC
   STAT-BUF MODE@ S-IFMT and S-IFLNK T=
   dir REL-LINK 0 unlinkat MUST-RC
   dir REL-B 0 unlinkat MUST-RC
   dir CLOSE-MUST
   IO-BUF 16 entropy dup 0 > TTRUE 16 <= TTRUE
   ROOT$ REMOVE-TREE ;

: MAIN ( -- )
   T-RESET
   LINUX-NUMBERS
   RECOVERY-LINUX-NUMBERS
   MACOS-NUMBERS
   RECOVERY-MACOS-NUMBERS
   FLAGS
   PRIMITIVES
   HELPERS
   RUNTIME
   T-REPORT
   s" fs-primitive-parity-test: ok" type cr ;

MAIN

;package
