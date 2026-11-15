\ fs-statx-test.f - FS-STATX:BIRTH and FS-STATX:SUBVOLUME on a file made
\ under the test's temporary directory, and their refusal of a closed one.
\ Run: bin/hb --load src/lib/fs-statx-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/lib/fs-statx.f

package FS-STATX-TEST

\ File stamps come from the kernel's coarse clock, which may trail
\ CLOCK_REALTIME; a bracket opened on the fine clock could miss the stamp.
5 constant CLOCK-REALTIME-COARSE
0 constant CLOCK-REALTIME
$01021994 constant TMPFS-MAGIC
$9123683E constant BTRFS-MAGIC
\ struct statfs on 64-bit Linux: f_type is its first cell, 120 bytes in all.
120 constant STATFS-BYTES

PROCESS-SYMBOLS
FUNCTION: CLOCK-CALL clock_gettime ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: FSTATFS-CALL fstatfs ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: FUTIMENS-CALL futimens ( n ^u8 -- i32 ) ;FUNCTION

create ROOT PATH-CAP allot
create PATH-BUF PATH-CAP allot
create SPEC 16 allot
create TIMES 32 allot
create STATFS-BUF STATFS-BYTES allot
variable ROOT-U

: ROOT$ ( -- ^u8 n ) ROOT ROOT-U @ ;
: PATH ( ^u8 n -- ^u8 n )
   ROOT$ 2swap PATH-BUF FS:JOIN-PATH PATH-BUF swap ;
: SETUP ( -- )
   s" habu-fs-statx" FS:MKTEMP-DIR {: a:^ u:n :}
   a ROOT u BYTE-COPY  u ROOT-U ! ;

\ The clock's time, seconds then nanoseconds.
: CLOCK ( n -- n n )
   SPEC CLOCK-CALL 0 T=
   SPEC LE:U64@  SPEC 8 + LE:U64@ ;

\ Whether the first time, seconds then nanoseconds, is not after the second.
: NOT-AFTER? ( n n n n -- bool ) {: s1 n1 s2 n2 :}
   s1 s2 < if true exit then
   s1 s2 = n1 n2 <= and ;

\ Access and modification time one second past the epoch, so neither can
\ pass for the birth time.
: AGE ( fd -- ) {: file:fd :}
   1 TIMES LE:U64!  0 TIMES 8 + LE:U64!
   1 TIMES 16 + LE:U64!  0 TIMES 24 + LE:U64!
   file FD>N TIMES FUTIMENS-CALL 0 T= ;

: MAGIC ( fd -- n )
   FD>N STATFS-BUF FSTATFS-CALL 0 T=
   STATFS-BUF LE:U64@ ;

: FS-NAME$ ( n -- ^u8 n ) {: m:n :}
   m TMPFS-MAGIC = if s" tmpfs" exit then
   m BTRFS-MAGIC = if s" btrfs" exit then
   s" magic " SB-RESET SB-APPEND  m FMT:SB-HEX  SB$ ;

: ON ( fd -- )
   s"  on " type MAGIC FS-NAME$ type cr ;

: SAY ( ^u8 n fd -- ) {: a:^ u:n file:fd :}
   s" fs-statx-test: " type a u type file ON ;

\ Birth lies between a coarse clock read before the file is made and a
\ fine one read after; a filesystem that keeps none is named and passes.
: CHECK-BIRTH ( -- fd )
   CLOCK-REALTIME-COARSE CLOCK {: bsec bns :}
   s" file" PATH s" abc" FS:WRITE-ALL
   s" file" PATH FS:OPEN-SIZED {: file:fd size:n ok:bool :}
   ok TTRUE  size 3 T=
   file AGE
   CLOCK-REALTIME CLOCK {: asec ans :}
   file FS-STATX:BIRTH {: sec ns got:bool :}
   got 0= if s" birth time not reported" file SAY file exit then
   bsec bns sec ns NOT-AFTER? TTRUE
   sec ns asec ans NOT-AFTER? TTRUE
   ns 1000000000 < TTRUE
   s" birth time reported" file SAY
   file ;

\ tmpfs keeps no subvolume; another filesystem's answer is stated.
: CHECK-SUBVOLUME ( fd -- ) {: file:fd :}
   file FS-STATX:SUBVOLUME {: id got:bool :}
   file MAGIC TMPFS-MAGIC = if got TFALSE then
   got if
      s" fs-statx-test: subvolume " type id FMT:.INT
      s"  reported" type file ON exit
   then
   s" subvolume not reported" file SAY ;

: CHECK-CLOSED ( fd -- ) {: file:fd :}
   file FD>N FS:CLOSE-FD 0 T=
   [: file FS-STATX:BIRTH drop 2drop ;] E-FS-STAT TTHROWSQ
   FS:ERRNO FS:EBADF T=
   FS:CAUSE-CLEAR
   [: file FS-STATX:SUBVOLUME drop drop ;] E-FS-STAT TTHROWSQ
   FS:ERRNO FS:EBADF T= ;

public
: MAIN ( -- )
   SETUP
   CHECK-BIRTH {: file:fd :}
   file CHECK-SUBVOLUME
   file CHECK-CLOSED
   ROOT$ FS:REMOVE-TREE
   T-REPORT s" fs-statx-test: ok" type cr ;
;package

FS-STATX-TEST:MAIN
