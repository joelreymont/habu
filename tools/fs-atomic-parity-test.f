\ fs-atomic-parity-test.f - target/recovery atomic syscall contract parity.

require lib/test.f
require tools/lint/text.f

package FS-ATOMIC-PARITY

\ typed-local-lint: allow-bare-local - source paths and needles preserve ptr u8.
: HAS ( ptr u8 n ptr u8 n -- ) {: path pathu:n needle needleu:n :}
   path pathu LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT needle needleu LINT-CONTAINS? TTRUE ;

\ typed-local-lint: allow-bare-local - source paths and needles preserve ptr u8.
: LACKS ( ptr u8 n ptr u8 n -- ) {: path pathu:n needle needleu:n :}
   path pathu LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT needle needleu LINT-CONTAINS? TFALSE ;

: LINUX-CONSTANTS ( -- )
   s" src/os/linux/sys.f" s" 34  constant NR-MKDIRAT" HAS
   s" src/os/linux/sys.f" s" 175 constant NR-GETEUID" HAS
   s" src/os/linux/sys.f" s" 278 constant NR-GETRANDOM" HAS
   s" src/os/linux/sys.f" s" $100 constant AT-SYMLINK-NOFOLLOW" HAS
   s" src/os/linux/sys.f" s" 7 $8000 MOVZ" HAS
   s" src/os/linux/sys.f" s" 7 $4000 MOVZ" HAS ;

: MACOS-CONSTANTS ( -- )
   s" src/os/macos/sys.f" s" $1DB constant NR-MKDIRAT" HAS
   s" src/os/macos/sys.f" s" $19  constant NR-GETEUID" HAS
   s" src/os/macos/sys.f" s" $1F4 constant NR-GETENTROPY" HAS
   s" src/os/macos/sys.f" s" $20 constant AT-SYMLINK-NOFOLLOW" HAS ;

: RECOVERY-CONSTANTS ( -- )
   s" bootstrap/cg/sys.fs" s" 34  constant NR-MKDIRAT" HAS
   s" bootstrap/cg/sys.fs" s" 175 constant NR-GETEUID" HAS
   s" bootstrap/cg/sys.fs" s" 278 constant NR-GETRANDOM" HAS
   s" bootstrap/cg/sys.fs" s" 475 constant NR-MKDIRAT" HAS
   s" bootstrap/cg/sys.fs" s" 25  constant NR-GETEUID" HAS
   s" bootstrap/cg/sys.fs" s" 500 constant NR-GETENTROPY" HAS
   s" bootstrap/cg/sys.fs" s" 7 $8000 MOVZ" HAS
   s" bootstrap/cg/sys.fs" s" 7 $4000 MOVZ" HAS ;

: PRIMITIVE-PARITY ( -- )
   s" src/habu/habu1.f" s\" s" mkdirat" ['] BMKDIRAT FPRIM-L\" HAS
   s" src/habu/habu1.f" s\" s" geteuid" ['] BGETEUID FPRIM-L\" HAS
   s" src/habu/habu1.f" s\" s" entropy" ['] BENTROPY FPRIM-L\" HAS
   s" bootstrap/cg/forth.fs" s\" s" mkdirat" ['] BMKDIRAT FPRIM-L\" HAS
   s" bootstrap/cg/forth.fs" s\" s" geteuid" ['] BGETEUID FPRIM-L\" HAS
   s" bootstrap/cg/forth.fs" s\" s" entropy" ['] BENTROPY FPRIM-L\" HAS
   s" src/core/checker.f" s" PRIM: mkdirat  PE-FD" HAS
   s" src/core/checker.f" s" PE-RC PE-OUT PRIM;" HAS ;

: LIBRARY-CONTRACT ( -- )
   s" lib/fs.f" s" $100000 constant FS-O-DIRECTORY" HAS
   s" lib/fs.f" s" 448 constant FS-MODE-0700" HAS
   s" lib/fs-atomic.f" s" AT-SYMLINK-NOFOLLOW OP-FSTATAT" HAS
   s" lib/fs-atomic.f" s" LINUX-AT-NOFOLLOW" LACKS
   s" lib/fs-atomic.f" s" MACOS-AT-NOFOLLOW" LACKS ;

: MAIN ( -- )
   T-RESET
   LINUX-CONSTANTS
   MACOS-CONSTANTS
   RECOVERY-CONSTANTS
   PRIMITIVE-PARITY
   LIBRARY-CONTRACT
   T-REPORT
   s" fs-atomic-parity-test: ok" type cr ;

MAIN

;package
