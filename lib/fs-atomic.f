\ fs-atomic.f - directory-bound durable atomic replacement.

require lib/errors.f
require lib/string.f
require lib/fs.f

package FS-ATOMIC
public

ENUM result 0
   VARIANT committed ;VARIANT
   VARIANT committed-unsynced
      FIELD sync n
      FIELD stage-close n
      FIELD parent-close n
   ;VARIANT
   VARIANT committed-close-failed
      FIELD stage-close n
      FIELD parent-close n
   ;VARIANT
   VARIANT aborted
      FIELD cause n
      FIELD temp-close n
      FIELD cleanup n
      FIELD stage-close n
      FIELD parent-close n
   ;VARIANT
;ENUM

private

$2F constant SLASH
$F constant NIBBLE-MASK
4 constant NIBBLE-BITS
16 constant RANDOM-BYTES
64 constant ATOMIC-RETRIES
1 constant ONE-LINK
$FFF constant MODE-PERM-MASK
$4841425541544F4D constant CONTEXT-MAGIC
12 constant STAGE-U

0 constant CTX-MAGIC
1 constant CTX-PARENT-FD
2 constant CTX-STAGE-FD
3 constant CTX-TEMP-FD
4 constant CTX-TEMP-DEV
5 constant CTX-TEMP-INO
6 constant CTX-TEMP-KNOWN
7 constant CTX-TEMP-LIVE
8 constant CTX-PUBLISHED
9 constant CTX-PARENT-SYNCED
10 constant CTX-SYNC-ERR
11 constant CTX-CLEANUP-ERR
12 constant CTX-TEMP-CLOSE-ERR
13 constant CTX-STAGE-CLOSE-ERR
14 constant CTX-PARENT-CLOSE-ERR
15 constant CTX-WRITE-OFF
16 constant CTX-ENTROPY-OFF
17 constant CTX-SOURCE-U
18 constant CTX-TARGET-U
19 constant CTX-TEMP-U
20 constant CTX-SOURCE-A
21 constant CTX-WRITE-XT
22 constant CTX-SYNC-XT
23 constant CTX-CLOSE-XT
24 constant CTX-FSTAT-XT
25 constant CTX-OPEN-XT
26 constant CTX-OPENAT-XT
27 constant CTX-FSTATAT-XT
28 constant CTX-RENAMEAT-XT
29 constant CTX-UNLINKAT-XT
30 constant CTX-MKDIRAT-XT
31 constant CTX-GETEUID-XT
32 constant CTX-ENTROPY-XT

40 constant CTX-FD-STAT
FS-STAT-CAP CELL / constant STAT-CELLS
FS-PATHZ-CAP CELL 1 - + CELL / constant PATHZ-CELLS
RANDOM-BYTES CELL 1 - + CELL / constant RANDOM-CELLS
CTX-FD-STAT STAT-CELLS + constant CTX-PATH-STAT
CTX-PATH-STAT STAT-CELLS + constant CTX-PARENT-Z
CTX-PARENT-Z PATHZ-CELLS + constant CTX-TARGET-Z
CTX-TARGET-Z PATHZ-CELLS + constant CTX-TEMP-Z
CTX-TEMP-Z PATHZ-CELLS + constant CTX-RANDOM
CTX-RANDOM RANDOM-CELLS + constant CTX-CELLS

create HEX-TABLE 16 allot
s" 0123456789abcdef" HEX-TABLE 16 BYTE-COPY
create STAGE-Z STAGE-U 1 + allot
s" .habu-atomic" STAGE-Z STAGE-U BYTE-COPY
0 STAGE-Z STAGE-U + c!

: FALSE ( -- bool )
   0 0= 0= ;

: TRUE ( -- bool )
   0 0= ;

TRUSTED: N-FIELD ( ptr a n -- ptr n )
   ptr-field ;

: N@ ( ptr a n -- n )
   N-FIELD @ ;

: N! ( n ptr a n -- )
   N-FIELD ! ;

: SOURCE-FIELD ( ptr a -- ptr ptr u8 )
   CTX-SOURCE-A ptr-field ;

: SOURCE@ ( ptr a -- ptr u8 )
   SOURCE-FIELD @ ;

: SOURCE! ( ptr u8 ptr a -- )
   SOURCE-FIELD ! ;

TRUSTED: SPAN ( ptr a n -- ptr u8 )
   cells + ;

: FD-STAT ( ptr a -- ptr u8 )
   CTX-FD-STAT SPAN ;

: PATH-STAT ( ptr a -- ptr u8 )
   CTX-PATH-STAT SPAN ;

: PARENT-Z ( ptr a -- ptr u8 )
   CTX-PARENT-Z SPAN ;

: TARGET-Z ( ptr a -- ptr u8 )
   CTX-TARGET-Z SPAN ;

: TEMP-Z ( ptr a -- ptr u8 )
   CTX-TEMP-Z SPAN ;

: RANDOM-BUF ( ptr a -- ptr u8 )
   CTX-RANDOM SPAN ;

: PARENT-FD@ ( ptr a -- fd )
   CTX-PARENT-FD N@ >FD ;

: PARENT-FD! ( fd ptr a -- )
   swap FD>N swap CTX-PARENT-FD N! ;

: PARENT-FD-CLEAR ( ptr a -- )
   -1 swap CTX-PARENT-FD N! ;

: STAGE-FD@ ( ptr a -- fd )
   CTX-STAGE-FD N@ >FD ;

: STAGE-FD! ( fd ptr a -- )
   swap FD>N swap CTX-STAGE-FD N! ;

: STAGE-FD-CLEAR ( ptr a -- )
   -1 swap CTX-STAGE-FD N! ;

: TEMP-FD@ ( ptr a -- fd )
   CTX-TEMP-FD N@ >FD ;

: TEMP-FD! ( fd ptr a -- )
   swap FD>N swap CTX-TEMP-FD N! ;

: TEMP-FD-CLEAR ( ptr a -- )
   -1 swap CTX-TEMP-FD N! ;

: PARENT-FD-LIVE? ( ptr a -- bool )
   PARENT-FD@ FD>N 0 >= ;

: STAGE-FD-LIVE? ( ptr a -- bool )
   STAGE-FD@ FD>N 0 >= ;

: TEMP-FD-LIVE? ( ptr a -- bool )
   TEMP-FD@ FD>N 0 >= ;

TRUSTED: CALL-WRITE ( ptr a fd ptr u8 n n -- n ) execute ;
TRUSTED: CALL-SYNC ( ptr a fd n -- rc ) execute ;
TRUSTED: CALL-CLOSE ( ptr a fd n -- rc ) execute ;
TRUSTED: CALL-FSTAT ( ptr a fd ptr u8 n -- rc ) execute ;
TRUSTED: CALL-OPEN ( ptr a ptr u8 n n n -- n ) execute ;
TRUSTED: CALL-OPENAT ( ptr a fd ptr u8 n n n -- n ) execute ;
TRUSTED: CALL-FSTATAT ( ptr a fd ptr u8 ptr u8 n n -- rc ) execute ;
TRUSTED: CALL-RENAMEAT ( ptr a fd ptr u8 fd ptr u8 n -- rc ) execute ;
TRUSTED: CALL-UNLINKAT ( ptr a fd ptr u8 n n -- rc ) execute ;
TRUSTED: CALL-MKDIRAT ( ptr a fd ptr u8 n n -- rc ) execute ;
TRUSTED: CALL-GETEUID ( ptr a n -- n ) execute ;
TRUSTED: CALL-ENTROPY ( ptr a ptr u8 n n -- n ) execute ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 through dispatch.
: OP-WRITE ( ptr a fd ptr u8 n -- n ) {: ctx:ptr fd:fd a u:n :}
   ctx fd a u ctx CTX-WRITE-XT N@ CALL-WRITE ;

: OP-SYNC ( ptr a fd -- rc ) {: ctx:ptr fd:fd :}
   ctx fd ctx CTX-SYNC-XT N@ CALL-SYNC ;

: OP-CLOSE ( ptr a fd -- rc ) {: ctx:ptr fd:fd :}
   ctx fd ctx CTX-CLOSE-XT N@ CALL-CLOSE ;

\ typed-local-lint: allow-bare-local - buf preserves ptr u8 through dispatch.
: OP-FSTAT ( ptr a fd ptr u8 -- rc ) {: ctx:ptr fd:fd buf :}
   ctx fd buf ctx CTX-FSTAT-XT N@ CALL-FSTAT ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through dispatch.
: OP-OPEN ( ptr a ptr u8 n n -- n ) {: ctx:ptr path flags:n mode:n :}
   ctx path flags mode ctx CTX-OPEN-XT N@ CALL-OPEN ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through dispatch.
: OP-OPENAT ( ptr a fd ptr u8 n n -- n ) {: ctx:ptr fd:fd path flags:n mode:n :}
   ctx fd path flags mode ctx CTX-OPENAT-XT N@ CALL-OPENAT ;

\ typed-local-lint: allow-bare-local - path and buf preserve ptr u8 through dispatch.
: OP-FSTATAT ( ptr a fd ptr u8 ptr u8 n -- rc )
   {: ctx:ptr fd:fd path buf flags:n :}
   ctx fd path buf flags ctx CTX-FSTATAT-XT N@ CALL-FSTATAT ;

\ typed-local-lint: allow-bare-local - paths preserve ptr u8 through dispatch.
: OP-RENAMEAT ( ptr a fd ptr u8 fd ptr u8 -- rc )
   {: ctx:ptr oldfd:fd oldpath newfd:fd newpath :}
   ctx oldfd oldpath newfd newpath ctx CTX-RENAMEAT-XT N@ CALL-RENAMEAT ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through dispatch.
: OP-UNLINKAT ( ptr a fd ptr u8 n -- rc ) {: ctx:ptr fd:fd path flags:n :}
   ctx fd path flags ctx CTX-UNLINKAT-XT N@ CALL-UNLINKAT ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through dispatch.
: OP-MKDIRAT ( ptr a fd ptr u8 n -- rc ) {: ctx:ptr fd:fd path mode:n :}
   ctx fd path mode ctx CTX-MKDIRAT-XT N@ CALL-MKDIRAT ;

: OP-GETEUID ( ptr a -- n ) {: ctx:ptr :}
   ctx ctx CTX-GETEUID-XT N@ CALL-GETEUID ;

\ typed-local-lint: allow-bare-local - buf preserves ptr u8 through dispatch.
: OP-ENTROPY ( ptr a ptr u8 n -- n ) {: ctx:ptr buf u:n :}
   ctx buf u ctx CTX-ENTROPY-XT N@ CALL-ENTROPY ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 through the syscall.
: SYSTEM-WRITE ( ptr a fd ptr u8 n -- n ) {: ctx:ptr fd:fd a u:n :}
   ctx drop fd a u write-fd ;

: SYSTEM-SYNC ( ptr a fd -- rc ) {: ctx:ptr fd:fd :}
   ctx drop fd fsync ;

: SYSTEM-CLOSE ( ptr a fd -- rc ) {: ctx:ptr fd:fd :}
   ctx drop fd close-rc ;

\ typed-local-lint: allow-bare-local - buf preserves ptr u8 through the syscall.
: SYSTEM-FSTAT ( ptr a fd ptr u8 -- rc ) {: ctx:ptr fd:fd buf :}
   ctx drop fd buf fstat64 ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through the syscall.
: SYSTEM-OPEN ( ptr a ptr u8 n n -- n ) {: ctx:ptr path flags:n mode:n :}
   ctx drop path flags mode open ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through the syscall.
: SYSTEM-OPENAT ( ptr a fd ptr u8 n n -- n ) {: ctx:ptr fd:fd path flags:n mode:n :}
   ctx drop fd path flags mode openat ;

\ typed-local-lint: allow-bare-local - path and buf preserve ptr u8 through the syscall.
: SYSTEM-FSTATAT ( ptr a fd ptr u8 ptr u8 n -- rc )
   {: ctx:ptr fd:fd path buf flags:n :}
   ctx drop fd path buf flags fstatat64 ;

\ typed-local-lint: allow-bare-local - paths preserve ptr u8 through the syscall.
: SYSTEM-RENAMEAT ( ptr a fd ptr u8 fd ptr u8 -- rc )
   {: ctx:ptr oldfd:fd oldpath newfd:fd newpath :}
   ctx drop oldfd oldpath newfd newpath renameat ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through the syscall.
: SYSTEM-UNLINKAT ( ptr a fd ptr u8 n -- rc ) {: ctx:ptr fd:fd path flags:n :}
   ctx drop fd path flags unlinkat ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through the syscall.
: SYSTEM-MKDIRAT ( ptr a fd ptr u8 n -- rc ) {: ctx:ptr fd:fd path mode:n :}
   ctx drop fd path mode mkdirat ;

: SYSTEM-GETEUID ( ptr a -- n )
   drop geteuid ;

\ typed-local-lint: allow-bare-local - buf preserves ptr u8 through the syscall.
: SYSTEM-ENTROPY ( ptr a ptr u8 n -- n ) {: ctx:ptr buf u:n :}
   ctx drop buf u entropy ;

: RESET-RUN ( ptr a -- ) {: ctx:ptr :}
   ctx PARENT-FD-CLEAR
   ctx STAGE-FD-CLEAR
   ctx TEMP-FD-CLEAR
   0 ctx CTX-TEMP-DEV N!
   0 ctx CTX-TEMP-INO N!
   0 ctx CTX-TEMP-KNOWN N!
   0 ctx CTX-TEMP-LIVE N!
   0 ctx CTX-PUBLISHED N!
   0 ctx CTX-PARENT-SYNCED N!
   0 ctx CTX-SYNC-ERR N!
   0 ctx CTX-CLEANUP-ERR N!
   0 ctx CTX-TEMP-CLOSE-ERR N!
   0 ctx CTX-STAGE-CLOSE-ERR N!
   0 ctx CTX-PARENT-CLOSE-ERR N!
   0 ctx CTX-WRITE-OFF N!
   0 ctx CTX-ENTROPY-OFF N!
   0 ctx CTX-TARGET-U N!
   0 ctx CTX-TEMP-U N! ;

: CONTEXT-VALID? ( ptr a -- bool )
   CTX-MAGIC N@ CONTEXT-MAGIC = ;

: INSTALL-SYSTEM ( ptr a -- ) {: ctx:ptr :}
   ['] SYSTEM-WRITE ctx CTX-WRITE-XT N!
   ['] SYSTEM-SYNC ctx CTX-SYNC-XT N!
   ['] SYSTEM-CLOSE ctx CTX-CLOSE-XT N!
   ['] SYSTEM-FSTAT ctx CTX-FSTAT-XT N!
   ['] SYSTEM-OPEN ctx CTX-OPEN-XT N!
   ['] SYSTEM-OPENAT ctx CTX-OPENAT-XT N!
   ['] SYSTEM-FSTATAT ctx CTX-FSTATAT-XT N!
   ['] SYSTEM-RENAMEAT ctx CTX-RENAMEAT-XT N!
   ['] SYSTEM-UNLINKAT ctx CTX-UNLINKAT-XT N!
   ['] SYSTEM-MKDIRAT ctx CTX-MKDIRAT-XT N!
   ['] SYSTEM-GETEUID ctx CTX-GETEUID-XT N!
   ['] SYSTEM-ENTROPY ctx CTX-ENTROPY-XT N! ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 while scanning.
: LAST-SLASH ( ptr u8 n -- n ) {: a u:n :}
   u begin dup 0 > while
      1- dup a + c@ SLASH = if exit then
   repeat
   drop -1 ;

\ typed-local-lint: allow-bare-local - a and dst preserve ptr u8.
: COPY-Z ( ptr u8 n ptr u8 -- ) {: a u:n dst :}
   a u dst FS-PATHZ-INTO drop ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 across owned copies.
: PREPARE-PATH ( ptr a ptr u8 n -- ) {: ctx:ptr a u:n :}
   u 0 <= if E-FS-PATH throw then
   a u FS-CHECK-PATH-BYTES
   a u 1- + c@ SLASH = if E-FS-PATH throw then
   a u LAST-SLASH {: slash:n :}
   slash 0 < if
      s" ." ctx PARENT-Z COPY-Z
      a u ctx TARGET-Z COPY-Z
      u ctx CTX-TARGET-U N!
      exit
   then
   slash 0= if
      s" /" ctx PARENT-Z COPY-Z
   else
      a slash ctx PARENT-Z COPY-Z
   then
   slash 1+ {: start:n :}
   u start - {: baseu:n :}
   baseu 0 <= if E-FS-PATH throw then
   a start + baseu ctx TARGET-Z COPY-Z
   baseu ctx CTX-TARGET-U N! ;

: HEX@ ( n -- u8 )
   HEX-TABLE + c@ ;

: TEMP-ROOM? ( ptr a n -- bool ) {: ctx:ptr add:n :}
   ctx CTX-TEMP-U N@ add + FS-PATH-CAP <= ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 through the copy.
: TEMP-APPEND ( ptr a ptr u8 n -- ) {: ctx:ptr a u:n :}
   ctx u TEMP-ROOM? 0= if E-FS-CAPACITY throw then
   a ctx TEMP-Z ctx CTX-TEMP-U N@ + u BYTE-COPY
   ctx CTX-TEMP-U N@ u + ctx CTX-TEMP-U N! ;

: TEMP-C! ( ptr a u8 -- ) {: ctx:ptr c:u8 :}
   ctx 1 TEMP-ROOM? 0= if E-FS-CAPACITY throw then
   c ctx TEMP-Z ctx CTX-TEMP-U N@ + c!
   ctx CTX-TEMP-U N@ 1+ ctx CTX-TEMP-U N! ;

: TEMP-BYTE ( ptr a u8 -- ) {: ctx:ptr b:u8 :}
   b NIBBLE-BITS rshift NIBBLE-MASK and HEX@ ctx swap TEMP-C!
   b NIBBLE-MASK and HEX@ ctx swap TEMP-C! ;

: BUILD-TEMP ( ptr a -- ) {: ctx:ptr :}
   0 ctx CTX-TEMP-U N!
   ctx s" .habu-" TEMP-APPEND
   0 begin dup RANDOM-BYTES < while
      ctx RANDOM-BUF over + c@ ctx swap TEMP-BYTE
      1+
   repeat drop
   0 ctx TEMP-Z ctx CTX-TEMP-U N@ + c! ;

: STAT-DEV@ ( ptr u8 -- n )
   HB-TARGET-LINUX? if FS-U64@ else FS-U32@ then ;

: STAT-INO@ ( ptr u8 -- n )
   8 + FS-U64@ ;

: STAT-MODE@ ( ptr u8 -- n )
   4 + FS-U16@ ;

: STAT-NLINK@ ( ptr u8 -- n )
   HB-TARGET-LINUX? if 20 + FS-U32@ else 6 + FS-U16@ then ;

: STAT-UID@ ( ptr u8 -- n )
   16 + FS-U32@ ;

: STAT-REGULAR? ( ptr u8 -- bool )
   STAT-MODE@ S-IFMT and S-IFREG = ;

: STAT-DIRECTORY? ( ptr u8 -- bool )
   STAT-MODE@ S-IFMT and S-IFDIR = ;

\ typed-local-lint: allow-bare-local - stat preserves ptr u8 through readers.
: STAGE-VALID? ( ptr a ptr u8 -- bool ) {: ctx:ptr stat :}
   stat STAT-DIRECTORY? 0= if FALSE exit then
   stat STAT-UID@ ctx OP-GETEUID <> if FALSE exit then
   stat STAT-MODE@ MODE-PERM-MASK and FS-MODE-0700 = ;

\ typed-local-lint: allow-bare-local - stat preserves ptr u8 through readers.
: IDENTITY? ( ptr a ptr u8 -- bool ) {: ctx:ptr stat :}
   stat STAT-REGULAR? 0= if FALSE exit then
   stat STAT-NLINK@ ONE-LINK <> if FALSE exit then
   stat STAT-DEV@ ctx CTX-TEMP-DEV N@ =
   stat STAT-INO@ ctx CTX-TEMP-INO N@ = and ;

: RAW>FD ( n -- fd )
   dup 0 < if drop E-FS-OPEN throw then
   >FD ;

: OPEN-PARENT ( ptr a -- ) {: ctx:ptr :}
   ctx ctx PARENT-Z FS-O-DIRECTORY FS-O-NOFOLLOW or 0 OP-OPEN RAW>FD
   ctx PARENT-FD!
   ctx ctx PARENT-FD@ ctx FD-STAT OP-FSTAT RC>N 0 <> if E-FS-STAT throw then
   ctx FD-STAT STAT-DIRECTORY? 0= if E-FS-STAT throw then ;

: OPEN-STAGE ( ptr a -- ) {: ctx:ptr :}
   ctx ctx PARENT-FD@ STAGE-Z
   FS-O-DIRECTORY FS-O-NOFOLLOW or 0 OP-OPENAT RAW>FD ctx STAGE-FD!
   ctx ctx STAGE-FD@ ctx PATH-STAT OP-FSTAT RC>N 0 <> if E-FS-STAT throw then
   ctx ctx PATH-STAT STAGE-VALID? 0= if E-FS-STAT throw then ;

: ENSURE-STAGE ( ptr a -- ) {: ctx:ptr :}
   ctx ctx PARENT-FD@ STAGE-Z FS-MODE-0700 OP-MKDIRAT drop
   ctx OPEN-STAGE ;

: FILL-RANDOM ( ptr a -- ) {: ctx:ptr :}
   0 ctx CTX-ENTROPY-OFF N!
   begin ctx CTX-ENTROPY-OFF N@ RANDOM-BYTES < while
      RANDOM-BYTES ctx CTX-ENTROPY-OFF N@ - {: left:n :}
      ctx ctx RANDOM-BUF ctx CTX-ENTROPY-OFF N@ + left OP-ENTROPY {: got:n :}
      got 0 <= if E-FS-IO throw then
      got left > if E-FS-IO throw then
      ctx CTX-ENTROPY-OFF N@ got + ctx CTX-ENTROPY-OFF N!
   repeat ;

: TEMP-OPENED? ( ptr a -- bool ) {: ctx:ptr :}
   ctx FILL-RANDOM
   ctx BUILD-TEMP
   ctx ctx STAGE-FD@ ctx TEMP-Z
   FS-O-WRONLY FS-O-CREAT or FS-O-EXCL or FS-O-NOFOLLOW or
   FS-MODE-0644 OP-OPENAT {: raw:n :}
   raw 0 < if FALSE exit then
   raw RAW>FD ctx TEMP-FD!
   TRUE ctx CTX-TEMP-LIVE N!
   TRUE ;

: CAPTURE-TEMP-ID ( ptr a -- ) {: ctx:ptr :}
   ctx ctx TEMP-FD@ ctx FD-STAT OP-FSTAT RC>N 0 <> if E-FS-STAT throw then
   ctx FD-STAT STAT-REGULAR? 0= if E-FS-STAT throw then
   ctx FD-STAT STAT-NLINK@ ONE-LINK <> if E-FS-STAT throw then
   ctx FD-STAT STAT-DEV@ ctx CTX-TEMP-DEV N!
   ctx FD-STAT STAT-INO@ ctx CTX-TEMP-INO N!
   TRUE ctx CTX-TEMP-KNOWN N! ;

: OPEN-UNIQUE ( ptr a -- ) {: ctx:ptr :}
   0 begin dup ATOMIC-RETRIES < while
      ctx TEMP-OPENED? if drop ctx CAPTURE-TEMP-ID exit then
      1+
   repeat
   drop E-FS-OPEN throw ;

: WRITE-BYTES ( ptr a -- ) {: ctx:ptr :}
   0 ctx CTX-WRITE-OFF N!
   begin ctx CTX-WRITE-OFF N@ ctx CTX-SOURCE-U N@ < while
      ctx CTX-SOURCE-U N@ ctx CTX-WRITE-OFF N@ - {: left:n :}
      ctx ctx TEMP-FD@ ctx SOURCE@ ctx CTX-WRITE-OFF N@ + left OP-WRITE {: wrote:n :}
      wrote 0 <= if E-FS-IO throw then
      wrote left > if E-FS-IO throw then
      ctx CTX-WRITE-OFF N@ wrote + ctx CTX-WRITE-OFF N!
   repeat ;

: SYNC-TEMP ( ptr a -- ) {: ctx:ptr :}
   ctx ctx TEMP-FD@ OP-SYNC RC>N 0 <> if E-FS-IO throw then ;

: CLOSE-TEMP ( ptr a -- ) {: ctx:ptr :}
   ctx ctx TEMP-FD@ OP-CLOSE RC>N {: raw:n :}
   ctx TEMP-FD-CLEAR
   raw 0 <> if
      raw ctx CTX-TEMP-CLOSE-ERR N!
      E-FS-IO throw
   then ;

: TEMP-IDENTITY? ( ptr a -- bool ) {: ctx:ptr :}
   ctx CTX-TEMP-KNOWN N@ 0= if FALSE exit then
   ctx ctx STAGE-FD@ ctx TEMP-Z ctx PATH-STAT AT-SYMLINK-NOFOLLOW OP-FSTATAT
   RC>N 0 <> if FALSE exit then
   ctx ctx PATH-STAT IDENTITY? ;

: VERIFY-TEMP ( ptr a -- )
   TEMP-IDENTITY? 0= if E-FS-STAT throw then ;

: PUBLISH ( ptr a -- ) {: ctx:ptr :}
   ctx ctx STAGE-FD@ ctx TEMP-Z ctx PARENT-FD@ ctx TARGET-Z OP-RENAMEAT
   RC>N 0 <> if E-FS-IO throw then
   FALSE ctx CTX-TEMP-LIVE N!
   TRUE ctx CTX-PUBLISHED N! ;

: SYNC-PARENT ( ptr a -- ) {: ctx:ptr :}
   ctx ctx PARENT-FD@ OP-SYNC RC>N {: raw:n :}
   raw 0 <> if
      raw ctx CTX-SYNC-ERR N!
      E-FS-IO throw
   then
   TRUE ctx CTX-PARENT-SYNCED N! ;

: CLOSE-STAGE ( ptr a -- ) {: ctx:ptr :}
   ctx ctx STAGE-FD@ OP-CLOSE RC>N {: raw:n :}
   ctx STAGE-FD-CLEAR
   raw 0 <> if
      raw ctx CTX-STAGE-CLOSE-ERR N!
      E-FS-IO throw
   then ;

: CLOSE-PARENT ( ptr a -- ) {: ctx:ptr :}
   ctx ctx PARENT-FD@ OP-CLOSE RC>N {: raw:n :}
   ctx PARENT-FD-CLEAR
   raw 0 <> if
      raw ctx CTX-PARENT-CLOSE-ERR N!
      E-FS-IO throw
   then ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through validation.
: VALIDATE ( ptr a ptr u8 n -- ) {: ctx:ptr path pathu:n :}
   ctx CTX-SOURCE-U N@ 0 < if E-FS-CAPACITY throw then
   ctx path pathu PREPARE-PATH ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 through the transaction.
: RUN ( ptr a ptr u8 n -- ) {: ctx:ptr path pathu:n :}
   ctx path pathu VALIDATE
   ctx OPEN-PARENT
   ctx ENSURE-STAGE
   ctx OPEN-UNIQUE
   ctx WRITE-BYTES
   ctx SYNC-TEMP
   ctx CLOSE-TEMP
   ctx VERIFY-TEMP
   ctx PUBLISH
   ctx SYNC-PARENT
   ctx CLOSE-STAGE
   ctx CLOSE-PARENT ;

\ typed-local-lint: allow-bare-local - path preserves ptr u8 across catch.
: RUN-KEEP ( ptr a ptr u8 n -- ptr a ptr u8 n ) {: ctx:ptr path pathu:n :}
   ctx path pathu RUN
   ctx path pathu ;

: REMEMBER-CLEANUP ( ptr a n -- ) {: ctx:ptr code:n :}
   ctx CTX-CLEANUP-ERR N@ 0= if code ctx CTX-CLEANUP-ERR N! then ;

: CLEANUP-CLOSE-TEMP ( ptr a -- ) {: ctx:ptr :}
   ctx TEMP-FD-LIVE? 0= if exit then
   ctx ctx TEMP-FD@ OP-CLOSE RC>N {: raw:n :}
   ctx TEMP-FD-CLEAR
   raw 0 <> if raw ctx CTX-TEMP-CLOSE-ERR N! then ;

: CLEANUP-TEMP ( ptr a -- ) {: ctx:ptr :}
   ctx CTX-TEMP-LIVE N@ 0= if exit then
   ctx TEMP-IDENTITY? 0= if ctx E-FS-STAT REMEMBER-CLEANUP exit then
   ctx ctx STAGE-FD@ ctx TEMP-Z 0 OP-UNLINKAT RC>N {: raw:n :}
   raw 0 <> if ctx raw REMEMBER-CLEANUP exit then
   FALSE ctx CTX-TEMP-LIVE N! ;

: CLEANUP-STAGE ( ptr a -- ) {: ctx:ptr :}
   ctx STAGE-FD-LIVE? 0= if exit then
   ctx ctx STAGE-FD@ OP-CLOSE RC>N {: raw:n :}
   ctx STAGE-FD-CLEAR
   raw 0 <> if raw ctx CTX-STAGE-CLOSE-ERR N! then ;

: CLEANUP-PARENT ( ptr a -- ) {: ctx:ptr :}
   ctx PARENT-FD-LIVE? 0= if exit then
   ctx ctx PARENT-FD@ OP-CLOSE RC>N {: raw:n :}
   ctx PARENT-FD-CLEAR
   raw 0 <> if raw ctx CTX-PARENT-CLOSE-ERR N! then ;

: CLEANUP ( ptr a -- )
   dup CLEANUP-CLOSE-TEMP
   dup CLEANUP-TEMP
   dup CLEANUP-STAGE
   CLEANUP-PARENT ;

: BAD-CONTEXT-RESULT ( -- result )
   E-FS-IO 0 0 0 0 construct result aborted ;

: ABORTED-RESULT ( ptr a n -- result ) {: ctx:ptr primary:n :}
   primary 0= if E-FS-IO else primary then
   ctx CTX-TEMP-CLOSE-ERR N@
   ctx CTX-CLEANUP-ERR N@
   ctx CTX-STAGE-CLOSE-ERR N@
   ctx CTX-PARENT-CLOSE-ERR N@
   construct result aborted ;

: COMMITTED-RESULT ( ptr a -- result ) {: ctx:ptr :}
   ctx CTX-PARENT-SYNCED N@ 0= if
      ctx CTX-SYNC-ERR N@ dup 0= if drop -1 then
      ctx CTX-STAGE-CLOSE-ERR N@
      ctx CTX-PARENT-CLOSE-ERR N@
      construct result committed-unsynced
      exit
   then
   ctx CTX-STAGE-CLOSE-ERR N@ ctx CTX-PARENT-CLOSE-ERR N@ 2dup or 0 <> if
      construct result committed-close-failed
      exit
   then
   2drop construct result committed ;

: RESULT-OF ( ptr a n -- result ) {: ctx:ptr primary:n :}
   ctx CTX-PUBLISHED N@ 0= if ctx primary ABORTED-RESULT exit then
   ctx COMMITTED-RESULT ;

\ typed-local-lint: allow-bare-local - path and src preserve ptr u8 through catch.
: TRY-CORE ( ptr a ptr u8 n ptr u8 n -- result )
   {: ctx:ptr path pathu:n src srcu:n :}
   ctx CONTEXT-VALID? 0= if BAD-CONTEXT-RESULT exit then
   ctx RESET-RUN
   src ctx SOURCE!
   srcu ctx CTX-SOURCE-U N!
   ctx path pathu [: RUN-KEEP ;] catch
   {: kept:ptr kept-path kept-u:n primary:n :}
   kept-path drop
   kept-u drop
   kept CLEANUP
   kept primary RESULT-OF ;

public

CTX-CELLS constant CONTEXT-CELLS

: CONTEXT-INIT ( ptr a -- ) {: ctx:ptr :}
   CONTEXT-MAGIC ctx CTX-MAGIC N!
   ctx INSTALL-SYSTEM
   ctx RESET-RUN ;

: TRY-WRITE-FILE ( ptr a ptr u8 n ptr u8 n -- result )
   TRY-CORE ;

;package
