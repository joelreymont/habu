\ fs-atomic-test.f - same-directory atomic replacement fixtures.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/task.f
require lib/fs-atomic.f

package FS-ATOMIC-EXTERNAL
public

: COMMITTED? ( FS-ATOMIC:result -- bool )
   MATCH FS-ATOMIC:result
      committed OF 0 0= ENDOF
      committed-degraded OF 2drop 2drop 0 0= 0= ENDOF
      aborted OF 2drop 2drop 2drop 0 0= 0= ENDOF
   ;MATCH ;

private
;package

package FS
private

variable TEST-SOURCE-FD

: TEST-TRACK-OPENAT ( fd ptr u8 n n -- n ) {: dir:fd path:ptr flags:n mode:n :}
   dir path flags mode openat dup 0 >= if
      flags O-DIRECTORY and 0= if dup TEST-SOURCE-FD ! then
   then ;

: TEST-SOURCE-CLOSE ( fd -- rc ) {: fd:fd :}
   fd FD>N TEST-SOURCE-FD @ = if
      fd close-rc RC>N dup 0 <> if throw then drop
      -45 >RC exit
   then
   fd close-rc ;

: TEST-SOURCE-READ ( fd ptr u8 n -- n )
   2drop drop -52 ;

public

: TEST-SYSTEM ( -- )
   INSTALL-SYSTEM
   ['] TEST-TRACK-OPENAT is OPENAT-OP
   -1 TEST-SOURCE-FD ! ;

: TEST-FAIL-CLOSE ( -- )
   ['] TEST-SOURCE-CLOSE is CLOSE-OP ;

: TEST-FAIL-READ ( -- )
   ['] TEST-SOURCE-READ is READ-OP ;

;package

package FS-ATOMIC
private

256 constant READ-CAP
24 constant WRITES
0 constant K-COMMITTED
1 constant K-DEGRADED
2 constant K-ABORTED

create ROOT-BUF FS-PATH-CAP allot
create TARGET-BUF FS-PATH-CAP allot
create AUTH-BUF FS-PATH-CAP allot
create SOURCE-BUF FS-PATH-CAP allot
create MISSING-BUF FS-PATH-CAP allot
create CANDIDATE-BUF FS-PATH-CAP allot
create PARENT-LINK-BUF FS-PATH-CAP allot
create LINK-TARGET-BUF FS-PATH-CAP allot
create REAL-PARENT-BUF FS-PATH-CAP allot
create REAL-SUB-BUF FS-PATH-CAP allot
create WALK-TARGET-BUF FS-PATH-CAP allot
create MID-LINK-BUF FS-PATH-CAP allot
create DEEP-TARGET-BUF FS-PATH-CAP allot
create UNSAFE-PARENT-BUF FS-PATH-CAP allot
create UNSAFE-TARGET-BUF FS-PATH-CAP allot
create AUTH-Z FS-PATHZ-CAP allot
create TARGET-Z FS-PATHZ-CAP allot
create READ-BUF READ-CAP allot
create NUL-PATH $61 c, 0 c, $62 c,
create CTX CONTEXT-BYTES allot
create CTX-A CONTEXT-BYTES allot
create CTX-B CONTEXT-BYTES allot

variable ROOT-U
variable TARGET-U
variable AUTH-U
variable SOURCE-PATH-U
variable MISSING-U
variable CANDIDATE-U
variable PARENT-LINK-U
variable LINK-TARGET-U
variable REAL-PARENT-U
variable REAL-SUB-U
variable WALK-TARGET-U
variable MID-LINK-U
variable DEEP-TARGET-U
variable UNSAFE-PARENT-U
variable UNSAFE-TARGET-U
variable KIND
variable AT-N
variable CODE
variable SOURCE-CLOSE-N
variable TEMP-CLOSE-N
variable CLEANUP-N
variable PARENT-CLOSE-N
variable ENTROPY-N
variable WRITE-N
variable DONE
variable READY
variable BAD
variable OBS
variable SWAPPED
variable SWAP-HOLD
variable WALK-CLOSE-FAILED

TASK:MIN-STACK TASK:TASK WRITER-A
TASK:MIN-STACK TASK:TASK WRITER-B
TASK:MIN-STACK TASK:TASK READER

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: TARGET$ ( -- ptr u8 n )
   TARGET-BUF TARGET-U @ ;

: AUTH$ ( -- ptr u8 n )
   AUTH-BUF AUTH-U @ ;

: SOURCE$ ( -- ptr u8 n )
   SOURCE-BUF SOURCE-PATH-U @ ;

: MISSING$ ( -- ptr u8 n )
   MISSING-BUF MISSING-U @ ;

: CANDIDATE$ ( -- ptr u8 n )
   CANDIDATE-BUF CANDIDATE-U @ ;

: PARENT-LINK$ ( -- ptr u8 n )
   PARENT-LINK-BUF PARENT-LINK-U @ ;

: LINK-TARGET$ ( -- ptr u8 n )
   LINK-TARGET-BUF LINK-TARGET-U @ ;

: REAL-PARENT$ ( -- ptr u8 n )
   REAL-PARENT-BUF REAL-PARENT-U @ ;

: REAL-SUB$ ( -- ptr u8 n )
   REAL-SUB-BUF REAL-SUB-U @ ;

: WALK-TARGET$ ( -- ptr u8 n )
   WALK-TARGET-BUF WALK-TARGET-U @ ;

: MID-LINK$ ( -- ptr u8 n )
   MID-LINK-BUF MID-LINK-U @ ;

: DEEP-TARGET$ ( -- ptr u8 n )
   DEEP-TARGET-BUF DEEP-TARGET-U @ ;

: UNSAFE-PARENT$ ( -- ptr u8 n )
   UNSAFE-PARENT-BUF UNSAFE-PARENT-U @ ;

: UNSAFE-TARGET$ ( -- ptr u8 n )
   UNSAFE-TARGET-BUF UNSAFE-TARGET-U @ ;

: PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: JOIN! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr up:ptr :}
   pa pu na nu dst JOIN-PATH up ! ;

: PATHS! ( -- )
   s" /tmp" s" hb-fs-atomic" MAKE-TEMP-DIR ROOT-BUF ROOT-U PATH!
   ROOT$ s" target.bin" TARGET-BUF TARGET-U JOIN!
   ROOT$ s" authority.bin" AUTH-BUF AUTH-U JOIN!
   ROOT$ s" source.bin" SOURCE-BUF SOURCE-PATH-U JOIN!
   ROOT$ s" missing.bin" MISSING-BUF MISSING-U JOIN!
   ROOT$ s" parent-link" PARENT-LINK-BUF PARENT-LINK-U JOIN!
   PARENT-LINK$ s" target.bin" LINK-TARGET-BUF LINK-TARGET-U JOIN!
   ROOT$ s" real-parent" REAL-PARENT-BUF REAL-PARENT-U JOIN!
   REAL-PARENT$ s" sub" REAL-SUB-BUF REAL-SUB-U JOIN!
   REAL-SUB$ s" target.bin" WALK-TARGET-BUF WALK-TARGET-U JOIN!
   ROOT$ s" mid-link" MID-LINK-BUF MID-LINK-U JOIN!
   MID-LINK$ s" sub/target.bin" DEEP-TARGET-BUF DEEP-TARGET-U JOIN!
   ROOT$ s" unsafe-parent" UNSAFE-PARENT-BUF UNSAFE-PARENT-U JOIN!
   UNSAFE-PARENT$ s" target.bin" UNSAFE-TARGET-BUF UNSAFE-TARGET-U JOIN! ;

: REMOVE-ENTRY ( ptr u8 n -- )
   2dup SYMLINK? if REMOVE-FILE exit then
   2dup EXISTS? if REMOVE-FILE exit then
   2drop ;

: RESET-TARGET ( -- )
   TARGET$ REMOVE-ENTRY
   TARGET$ s" oldbytes" WRITE-ALL ;

: RESET-AUTH ( -- )
   AUTH$ REMOVE-ENTRY
   AUTH$ s" authority" WRITE-ALL ;

: EXPECT-FILE ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n expected:ptr expectedu:n :}
   path pathu READ-BUF READ-CAP READ-ALL expectedu T=
   READ-BUF expectedu expected expectedu T$= ;

: EXPECT-TARGET ( ptr u8 n -- )
   TARGET$ 2swap EXPECT-FILE ;

: EXPECT-OLD ( -- )
   s" oldbytes" EXPECT-TARGET ;

: STAGE>N ( stage -- n )
   MATCH stage
      validate-destination OF 0 ENDOF
      validate-source OF 1 ENDOF
      walk-source OF 2 ENDOF
      open-source OF 3 ENDOF
      stat-source OF 4 ENDOF
      close-source OF 5 ENDOF
      open-parent OF 6 ENDOF
      walk-parent OF 7 ENDOF
      stat-parent OF 8 ENDOF
      create-temp OF 9 ENDOF
      stat-temp OF 10 ENDOF
      read-source OF 11 ENDOF
      write-temp OF 12 ENDOF
      chmod-temp OF 13 ENDOF
      sync-temp OF 14 ENDOF
      close-temp OF 15 ENDOF
      verify-temp OF 16 ENDOF
      publish OF 17 ENDOF
      sync-parent OF 18 ENDOF
      close-parent OF 19 ENDOF
   ;MATCH ;

: CLEAR-RESULT ( -- )
   -1 KIND !
   -1 AT-N !
   0 CODE !
   0 SOURCE-CLOSE-N !
   0 TEMP-CLOSE-N !
   0 CLEANUP-N !
   0 PARENT-CLOSE-N ! ;

: STORE-RESULT ( result -- )
   CLEAR-RESULT
   MATCH result
      committed OF K-COMMITTED KIND ! ENDOF
      committed-degraded OF
         PARENT-CLOSE-N ! CLEANUP-N !
         CODE ! STAGE>N AT-N !
         K-DEGRADED KIND !
      ENDOF
      aborted OF
         PARENT-CLOSE-N ! CLEANUP-N ! TEMP-CLOSE-N !
         SOURCE-CLOSE-N ! CODE ! STAGE>N AT-N !
         K-ABORTED KIND !
      ENDOF
   ;MATCH ;

: EXPECT ( n n n n n n n -- )
   {: kind:n at:n code:n source-close:n temp-close:n cleanup:n parent-close:n :}
   KIND @ kind T=
   AT-N @ at T=
   CODE @ code T=
   SOURCE-CLOSE-N @ source-close T=
   TEMP-CLOSE-N @ temp-close T=
   CLEANUP-N @ cleanup T=
   PARENT-CLOSE-N @ parent-close T= ;

: TRY-WRITE ( ptr u8 n -- )
   CTX CONTEXT-BYTES TARGET$ 2swap WRITE STORE-RESULT ;

: TRY-COPY ( -- )
   CTX CONTEXT-BYTES SOURCE$ TARGET$ COPY STORE-RESULT ;

: SYSTEM ( -- )
   INSTALL-SYSTEM
   FS:TEST-SYSTEM
   0 WRITE-N !
   0 SWAPPED !
   -1 SWAP-HOLD !
   0 WALK-CLOSE-FAILED ! ;

: FILL ( ptr u8 n n -- ) {: buf:ptr u:n value:n :}
   0 begin dup u < while
      value $FF and buf over + c!
      1+
   repeat drop ;

: FIXED-ENTROPY ( ptr u8 n -- n ) {: buf:ptr u:n :}
   buf u ENTROPY-N @ FILL
   u ;

: SEQUENCE-ENTROPY ( ptr u8 n -- n ) {: buf:ptr u:n :}
   buf u ENTROPY-N @ FILL
   ENTROPY-N @ 1+ ENTROPY-N !
   u ;

: SHORT-ENTROPY ( ptr u8 n -- n ) {: buf:ptr u:n :}
   u 0 <= if -50 exit then
   ENTROPY-N @ $FF and buf c!
   ENTROPY-N @ 1+ ENTROPY-N !
   1 ;

: FAIL-ENTROPY ( ptr u8 n -- n )
   2drop -50 ;

: FIXED! ( n -- )
   ENTROPY-N !
   ['] FIXED-ENTROPY is ENTROPY-OP ;

: SEQUENCE! ( n -- )
   ENTROPY-N !
   ['] SEQUENCE-ENTROPY is ENTROPY-OP ;

: CANDIDATE! ( n -- )
   FIXED!
   CTX FILL-RAND
   CTX BUILD-TEMP
   ROOT$ CTX TEMP CTX CTX.TEMP-U @ CANDIDATE-BUF JOIN-PATH CANDIDATE-U ! ;

: SHORT-WRITE ( fd ptr u8 n -- n ) {: fd:fd a:ptr u:n :}
   1 WRITE-N +!
   fd a u 2 min write-fd ;

: ZERO-WRITE ( fd ptr u8 n -- n )
   2drop drop 0 ;

: NEGATIVE-WRITE ( fd ptr u8 n -- n )
   2drop drop -31 ;

: OVERSIZED-WRITE ( fd ptr u8 n -- n ) {: fd:fd a:ptr u:n :}
   fd drop a drop u 1+ ;

: FAIL-RENAME ( fd ptr u8 fd ptr u8 -- rc )
   2drop 2drop -41 >RC ;

: FAIL-PARENT-STAT ( fd ptr u8 -- rc )
   2drop -53 >RC ;

: FAIL-TEMP-STAT ( fd ptr u8 -- rc ) {: fd:fd stat:ptr :}
   CTX STAGE@ STAGE>N 10 = if -55 >RC exit then
   fd stat fstat64 ;

: FAIL-TEMP-OPEN ( fd ptr u8 n n -- n )
   {: dir:fd path:ptr flags:n mode:n :}
   path 6 s" .habu-" STR= if -57 exit then
   dir path flags mode openat ;

: SWAP-TEMP-STAT ( fd ptr u8 ptr u8 -- rc )
   {: dir:fd path:ptr stat:ptr :}
   SWAPPED @ 0= if
      dir path FS:O-NOFOLLOW 0 openat dup 0 < if THROW-CODE then
      SWAP-HOLD !
      dir path 0 unlinkat RC>N dup 0 <> if THROW-CODE then drop
      dir path FS:O-WRONLY FS:O-CREAT or FS:O-EXCL or FS:O-NOFOLLOW or
      MODE-0600 openat dup 0 < if THROW-CODE then >FD
      close-rc RC>N dup 0 <> if THROW-CODE then drop
      SWAP-HOLD @ >FD close-rc RC>N dup 0 <> if THROW-CODE then drop
      -1 SWAP-HOLD !
      1 SWAPPED !
   then
   dir path stat fstatat-nofollow ;

: FAIL-UNLINK ( fd ptr u8 n -- rc )
   2drop drop -42 >RC ;

: FAIL-FCHMOD ( fd n -- rc )
   2drop -47 >RC ;

: FAIL-TEMP-SYNC ( fd -- rc ) {: fd:fd :}
   CTX STAGE@ STAGE>N 14 = if fd drop -43 >RC exit then
   fd fsync ;

: FAIL-PARENT-SYNC ( fd -- rc ) {: fd:fd :}
   CTX STAGE@ STAGE>N 18 = if fd drop -48 >RC exit then
   fd fsync ;

: FAIL-TEMP-CLOSE ( fd -- rc ) {: fd:fd :}
   CTX STAGE@ STAGE>N 15 = if
      fd close-rc RC>N dup 0 <> if THROW-CODE then drop
      -45 >RC exit
   then
   fd close-rc ;

: FAIL-WALK-CLOSE ( fd -- rc ) {: fd:fd :}
   CTX STAGE@ STAGE>N 7 = WALK-CLOSE-FAILED @ 0= and if
      fd close-rc RC>N dup 0 <> if THROW-CODE then drop
      1 WALK-CLOSE-FAILED !
      -58 >RC exit
   then
   fd close-rc ;

: FAIL-PARENT-CLOSE ( fd -- rc ) {: fd:fd :}
   CTX STAGE@ STAGE>N 19 = if
      fd close-rc RC>N dup 0 <> if THROW-CODE then drop
      -49 >RC exit
   then
   fd close-rc ;

: ENTRY? ( ptr u8 n -- bool )
   2dup EXISTS? if 2drop 0 0= exit then
   SYMLINK? ;

: NO-TEMP ( ptr n -- ) {: ctx:ptr :}
   ROOT$ ctx TEMP ctx CTX.TEMP-U @ CANDIDATE-BUF JOIN-PATH CANDIDATE-U !
   CANDIDATE$ ENTRY? TFALSE ;

: NO-TEMPS ( -- )
   CTX NO-TEMP ;

: LINK-RC ( -- n )
   HB-TARGET-LINUX? if -40 exit then
   HB-TARGET-MACOS? if -62 exit then
   FS-TARGET-UNKNOWN ;

: SUCCESS ( -- )
   SYSTEM RESET-TARGET
   s" newbytes" TRY-WRITE
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   s" newbytes" EXPECT-TARGET
   NO-TEMPS ;

: MUST-COMMIT-RESULTS ( -- )
   CONTEXT-BYTES CELL mod 0 T=
   construct result committed MUST-COMMIT
   [: construct stage publish -41 0 0 construct result committed-degraded
      MUST-COMMIT ;] catch -41 T=
   [: construct stage create-temp -57 0 0 0 0 construct result aborted
      MUST-COMMIT ;] catch -57 T= ;

: EXPLICIT-THROW-BOUNDARY ( -- )
   SYSTEM RESET-TARGET
   [: CTX CONTEXT-BYTES s" ." s" x" WRITE MUST-COMMIT ;] catch
   E-FS-PATH-UNSAFE T=
   [: CTX CONTEXT-BYTES MISSING$ TARGET$ COPY MUST-COMMIT ;] catch -2 T= ;

: EXISTING-SYMLINK ( -- )
   SYSTEM RESET-TARGET RESET-AUTH
   TARGET$ REMOVE-FILE
   AUTH$ TARGET$ MAKE-SYMLINK
   s" replacement" TRY-WRITE
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   s" replacement" EXPECT-TARGET
   AUTH$ s" authority" EXPECT-FILE ;

: BROKEN-SYMLINK ( -- )
   SYSTEM RESET-TARGET
   TARGET$ REMOVE-FILE
   MISSING$ TARGET$ MAKE-SYMLINK
   s" replacement" TRY-WRITE
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   s" replacement" EXPECT-TARGET
   MISSING$ EXISTS? TFALSE ;

: EXISTING-HARDLINK ( -- )
   SYSTEM RESET-TARGET RESET-AUTH
   TARGET$ REMOVE-FILE
   AUTH$ AUTH-Z FS-PATHZ-INTO drop
   TARGET$ TARGET-Z FS-PATHZ-INTO drop
   AUTH-Z TARGET-Z link RC>N 0 T=
   s" replacement" TRY-WRITE
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   s" replacement" EXPECT-TARGET
   AUTH$ s" authority" EXPECT-FILE ;

: RETRY ( -- )
   SYSTEM RESET-TARGET
   17 CANDIDATE!
   CANDIDATE$ s" occupied" WRITE-ALL
   17 SEQUENCE!
   s" retry-ok" TRY-WRITE
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   CANDIDATE$ s" occupied" EXPECT-FILE
   CANDIDATE$ REMOVE-FILE ;

: RETRY-EXHAUSTED ( -- )
   SYSTEM RESET-TARGET
   21 CANDIDATE!
   CANDIDATE$ s" occupied" WRITE-ALL
   21 FIXED!
   s" no-write" TRY-WRITE
   K-ABORTED 9 RC-EXISTS 0 0 0 0 EXPECT
   EXPECT-OLD
   CANDIDATE$ REMOVE-FILE ;

: ENTROPY-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-ENTROPY is ENTROPY-OP
   s" entropy" TRY-WRITE
   K-ABORTED 9 -50 0 0 0 0 EXPECT
   EXPECT-OLD ;

: SHORT-ENTROPY-SUCCESS ( -- )
   SYSTEM RESET-TARGET
   0 ENTROPY-N !
   ['] SHORT-ENTROPY is ENTROPY-OP
   s" short-entropy" TRY-WRITE
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   ENTROPY-N @ RAND-U T=
   s" short-entropy" EXPECT-TARGET ;

: PARENT-STAT-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-PARENT-STAT is FSTAT-OP
   s" parent-stat" TRY-WRITE
   K-ABORTED 8 -53 0 0 0 0 EXPECT
   EXPECT-OLD ;

: WALK-CLOSE-FAILURE ( -- )
   SYSTEM
   REAL-PARENT$ EXISTS? 0= if REAL-PARENT$ MAKE-DIR then
   REAL-SUB$ EXISTS? 0= if REAL-SUB$ MAKE-DIR then
   WALK-TARGET$ s" old-walk" WRITE-ALL
   ['] FAIL-WALK-CLOSE is CLOSE-OP
   CTX CONTEXT-BYTES WALK-TARGET$ s" new-walk" WRITE STORE-RESULT
   K-ABORTED 7 -58 0 0 0 0 EXPECT
   WALK-TARGET$ s" old-walk" EXPECT-FILE ;

: TEMP-OPEN-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-TEMP-OPEN is OPENAT-OP
   s" temp-open" TRY-WRITE
   K-ABORTED 9 -57 0 0 0 0 EXPECT
   EXPECT-OLD
   NO-TEMPS ;

: TEMP-STAT-FAILURE ( -- )
   SYSTEM RESET-TARGET
   61 CANDIDATE!
   61 FIXED!
   ['] FAIL-TEMP-STAT is FSTAT-OP
   s" temp-stat" TRY-WRITE
   K-ABORTED 10 -55 0 0 E-FS-STAT 0 EXPECT
   EXPECT-OLD
   CANDIDATE$ FILE? TTRUE
   CANDIDATE$ REMOVE-FILE ;

: SHORT-WRITES ( -- )
   SYSTEM RESET-TARGET
   ['] SHORT-WRITE is WRITE-OP
   s" short-ok" TRY-WRITE
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   WRITE-N @ 1 > TTRUE
   s" short-ok" EXPECT-TARGET ;

: WRITE-ZERO ( -- )
   SYSTEM RESET-TARGET
   ['] ZERO-WRITE is WRITE-OP
   s" zero" TRY-WRITE
   K-ABORTED 12 E-FS-IO 0 0 0 0 EXPECT
   EXPECT-OLD
   NO-TEMPS ;

: WRITE-NEGATIVE ( -- )
   SYSTEM RESET-TARGET
   ['] NEGATIVE-WRITE is WRITE-OP
   s" negative" TRY-WRITE
   K-ABORTED 12 -31 0 0 0 0 EXPECT
   EXPECT-OLD ;

: WRITE-OVERSIZED ( -- )
   SYSTEM RESET-TARGET
   ['] OVERSIZED-WRITE is WRITE-OP
   s" oversized" TRY-WRITE
   K-ABORTED 12 E-FS-IO 0 0 0 0 EXPECT
   EXPECT-OLD ;

: TEMP-SYNC-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-TEMP-SYNC is SYNC-OP
   s" unsynced-temp" TRY-WRITE
   K-ABORTED 14 -43 0 0 0 0 EXPECT
   EXPECT-OLD ;

: PARENT-SYNC-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-PARENT-SYNC is SYNC-OP
   s" published-parent" TRY-WRITE
   K-DEGRADED 18 -48 0 0 0 0 EXPECT
   s" published-parent" EXPECT-TARGET ;

: TEMP-CLOSE-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-TEMP-CLOSE is CLOSE-OP
   s" close-temp" TRY-WRITE
   K-ABORTED 15 -45 0 -45 0 0 EXPECT
   EXPECT-OLD ;

: PARENT-CLOSE-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-PARENT-CLOSE is CLOSE-OP
   s" close-parent" TRY-WRITE
   K-DEGRADED 19 -49 0 0 0 -49 EXPECT
   s" close-parent" EXPECT-TARGET ;

: RENAME-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-RENAME is RENAME-OP
   s" rename" TRY-WRITE
   K-ABORTED 17 -41 0 0 0 0 EXPECT
   EXPECT-OLD ;

: TEMP-SUBSTITUTION ( -- )
   SYSTEM RESET-TARGET
   53 CANDIDATE!
   53 FIXED!
   ['] SWAP-TEMP-STAT is FSTATAT-OP
   s" substituted" TRY-WRITE
   K-ABORTED 16 E-FS-STAT 0 0 E-FS-STAT 0 EXPECT
   EXPECT-OLD
   CANDIDATE$ FILE? TTRUE
   CANDIDATE$ REMOVE-FILE ;

: CHMOD-FAILURE ( -- )
   SYSTEM RESET-TARGET
   ['] FAIL-FCHMOD is FCHMOD-OP
   s" chmod" TRY-WRITE
   K-ABORTED 13 -47 0 0 0 0 EXPECT
   EXPECT-OLD ;

: COMBINED-FAILURE ( -- )
   SYSTEM RESET-TARGET
   31 CANDIDATE!
   31 FIXED!
   ['] ZERO-WRITE is WRITE-OP
   ['] FAIL-UNLINK is UNLINK-OP
   s" combined" TRY-WRITE
   K-ABORTED 12 E-FS-IO 0 0 -42 0 EXPECT
   EXPECT-OLD
   SYSTEM
   CANDIDATE$ FILE? TTRUE
   CANDIDATE$ REMOVE-FILE ;

: COPY-SUCCESS ( -- )
   SYSTEM RESET-TARGET
   SOURCE$ s" source-data" WRITE-ALL
   SOURCE$ FS:MODE-0755 CHMOD-MODE
   TRY-COPY
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   s" source-data" EXPECT-TARGET
   TARGET$ STAT-MODE PERM-MASK and FS:MODE-0755 T= ;

: COPY-EXACT-MODE ( -- )
   SYSTEM RESET-TARGET
   SOURCE$ s" exact-mode" WRITE-ALL
   SOURCE$ PERM-MASK CHMOD-MODE
   TRY-COPY
   K-COMMITTED -1 0 0 0 0 0 EXPECT
   TARGET$ STAT-MODE PERM-MASK and PERM-MASK T= ;

: COPY-SYMLINK-REJECTED ( -- )
   SYSTEM RESET-TARGET RESET-AUTH
   SOURCE$ REMOVE-ENTRY
   AUTH$ SOURCE$ MAKE-SYMLINK
   TRY-COPY
   K-ABORTED 3 LINK-RC 0 0 0 0 EXPECT
   EXPECT-OLD ;

: SOURCE-CLOSE-FAILURE ( -- )
   SYSTEM RESET-TARGET
   SOURCE$ REMOVE-ENTRY
   SOURCE$ s" source-data" WRITE-ALL
   FS:TEST-FAIL-CLOSE
   TRY-COPY
   K-ABORTED 5 -45 -45 0 0 0 EXPECT
   EXPECT-OLD ;

: SOURCE-READ-FAILURE ( -- )
   SYSTEM RESET-TARGET
   SOURCE$ REMOVE-ENTRY
   SOURCE$ s" source-data" WRITE-ALL
   FS:TEST-FAIL-READ
   TRY-COPY
   K-ABORTED 11 -52 0 0 0 0 EXPECT
   EXPECT-OLD ;

: DESTINATION-DIRECTORY ( -- )
   SYSTEM
   TARGET$ REMOVE-ENTRY
   TARGET$ MAKE-DIR
   s" directory" TRY-WRITE
   K-ABORTED 17 CODE @ 0 0 0 0 EXPECT
   CODE @ 0 < TTRUE
   TARGET$ DIR? TTRUE
   TARGET$ REMOVE-DIR ;

: INVALID-PATHS ( -- )
   SYSTEM
   CTX CONTEXT-BYTES NUL-PATH 3 s" x" WRITE STORE-RESULT
   K-ABORTED 0 E-FS-PATH-UNSAFE 0 0 0 0 EXPECT
   CTX CONTEXT-BYTES s" /tmp/" s" x" WRITE STORE-RESULT
   K-ABORTED 0 E-FS-PATH-UNSAFE 0 0 0 0 EXPECT
   CTX CONTEXT-BYTES s" ." s" x" WRITE STORE-RESULT
   K-ABORTED 0 E-FS-PATH-UNSAFE 0 0 0 0 EXPECT
   CTX CONTEXT-BYTES s" .." s" x" WRITE STORE-RESULT
   K-ABORTED 0 E-FS-PATH-UNSAFE 0 0 0 0 EXPECT ;

: NEGATIVE-LENGTH ( -- )
   SYSTEM RESET-TARGET
   [: CTX CONTEXT-BYTES TARGET$ s" ignored" drop -1 WRITE drop ;] catch
   E-FS-CAPACITY T=
   EXPECT-OLD ;

: PARENT-SYMLINK-REJECTED ( -- )
   SYSTEM
   PARENT-LINK$ REMOVE-ENTRY
   ROOT$ PARENT-LINK$ MAKE-SYMLINK
   CTX CONTEXT-BYTES LINK-TARGET$ s" x" WRITE STORE-RESULT
   K-ABORTED 7 LINK-RC 0 0 0 0 EXPECT
   PARENT-LINK$ REMOVE-FILE ;

: INTERMEDIATE-SYMLINK-REJECTED ( -- )
   SYSTEM
   REAL-PARENT$ EXISTS? 0= if REAL-PARENT$ MAKE-DIR then
   REAL-SUB$ EXISTS? 0= if REAL-SUB$ MAKE-DIR then
   REAL-PARENT$ MID-LINK$ MAKE-SYMLINK
   CTX CONTEXT-BYTES DEEP-TARGET$ s" x" WRITE STORE-RESULT
   K-ABORTED 7 LINK-RC 0 0 0 0 EXPECT
   MID-LINK$ REMOVE-FILE ;

: UNSAFE-PARENT-REJECTED ( -- )
   SYSTEM
   UNSAFE-PARENT$ MAKE-DIR
   UNSAFE-PARENT$ PERM-MASK CHMOD-MODE
   CTX CONTEXT-BYTES UNSAFE-TARGET$ s" x" WRITE STORE-RESULT
   K-ABORTED 8 E-FS-PATH-UNSAFE 0 0 0 0 EXPECT ;

: COMMITTED? ( result -- bool )
   FS-ATOMIC-EXTERNAL:COMMITTED? ;

: BAD+ ( -- )
   1 BAD atomic-add drop ;

: WRITER ( ptr n ptr u8 n -- ) {: ctx:ptr a:ptr u:n :}
   WRITES 0 ?do
      ctx CONTEXT-BYTES TARGET$ a u WRITE COMMITTED? 0= if BAD+ then
      TASK:PAUSE
   loop
   1 DONE atomic-add drop ;

: WRITE-A ( -- )
   CTX-A s" writer-a" WRITER ;

: WRITE-B ( -- )
   CTX-B s" writer-b" WRITER ;

: VALID-CONTENT? ( -- bool )
   TARGET$ READ-BUF READ-CAP READ-ALL
   dup 8 <> if drop 0 0= 0= exit then
   drop READ-BUF 8 s" oldbytes" STR=
   READ-BUF 8 s" writer-a" STR= or
   READ-BUF 8 s" writer-b" STR= or ;

: READ-WORK ( -- )
   1 READY atomic-add drop
   begin DONE atomic@ 2 < while
      VALID-CONTENT? 0= if BAD+ then
      1 OBS atomic-add drop
      TASK:PAUSE
   repeat ;

: WAIT-DONE ( ptr a -- ) {: task:ptr :}
   begin task TASK:DONE? 0= while TASK:PAUSE repeat ;

: CONCURRENT ( -- )
   SYSTEM RESET-TARGET
   0 DONE ! 0 READY ! 0 BAD ! 0 OBS !
   ['] READ-WORK READER TASK:ACTIVATE
   begin READY atomic@ 0= while TASK:PAUSE repeat
   ['] WRITE-A WRITER-A TASK:ACTIVATE
   ['] WRITE-B WRITER-B TASK:ACTIVATE
   WRITER-A WAIT-DONE
   WRITER-B WAIT-DONE
   READER WAIT-DONE
   BAD @ 0 T=
   OBS @ 0 > TTRUE
   CTX-A NO-TEMP
   CTX-B NO-TEMP
   WRITER-A TASK:KILL
   WRITER-B TASK:KILL
   READER TASK:KILL ;

: CLEANUP ( -- )
   SYSTEM
   ROOT$ EXISTS? if ROOT$ REMOVE-TREE then ;

: MAIN ( -- )
   T-RESET
   PATHS!
   SUCCESS
   MUST-COMMIT-RESULTS
   EXPLICIT-THROW-BOUNDARY
   EXISTING-SYMLINK
   BROKEN-SYMLINK
   EXISTING-HARDLINK
   RETRY
   RETRY-EXHAUSTED
   ENTROPY-FAILURE
   SHORT-ENTROPY-SUCCESS
   PARENT-STAT-FAILURE
   WALK-CLOSE-FAILURE
   TEMP-OPEN-FAILURE
   TEMP-STAT-FAILURE
   SHORT-WRITES
   WRITE-ZERO
   WRITE-NEGATIVE
   WRITE-OVERSIZED
   TEMP-SYNC-FAILURE
   PARENT-SYNC-FAILURE
   TEMP-CLOSE-FAILURE
   PARENT-CLOSE-FAILURE
   RENAME-FAILURE
   TEMP-SUBSTITUTION
   CHMOD-FAILURE
   COMBINED-FAILURE
   COPY-SUCCESS
   COPY-EXACT-MODE
   COPY-SYMLINK-REJECTED
   SOURCE-CLOSE-FAILURE
   SOURCE-READ-FAILURE
   DESTINATION-DIRECTORY
   INVALID-PATHS
   NEGATIVE-LENGTH
   PARENT-SYMLINK-REJECTED
   INTERMEDIATE-SYMLINK-REJECTED
   UNSAFE-PARENT-REJECTED
   CONCURRENT
   CLEANUP
   T-REPORT
   s" fs-atomic-test: ok" type cr ;

MAIN

;package
