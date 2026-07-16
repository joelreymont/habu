\ fs-stream-test.f - checked no-follow streaming fixtures.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fs-stream.f

package FS-STREAM-EXTERNAL
public

: RESULT>KIND ( FS:stream-result -- n )
   MATCH FS:stream-result
      ok OF drop 0 ENDOF
      failed OF 2drop 1 ENDOF
      close-failed OF drop 2 ENDOF
      failed-close OF 2drop drop 3 ENDOF
   ;MATCH ;

private
;package

package FS
private

32 constant OUT-CAP
0 constant K-OK
1 constant K-FAILED
2 constant K-CLOSE-FAILED
3 constant K-FAILED-CLOSE

create ROOT-BUF FS-PATH-CAP allot
create FILE-BUF FS-PATH-CAP allot
create EMPTY-BUF FS-PATH-CAP allot
create LINK-BUF FS-PATH-CAP allot
create BROKEN-BUF FS-PATH-CAP allot
create DIR-BUF FS-PATH-CAP allot
create MISSING-BUF FS-PATH-CAP allot
create SWAP-BUF FS-PATH-CAP allot
create AUTH-BUF FS-PATH-CAP allot
create DIR-LINK-BUF FS-PATH-CAP allot
create REAL-NESTED-BUF FS-PATH-CAP allot
create NESTED-BUF FS-PATH-CAP allot
create PATH-Z FS-PATHZ-CAP allot
create SWAP-Z FS-PATHZ-CAP allot
create AUTH-Z FS-PATHZ-CAP allot
create CHUNK 3 allot
create STAT-BUF FS-STAT-CAP allot
create OUT OUT-CAP allot
variable USER

variable ROOT-U
variable FILE-U
variable EMPTY-U
variable LINK-U
variable BROKEN-U
variable DIR-U
variable MISSING-U
variable SWAP-U
variable AUTH-U
variable DIR-LINK-U
variable REAL-NESTED-U
variable NESTED-U
variable OUT-U
variable CALLS
variable KIND
variable AT-N
variable CODE
variable CLOSE-N
variable MODE
variable FILE-FD
variable SWAPPED

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: FILE$ ( -- ptr u8 n )
   FILE-BUF FILE-U @ ;

: EMPTY$ ( -- ptr u8 n )
   EMPTY-BUF EMPTY-U @ ;

: LINK$ ( -- ptr u8 n )
   LINK-BUF LINK-U @ ;

: BROKEN$ ( -- ptr u8 n )
   BROKEN-BUF BROKEN-U @ ;

: DIR$ ( -- ptr u8 n )
   DIR-BUF DIR-U @ ;

: MISSING$ ( -- ptr u8 n )
   MISSING-BUF MISSING-U @ ;

: SWAP$ ( -- ptr u8 n )
   SWAP-BUF SWAP-U @ ;

: AUTH$ ( -- ptr u8 n )
   AUTH-BUF AUTH-U @ ;

: DIR-LINK$ ( -- ptr u8 n )
   DIR-LINK-BUF DIR-LINK-U @ ;

: REAL-NESTED$ ( -- ptr u8 n )
   REAL-NESTED-BUF REAL-NESTED-U @ ;

: NESTED$ ( -- ptr u8 n )
   NESTED-BUF NESTED-U @ ;

: PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: JOIN! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr up:ptr :}
   pa pu na nu dst JOIN-PATH up ! ;

: PATHS! ( -- )
   s" /tmp" s" hb-fs-stream" MAKE-TEMP-DIR ROOT-BUF ROOT-U PATH!
   ROOT$ s" file.bin" FILE-BUF FILE-U JOIN!
   ROOT$ s" empty.bin" EMPTY-BUF EMPTY-U JOIN!
   ROOT$ s" link.bin" LINK-BUF LINK-U JOIN!
   ROOT$ s" broken.bin" BROKEN-BUF BROKEN-U JOIN!
   ROOT$ s" dir" DIR-BUF DIR-U JOIN!
   ROOT$ s" missing.bin" MISSING-BUF MISSING-U JOIN!
   ROOT$ s" swap.bin" SWAP-BUF SWAP-U JOIN!
   ROOT$ s" authority.bin" AUTH-BUF AUTH-U JOIN!
   ROOT$ s" dir-link" DIR-LINK-BUF DIR-LINK-U JOIN!
   DIR$ s" nested.bin" REAL-NESTED-BUF REAL-NESTED-U JOIN!
   DIR-LINK$ s" nested.bin" NESTED-BUF NESTED-U JOIN!
   SWAP$ SWAP-Z FS-PATHZ-INTO drop
   AUTH$ AUTH-Z FS-PATHZ-INTO drop ;

: FIXTURE ( -- )
   FILE$ s" abcdefg" WRITE-ALL
   EMPTY$ s" " WRITE-ALL
   DIR$ MAKE-DIR
   FILE$ LINK$ MAKE-SYMLINK
   MISSING$ BROKEN$ MAKE-SYMLINK
   SWAP$ s" original" WRITE-ALL
   AUTH$ s" authority" WRITE-ALL
   REAL-NESTED$ s" nested" WRITE-ALL
   DIR$ DIR-LINK$ MAKE-SYMLINK ;

: STAGE>N ( stream-stage -- n )
   MATCH stream-stage
      validate OF 0 ENDOF
      walk OF 1 ENDOF
      open OF 2 ENDOF
      stat OF 3 ENDOF
      read OF 4 ENDOF
      callback OF 5 ENDOF
   ;MATCH ;

: CLEAR-RESULT ( -- )
   -1 KIND !
   -1 AT-N !
   0 CODE !
   0 CLOSE-N !
   0 MODE ! ;

: STORE-RESULT ( stream-result -- )
   CLEAR-RESULT
   MATCH stream-result
      ok OF MODE ! K-OK KIND ! ENDOF
      failed OF CODE ! STAGE>N AT-N ! K-FAILED KIND ! ENDOF
      close-failed OF CLOSE-N ! K-CLOSE-FAILED KIND ! ENDOF
      failed-close OF CLOSE-N ! CODE ! STAGE>N AT-N ! K-FAILED-CLOSE KIND ! ENDOF
   ;MATCH ;

: EXPECT ( n n n n -- ) {: kind:n at:n code:n close:n :}
   KIND @ kind T=
   AT-N @ at T=
   CODE @ code T=
   CLOSE-N @ close T= ;

: TRACK-OPENAT ( fd ptr u8 n n -- n ) {: dir:fd path:ptr flags:n mode:n :}
   dir path flags mode openat dup 0 >= if
      flags O-DIRECTORY and 0= if dup FILE-FD ! then
   then ;

: RESET-OPS ( -- )
   INSTALL-SYSTEM
   ['] TRACK-OPENAT is OPENAT-OP
   0 OUT-U !
   0 CALLS !
   -1 FILE-FD !
   0 SWAPPED ! ;

: COLLECT ( ptr n ptr u8 n -- ) {: user:ptr a:ptr u:n :}
   user USER <> if E-FS-IO throw then
   OUT-U @ u + OUT-CAP > if E-FS-CAPACITY throw then
   a OUT OUT-U @ + u BYTE-COPY
   u OUT-U +!
   1 CALLS +! ;

: FAIL-CALLBACK ( ptr n ptr u8 n -- )
   2drop drop -34 throw ;

: FAIL-STAT ( fd ptr u8 -- rc )
   2drop -31 >RC ;

: FAIL-READ ( fd ptr u8 n -- n )
   2drop drop -32 ;

: OVERSIZED-READ ( fd ptr u8 n -- n ) {: fd:fd a:ptr u:n :}
   fd drop a drop u 1+ ;

: FAIL-CLOSE ( fd -- rc ) {: fd:fd :}
   fd FD>N FILE-FD @ = if fd close-rc drop -33 >RC exit then
   fd close-rc ;

: SWAP-OPENAT ( fd ptr u8 n n -- n ) {: dir:fd path:ptr flags:n mode:n :}
   SWAPPED @ 0= if
      SWAP-Z unlink dup 0 <> if exit then drop
      AUTH-Z SWAP-Z symlink dup 0 <> if exit then drop
      1 SWAPPED !
   then
   dir path flags mode TRACK-OPENAT ;

\ typed-local-lint: allow-bare-local - q preserves its quotation effect.
: STREAM ( ptr u8 n [ ptr n ptr u8 n -- ] -- ) {: path:ptr pathu:n q :}
   path pathu PATH-Z FS-PATHZ-CAP CHUNK 3 STAT-BUF FS-STAT-CAP USER q
   STREAM-REGULAR STORE-RESULT ;

: REGULAR ( -- )
   RESET-OPS
   FILE$ [: COLLECT ;] STREAM
   K-OK -1 0 0 EXPECT
   MODE @ S-IFMT and S-IFREG = TTRUE
   CALLS @ 3 T=
   OUT OUT-U @ s" abcdefg" T$= ;

: EMPTY ( -- )
   RESET-OPS
   EMPTY$ [: COLLECT ;] STREAM
   K-OK -1 0 0 EXPECT
   CALLS @ 0 T= ;

: EXPECT-OPEN-FAILURE ( ptr u8 n -- )
   RESET-OPS
   [: COLLECT ;] STREAM
   K-FAILED 2 CODE @ 0 EXPECT
   CODE @ 0 < TTRUE ;

: LINK-RC ( -- n )
   HB-TARGET-LINUX? if -40 exit then
   HB-TARGET-MACOS? if -62 exit then
   FS-TARGET-UNKNOWN ;

: EXPECT-LINK-FAILURE ( ptr u8 n -- )
   RESET-OPS
   [: COLLECT ;] STREAM
   K-FAILED 2 LINK-RC 0 EXPECT ;

: LINKS ( -- )
   LINK$ EXPECT-LINK-FAILURE
   BROKEN$ EXPECT-LINK-FAILURE
   MISSING$ EXPECT-OPEN-FAILURE ;

: INTERMEDIATE-LINK ( -- )
   RESET-OPS
   NESTED$ [: COLLECT ;] STREAM
   K-FAILED 1 LINK-RC 0 EXPECT ;

: DIRECTORY ( -- )
   RESET-OPS
   DIR$ [: COLLECT ;] STREAM
   K-FAILED 3 E-FS-STAT 0 EXPECT ;

: STAT-FAILURE ( -- )
   RESET-OPS
   ['] FAIL-STAT is FSTAT-OP
   FILE$ [: COLLECT ;] STREAM
   K-FAILED 3 -31 0 EXPECT ;

: READ-FAILURE ( -- )
   RESET-OPS
   ['] FAIL-READ is READ-OP
   FILE$ [: COLLECT ;] STREAM
   K-FAILED 4 -32 0 EXPECT ;

: READ-OVERSIZED ( -- )
   RESET-OPS
   ['] OVERSIZED-READ is READ-OP
   FILE$ [: COLLECT ;] STREAM
   K-FAILED 4 E-FS-IO 0 EXPECT ;

: CALLBACK-FAILURE ( -- )
   RESET-OPS
   FILE$ [: FAIL-CALLBACK ;] STREAM
   K-FAILED 5 -34 0 EXPECT ;

: CLOSE-FAILURE-ONLY ( -- )
   RESET-OPS
   ['] FAIL-CLOSE is CLOSE-OP
   EMPTY$ [: COLLECT ;] STREAM
   K-CLOSE-FAILED -1 0 -33 EXPECT ;

: COMBINED ( -- )
   RESET-OPS
   ['] FAIL-READ is READ-OP
   ['] FAIL-CLOSE is CLOSE-OP
   FILE$ [: COLLECT ;] STREAM
   K-FAILED-CLOSE 4 -32 -33 EXPECT ;

: SWAP-PROTECTED ( -- )
   RESET-OPS
   ['] SWAP-OPENAT is OPENAT-OP
   SWAP$ [: COLLECT ;] STREAM
   K-FAILED 2 LINK-RC 0 EXPECT
   AUTH$ READ-BUF OUT-CAP READ-ALL 9 T=
   READ-BUF 9 s" authority" T$= ;

: INVALID ( -- )
   RESET-OPS
   FILE$ PATH-Z FS-PATHZ-CAP CHUNK 0 STAT-BUF FS-STAT-CAP USER [: COLLECT ;]
   STREAM-REGULAR STORE-RESULT
   K-FAILED 0 E-FS-CAPACITY 0 EXPECT
   FILE$ PATH-Z FS-PATHZ-CAP CHUNK 3 STAT-BUF STREAM-STAT-BYTES 1- USER [: COLLECT ;]
   STREAM-REGULAR STORE-RESULT
   K-FAILED 0 E-FS-CAPACITY 0 EXPECT
   FILE$ PATH-Z 1 CHUNK 3 STAT-BUF FS-STAT-CAP USER [: COLLECT ;]
   STREAM-REGULAR STORE-RESULT
   K-FAILED 0 E-FS-CAPACITY 0 EXPECT ;

: EXTERNAL-MATCH ( -- )
   0 construct stream-result ok FS-STREAM-EXTERNAL:RESULT>KIND K-OK T= ;

: CLEANUP ( -- )
   RESET-OPS
   ROOT$ REMOVE-TREE ;

: MAIN ( -- )
   T-RESET
   PATHS!
   FIXTURE
   REGULAR
   EMPTY
   LINKS
   INTERMEDIATE-LINK
   DIRECTORY
   STAT-FAILURE
   READ-FAILURE
   READ-OVERSIZED
   CALLBACK-FAILURE
   CLOSE-FAILURE-ONLY
   COMBINED
   SWAP-PROTECTED
   INVALID
   EXTERNAL-MATCH
   CLEANUP
   T-REPORT
   s" fs-stream-test: ok" type cr ;

MAIN

;package
