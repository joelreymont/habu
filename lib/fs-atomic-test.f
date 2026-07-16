\ fs-atomic-test.f - focused staged atomic replacement tests.
\ Run: bin/hb --load lib/fs-atomic-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/ffi.f
require lib/task.f
require lib/fs-atomic.f

package FS-ATOMIC
private

128 constant T-READ-CAP
32 constant T-WRITES
0 constant T-COMMITTED
1 constant T-UNSYNCED
2 constant T-CLOSE-FAILED
3 constant T-ABORTED

create T-ROOT-BUF FS-PATH-CAP allot
create T-TARGET-BUF FS-PATH-CAP allot
create T-TARGET-Z FS-PATHZ-CAP allot
create T-STAGE-BUF FS-PATH-CAP allot
create T-CANDIDATE-BUF FS-PATH-CAP allot
create T-MISSING-BUF FS-PATH-CAP allot
create T-READ-BUF T-READ-CAP allot
create T-NUL-PATH 97 c, 0 c, 98 c,
create T-CTX FS-ATOMIC:CONTEXT-CELLS cells allot
create T-CTX-A FS-ATOMIC:CONTEXT-CELLS cells allot
create T-CTX-B FS-ATOMIC:CONTEXT-CELLS cells allot

variable T-ROOT-U
variable T-TARGET-U
variable T-STAGE-U
variable T-CANDIDATE-U
variable T-MISSING-U
variable T-KIND
variable T-CAUSE
variable T-TEMP-CLOSE
variable T-CLEANUP-ERR
variable T-STAGE-CLOSE
variable T-PARENT-CLOSE
variable T-SYNC-ERR
variable T-ENTROPY-N
variable T-WRITE-N
variable T-SYNC-N
variable T-CLOSE-N
variable T-FILE-N
variable T-DONE
variable T-READY
variable T-BAD
variable T-OBS
variable T-OBS-OFF
variable T-OBS-END

TASK:MIN-STACK TASK:TASK T-WRITER-A
TASK:MIN-STACK TASK:TASK T-WRITER-B
TASK:MIN-STACK TASK:TASK T-READER

: T-ROOT$ ( -- ptr u8 n )
   T-ROOT-BUF T-ROOT-U @ ;

: T-TARGET$ ( -- ptr u8 n )
   T-TARGET-BUF T-TARGET-U @ ;

: T-STAGE$ ( -- ptr u8 n )
   T-STAGE-BUF T-STAGE-U @ ;

: T-CANDIDATE$ ( -- ptr u8 n )
   T-CANDIDATE-BUF T-CANDIDATE-U @ ;

: T-MISSING$ ( -- ptr u8 n )
   T-MISSING-BUF T-MISSING-U @ ;

: T-JOIN! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr up:ptr :}
   pa pu na nu dst JOIN-PATH up ! ;

: T-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: T-PATHS! ( -- )
   s" /tmp" s" hb-fs-atomic-test" MAKE-TEMP-DIR
   T-ROOT-BUF T-ROOT-U T-PATH!
   T-ROOT$ s" target.bin" T-TARGET-BUF T-TARGET-U T-JOIN!
   T-ROOT$ s" .habu-atomic" T-STAGE-BUF T-STAGE-U T-JOIN!
   T-ROOT$ s" missing.bin" T-MISSING-BUF T-MISSING-U T-JOIN!
   T-TARGET$ T-TARGET-Z FS-PATHZ-INTO drop ;

: T-RESULT-RESET ( -- )
   -1 T-KIND !
   0 T-CAUSE !
   0 T-TEMP-CLOSE !
   0 T-CLEANUP-ERR !
   0 T-STAGE-CLOSE !
   0 T-PARENT-CLOSE !
   0 T-SYNC-ERR ! ;

: T-STORE-RESULT ( result -- )
   T-RESULT-RESET
   MATCH result
      committed OF T-COMMITTED T-KIND ! ENDOF
      committed-unsynced OF
         T-PARENT-CLOSE ! T-STAGE-CLOSE ! T-SYNC-ERR !
         T-UNSYNCED T-KIND !
      ENDOF
      committed-close-failed OF
         T-PARENT-CLOSE ! T-STAGE-CLOSE !
         T-CLOSE-FAILED T-KIND !
      ENDOF
      aborted OF
         T-PARENT-CLOSE ! T-STAGE-CLOSE ! T-CLEANUP-ERR !
         T-TEMP-CLOSE ! T-CAUSE ! T-ABORTED T-KIND !
      ENDOF
   ;MATCH ;

: T-EXPECT ( n n n n n n n -- )
   {: kind:n cause:n temp-close:n cleanup:n sync:n stage-close:n parent-close:n :}
   T-KIND @ kind T=
   T-CAUSE @ cause T=
   T-TEMP-CLOSE @ temp-close T=
   T-CLEANUP-ERR @ cleanup T=
   T-SYNC-ERR @ sync T=
   T-STAGE-CLOSE @ stage-close T=
   T-PARENT-CLOSE @ parent-close T= ;

\ typed-local-lint: allow-bare-local - src preserves ptr u8 through the transaction.
: T-TRY ( ptr u8 n -- ) {: src srcu:n :}
   T-CTX T-TARGET$ src srcu TRY-CORE T-STORE-RESULT ;

: T-RESET-CTX ( -- )
   T-CTX CONTEXT-INIT ;

: T-RESET-TARGET ( -- )
   T-TARGET$ s" oldbytes" WRITE-ALL ;

\ typed-local-lint: allow-bare-local - expected preserves ptr u8 through comparison.
: T-EXPECT-TARGET ( ptr u8 n -- ) {: expected expectedu:n :}
   T-TARGET$ T-READ-BUF T-READ-CAP READ-ALL expectedu T=
   T-READ-BUF expectedu expected expectedu T$= ;

: T-EXPECT-OLD ( -- )
   s" oldbytes" T-EXPECT-TARGET ;

: T-EMPTY-STAGE? ( -- bool )
   0 T-FILE-N !
   T-STAGE$ [: 2drop 1 T-FILE-N +! ;] WALK-FILES
   T-FILE-N @ 0= ;

\ typed-local-lint: allow-bare-local - buf preserves ptr u8 through deterministic fill.
: T-FILL-ENTROPY ( ptr u8 n n -- ) {: buf u:n value:n :}
   0 begin dup u < while
      value $FF and buf over + c!
      1+
   repeat drop ;

\ typed-local-lint: allow-bare-local - buf preserves ptr u8 through the hook.
: T-FIXED-ENTROPY ( ptr a ptr u8 n -- n ) {: ctx:ptr buf u:n :}
   ctx drop
   buf u T-ENTROPY-N @ T-FILL-ENTROPY
   u ;

\ typed-local-lint: allow-bare-local - buf preserves ptr u8 through the hook.
: T-SEQUENCE-ENTROPY ( ptr a ptr u8 n -- n ) {: ctx:ptr buf u:n :}
   ctx drop
   buf u T-ENTROPY-N @ T-FILL-ENTROPY
   T-ENTROPY-N @ 1+ T-ENTROPY-N !
   u ;

: T-INSTALL-FIXED-ENTROPY ( ptr a n -- ) {: ctx:ptr value:n :}
   value T-ENTROPY-N !
   ['] T-FIXED-ENTROPY ctx CTX-ENTROPY-XT N! ;

: T-INSTALL-SEQUENCE-ENTROPY ( ptr a n -- ) {: ctx:ptr value:n :}
   value T-ENTROPY-N !
   ['] T-SEQUENCE-ENTROPY ctx CTX-ENTROPY-XT N! ;

: T-CANDIDATE! ( ptr a n -- ) {: ctx:ptr value:n :}
   ctx value T-INSTALL-FIXED-ENTROPY
   ctx FILL-RANDOM
   ctx BUILD-TEMP
   T-STAGE$ ctx TEMP-Z ctx CTX-TEMP-U N@
   T-CANDIDATE-BUF JOIN-PATH T-CANDIDATE-U ! ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 through the syscall.
: T-SHORT-WRITE ( ptr a fd ptr u8 n -- n ) {: ctx:ptr fd:fd a u:n :}
   1 T-WRITE-N +!
   ctx fd a u 2 min SYSTEM-WRITE ;

\ typed-local-lint: allow-bare-local - a is deliberately not written.
: T-ZERO-WRITE ( ptr a fd ptr u8 n -- n )
   2drop 2drop 0 ;

\ typed-local-lint: allow-bare-local - a is deliberately not written.
: T-NEGATIVE-WRITE ( ptr a fd ptr u8 n -- n )
   2drop 2drop -1 ;

\ typed-local-lint: allow-bare-local - a is deliberately not written.
: T-OVERSIZED-WRITE ( ptr a fd ptr u8 n -- n ) {: ctx:ptr fd:fd a:ptr u:n :}
   ctx drop fd drop a drop u 1+ ;

: T-FAIL-SYNC-SECOND ( ptr a fd -- rc ) {: ctx:ptr fd:fd :}
   1 T-SYNC-N +!
   T-SYNC-N @ 2 = if -1 >RC exit then
   ctx fd SYSTEM-SYNC ;

: T-FAIL-CLOSE-FIRST ( ptr a fd -- rc ) {: ctx:ptr fd:fd :}
   1 T-CLOSE-N +!
   ctx fd SYSTEM-CLOSE {: rc:rc :}
   T-CLOSE-N @ 1 = if -1 >RC exit then
   rc ;

: T-FAIL-CLOSE-SECOND ( ptr a fd -- rc ) {: ctx:ptr fd:fd :}
   1 T-CLOSE-N +!
   ctx fd SYSTEM-CLOSE {: rc:rc :}
   T-CLOSE-N @ 2 = if -1 >RC exit then
   rc ;

\ typed-local-lint: allow-bare-local - paths are not consumed by the failure hook.
: T-FAIL-RENAME ( ptr a fd ptr u8 fd ptr u8 -- rc )
   2drop 2drop drop -1 >RC ;

\ typed-local-lint: allow-bare-local - path is not consumed by the failure hook.
: T-FAIL-UNLINK ( ptr a fd ptr u8 n -- rc )
   2drop 2drop -1 >RC ;

: T-TEST-STAGE-SYMLINK ( -- )
   T-RESET-TARGET
   T-TARGET$ T-STAGE$ MAKE-SYMLINK
   T-RESET-CTX
   s" replacement" T-TRY
   T-ABORTED E-FS-OPEN 0 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-STAGE$ SYMLINK? TTRUE
   T-STAGE$ REMOVE-FILE ;

: T-TEST-STAGE-FILE ( -- )
   T-STAGE$ s" occupied" WRITE-ALL
   T-RESET-CTX
   s" replacement" T-TRY
   T-ABORTED E-FS-OPEN 0 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-STAGE$ REMOVE-FILE ;

: T-TEST-STAGE-MODE ( -- )
   T-STAGE$ MAKE-DIR
   T-STAGE$ 493 CHMOD-MODE
   T-RESET-CTX
   s" replacement" T-TRY
   T-ABORTED E-FS-STAT 0 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-STAGE$ REMOVE-DIR ;

: T-TEST-SUCCESS ( -- )
   T-RESET-CTX
   s" newbytes" T-TRY
   T-COMMITTED 0 0 0 0 0 0 T-EXPECT
   s" newbytes" T-EXPECT-TARGET
   T-STAGE$ DIR? TTRUE
   T-EMPTY-STAGE? TTRUE ;

: T-TEST-STAGE-COLLISION ( -- )
   T-RESET-CTX
   s" againxxx" T-TRY
   T-COMMITTED 0 0 0 0 0 0 T-EXPECT
   s" againxxx" T-EXPECT-TARGET
   T-EMPTY-STAGE? TTRUE ;

: T-TEST-RETRY ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   T-CTX 17 T-CANDIDATE!
   T-CANDIDATE$ s" occupied" WRITE-ALL
   T-CTX 17 T-INSTALL-SEQUENCE-ENTROPY
   s" retry-ok" T-TRY
   T-COMMITTED 0 0 0 0 0 0 T-EXPECT
   s" retry-ok" T-EXPECT-TARGET
   T-CANDIDATE$ FILE? TTRUE
   T-CANDIDATE$ REMOVE-FILE ;

: T-TEST-RETRY-EXHAUSTED ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   T-CTX 33 T-CANDIDATE!
   T-CANDIDATE$ s" occupied" WRITE-ALL
   T-CTX 33 T-INSTALL-FIXED-ENTROPY
   s" no-write" T-TRY
   T-ABORTED E-FS-OPEN 0 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-CANDIDATE$ T-READ-BUF T-READ-CAP READ-ALL 8 T=
   T-READ-BUF 8 s" occupied" T$=
   T-CANDIDATE$ REMOVE-FILE ;

: T-TEST-SHORT-WRITES ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   0 T-WRITE-N !
   ['] T-SHORT-WRITE T-CTX CTX-WRITE-XT N!
   s" short-ok" T-TRY
   T-COMMITTED 0 0 0 0 0 0 T-EXPECT
   T-WRITE-N @ 1 > TTRUE
   s" short-ok" T-EXPECT-TARGET ;

: T-TEST-WRITE-ZERO ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   T-CTX 41 T-INSTALL-FIXED-ENTROPY
   ['] T-ZERO-WRITE T-CTX CTX-WRITE-XT N!
   s" zero-fail" T-TRY
   T-ABORTED E-FS-IO 0 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-EMPTY-STAGE? TTRUE ;

: T-TEST-WRITE-NEGATIVE ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   ['] T-NEGATIVE-WRITE T-CTX CTX-WRITE-XT N!
   s" neg-fail" T-TRY
   T-ABORTED E-FS-IO 0 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-EMPTY-STAGE? TTRUE ;

: T-TEST-WRITE-OVERSIZED ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   ['] T-OVERSIZED-WRITE T-CTX CTX-WRITE-XT N!
   s" huge-fail" T-TRY
   T-ABORTED E-FS-IO 0 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-EMPTY-STAGE? TTRUE ;

: T-TEST-TEMP-CLOSE ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   0 T-CLOSE-N !
   ['] T-FAIL-CLOSE-FIRST T-CTX CTX-CLOSE-XT N!
   s" closebad" T-TRY
   T-ABORTED E-FS-IO -1 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-EMPTY-STAGE? TTRUE ;

: T-TEST-RENAME ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   ['] T-FAIL-RENAME T-CTX CTX-RENAMEAT-XT N!
   s" renamebad" T-TRY
   T-ABORTED E-FS-IO 0 0 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-EMPTY-STAGE? TTRUE ;

: T-TEST-CLEANUP-FAILURE ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   T-CTX 57 T-CANDIDATE!
   T-CTX 57 T-INSTALL-FIXED-ENTROPY
   ['] T-ZERO-WRITE T-CTX CTX-WRITE-XT N!
   ['] T-FAIL-UNLINK T-CTX CTX-UNLINKAT-XT N!
   s" cleanupbad" T-TRY
   T-ABORTED E-FS-IO 0 -1 0 0 0 T-EXPECT
   T-EXPECT-OLD
   T-CANDIDATE$ FILE? TTRUE
   T-CANDIDATE$ REMOVE-FILE ;

: T-TEST-PARENT-SYNC ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   0 T-SYNC-N !
   ['] T-FAIL-SYNC-SECOND T-CTX CTX-SYNC-XT N!
   s" published" T-TRY
   T-UNSYNCED 0 0 0 -1 0 0 T-EXPECT
   s" published" T-EXPECT-TARGET
   T-EMPTY-STAGE? TTRUE ;

: T-TEST-STAGE-CLOSE ( -- )
   T-RESET-TARGET
   T-RESET-CTX
   0 T-CLOSE-N !
   ['] T-FAIL-CLOSE-SECOND T-CTX CTX-CLOSE-XT N!
   s" committed" T-TRY
   T-CLOSE-FAILED 0 0 0 0 -1 0 T-EXPECT
   s" committed" T-EXPECT-TARGET ;

: T-TEST-NUL ( -- )
   T-RESET-CTX
   T-CTX T-NUL-PATH 3 s" x" TRY-CORE T-STORE-RESULT
   T-ABORTED E-FS-PATH-UNSAFE 0 0 0 0 0 T-EXPECT ;

: T-TEST-TRAILING-SLASH ( -- )
   T-RESET-CTX
   T-CTX s" /tmp/" s" x" TRY-CORE T-STORE-RESULT
   T-ABORTED E-FS-PATH 0 0 0 0 0 T-EXPECT ;

: T-BAD+ ( -- )
   1 T-BAD atomic-add drop ;

: T-COMMITTED? ( result -- bool )
   MATCH result
      committed OF TRUE ENDOF
      committed-unsynced OF 2drop drop FALSE ENDOF
      committed-close-failed OF 2drop FALSE ENDOF
      aborted OF 2drop 2drop drop FALSE ENDOF
   ;MATCH ;

\ typed-local-lint: allow-bare-local - src preserves ptr u8 through repeated transactions.
: T-WRITER ( ptr a ptr u8 n -- ) {: ctx:ptr src srcu:n :}
   ctx CONTEXT-INIT
   T-WRITES 0 ?do
      ctx T-TARGET$ src srcu TRY-WRITE-FILE T-COMMITTED? 0= if T-BAD+ then
      TASK:PAUSE
   loop
   1 T-DONE atomic-add drop ;

: T-WRITE-A ( -- )
   T-CTX-A s" writer-a" T-WRITER ;

: T-WRITE-B ( -- )
   T-CTX-B s" writer-b" T-WRITER ;

: T-OBS-CONTENT? ( n -- bool ) {: u:n :}
   u 8 <> if FALSE exit then
   T-READ-BUF 8 s" oldbytes" STR=
   T-READ-BUF 8 s" writer-a" STR= or
   T-READ-BUF 8 s" writer-b" STR= or ;

: T-OBSERVE ( -- )
   T-TARGET-Z open-rd {: raw:n :}
   raw 0 < if T-BAD+ exit then
   raw RAW>FD {: fd:fd :}
   0 T-OBS-OFF !
   0 T-OBS-END !
   begin T-OBS-OFF @ 9 < T-OBS-END @ 0= and while
      fd T-READ-BUF T-OBS-OFF @ + 9 T-OBS-OFF @ - read-fd {: got:n :}
      got 0 < if fd close-rc drop T-BAD+ exit then
      got 0= if
         1 T-OBS-END !
      else
         T-OBS-OFF @ got + T-OBS-OFF !
      then
   repeat
   fd close-rc RC>N 0 <> if T-BAD+ then
   T-OBS-OFF @ T-OBS-CONTENT? 0= if T-BAD+ then
   1 T-OBS atomic-add drop ;

: T-READ-WORK ( -- )
   1 T-READY atomic-add drop
   begin T-DONE atomic@ 2 < while
      T-OBSERVE
      TASK:PAUSE
   repeat
   T-OBSERVE ;

: T-WAIT-DONE ( ptr a -- ) {: task:ptr :}
   begin task TASK:DONE? 0= while TASK:PAUSE repeat ;

: T-TEST-CONCURRENT ( -- )
   T-RESET-TARGET
   0 T-DONE ! 0 T-READY ! 0 T-BAD ! 0 T-OBS !
   ['] T-READ-WORK T-READER TASK:ACTIVATE
   begin T-READY atomic@ 1 < while TASK:PAUSE repeat
   ['] T-WRITE-A T-WRITER-A TASK:ACTIVATE
   ['] T-WRITE-B T-WRITER-B TASK:ACTIVATE
   T-WRITER-A T-WAIT-DONE
   T-WRITER-B T-WAIT-DONE
   T-READER T-WAIT-DONE
   T-BAD @ 0 T=
   T-OBS @ 0 > TTRUE
   T-EMPTY-STAGE? TTRUE
   T-WRITER-A TASK:KILL
   T-WRITER-B TASK:KILL
   T-READER TASK:KILL ;

: T-CLEANUP ( -- )
   T-ROOT$ EXISTS? if T-ROOT$ REMOVE-TREE then ;

: T-MAIN ( -- )
   T-RESET
   T-PATHS!
   T-RESET-TARGET
   T-TEST-STAGE-SYMLINK
   T-TEST-STAGE-FILE
   T-TEST-STAGE-MODE
   T-TEST-SUCCESS
   T-TEST-STAGE-COLLISION
   T-TEST-RETRY
   T-TEST-RETRY-EXHAUSTED
   T-TEST-SHORT-WRITES
   T-TEST-WRITE-ZERO
   T-TEST-WRITE-NEGATIVE
   T-TEST-WRITE-OVERSIZED
   T-TEST-TEMP-CLOSE
   T-TEST-RENAME
   T-TEST-CLEANUP-FAILURE
   T-TEST-PARENT-SYNC
   T-TEST-STAGE-CLOSE
   T-TEST-NUL
   T-TEST-TRAILING-SLASH
   T-TEST-CONCURRENT
   T-CLEANUP
   T-REPORT
   s" fs-atomic-test: ok" type cr ;

T-MAIN

;package
