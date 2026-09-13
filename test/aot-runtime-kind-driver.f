\ Built after the real common emitter source by build-fixpoint-test.f. These
\ compact-record fixtures test runtime membership; real full/partial boot is
\ covered by hb-open-failure and aot-wid-restore.
package AOT-KIND-TEST
using AOT-BUF

: W32! ( n ptr u8 -- ) {: value:n dst:ptr :}
   4 0 ?do value i 8 * rshift dst i + c! loop ;

: RESET ( -- )
   256 AOT-NAMES-RESERVE
   0 AOT-NAMES-LEN !  0 AOT-REC-N ! ;

: ADD ( ptr u8 n n n n -- ) {: a:ptr u:n first:n second:n wid:n :}
   AOT-NAMES-LEN @ {: off:n :}
   AOT-NAMES-BUF@ off + {: name:ptr :}
   u name c!  a name 1+ u BYTE-COPY
   off u + 1+ AOT-NAMES-LEN !
   AOT-REC-BUF@ AOT-REC-MAX 48 * + AOT-REC-N @ AOT-CREC-ROW * + {: rec:ptr :}
   first rec W32!  second rec 4 + W32!
   off rec 8 + W32!  0 rec 12 + W32!  wid rec 16 + W32!
   AOT-REC-N @ 1+ AOT-REC-N ! ;

: ANCHORS ( n -- ) {: mode:n :}
   mode 7 <> if s" checker-reg" 3 4 $FFFFFFFF ADD then
   mode 3 <> if s" prefix-mark" 5 6 $FFFFFFFF ADD then
   mode 4 <> if
      s" DECLARATIONS" 0 4 mode 6 = if 0 else 4 then ADD
   then
   mode 5 <> if s" CURSORS" 4 4 5 ADD then ;

public

: WRONG ( -- ) s" runtime-kind: wrong capture classification" 75 die ;

: RUN ( n -- ) {: mode:n :}
   RESET
   mode 1 = if s" PARTIAL-REPL" 0 4 0 ADD then
   mode 2 >= if mode ANCHORS then
   AOT-RUNTIME:COMPLETE? if mode 2 <> if WRONG then
   else mode 2 = if WRONG then then
   mode 3 >= if s" runtime-kind: accepted an incomplete runtime" 75 die then ;

;package
