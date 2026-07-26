\ process-pty-handle.f - linear PTY supervisor authority registry.

require lib/errors.f
require lib/prelude.f

DEFLINEAR process-pty-reservation
DEFLINEAR process-pty-handle
DEFLINEAR process-pty-teardown

package PROCESS-PTY

\ The six supervisor roles. Each is a distinct one-cell nominal family so the
\ checker refuses a cross-role swap: a supervisor pid is not a process-group id
\ is not a target pid, and the three watch descriptors are not interchangeable
\ either. They are declared INSIDE the package and stay private to it: every
\ user is in this package (this file and lib/process-pty-io.f, which reopens
\ it), and the public lifecycle surface converts each role back to `pid` or `fd`
\ at the boundary, so no caller outside ever names one.

TYPEFAMILY sup-pid 0
TYPEFAMILY pgrp 0
TYPEFAMILY target-pid 0
TYPEFAMILY group-watch 0
TYPEFAMILY target-watch 0
TYPEFAMILY sup-watch 0

$10 constant SLOT-CAP
$FF constant SLOT-MASK
$7FFFFFFFFFFFFF constant GEN-MAX
1 constant F-GETFD

create SLOT-GEN SLOT-CAP cells allot
create SLOT-ACTIVE SLOT-CAP cells allot
create SLOT-LIVE SLOT-CAP cells allot
create SLOT-DRAIN SLOT-CAP cells allot
SLOT-CAP TYPED-BUFFER SLOT-SUP sup-pid
SLOT-CAP TYPED-BUFFER SLOT-GROUP pgrp
SLOT-CAP TYPED-BUFFER SLOT-TARGET target-pid
create SLOT-MASTER SLOT-CAP cells allot
create SLOT-LIFE SLOT-CAP cells allot
create SLOT-DONE SLOT-CAP cells allot
create SLOT-ANCHOR SLOT-CAP cells allot
SLOT-CAP TYPED-BUFFER SLOT-GROUP-WATCH group-watch
SLOT-CAP TYPED-BUFFER SLOT-TARGET-WATCH target-watch
SLOT-CAP TYPED-BUFFER SLOT-SUP-WATCH sup-watch
create SLOT-OWNER SLOT-CAP cells allot

TRUSTED: N>HANDLE ( n -- process-pty-handle ) ;
TRUSTED: HANDLE>N ( process-pty-handle -- n ) ;
TRUSTED: N>RESERVATION ( n -- process-pty-reservation ) ;
TRUSTED: RESERVATION>N ( process-pty-reservation -- n ) ;
TRUSTED: N>TEARDOWN ( n -- process-pty-teardown ) ;
TRUSTED: TEARDOWN>N ( process-pty-teardown -- n ) ;
TRUSTED: PID>SUP ( pid -- sup-pid ) ;
TRUSTED: SUP>PID ( sup-pid -- pid ) ;
TRUSTED: PID>PGRP ( pid -- pgrp ) ;
TRUSTED: PGRP>PID ( pgrp -- pid ) ;
TRUSTED: PID>TARGET ( pid -- target-pid ) ;
TRUSTED: TARGET>PID ( target-pid -- pid ) ;
TRUSTED: FD>GROUP-WATCH ( fd -- group-watch ) ;
TRUSTED: GROUP-WATCH>FD ( group-watch -- fd ) ;
TRUSTED: FD>TARGET-WATCH ( fd -- target-watch ) ;
TRUSTED: TARGET-WATCH>FD ( target-watch -- fd ) ;
TRUSTED: FD>SUP-WATCH ( fd -- sup-watch ) ;
TRUSTED: SUP-WATCH>FD ( sup-watch -- fd ) ;

: CELL-AT ( ptr a idx -- ptr a ) {: base:ptr idx:idx :}
   base idx IDX>N cells + ;

: GEN@ ( idx -- n )
   SLOT-GEN swap CELL-AT @ ;

: GEN! ( n idx -- )
   SLOT-GEN swap CELL-AT ! ;

: ACTIVE@ ( idx -- bool )
   SLOT-ACTIVE swap CELL-AT @ 0 <> ;

: ACTIVE! ( bool idx -- )
   SLOT-ACTIVE swap CELL-AT ! ;

: LIVE@ ( idx -- bool )
   SLOT-LIVE swap CELL-AT @ 0 <> ;

: LIVE! ( bool idx -- )
   SLOT-LIVE swap CELL-AT ! ;

: DRAIN@ ( idx -- bool )
   SLOT-DRAIN swap CELL-AT @ 0 <> ;

: DRAIN! ( bool idx -- )
   SLOT-DRAIN swap CELL-AT ! ;

: SUP@ ( idx -- sup-pid )
   IDX>N SLOT-SUP @ ;

: SUP! ( sup-pid idx -- )
   IDX>N SLOT-SUP ! ;

: GROUP@ ( idx -- pgrp )
   IDX>N SLOT-GROUP @ ;

: GROUP! ( pgrp idx -- )
   IDX>N SLOT-GROUP ! ;

: TARGET@ ( idx -- target-pid )
   IDX>N SLOT-TARGET @ ;

: TARGET! ( target-pid idx -- )
   IDX>N SLOT-TARGET ! ;

: MASTER@ ( idx -- fd )
   SLOT-MASTER swap CELL-AT @ >FD ;

: MASTER! ( fd idx -- )
   SLOT-MASTER swap CELL-AT ! ;

: LIFE@ ( idx -- fd )
   SLOT-LIFE swap CELL-AT @ >FD ;

: LIFE! ( fd idx -- )
   SLOT-LIFE swap CELL-AT ! ;

: DONE@ ( idx -- fd )
   SLOT-DONE swap CELL-AT @ >FD ;

: DONE! ( fd idx -- )
   SLOT-DONE swap CELL-AT ! ;

: ANCHOR@ ( idx -- fd )
   SLOT-ANCHOR swap CELL-AT @ >FD ;

: ANCHOR! ( fd idx -- )
   SLOT-ANCHOR swap CELL-AT ! ;

: GROUP-WATCH@ ( idx -- group-watch )
   IDX>N SLOT-GROUP-WATCH @ ;

: GROUP-WATCH! ( group-watch idx -- )
   IDX>N SLOT-GROUP-WATCH ! ;

: TARGET-WATCH@ ( idx -- target-watch )
   IDX>N SLOT-TARGET-WATCH @ ;

: TARGET-WATCH! ( target-watch idx -- )
   IDX>N SLOT-TARGET-WATCH ! ;

: SUP-WATCH@ ( idx -- sup-watch )
   IDX>N SLOT-SUP-WATCH @ ;

: SUP-WATCH! ( sup-watch idx -- )
   IDX>N SLOT-SUP-WATCH ! ;

: OWNER@ ( idx -- pid )
   SLOT-OWNER swap CELL-AT @ >PID ;

: OWNER! ( pid idx -- )
   SLOT-OWNER swap CELL-AT ! ;

: SLOT-CLEAR ( idx -- ) {: idx:idx :}
   false idx ACTIVE!
   false idx LIVE!
   false idx DRAIN!
   -1 >PID PID>SUP idx SUP!
   -1 >PID PID>PGRP idx GROUP!
   -1 >PID PID>TARGET idx TARGET!
   -1 >FD idx MASTER!
   -1 >FD idx LIFE!
   -1 >FD idx DONE!
   -1 >FD idx ANCHOR!
   -1 >FD FD>GROUP-WATCH idx GROUP-WATCH!
   -1 >FD FD>TARGET-WATCH idx TARGET-WATCH!
   -1 >FD FD>SUP-WATCH idx SUP-WATCH!
   -1 >PID idx OWNER! ;

: SLOT-INIT ( idx -- ) {: idx:idx :}
   0 idx GEN!
   idx SLOT-CLEAR ;

: INIT ( -- )
   0 begin dup SLOT-CAP < while
      dup >IDX SLOT-INIT
      1+
   repeat drop ;

: FREE? ( idx -- bool )
   ACTIVE@ 0= ;

: USABLE? ( idx -- bool ) {: idx:idx :}
   idx FREE? idx GEN@ GEN-MAX < and ;

: FIND-FREE ( -- idx )
   0 >IDX begin dup IDX>N SLOT-CAP < while
      dup USABLE? if exit then
      IDX>N 1+ >IDX
   repeat drop
   E-PROC-PTY-CAPACITY throw ;

: ROOM? ( -- bool )
   0 >IDX begin dup IDX>N SLOT-CAP < while
      dup USABLE? if drop true exit then
      IDX>N 1+ >IDX
   repeat drop false ;

: NEXT-GEN ( idx -- n )
   GEN@ 1+ ;

: RELEASE ( idx -- )
   SLOT-CLEAR ;

: STORE ( sup-pid pgrp target-pid fd fd fd fd group-watch target-watch sup-watch idx -- )
   {: sup:sup-pid group:pgrp target:target-pid master:fd life:fd done:fd anchor:fd group-watch:group-watch target-watch:target-watch sup-watch:sup-watch idx:idx :}
   sup idx SUP!
   group idx GROUP!
   target idx TARGET!
   master idx MASTER!
   life idx LIFE!
   done idx DONE!
   anchor idx ANCHOR!
   group-watch idx GROUP-WATCH!
   target-watch idx TARGET-WATCH!
   sup-watch idx SUP-WATCH! ;

: PACK ( idx -- n ) {: idx:idx :}
   idx GEN@ 8 lshift idx IDX>N or ;

: UNPACK-IDX ( n -- idx )
   SLOT-MASK and >IDX ;

: UNPACK-GEN ( n -- n )
   8 rshift ;

: VALID? ( n -- bool ) {: raw:n :}
   raw UNPACK-IDX {: idx:idx :}
   idx IDX>N SLOT-CAP >= if false exit then
   idx ACTIVE@ 0= if false exit then
   raw UNPACK-GEN idx GEN@ = ;

: RESERVE ( -- process-pty-reservation )
   getpid dup 0 <= if drop E-PROC-SPAWN throw then >PID {: owner:pid :}
   FIND-FREE dup NEXT-GEN over GEN!
   owner over OWNER!
   true over ACTIVE!
   PACK N>RESERVATION ;

: OPEN-RAW ( n -- n )
   dup VALID? 0= if E-PROC-PTY-HANDLE throw then
   dup UNPACK-IDX OWNER@ PID>N getpid <> if E-PROC-PTY-HANDLE throw then ;

: OPEN-RESERVATION ( process-pty-reservation -- n )
   RESERVATION>N OPEN-RAW
   dup UNPACK-IDX LIVE@ if E-PROC-PTY-HANDLE throw then
   dup UNPACK-IDX DRAIN@ if E-PROC-PTY-HANDLE throw then ;

: OPEN-HANDLE ( process-pty-handle -- n )
   HANDLE>N OPEN-RAW
   dup UNPACK-IDX LIVE@ 0= if E-PROC-PTY-HANDLE throw then
   dup UNPACK-IDX DRAIN@ if E-PROC-PTY-HANDLE throw then ;

: OPEN-TEARDOWN ( process-pty-teardown -- n )
   TEARDOWN>N OPEN-RAW
   dup UNPACK-IDX LIVE@ if E-PROC-PTY-HANDLE throw then
   dup UNPACK-IDX DRAIN@ 0= if E-PROC-PTY-HANDLE throw then ;

: CANCEL ( process-pty-reservation -- )
   OPEN-RESERVATION UNPACK-IDX RELEASE ;

: PID-POSITIVE? ( pid -- bool )
   PID>N 0 > ;

: FD-VALID? ( fd -- bool )
   FD>N dup 0 < if drop false exit then
   F-GETFD 0 fcntl 0 >= ;

: DISTINCT-PIDS? ( pid sup-pid pgrp target-pid -- bool )
   {: owner:pid sup:sup-pid group:pgrp target:target-pid :}
   owner PID>N sup SUP>PID PID>N <>
   owner PID>N group PGRP>PID PID>N <> and
   owner PID>N target TARGET>PID PID>N <> and
   sup SUP>PID PID>N group PGRP>PID PID>N <> and
   sup SUP>PID PID>N target TARGET>PID PID>N <> and
   group PGRP>PID PID>N target TARGET>PID PID>N <> and ;

: DISTINCT-FDS? ( fd fd fd fd group-watch target-watch sup-watch -- bool )
   {: master:fd life:fd done:fd anchor:fd group-watch:group-watch target-watch:target-watch sup-watch:sup-watch :}
   master FD>N life FD>N <>
   master FD>N done FD>N <> and
   master FD>N anchor FD>N <> and
   master FD>N group-watch GROUP-WATCH>FD FD>N <> and
   master FD>N target-watch TARGET-WATCH>FD FD>N <> and
   master FD>N sup-watch SUP-WATCH>FD FD>N <> and
   life FD>N done FD>N <> and
   life FD>N anchor FD>N <> and
   life FD>N group-watch GROUP-WATCH>FD FD>N <> and
   life FD>N target-watch TARGET-WATCH>FD FD>N <> and
   life FD>N sup-watch SUP-WATCH>FD FD>N <> and
   done FD>N anchor FD>N <> and
   done FD>N group-watch GROUP-WATCH>FD FD>N <> and
   done FD>N target-watch TARGET-WATCH>FD FD>N <> and
   done FD>N sup-watch SUP-WATCH>FD FD>N <> and
   anchor FD>N group-watch GROUP-WATCH>FD FD>N <> and
   anchor FD>N target-watch TARGET-WATCH>FD FD>N <> and
   anchor FD>N sup-watch SUP-WATCH>FD FD>N <> and
   group-watch GROUP-WATCH>FD FD>N target-watch TARGET-WATCH>FD FD>N <> and
   group-watch GROUP-WATCH>FD FD>N sup-watch SUP-WATCH>FD FD>N <> and
   target-watch TARGET-WATCH>FD FD>N sup-watch SUP-WATCH>FD FD>N <> and ;

: COMMIT-PIDS-VALID? ( n sup-pid pgrp target-pid -- bool )
   {: raw:n sup:sup-pid group:pgrp target:target-pid :}
   sup SUP>PID PID-POSITIVE?
   group PGRP>PID PID-POSITIVE? and
   target TARGET>PID PID-POSITIVE? and
   raw UNPACK-IDX OWNER@ sup group target DISTINCT-PIDS? and ;

: COMMIT-FDS-VALID? ( fd fd fd fd group-watch target-watch sup-watch -- bool )
   {: master:fd life:fd done:fd anchor:fd group-watch:group-watch target-watch:target-watch sup-watch:sup-watch :}
   master FD-VALID? life FD-VALID? and
   done FD-VALID? and
   anchor FD-VALID? and
   group-watch GROUP-WATCH>FD FD-VALID? and
   target-watch TARGET-WATCH>FD FD-VALID? and
   sup-watch SUP-WATCH>FD FD-VALID? and
   master life done anchor group-watch target-watch sup-watch DISTINCT-FDS? and ;

: COMMIT-VALID? ( n sup-pid pgrp target-pid fd fd fd fd group-watch target-watch sup-watch -- bool )
   {: raw:n sup:sup-pid group:pgrp target:target-pid master:fd life:fd done:fd anchor:fd group-watch:group-watch target-watch:target-watch sup-watch:sup-watch :}
   raw sup group target COMMIT-PIDS-VALID?
   master life done anchor group-watch target-watch sup-watch COMMIT-FDS-VALID? and ;

: COMMIT-REJECT ( n -- )
   UNPACK-IDX RELEASE
   E-PROC-PTY-HANDLE throw ;

: COMMIT-RAW ( n sup-pid pgrp target-pid fd fd fd fd group-watch target-watch sup-watch -- process-pty-handle )
   {: raw:n sup:sup-pid group:pgrp target:target-pid master:fd life:fd done:fd anchor:fd group-watch:group-watch target-watch:target-watch sup-watch:sup-watch :}
   raw sup group target master life done anchor group-watch target-watch sup-watch
   COMMIT-VALID? 0= if raw COMMIT-REJECT then
   raw UNPACK-IDX {: idx:idx :}
   sup group target master life done anchor group-watch target-watch sup-watch idx STORE
   true idx LIVE!
   raw N>HANDLE ;

: COMMIT ( process-pty-reservation sup-pid pgrp target-pid fd fd fd fd group-watch target-watch sup-watch -- process-pty-handle )
   >r >r >r >r >r >r >r >r >r >r OPEN-RESERVATION
   r> r> r> r> r> r> r> r> r> r> COMMIT-RAW ;

: HANDLE-IDX ( process-pty-handle -- process-pty-handle idx )
   OPEN-HANDLE {: raw:n :}
   raw N>HANDLE raw UNPACK-IDX ;

: HANDLE-CHECK ( process-pty-handle -- process-pty-handle )
   HANDLE-IDX drop ;

: HANDLE-SUP@ ( process-pty-handle -- process-pty-handle sup-pid )
   HANDLE-IDX SUP@ ;

: HANDLE-GROUP@ ( process-pty-handle -- process-pty-handle pgrp )
   HANDLE-IDX GROUP@ ;

: HANDLE-TARGET@ ( process-pty-handle -- process-pty-handle target-pid )
   HANDLE-IDX TARGET@ ;

: HANDLE-MASTER@ ( process-pty-handle -- process-pty-handle fd )
   HANDLE-IDX MASTER@ ;

: TAKE ( process-pty-handle -- process-pty-teardown )
   OPEN-HANDLE {: raw:n :}
   raw UNPACK-IDX {: idx:idx :}
   false idx LIVE!
   true idx DRAIN!
   raw N>TEARDOWN ;

: TEARDOWN-IDX ( process-pty-teardown -- process-pty-teardown idx )
   OPEN-TEARDOWN {: raw:n :}
   raw N>TEARDOWN raw UNPACK-IDX ;

: TEARDOWN-SUP@ ( process-pty-teardown -- process-pty-teardown sup-pid )
   TEARDOWN-IDX SUP@ ;

: TEARDOWN-GROUP@ ( process-pty-teardown -- process-pty-teardown pgrp )
   TEARDOWN-IDX GROUP@ ;

: TEARDOWN-TARGET@ ( process-pty-teardown -- process-pty-teardown target-pid )
   TEARDOWN-IDX TARGET@ ;

: TEARDOWN-FD-TAKE ( process-pty-teardown ptr a -- process-pty-teardown fd )
   {: slot:ptr :}
   OPEN-TEARDOWN {: raw:n :}
   raw N>TEARDOWN
   raw UNPACK-IDX {: idx:idx :}
   slot idx CELL-AT @ >FD {: fd:fd :}
   -1 slot idx CELL-AT !
   fd ;

: TEARDOWN-MASTER-TAKE ( process-pty-teardown -- process-pty-teardown fd )
   SLOT-MASTER TEARDOWN-FD-TAKE ;

: TEARDOWN-LIFE-TAKE ( process-pty-teardown -- process-pty-teardown fd )
   SLOT-LIFE TEARDOWN-FD-TAKE ;

: TEARDOWN-DONE-TAKE ( process-pty-teardown -- process-pty-teardown fd )
   SLOT-DONE TEARDOWN-FD-TAKE ;

: TEARDOWN-ANCHOR-TAKE ( process-pty-teardown -- process-pty-teardown fd )
   SLOT-ANCHOR TEARDOWN-FD-TAKE ;

: TEARDOWN-GROUP-WATCH-TAKE ( process-pty-teardown -- process-pty-teardown group-watch )
   TEARDOWN-IDX {: idx:idx :}
   idx GROUP-WATCH@ {: watch:group-watch :}
   -1 >FD FD>GROUP-WATCH idx GROUP-WATCH!
   watch ;

: TEARDOWN-TARGET-WATCH-TAKE ( process-pty-teardown -- process-pty-teardown target-watch )
   TEARDOWN-IDX {: idx:idx :}
   idx TARGET-WATCH@ {: watch:target-watch :}
   -1 >FD FD>TARGET-WATCH idx TARGET-WATCH!
   watch ;

: TEARDOWN-SUP-WATCH-TAKE ( process-pty-teardown -- process-pty-teardown sup-watch )
   TEARDOWN-IDX {: idx:idx :}
   idx SUP-WATCH@ {: watch:sup-watch :}
   -1 >FD FD>SUP-WATCH idx SUP-WATCH!
   watch ;

: FD-MOVED? ( fd -- bool )
   FD>N 0 < ;

: DRAINED? ( idx -- bool ) {: idx:idx :}
   idx MASTER@ FD-MOVED?
   idx LIFE@ FD-MOVED? and
   idx DONE@ FD-MOVED? and
   idx ANCHOR@ FD-MOVED? and
   idx GROUP-WATCH@ GROUP-WATCH>FD FD-MOVED? and
   idx TARGET-WATCH@ TARGET-WATCH>FD FD-MOVED? and
   idx SUP-WATCH@ SUP-WATCH>FD FD-MOVED? and ;

: TEARDOWN-DONE ( process-pty-teardown -- )
   OPEN-TEARDOWN UNPACK-IDX
   dup DRAINED? 0= if drop E-PROC-PTY-HANDLE throw then
   RELEASE ;

INIT

;package
