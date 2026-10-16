\ process-pty-handle.f - linear PTY target authority registry.

require src/lib/errors.f
require src/lib/prelude.f

package PROCESS-PTY

DEFLINEAR process-pty-reservation
DEFLINEAR process-pty-handle
DEFLINEAR process-pty-teardown

$10 constant SLOT-CAP
$FF constant SLOT-MASK
$7FFFFFFFFFFFFF constant GEN-MAX
1 constant F-GETFD

create SLOT-GEN SLOT-CAP cells allot
create SLOT-ACTIVE SLOT-CAP cells allot
create SLOT-LIVE SLOT-CAP cells allot
create SLOT-DRAIN SLOT-CAP cells allot
create SLOT-TARGET SLOT-CAP cells allot
create SLOT-MASTER SLOT-CAP cells allot
create SLOT-DONE SLOT-CAP cells allot
create SLOT-WATCH SLOT-CAP cells allot
create SLOT-GATE SLOT-CAP cells allot
create SLOT-OWNER SLOT-CAP cells allot

LINEAR: N>HANDLE ( n -- process-pty-handle )
LINEAR: HANDLE>N ( process-pty-handle -- n )
LINEAR: N>RESERVATION ( n -- process-pty-reservation )
LINEAR: RESERVATION>N ( process-pty-reservation -- n )
LINEAR: N>TEARDOWN ( n -- process-pty-teardown )
LINEAR: TEARDOWN>N ( process-pty-teardown -- n )

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

: TARGET@ ( idx -- pid )
   SLOT-TARGET swap CELL-AT @ >PID ;

: TARGET! ( pid idx -- )
   SLOT-TARGET swap CELL-AT ! ;

: MASTER@ ( idx -- fd )
   SLOT-MASTER swap CELL-AT @ >FD ;

: MASTER! ( fd idx -- )
   SLOT-MASTER swap CELL-AT ! ;

: DONE@ ( idx -- fd )
   SLOT-DONE swap CELL-AT @ >FD ;

: DONE! ( fd idx -- )
   SLOT-DONE swap CELL-AT ! ;

: WATCH@ ( idx -- fd )
   SLOT-WATCH swap CELL-AT @ >FD ;

: WATCH! ( fd idx -- )
   SLOT-WATCH swap CELL-AT ! ;

: GATE@ ( idx -- fd )
   SLOT-GATE swap CELL-AT @ >FD ;

: GATE! ( fd idx -- )
   SLOT-GATE swap CELL-AT ! ;

: OWNER@ ( idx -- pid )
   SLOT-OWNER swap CELL-AT @ >PID ;

: OWNER! ( pid idx -- )
   SLOT-OWNER swap CELL-AT ! ;

: SLOT-CLEAR ( idx -- ) {: idx:idx :}
   false idx ACTIVE!
   false idx LIVE!
   false idx DRAIN!
   -1 >PID idx TARGET!
   -1 >FD idx MASTER!
   -1 >FD idx DONE!
   -1 >FD idx WATCH!
   -1 >FD idx GATE!
   -1 >PID idx OWNER! ;

: SLOT-INIT ( idx -- ) {: idx:idx :}
   0 idx GEN!
   idx SLOT-CLEAR ;

: INIT ( -- )
   0 begin dup SLOT-CAP < while
      dup >IDX SLOT-INIT
      1+
   repeat drop ;

: USABLE? ( idx -- bool ) {: idx:idx :}
   idx ACTIVE@ 0= idx GEN@ GEN-MAX < and ;

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

: RELEASE ( idx -- )
   SLOT-CLEAR ;

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
   FIND-FREE dup GEN@ 1+ over GEN!
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

: FD-VALID? ( fd -- bool )
   FD>N dup 0 < if drop false exit then
   F-GETFD 0 fcntl 0 >= ;

: DISTINCT-FDS? ( fd fd fd fd -- bool )
   {: master:fd done:fd watch:fd gate:fd :}
   master FD>N done FD>N <>
   master FD>N watch FD>N <> and
   master FD>N gate FD>N <> and
   done FD>N watch FD>N <> and
   done FD>N gate FD>N <> and
   watch FD>N gate FD>N <> and ;

: COMMIT-VALID? ( n pid fd fd fd fd -- bool )
   {: raw:n target:pid master:fd done:fd watch:fd gate:fd :}
   target PID>N 0 >
   target PID>N raw UNPACK-IDX OWNER@ PID>N <> and
   master FD-VALID? and
   done FD-VALID? and
   watch FD-VALID? and
   gate FD-VALID? and
   master done watch gate DISTINCT-FDS? and ;

: COMMIT-REJECT ( n -- )
   UNPACK-IDX RELEASE
   E-PROC-PTY-HANDLE throw ;

: COMMIT-RAW ( n pid fd fd fd fd -- process-pty-handle )
   {: raw:n target:pid master:fd done:fd watch:fd gate:fd :}
   raw target master done watch gate COMMIT-VALID? 0= if raw COMMIT-REJECT then
   raw UNPACK-IDX {: idx:idx :}
   target idx TARGET!
   master idx MASTER!
   done idx DONE!
   watch idx WATCH!
   gate idx GATE!
   true idx LIVE!
   raw N>HANDLE ;

: COMMIT ( process-pty-reservation pid fd fd fd fd -- process-pty-handle )
   >r >r >r >r >r OPEN-RESERVATION
   r> r> r> r> r> COMMIT-RAW ;

: HANDLE-IDX ( process-pty-handle -- process-pty-handle idx )
   OPEN-HANDLE {: raw:n :}
   raw N>HANDLE raw UNPACK-IDX ;

: HANDLE-TARGET@ ( process-pty-handle -- process-pty-handle pid )
   HANDLE-IDX TARGET@ ;

: HANDLE-MASTER@ ( process-pty-handle -- process-pty-handle fd )
   HANDLE-IDX MASTER@ ;

: HANDLE-GATE-TAKE ( process-pty-handle -- process-pty-handle fd )
   HANDLE-IDX {: idx:idx :}
   idx GATE@ {: gate:fd :}
   -1 >FD idx GATE!
   gate ;

: TAKE ( process-pty-handle -- process-pty-teardown )
   OPEN-HANDLE {: raw:n :}
   raw UNPACK-IDX {: idx:idx :}
   false idx LIVE!
   true idx DRAIN!
   raw N>TEARDOWN ;

: TEARDOWN-IDX ( process-pty-teardown -- process-pty-teardown idx )
   OPEN-TEARDOWN {: raw:n :}
   raw N>TEARDOWN raw UNPACK-IDX ;

: TEARDOWN-TARGET@ ( process-pty-teardown -- process-pty-teardown pid )
   TEARDOWN-IDX TARGET@ ;

: TEARDOWN-FD-TAKE ( process-pty-teardown ptr n -- process-pty-teardown fd )
   {: slot:ptr :}
   OPEN-TEARDOWN {: raw:n :}
   raw N>TEARDOWN
   raw UNPACK-IDX {: idx:idx :}
   slot idx CELL-AT @ >FD {: fd:fd :}
   -1 slot idx CELL-AT !
   fd ;

: TEARDOWN-MASTER-TAKE ( process-pty-teardown -- process-pty-teardown fd )
   SLOT-MASTER TEARDOWN-FD-TAKE ;

: TEARDOWN-DONE-TAKE ( process-pty-teardown -- process-pty-teardown fd )
   SLOT-DONE TEARDOWN-FD-TAKE ;

: TEARDOWN-WATCH-TAKE ( process-pty-teardown -- process-pty-teardown fd )
   SLOT-WATCH TEARDOWN-FD-TAKE ;

: TEARDOWN-GATE-TAKE ( process-pty-teardown -- process-pty-teardown fd )
   SLOT-GATE TEARDOWN-FD-TAKE ;

: FD-MOVED? ( fd -- bool )
   FD>N 0 < ;

: DRAINED? ( idx -- bool ) {: idx:idx :}
   idx MASTER@ FD-MOVED?
   idx DONE@ FD-MOVED? and
   idx WATCH@ FD-MOVED? and
   idx GATE@ FD-MOVED? and ;

: TEARDOWN-DONE ( process-pty-teardown -- )
   OPEN-TEARDOWN UNPACK-IDX
   dup DRAINED? 0= if drop E-PROC-PTY-HANDLE throw then
   RELEASE ;

INIT

;package
