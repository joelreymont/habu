\ process-pty-handle.f - linear PTY supervisor authority registry.

require lib/errors.f
require lib/prelude.f

DEFLINEAR process-pty-reservation
DEFLINEAR process-pty-handle
DEFLINEAR process-pty-teardown

package PROCESS-PTY

$10 constant SLOT-CAP
$FF constant SLOT-MASK
$7FFFFFFFFFFFFF constant GEN-MAX
1 constant F-GETFD

create SLOT-GEN SLOT-CAP cells allot
create SLOT-ACTIVE SLOT-CAP cells allot
create SLOT-LIVE SLOT-CAP cells allot
create SLOT-DRAIN SLOT-CAP cells allot
create SLOT-SUP SLOT-CAP cells allot
create SLOT-TARGET SLOT-CAP cells allot
create SLOT-MASTER SLOT-CAP cells allot
create SLOT-LIFE SLOT-CAP cells allot
create SLOT-DONE SLOT-CAP cells allot
create SLOT-OWNER SLOT-CAP cells allot

TRUSTED: N>HANDLE ( n -- process-pty-handle ) ;
TRUSTED: HANDLE>N ( process-pty-handle -- n ) ;
TRUSTED: N>RESERVATION ( n -- process-pty-reservation ) ;
TRUSTED: RESERVATION>N ( process-pty-reservation -- n ) ;
TRUSTED: N>TEARDOWN ( n -- process-pty-teardown ) ;
TRUSTED: TEARDOWN>N ( process-pty-teardown -- n ) ;

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

: SUP@ ( idx -- pid )
   SLOT-SUP swap CELL-AT @ >PID ;

: SUP! ( pid idx -- )
   SLOT-SUP swap CELL-AT ! ;

: TARGET@ ( idx -- pid )
   SLOT-TARGET swap CELL-AT @ >PID ;

: TARGET! ( pid idx -- )
   SLOT-TARGET swap CELL-AT ! ;

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

: OWNER@ ( idx -- pid )
   SLOT-OWNER swap CELL-AT @ >PID ;

: OWNER! ( pid idx -- )
   SLOT-OWNER swap CELL-AT ! ;

: SLOT-CLEAR ( idx -- ) {: idx:idx :}
   false idx ACTIVE!
   false idx LIVE!
   false idx DRAIN!
   -1 >PID idx SUP!
   -1 >PID idx TARGET!
   -1 >FD idx MASTER!
   -1 >FD idx LIFE!
   -1 >FD idx DONE!
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

: STORE ( pid pid fd fd fd idx -- )
   {: sup:pid target:pid master:fd life:fd done:fd idx:idx :}
   sup idx SUP!
   target idx TARGET!
   master idx MASTER!
   life idx LIFE!
   done idx DONE! ;

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

: DISTINCT-PIDS? ( pid pid pid -- bool )
   {: owner:pid sup:pid target:pid :}
   owner PID>N sup PID>N <>
   owner PID>N target PID>N <> and
   sup PID>N target PID>N <> and ;

: DISTINCT-FDS? ( fd fd fd -- bool )
   {: master:fd life:fd done:fd :}
   master FD>N life FD>N <>
   master FD>N done FD>N <> and
   life FD>N done FD>N <> and ;

: COMMIT-PIDS-VALID? ( n pid pid -- bool )
   {: raw:n sup:pid target:pid :}
   sup PID-POSITIVE? target PID-POSITIVE? and
   raw UNPACK-IDX OWNER@ sup target DISTINCT-PIDS? and ;

: COMMIT-FDS-VALID? ( fd fd fd -- bool )
   {: master:fd life:fd done:fd :}
   master FD-VALID? life FD-VALID? and
   done FD-VALID? and
   master life done DISTINCT-FDS? and ;

: COMMIT-VALID? ( n pid pid fd fd fd -- bool )
   {: raw:n sup:pid target:pid master:fd life:fd done:fd :}
   raw sup target COMMIT-PIDS-VALID?
   master life done COMMIT-FDS-VALID? and ;

: COMMIT-REJECT ( n -- )
   UNPACK-IDX RELEASE
   E-PROC-PTY-HANDLE throw ;

: COMMIT-RAW ( n pid pid fd fd fd -- process-pty-handle )
   {: raw:n sup:pid target:pid master:fd life:fd done:fd :}
   raw sup target master life done COMMIT-VALID? 0= if raw COMMIT-REJECT then
   raw UNPACK-IDX {: idx:idx :}
   sup target master life done idx STORE
   true idx LIVE!
   raw N>HANDLE ;

: COMMIT ( process-pty-reservation pid pid fd fd fd -- process-pty-handle )
   >r >r >r >r >r OPEN-RESERVATION r> r> r> r> r> COMMIT-RAW ;

: VIEW ( process-pty-handle -- process-pty-handle pid pid fd fd fd )
   OPEN-HANDLE {: raw:n :}
   raw N>HANDLE
   raw UNPACK-IDX {: idx:idx :}
   idx SUP@ idx TARGET@ idx MASTER@ idx LIFE@ idx DONE@ ;

: TAKE ( process-pty-handle -- process-pty-teardown )
   OPEN-HANDLE {: raw:n :}
   raw UNPACK-IDX {: idx:idx :}
   false idx LIVE!
   true idx DRAIN!
   raw N>TEARDOWN ;

: TEARDOWN-VIEW ( process-pty-teardown -- process-pty-teardown pid pid fd fd fd )
   OPEN-TEARDOWN {: raw:n :}
   raw N>TEARDOWN
   raw UNPACK-IDX {: idx:idx :}
   idx SUP@ idx TARGET@ idx MASTER@ idx LIFE@ idx DONE@ ;

: TEARDOWN-DONE ( process-pty-teardown -- )
   OPEN-TEARDOWN UNPACK-IDX RELEASE ;

INIT

;package
