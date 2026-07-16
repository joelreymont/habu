\ process-pty-handle.f - linear PTY supervisor handle registry.

require lib/errors.f
require lib/prelude.f

DEFLINEAR process-pty-reservation
DEFLINEAR process-pty-handle

package PROCESS-PTY

$10 constant SLOT-CAP
$FF constant SLOT-MASK
$7FFFFFFFFFFFFF constant GEN-MAX

create SLOT-GEN SLOT-CAP cells allot
create SLOT-ACTIVE SLOT-CAP cells allot
create SLOT-LIVE SLOT-CAP cells allot
create SLOT-SUP SLOT-CAP cells allot
create SLOT-TARGET SLOT-CAP cells allot
create SLOT-DONE SLOT-CAP cells allot
create SLOT-OWNER SLOT-CAP cells allot

TRUSTED: N>HANDLE ( n -- process-pty-handle ) ;
TRUSTED: HANDLE>N ( process-pty-handle -- n ) ;
TRUSTED: N>RESERVATION ( n -- process-pty-reservation ) ;
TRUSTED: RESERVATION>N ( process-pty-reservation -- n ) ;

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

: SUP@ ( idx -- pid )
   SLOT-SUP swap CELL-AT @ >PID ;

: SUP! ( pid idx -- )
   SLOT-SUP swap CELL-AT ! ;

: TARGET@ ( idx -- pid )
   SLOT-TARGET swap CELL-AT @ >PID ;

: TARGET! ( pid idx -- )
   SLOT-TARGET swap CELL-AT ! ;

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
   -1 >PID idx SUP!
   -1 >PID idx TARGET!
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

: FIND-FREE ( -- idx )
   0 >IDX begin dup IDX>N SLOT-CAP < while
      dup FREE? if exit then
      IDX>N 1+ >IDX
   repeat drop
   E-PROC-PTY-CAPACITY throw ;

: NEXT-GEN ( idx -- n ) {: idx:idx :}
   idx GEN@ dup GEN-MAX >= if drop E-PROC-PTY-CAPACITY throw then
   1+ ;

: RELEASE ( idx -- )
   SLOT-CLEAR ;

: STORE ( pid pid fd idx -- )
   {: sup:pid target:pid done:fd idx:idx :}
   sup idx SUP!
   target idx TARGET!
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
   FIND-FREE dup NEXT-GEN over GEN!
   getpid dup 0 <= if drop E-PROC-SPAWN throw then >PID over OWNER!
   true over ACTIVE!
   PACK N>RESERVATION ;

: OPEN-RAW ( n -- n )
   dup VALID? 0= if E-PROC-PTY-HANDLE throw then
   dup UNPACK-IDX OWNER@ PID>N getpid <> if E-PROC-PTY-HANDLE throw then ;

: OPEN-RESERVATION ( process-pty-reservation -- n )
   RESERVATION>N OPEN-RAW
   dup UNPACK-IDX LIVE@ if E-PROC-PTY-HANDLE throw then ;

: OPEN ( process-pty-handle -- n )
   HANDLE>N OPEN-RAW
   dup UNPACK-IDX LIVE@ 0= if E-PROC-PTY-HANDLE throw then ;

: CANCEL ( process-pty-reservation -- )
   OPEN-RESERVATION UNPACK-IDX RELEASE ;

: COMMIT-RAW ( n pid pid fd -- process-pty-handle )
   {: raw:n sup:pid target:pid done:fd :}
   raw UNPACK-IDX {: idx:idx :}
   sup target done idx STORE
   true idx LIVE!
   raw N>HANDLE ;

: COMMIT ( process-pty-reservation pid pid fd -- process-pty-handle )
   >r >r >r OPEN-RESERVATION r> r> r> COMMIT-RAW ;

: TAKE ( process-pty-handle -- pid pid fd )
   OPEN UNPACK-IDX {: idx:idx :}
   idx SUP@ idx TARGET@ idx DONE@
   idx RELEASE ;

public

: HANDLE-PID ( process-pty-handle -- process-pty-handle pid )
   OPEN {: raw:n :}
   raw N>HANDLE raw UNPACK-IDX TARGET@ ;

INIT

;package
