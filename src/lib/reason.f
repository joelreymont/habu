\ reason.f - a failure's stated text, package REASON. A throw code's ERROR:TEXT$
\ is one static text; REASON:THROW throws a code together with a text the
\ thrower composed, the numbers that made it fail, and REASON:TEXT$ answers it
\ at the catch. ERROR cannot keep it: it is sealed and loads below TASK and BUF.
\ CONTRACT. A code is thrown through REASON:THROW at every place or at none, and
\ a catch reads REASON:TEXT$ before it runs anything else. So a text pairs only
\ with its own throw, unless a cleanup run while that throw unwinds (`finally`)
\ throws the same code through REASON:THROW. A code a task ends with, as
\ TASK:JOIN's err, carries no text: the joiner's REASON:TEXT$ reads its own.
\ STORAGE CLASS. TASK-LOCAL: one code cell and one BUF per task, opened on its
\ first REASON:THROW and released by TASK:AT-EXIT; the main task's lasts the
\ process.

require src/lib/byte-buffer.f
require src/lib/task.f

package REASON

$1000 constant TEXT-CAP

\ The code of the task's last REASON:THROW, zero while LAST-TEXT holds none.
TASK:+USER LAST-CODE n
BUF:HDR-BYTES TASK:+USERS LAST-TEXT u8

: DISPOSE ( -- )
   0 LAST-CODE !
   0 LAST-TEXT BUF:DISPOSE ;

: REGISTER ( -- )
   TASK:SELF-N 0= if exit then
   [: DISPOSE ;] TASK:SELF TASK:AT-EXIT ;

\ The main task has no exit chain. Elsewhere a refused registration releases
\ the buffer, so a live buffer always has its release armed.
: OPEN ( -- )
   0 LAST-TEXT BUF:LIVE? if exit then
   0 LAST-TEXT TEXT-CAP BUF:N>BLEN BUF:INIT
   [: REGISTER ;] catch {: code:n :}
   code 0<> if 0 LAST-TEXT BUF:DISPOSE code throw then ;

public

\ The text of the calling task's last REASON:THROW when that threw code n, else
\ empty; valid until the task's next REASON:THROW.
: TEXT$ ( n -- ptr u8 n ) {: code:n :}
   code 0=  LAST-CODE @ code <>  or if s" " exit then
   0 LAST-TEXT BUF:SPAN$ BUF:BLEN>N ;

\ Throws code n with the span kept as the calling task's text, replacing any
\ earlier one. The span is copied first, so it may be the caller's scratch; a
\ copy that throws passes its own code on with no text kept.
: THROW ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   0 LAST-CODE !
   OPEN
   0 LAST-TEXT BUF:CLEAR
   a u BUF:N>BLEN 0 LAST-TEXT BUF:APPEND-SPAN
   code LAST-CODE !
   code throw ;

;package
