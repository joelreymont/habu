\ reason.f - a failure's stated text, package REASON. A throw code's ERROR:TEXT$
\ is one static text; REASON:THROW throws a code together with a text the
\ thrower composed, the numbers that made it fail, and REASON:TEXT$ answers it
\ at the catch. ERROR cannot keep it: it is sealed and loads below BUF.
\ CONTRACT. A code leaves its package through REASON:THROW at every place or at
\ none; a throw its own package catches and throws on through REASON:THROW is
\ not a place. A catch reads REASON:TEXT$ before it runs anything else. So a
\ text pairs only with its own throw, unless a cleanup run while that throw
\ unwinds (`finally`) throws the same code through REASON:THROW. A code a task
\ ends with, as TASK:JOIN's err, carries no text: the joiner's REASON:TEXT$
\ reads its own.
\ STORAGE CLASS. TASK-LOCAL: one code cell and one BUF per task in the
\ REASON-ABI band (src/habu/layout.f); a buffer opens on the task's first
\ REASON:THROW and TASK releases it as each activation ends; the main task's
\ lasts the process.

require src/habu/layout.f
require src/core/exit-status.f
require src/lib/byte-buffer.f

package REASON

$1000 constant TEXT-CAP

\ src/habu/layout.f loads below BUF, so the band states the header's width in
\ cells; a narrower band would let BUF write into the next one.
: BAND-AGREE ( -- )
   BUF:HDR-BYTES  REASON-ABI:END REASON-ABI:TEXT-OFF -  <> if
      s" reason: BUF's header differs from the REASON-ABI band's text bytes"
      EXIT-STATUS:INTERNAL die
   then ;
BAND-AGREE

\ The code of the task's last REASON:THROW, zero while LAST-TEXT holds none.
: LAST-CODE ( -- ^n )
   data-base REASON-ABI:CODE-OFF + ;

: LAST-TEXT ( -- ^u8 )
   data-base BYTE-VIEW REASON-ABI:TEXT-OFF + ;

: OPEN ( -- )
   LAST-TEXT BUF:LIVE? if exit then
   LAST-TEXT TEXT-CAP BUF:N>BLEN BUF:INIT ;

public

\ The text of the calling task's last REASON:THROW when that threw code n, else
\ empty; valid until the task's next REASON:THROW.
: TEXT$ ( n -- ^u8 n ) {: code:n :}
   code 0=  LAST-CODE @ code <>  or if s" " exit then
   LAST-TEXT BUF:SPAN$ BUF:BLEN>N ;

\ Throws code n with the span kept as the calling task's text, replacing any
\ earlier one. The span is copied first, so it may be the caller's scratch; a
\ copy that throws passes its own code on with no text kept.
: THROW ( ^u8 n n -- ) {: a:^ u:n code:n :}
   0 LAST-CODE !
   OPEN
   LAST-TEXT BUF:CLEAR
   a u BUF:N>BLEN LAST-TEXT BUF:APPEND-SPAN
   code LAST-CODE !
   code throw ;

\ Disposes the calling task's text.
: RELEASE ( -- )
   0 LAST-CODE !
   LAST-TEXT BUF:DISPOSE ;

;package
