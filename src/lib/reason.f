split-str
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
\ reads its own. A text is written between REASON:OPEN and REASON:STOP; only
\ REASON's part words and pure reads run between them, since an OPEN there
\ would clear the text being written.
\ STORAGE CLASS. TASK-LOCAL: one code cell and two BUFs per task, the last text
\ and the next, in the REASON-ABI band (src/habu/layout.f); a buffer grows on
\ its first append and TASK releases both as each activation ends; the main
\ task's buffers last the process.

require src/habu/layout.f
require src/core/exit-status.f
require src/lib/byte-buffer.f
require src/lib/fmt.f
require src/lib/unicode/visible.f

package REASON
EXPORT BOOT:data-base

\ src/habu/layout.f loads below BUF, so the band states the headers' width in
\ cells; a narrower band would let BUF write into the next one.
: BAND-AGREE ( -- )
   REASON-ABI:NEXT-OFF REASON-ABI:TEXT-OFF -  BUF:HDR-BYTES <>
   REASON-ABI:END REASON-ABI:NEXT-OFF -  BUF:HDR-BYTES <>  or if
      s" reason: BUF's header differs from the REASON-ABI band's text bytes"
      EXIT-STATUS:INTERNAL die
   then ;
BAND-AGREE

\ The code of the task's last REASON:THROW, zero while LAST-TEXT holds none.
: LAST-CODE ( -- ^n )
   data-base REASON-ABI:CODE-OFF + ;

: LAST-TEXT ( -- ^u8 )
   data-base BYTE-VIEW REASON-ABI:TEXT-OFF + ;

: NEXT-TEXT ( -- ^u8 )
   data-base BYTE-VIEW REASON-ABI:NEXT-OFF + ;

public

\ The text of the calling task's last REASON:THROW when that threw code n, else
\ empty; valid until the task's next REASON:THROW or REASON:STOP.
: TEXT$ ( n -- ^u8 n ) {: code:n :}
   code 0=  LAST-CODE @ code <>  or if s" " exit then
   LAST-TEXT BUF:SPAN$ BUF:BLEN>N ;

\ Throws code n with the span kept as the calling task's text, replacing any
\ earlier one. The span is copied first, so it may be the caller's scratch; a
\ copy that throws passes its own code on with no text kept.
: THROW ( ^u8 n n -- ) {: a:^ u:n code:n :}
   0 LAST-CODE !
   LAST-TEXT BUF:CLEAR
   a u BUF:N>BLEN LAST-TEXT BUF:APPEND-SPAN
   code LAST-CODE !
   code throw ;

\ The most bytes of the input a quoted part shows; a caller cuts there and
\ passes the cut flag.
32 constant QUOTE-MOST

\ Empties the next text.
: OPEN ( -- )  NEXT-TEXT BUF:CLEAR ;

\ Appends the span as given.
: APPEND ( ^u8 n -- )  BUF:N>BLEN NEXT-TEXT BUF:APPEND-SPAN ;

\ Appends `L:C: `, the line then the column.
: AT ( n n -- ) {: line:n col:n :}
   line FMT:INT$ APPEND  s" :" APPEND  col FMT:INT$ APPEND  s" : " APPEND ;

: EXPECTED ( -- )  s" expected " APPEND ;

: FOUND ( -- )  s" , found " APPEND ;

: FIX ( -- )  s" ; " APPEND ;

\ Appends the span's visible form.
: VISIBLE ( ^u8 n -- ) {: a:^ u:n :}
   a u NEXT-TEXT UNICODE-VISIBLE:APPEND ;

\ Appends the span's visible form in backticks, with `...` before the closing
\ one when the flag is true.
: QUOTE ( ^u8 n bool -- ) {: a:^ u:n cut:bool :}
   s" `" APPEND
   a u VISIBLE
   cut if s" ..." APPEND then
   s" `" APPEND ;

\ Appends `byte $HH`.
: BYTE ( n -- )  s" byte $" APPEND  2 FMT:HEX-WIDTH$ APPEND ;

\ Throws code n with the next text through REASON:THROW.
: STOP ( n -- ) {: code:n :}
   NEXT-TEXT BUF:SPAN$ BUF:BLEN>N code THROW ;

\ Disposes the calling task's texts.
: RELEASE ( -- )
   0 LAST-CODE !
   LAST-TEXT BUF:DISPOSE
   NEXT-TEXT BUF:DISPOSE ;

;package
