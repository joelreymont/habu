split-str
\ reason.f - a failure's stated text, package REASON. A throw code's ERROR:TEXT$
\ is one static text; REASON:THROW throws a code together with a text the
\ thrower composed, the numbers that made it fail, and REASON:TEXT$ answers it
\ at the catch. ERROR cannot keep it: it is sealed and loads below VEC.
\ CONTRACT. A code leaves its package through REASON:THROW at every place or at
\ none; a throw its own package catches and throws on through REASON:THROW is
\ not a place. A catch reads REASON:TEXT$ before it runs anything else. So a
\ text pairs only with its own throw, unless a cleanup run while that throw
\ unwinds (`finally`) throws the same code through REASON:THROW. A code a task
\ ends with, as TASK:JOIN's err, carries no text: the joiner's REASON:TEXT$
\ reads its own. A text is written between REASON:OPEN and REASON:STOP; only
\ REASON's part words and pure reads run between them, since an OPEN there
\ would clear the text being written.
\ STORAGE CLASS. TASK-LOCAL: per task one code cell, two vector cells, the
\ last text and the next, and a hide count, in the REASON-ABI band
\ (src/habu/layout.f); a vector is made on its first use and TASK releases
\ both as each activation ends; the main task's vectors last the process.

require src/habu/layout.f
require src/core/exit-status.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/fmt.f
require src/lib/unicode/visible.f

package REASON
EXPORT BOOT:data-base

\ The band states each cell's width in bytes; a narrower one would let the
\ last cell's store reach the next band.
: BAND-AGREE ( -- )
   REASON-ABI:NEXT-OFF REASON-ABI:TEXT-OFF -  CELL <>
   REASON-ABI:END REASON-ABI:HIDE-OFF -  CELL <>  or if
      s" reason: a cell differs from the REASON-ABI band's widths"
      EXIT-STATUS:INTERNAL die
   then ;
BAND-AGREE

\ The code of the task's last REASON:THROW, zero while LAST-TEXT holds none.
: LAST-CODE ( -- ^n )
   data-base REASON-ABI:CODE-OFF + ;

: HIDE-COUNT ( -- ^n )
   data-base REASON-ABI:HIDE-OFF + ;

\ The band's text cells hold a vector's address.
CAST: >TEXT-CELL ( ^n -- ^^VEC:vector<u8> )

: LAST-CELL ( -- ^^VEC:vector<u8> )
   data-base REASON-ABI:TEXT-OFF + >TEXT-CELL ;

: NEXT-CELL ( -- ^^VEC:vector<u8> )
   data-base REASON-ABI:NEXT-OFF + >TEXT-CELL ;

\ The cell's vector, made on its first use; the cell is NULL-PTR until then.
: MADE ( ^^VEC:vector<u8> -- ^VEC:vector<u8> ) {: c:^ :}
   c @ NULL-PTR = if HEAP:ALLOCATOR 0 [type] u8 VEC:NEW c ! then
   c @ ;

: LAST-TEXT ( -- ^VEC:vector<u8> )  LAST-CELL MADE ;

: NEXT-TEXT ( -- ^VEC:vector<u8> )  NEXT-CELL MADE ;

: DELETE-TEXT ( ^^VEC:vector<u8> -- ) {: c:^ :}
   c @ VEC:DELETE  NULL-PTR c ! ;

public

\ Whether input is hidden: inside HIDDEN.
: HIDDEN? ( -- bool )  HIDE-COUNT @ 0 > ;

\ Runs the quotation with VISIBLE, QUOTE and BYTE writing `(hidden)` for the
\ input's bytes; nested runs stay hidden until the outermost ends.
: HIDDEN ( R [ R -- S ] -- S )
   {: body :}
   1 HIDE-COUNT +!  body [: -1 HIDE-COUNT +! ;] finally ;

\ The text of the calling task's last REASON:THROW when that threw code n, else
\ empty; valid until the task's next REASON:THROW or REASON:STOP.
: TEXT$ ( n -- str ) {: code:n :}
   code 0=  LAST-CODE @ code <>  or if s" " exit then
   LAST-TEXT VEC:SPAN SPAN:$ ;

\ Throws code n with the span kept as the calling task's text, replacing any
\ earlier one. The span is appended after the old text, then moved to the
\ front, so it may be the caller's scratch or the old text, a rethrow's
\ REASON:TEXT$; a copy that throws passes its own code on with no text kept.
: THROW ( str n -- ) {: a:^ u:n code:n :}
   0 LAST-CODE !
   LAST-TEXT {: t:^ :}
   t VEC:COUNT {: k:n :}
   a u t VEC:APPEND
   t VEC:SPAN SPAN:$ drop {: p:^ :}
   p k + p u BYTE-COPY
   t u VEC:RESIZE
   code LAST-CODE !
   code throw ;

\ The most bytes of the input a quoted part shows; a caller cuts there and
\ passes the cut flag.
32 constant QUOTE-MOST

\ Empties the next text.
: OPEN ( -- )  NEXT-TEXT 0 VEC:RESIZE ;

\ Appends the span as given.
: APPEND ( str -- )  NEXT-TEXT VEC:APPEND ;

\ Appends `L:C: `, the line then the column.
: AT ( n n -- ) {: line:n col:n :}
   line FMT:INT$ APPEND  s" :" APPEND  col FMT:INT$ APPEND  s" : " APPEND ;

: EXPECTED ( -- )  s" expected " APPEND ;

: FOUND ( -- )  s" , found " APPEND ;

: FIX ( -- )  s" ; " APPEND ;

\ Appends the span's visible form, or `(hidden)` inside HIDDEN.
: VISIBLE ( str -- ) {: a:^ u:n :}
   HIDDEN? if s" (hidden)" APPEND exit then
   a u NEXT-TEXT UNICODE-VISIBLE:APPEND ;

\ Appends the span's visible form in backticks, with `...` before the closing
\ one when the flag is true, or `(hidden)` alone inside HIDDEN.
: QUOTE ( str bool -- ) {: a:^ u:n cut:bool :}
   HIDDEN? if s" (hidden)" APPEND exit then
   s" `" APPEND
   a u VISIBLE
   cut if s" ..." APPEND then
   s" `" APPEND ;

\ Appends `byte $HH`, or `(hidden)` inside HIDDEN.
: BYTE ( n -- )
   HIDDEN? if drop s" (hidden)" APPEND exit then
   s" byte $" APPEND  2 FMT:HEX-WIDTH$ APPEND ;

\ Throws code n with the next text through REASON:THROW.
: STOP ( n -- ) {: code:n :}
   NEXT-TEXT VEC:SPAN SPAN:$ code THROW ;

\ Deletes the calling task's texts.
: RELEASE ( -- )
   0 LAST-CODE !
   LAST-CELL DELETE-TEXT
   NEXT-CELL DELETE-TEXT ;

;package
