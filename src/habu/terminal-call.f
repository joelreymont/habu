\ The engine calls that have no continuation, shared by lowering and capture.
require src/lib/prelude.f
require src/habu/layout.f
require src/habu/wl.f

package TERMINAL-CALL

\ This engine's actual text extent, as boot published it (layout.f
\ TEXT-SIZE-CELL), authenticates the primitive binding; a source definition
\ with the same name must retain its continuation.
: ENGINE-TEXT ( -- n n )
   data-base RBASE-CELL + @  data-base TEXT-SIZE-CELL + @ ;

: IN-ENGINE-TEXT? ( n -- bool ) {: entry:n :}
   ENGINE-TEXT {: base:n bytes:n :}
   entry base >= entry base - bytes < and ;

public

\ Whether a call of `throw` or `die` binds the engine's own word.
: BOUND? ( ptr u8 n n -- bool ) {: name:ptr size:n entry:n :}
   name size s" throw" CORE-STR=CI name size s" die" CORE-STR=CI or 0= if false exit then
   name size WL:PRIMITIVES search-wl entry <> if false exit then
   entry IN-ENGINE-TEXT? ;

;package
