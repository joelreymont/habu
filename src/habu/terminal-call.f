\ The engine calls that have no continuation, shared by lowering and capture.
require lib/prelude.f
require src/habu/layout.f

package TERMINAL-CALL

\ This engine's actual text extent authenticates the primitive binding; a
\ source definition with the same global name must retain its continuation.
TRUSTED: ENGINE-TEXT ( -- n n )
   data-base RBASE-CELL + @
   dup CODE-OFF - IMAGE-TEXT-SIZE-OFF + @ IMAGE-TEXT-CONTENT-ADJ - ;

public

\ Whether an entry lies in this engine's own text. It is the one test that an
\ engine binding is the engine's: BOUND? below asks it of `throw` and `die`, and
\ src/compiler/native/dict.f HELPER-TARGET of a sealed helper such as (DIV-ZERO).
: IN-ENGINE-TEXT? ( n -- bool ) {: entry:n :}
   ENGINE-TEXT {: base:n bytes:n :}
   entry base >= entry base - bytes < and ;

: BOUND? ( ptr u8 n n -- bool ) {: name:ptr size:n entry:n :}
   name size s" throw" CORE-STR=CI name size s" die" CORE-STR=CI or 0= if false exit then
   name size 0 search-wl entry <> if false exit then
   entry IN-ENGINE-TEXT? ;

;package
