\ image.fs -- shared executable image buffer for bootstrap target writers.
\ Both buffers the seed uses -- this one and each writer's code scratch -- follow
\ what the emission needs. They used to be carved from the dictionary at a fixed
\ size, so a program that outgrew them was either refused by a magic page or,
\ because this cursor had no bounds check at all, written straight past the end
\ into the Gforth dictionary. Growing to size removes both failures.

s" image: buffer overrun" exception constant E-M-OVER

\ Grow a heap buffer so it holds at least n bytes. The same idiom the IR buffer
\ in icode.fs uses: allocate once, resize on demand, throw when the heap says no.
: BUF-FIT ( n buf cap -- )  {: n bp cp :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   n cp @ <= if exit then
   cp @ if  bp @ n resize throw  else  n allocate throw  then
   bp !  n cp ! ;

variable MBUF-A   variable MBUF-CAP
variable MP
variable MLEN

: MBUF ( -- addr )  MBUF-A @ ;

: M-HERE ( -- off )  MP @ MBUF - ;

\ Resizing can move the buffer, so carry the cursor across the move as an offset.
: M-FIT ( n -- )  M-HERE {: at :}  MBUF-A MBUF-CAP BUF-FIT  MBUF at + MP ! ; \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.

: M-RESET ( -- )  MBUF MP ! ;

\ Name the buffer, what the write needed and what it has. An overrun is never a
\ silent scribble past the end and never a bare exit.
: M-ROOM ( n -- )  M-HERE + {: need :} \ typed-local-lint: allow-bare-local - stock Gforth rejects Habu type suffixes.
   need MBUF-CAP @ > if
      cr ." cg: image buffer overrun: MBUF needs " need .
      ." bytes, has " MBUF-CAP @ . cr
      E-M-OVER throw
   then ;

: M8  ( b -- )  1 M-ROOM  MP @ c!  1 MP +! ;

: M16 ( h -- )  dup M8  8 rshift M8 ;

: M32 ( w -- )  dup M16  16 rshift M16 ;

: M64 ( x -- )  dup M32  32 rshift M32 ;

: M-ZEROS ( n -- )  0 max 0 ?do 0 M8 loop ;

: M-BYTES ( addr u -- )  dup M-ROOM  dup >r  MP @ swap move  r> MP +! ;

: M-NAME16 ( addr u -- )  dup >r  M-BYTES  16 r> - M-ZEROS ;

: M-PAD ( off -- )  M-HERE - M-ZEROS ;
