\ debug-watch.f - shared REPL watch cells for stepper and breakpoints.
\ Loaded on demand: `require src/habu/debug.f` pulls this file and stepper.f
\ with it. Nothing outside the debugger requires it, so it is not in the
\ engine manifest (src/habu/native-runtime.f).
\
\ Load after repl.f and before stepper/debug.f. The watch table itself lives in
\ normal dictionary data; fixed DATA header cells only publish its address/count
\ to the signal-safe breakpoint handler.

8 constant BPW-MAX

\ The table stores DATA pointers, so it is declared storage and needs no trust
\ row of its own (dot habu-refuse-a-ptr-5ad2734e); its helpers refine one fixed
\ DATA cell and print its raw address.
\ Retirement: habu-builder-trust-rows-c5d41af6.
BPW-MAX TYPED-BUFFER BPW-TAB ptr n
variable BPW-IDX
variable BPW-LAST

TRUSTED: BPW-PRINT-ADDR ( ptr n -- )
   . ;

TRUSTED: BPW-DATA-CELL ( n -- ptr n )
   DATAB + ;

: BPW-BASE! ( -- )
   DATAB BPWBASE-CELL + {: cell:ptr :}
   cell ptr-cell-mark
   0 BPW-TAB cell ! ;

: BPW-N@ ( -- n )
   DATAB BPWN-CELL + @ ;

: BPW-N! ( n -- )
   DATAB BPWN-CELL + ! ;

: BPW-SLOT ( n -- ptr ptr n )
   BPW-TAB ;

: BPW-FIND ( ptr n -- n )
   {: addr:ptr :}
   0 begin dup BPW-N@ < while
      dup BPW-SLOT @ addr = if exit then
      1+
   repeat
   drop -1 ;

: BPW-CLEAR ( -- )
   0 BPW-N! ;

: BPW+ ( ptr n -- )
   {: addr:ptr :}
   addr BPW-FIND 0 >= if exit then
   BPW-N@ BPW-MAX >= if s" bpw: table full" type 76 throw then
   addr BPW-N@ BPW-SLOT !
   BPW-N@ 1+ BPW-N! ;

: BPW- ( ptr n -- )
   BPW-FIND dup 0 < if drop exit then
   BPW-IDX !
   BPW-N@ 1- BPW-LAST !
   BPW-LAST @ BPW-IDX @ <> if BPW-LAST @ BPW-SLOT @ BPW-IDX @ BPW-SLOT ! then
   BPW-LAST @ BPW-N! ;

: BPW-CELL+ ( n -- )
   BPW-DATA-CELL BPW+ ;

: BPW. ( -- )
   0 begin dup BPW-N@ < while
      dup BPW-SLOT @ dup BPW-PRINT-ADDR @ .
      1+
   repeat drop ;

: BPW-DUMP ( -- )
   BPW-N@ 0 <= if exit then
   s" watch:" type cr
   BPW. ;

: BPW-INSTALL ( -- )
   BPW-BASE!
   BPW-CLEAR ;

BPW-INSTALL
