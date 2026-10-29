\ debug-watch.f - REPL watch cells shared by stepper.f and debug.f, which with
\ this file make package DEBUG. The product bakes it after repl.f
\ (native-runtime.f); the hb-stdin recovery engine gets it through
\ `require src/habu/debug.f`. The table lives in dictionary data; fixed DATA
\ header cells publish its address and count to the signal-safe breakpoint handler.

package DEBUG

8 constant BPW-MAX

\ The table stores DATA pointers, so it is declared storage;
\ its helper prints an address as its distance
\ from zero.
BPW-MAX TYPED-BUFFER BPW-TAB ptr n
variable BPW-IDX
variable BPW-LAST

: BPW-PRINT-ADDR ( ptr n -- )
   BYTE-VIEW NULL-PTR BYTE-VIEW - . ;

\ The cell holds the table's ADDRESS, so it is reached through `ptr-field`,
\ the declared pointer-cell door, not by reading a plain DATA offset: nothing
\ fetched straight out of a base address is an address.
: BPW-BASE! ( -- )
   data-base BPWBASE-CELL + 0 ptr-field {: cell:ptr :}
   cell ptr-cell-mark
   0 BPW-TAB cell ! ;

: BPW-N@ ( -- n )
   data-base BPWN-CELL + @ ;

: BPW-N! ( n -- )
   data-base BPWN-CELL + ! ;

: BPW-SLOT ( n -- ptr ptr n )
   BPW-TAB ;

: BPW-FIND ( ptr n -- n )
   {: addr:ptr :}
   0 begin dup BPW-N@ < while
      dup BPW-SLOT @ addr = if exit then
      1+
   repeat
   drop -1 ;

public

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

: BPW. ( -- )
   0 begin dup BPW-N@ < while
      dup BPW-SLOT @ dup BPW-PRINT-ADDR @ .
      1+
   repeat drop ;

: BPW-INSTALL ( -- )
   BPW-BASE!
   BPW-CLEAR ;

BPW-INSTALL

;package
