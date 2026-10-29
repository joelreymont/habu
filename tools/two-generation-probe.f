\ two-generation-probe.f - child fixture for tools/two-generation-build.f: prints
\ on ONE stdout line the live image's checker symbol count, signature and
\ no-return store bytes and caps, relocation rows, boot heap and data-space cap.
\ Its own definitions add the same constant to each number, so the parent compares
\ two lines, never a line against a source figure. Nothing may print with `.`:
\ that ends the line.

package TWO-GEN-PROBE

48 constant TGP-ZERO               \ ASCII '0'
32 constant TGP-SPACE              \ ASCII ' '

: TGP-SYMS ( -- n ) SYM-N @ ;
: TGP-USIGS ( -- n ) UEND @ CELL + ;
: TGP-UCAP ( -- n ) USIGS-CAP-U @ ;
: TGP-NORETS ( -- n ) NORET-END @ CELL + ;
: TGP-NCAP ( -- n ) NORET-CAP-U @ ;
: TGP-ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;
: TGP-HEAP ( -- n ) here data-base - ;
: TGP-DPCAP ( -- n ) DATA-SIZE PROF-CNT-BYTES - ;

: TGP-U. ( n -- ) {: v:n :}
   v 0 < if s" two-gen-probe: negative count" 76 die then
   v 10 >= if v 10 / RECURSE then
   v 10 mod TGP-ZERO + emit ;

: TGP-FIELD ( ptr u8 n n -- ) {: a:ptr u:n v:n :}
   a u type v TGP-U. TGP-SPACE emit ;

: TGP-MAIN ( -- )
   s" sym-n " TGP-SYMS TGP-FIELD
   s" usigs " TGP-USIGS TGP-FIELD
   s" cap " TGP-UCAP TGP-FIELD
   s" norets " TGP-NORETS TGP-FIELD
   s" cap " TGP-NCAP TGP-FIELD
   s" rows " TGP-ROWS TGP-FIELD
   s" heap " TGP-HEAP TGP-FIELD
   s" dp-cap " TGP-DPCAP TGP-FIELD
   cr ;

TGP-MAIN

;package
