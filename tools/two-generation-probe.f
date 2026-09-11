\ two-generation-probe.f - child fixture for tools/two-generation-build.f.
\
\ Prints, as ONE line on stdout, the shape the running engine image carries:
\ the checker symbol count, the persisted signature and no-return stores as
\ content bytes and baked cap, the relocation row count, the boot heap
\ (here - data-base) and the data-space cap that heap is bounded by. Every
\ field is read out of the live image, so the line describes the artefact and
\ not the source that made it. The fixture's own definitions land before the
\ read, so each number carries the same small constant of its own; the parent
\ compares two such lines, never a line against a source figure.
\
\ Every engine field goes through a named trusted shim: a seed-lineage engine
\ refuses these names inside a checked definition, and the parent must be able
\ to probe every generation with one fixture.
\
\ Nothing here may print a number the way the engine's own `.` does: that ends
\ the line, and the whole shape has to arrive as one line.

package TWO-GEN-PROBE

48 constant TGP-ZERO               \ ASCII '0'
32 constant TGP-SPACE              \ ASCII ' '

TRUSTED: TGP-SYMS ( -- n ) SYM-N @ ;
TRUSTED: TGP-USIGS ( -- n ) UEND @ CELL + ;
TRUSTED: TGP-UCAP ( -- n ) USIGS-CAP-U @ ;
TRUSTED: TGP-NORETS ( -- n ) NORET-END @ CELL + ;
TRUSTED: TGP-NCAP ( -- n ) NORET-CAP-U @ ;
TRUSTED: TGP-ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;
TRUSTED: TGP-HEAP ( -- n ) here data-base - ;
TRUSTED: TGP-DPCAP ( -- n ) DATA-SIZE PROF-CNT-BYTES - ;

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
