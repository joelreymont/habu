\ env-base.f - shared startup argv/envp access over captured DATA cells.

PTR-VARIABLE ENV-DATA-PTR
data-base ENV-DATA-PTR !
: ENV-DATA ( -- ptr n ) ENV-DATA-PTR @ ;
$2D constant ENV-DASH

: ARGC ( -- n )
   ENV-DATA ARGC-CELL + @ ;

: ARGV-BASE ( -- ptr ptr u8 )
   ENV-DATA ARGV-CELL CELL / ptr-field @ ;

: ARGV ( n -- ptr u8 )
   ARGV-BASE swap ptr-field @ ;

: ENVP-BASE ( -- ptr ptr u8 )
   ENV-DATA ENVP-CELL CELL / ptr-field @ ;

: ENVP ( n -- ptr u8 )
   ENVP-BASE swap ptr-field @ ;

: ZBYTE@ ( ptr u8 n -- u8 )
   + c@ ;

: ZBYTE! ( u8 ptr u8 n -- )
   + c! ;

: ZPTR+ ( ptr u8 n -- ptr u8 )
   + ;

: ZLEN ( ptr u8 -- n )
   0 begin 2dup ZBYTE@ 0= 0= while 1 + repeat swap drop ;

: ARGV$ ( n -- ptr u8 n )
   ARGV dup ZLEN ;

: ENV-FALSE ( -- bool )
   0 0= 0= ;

: ENV=? ( ptr u8 ptr u8 n -- bool )
   {: z:ptr a:ptr u:n :}
   u 0 ?do z i ZBYTE@ a i ZBYTE@ = 0= if unloop ENV-FALSE exit then loop
   z u ZBYTE@ $3D = ;

: NULL$ ( -- ptr u8 n )
   NULL-PTR 0 ;

\ Source loaders observe environment reads alongside their filesystem inputs.
\ The optional callback starts empty in each process, including saved images.
package ENV-INPUT
private

variable OBSERVER
CAST: OBSERVER-XT ( n -- [ ptr u8 n ptr u8 n -- ] )
CAST: OBSERVER-N ( [ ptr u8 n ptr u8 n -- ] -- n )

public

: CURRENT$ ( ptr u8 n -- ptr u8 n )
   {: query:ptr queryu:n :}
   ENVP-BASE 0= if NULL$ exit then
   0 begin dup ENVP 0= 0= while
      dup ENVP query queryu ENV=? if ENVP queryu 1 + ZPTR+ dup ZLEN exit then
      1 +
   repeat
   drop NULL$ ;

: USE ( [ ptr u8 n ptr u8 n -- ] -- ) OBSERVER-N OBSERVER xt! ;

: RESET ( -- ) 0 OBSERVER ! ;

: OBSERVE ( ptr u8 n ptr u8 n -- )
   OBSERVER @ dup 0= if drop 2drop 2drop exit then OBSERVER-XT execute ;

: OWNED-CELLS ( [ ptr u8 -- ] -- )
   OBSERVER BYTE-VIEW swap execute ;

;package

: GETENV ( ptr u8 n -- ptr u8 n )
   {: query:ptr queryu:n :}
   query queryu ENV-INPUT:CURRENT$ {: value:ptr valueu:n :}
   query queryu value valueu ENV-INPUT:OBSERVE
   value valueu ;

\ TMP-PATH's answer is a path of at most PATH-CAP bytes (src/core/util.f) with
\ no NUL: a caller that opens it hands it to PATH0 or PATHZ, which add one in
\ a buffer of their own.
create TPB PATH-CAP allot
PTR-VARIABLE TPP
variable TPQ
PTR-VARIABLE TPS
variable TPU

: TPP@ ( -- ptr u8 )
   TPP @ ;

: TPS@ ( -- ptr u8 )
   TPS @ ;

: TPS! ( ptr u8 -- )
   TPS ! ;

\ The name has to fit the room the root and its slash leave. Its length is
\ compared with that room, never added to the root's, so neither a negative
\ length nor one that wraps the sum passes.
: TMP-PATH-CHECK ( n -- )
   dup 0 < swap PATH-CAP TPQ @ - 1 - > or
   if s" env: TMP-PATH exceeds buffer" 76 die then ;

: TMP-PATH-COPY-SRC ( ptr u8 n -- )
   0 ?do dup i ZBYTE@ TPB TPQ @ 1 + i + ZBYTE! loop drop ;

: TMP-PATH ( ptr u8 n -- ptr u8 n )
   TPU ! TPS!
   s" TMPDIR" GETENV dup 0 = if drop drop s" /tmp" then TPQ ! TPP !
   TPU @ TMP-PATH-CHECK
   TPQ @ 0 ?do TPP@ i ZBYTE@ TPB i ZBYTE! loop
   $2F TPB TPQ @ ZBYTE!
   TPS@ TPU @ TMP-PATH-COPY-SRC
   TPB TPQ @ 1 + TPU @ + ;

\ Clear transient path cursors before capturing an image.
: ENV-SNAPSHOT-PREPARE ( -- )
   ENV-INPUT:RESET
   NULL$ drop TPP !  0 TPQ !  NULL$ drop TPS!  0 TPU ! ;
