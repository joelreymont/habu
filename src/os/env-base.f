\ env-base.f - shared startup argv/envp access over captured DATA cells.
\ Trusted rows expose the fixed startup cells, raw C-string/vector operations,
\ typed empty result, and bounded temporary-path scratch.
\ Retirement: habu-raw-self-path-4514ffd3.

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

\ The comparison cursors hold C-string addresses, so they are declared pointer
\ cells (dot habu-refuse-a-ptr-5ad2734e); an undeclared cell never holds one.
\ PTR-VARIABLE, not TYPED-VARIABLE: this file loads before src/core/include.f
\ arms the generated-declaration evaluator that the typed definers need.
PTR-VARIABLE ENV-Z
PTR-VARIABLE ENV-A
variable ENV-U

: ENV-Z@ ( -- ptr u8 )
   ENV-Z @ ;

: ENV-A@ ( -- ptr u8 )
   ENV-A @ ;

: ENV-Z! ( ptr u8 -- )
   ENV-Z ! ;

: ENV-A! ( ptr u8 -- )
   ENV-A ! ;

: ENV=? ( ptr u8 ptr u8 n -- bool )
   ENV-U ! ENV-A! ENV-Z!
   ENV-U @ 0 ?do ENV-Z@ i ZBYTE@ ENV-A@ i ZBYTE@ = 0= if unloop ENV-FALSE exit then loop
   ENV-Z@ ENV-U @ ZBYTE@ $3D = ;

TRUSTED: NULL$ ( -- ptr u8 n )
   0 0 ;

PTR-VARIABLE ENV-QA
variable ENV-QU

: ENV-QA@ ( -- ptr u8 )
   ENV-QA @ ;

: ENV-QA! ( ptr u8 -- )
   ENV-QA ! ;

: GETENV ( ptr u8 n -- ptr u8 n )
   ENV-QU ! ENV-QA!
   ENVP-BASE 0= if NULL$ exit then
   0 begin dup ENVP 0= 0= while
      dup ENVP ENV-QA@ ENV-QU @ ENV=? if ENVP ENV-QU @ 1 + ZPTR+ dup ZLEN exit then
      1 +
   repeat
   drop NULL$ ;

$100 constant TMP-PATH-CAP
create TPB TMP-PATH-CAP allot
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

: TMP-PATH-CHECK ( n -- )
   TMP-PATH-CAP > if s" env: TMP-PATH exceeds buffer" 76 die then ;

: TMP-PATH-COPY-SRC ( ptr u8 n -- )
   0 ?do dup i ZBYTE@ TPB TPQ @ 1 + i + ZBYTE! loop drop ;

: TMP-PATH ( ptr u8 n -- ptr u8 n )
   TPU ! TPS!
   s" HB_TMP" GETENV dup 0 = if drop drop s" /tmp" then TPQ ! TPP !
   TPQ @ 1 + TPU @ + TMP-PATH-CHECK
   TPQ @ 0 ?do TPP@ i ZBYTE@ TPB i ZBYTE! loop
   $2F TPB TPQ @ ZBYTE!
   TPS@ TPU @ TMP-PATH-COPY-SRC
   TPB TPQ @ 1 + TPU @ + ;

\ Clear transient environment and path cursors before capturing an image.
: ENV-SNAPSHOT-PREPARE ( -- )
   NULL$ drop ENV-Z!  NULL$ drop ENV-A!  0 ENV-U !
   NULL$ drop ENV-QA!  0 ENV-QU !
   NULL$ drop TPP !  0 TPQ !  NULL$ drop TPS!  0 TPU ! ;
