split-str
\ env-base.f - startup argv/envp access. ARGC-CELL, ARGV-CELL and ENVP-CELL sit
\ at fixed offsets below DATA-START; every process, image or task reads them
\ through its own DATA base.

package ENV-BASE
private
EXPORT BOOT:data-base
: ENV-DATA ( -- ^n ) data-base ;
global
$2D constant ENV-DASH

: ARGC ( -- n )
   ENV-DATA ARGC-CELL + @ ;

: ARGV-BASE ( -- ^^u8 )
   ENV-DATA ARGV-CELL CELL / ptr-field @ ;

: ARGV ( n -- ^u8 )
   ARGV-BASE swap ptr-field @ ;

: ENVP-BASE ( -- ^^u8 )
   ENV-DATA ENVP-CELL CELL / ptr-field @ ;
;package

: ENVP ( n -- ^u8 )
   ENVP-BASE swap ptr-field @ ;

: ZBYTE@ ( ^u8 n -- u8 )
   + c@ ;

: ZBYTE! ( u8 ^u8 n -- )
   + c! ;

: ZPTR+ ( ^u8 n -- ^u8 )
   + ;

: ZLEN ( ^u8 -- n )
   0 begin 2dup ZBYTE@ 0= 0= while 1 + repeat swap drop ;

: ARGV$ ( n -- str )
   ARGV dup ZLEN ;

: ENV-FALSE ( -- bool )
   0 0= 0= ;

: ENV=? ( ^u8 str -- bool )
   {: z:^ a:^ u:n :}
   u 0 ?do z i ZBYTE@ a i ZBYTE@ = 0= if unloop ENV-FALSE exit then loop
   z u ZBYTE@ $3D = ;

: NULL$ ( -- str )
   NULL-PTR 0 ;

\ CURRENT$ reads a variable unrecorded; GETENV records the read in the
\ active compile-reads recording.
package ENV-INPUT
public

: CURRENT$ ( str -- str )
   {: query:^ queryu:n :}
   ENVP-BASE 0= if NULL$ exit then
   0 begin dup ENVP 0= 0= while
      dup ENVP query queryu ENV=? if ENVP queryu 1 + ZPTR+ dup ZLEN exit then
      1 +
   repeat
   drop NULL$ ;

;package

: GETENV ( str -- str )
   {: query:^ queryu:n :}
   query queryu ENV-INPUT:CURRENT$ {: value:^ valueu:n :}
   query queryu value valueu COMPILE-READS:ENV+
   value valueu ;

\ TMP-PATH's answer is a path of at most PATH-CAP bytes with no NUL: a
\ caller that opens it hands it to PATH0 or PATHZ, which add one in a buffer
\ of their own.
create TPB PATH-CAP SCRATCH-ALLOT
PTR-VARIABLE TPP
SCRATCH-VARIABLE TPQ
PTR-VARIABLE TPS
SCRATCH-VARIABLE TPU

: TPP@ ( -- ^u8 )
   TPP @ ;

: TPS@ ( -- ^u8 )
   TPS @ ;

: TPS! ( ^u8 -- )
   TPS ! ;

\ The name has to fit the room the root and its slash leave. Its length is
\ compared with that room, never added to the root's, so neither a negative
\ length nor one that wraps the sum passes.
: TMP-PATH-CHECK ( n -- )
   dup 0 < swap PATH-CAP TPQ @ - 1 - > or
   if s" env: TMP-PATH exceeds buffer" EXIT-STATUS:FULL die then ;

: TMP-PATH-COPY-SRC ( str -- )
   0 ?do dup i ZBYTE@ TPB TPQ @ 1 + i + ZBYTE! loop drop ;

: TMP-PATH ( str -- str )
   TPU ! TPS!
   s" TMPDIR" GETENV dup 0 = if drop drop s" /tmp" then TPQ ! TPP !
   TPU @ TMP-PATH-CHECK
   TPQ @ 0 ?do TPP@ i ZBYTE@ TPB i ZBYTE! loop
   $2F TPB TPQ @ ZBYTE!
   TPS@ TPU @ TMP-PATH-COPY-SRC
   TPB TPQ @ 1 + TPU @ + ;
