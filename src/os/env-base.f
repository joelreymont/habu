\ env-base.f - shared startup argv/envp access over captured DATA cells.

data-base constant ENV-DATA
$2D constant ENV-DASH
s" ENV-DATA" s" -- ptr n" TRUST
s" ENV-DASH" s" -- n" TRUST

: ARGC ( -- n )
   ENV-DATA ARGC-CELL + @ ;
s" ARGC" s" -- n" TRUST

TRUSTED: ARGV-BASE ( -- ptr ptr u8 )
   ENV-DATA ARGV-CELL + @ ;

: ARGV ( n -- ptr u8 )
   ARGV-BASE swap ptr-field @ ;

TRUSTED: ENVP-BASE ( -- ptr ptr u8 )
   ENV-DATA ENVP-CELL + @ ;

: ENVP ( n -- ptr u8 )
   ENVP-BASE swap ptr-field @ ;

: ZLEN ( ptr u8 -- n ) {: z:ptr :}
   0 begin z over + c@ 0= 0= while 1 + repeat ;
s" ZLEN" s" ptr u8 -- n" TRUST

: ARGV$ ( i -- a u )
   ARGV dup ZLEN ;
s" ARGV$" s" n -- ptr u8 n" TRUST

: ENV-FALSE ( -- bool )
   0 0= 0= ;
s" ENV-FALSE" s" -- bool" TRUST

: ENV=? ( ptr u8 ptr u8 n -- bool ) {: z:ptr a:ptr u :}
   u 0 ?do z i + c@ a i + c@ = 0= if unloop 0 0= 0= exit then loop
   z u + c@ 61 = ;
s" ENV=?" s" ptr u8 ptr u8 n -- bool" TRUST

TRUSTED: NULL$ ( -- ptr u8 n )
   0 0 ;

: GETENV ( ptr u8 n -- ptr u8 n ) {: a u :}
   ENVP-BASE 0= if NULL$ exit then
   0 begin dup ENVP 0= 0= while
      dup ENVP a u ENV=? if ENVP u + 1 + dup ZLEN exit then
      1 +
   repeat
   drop NULL$ ;
s" GETENV" s" ptr u8 n -- ptr u8 n" TRUST

256 constant TMP-PATH-CAP
s" TMP-PATH-CAP" s" -- n" TRUST
create TPB TMP-PATH-CAP allot
variable TPP
variable TPQ

: TPP@ ( -- ptr u8 )
   TPP @ ;
s" TPP@" s" -- ptr u8" TRUST

: TMP-PATH-CHECK ( n -- )
   TMP-PATH-CAP > if s" env: TMP-PATH exceeds buffer" 76 die then ;
s" TMP-PATH-CHECK" s" n --" TRUST

: TMP-PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   s" HB_TMP" GETENV dup 0 = if drop drop s" /tmp" then TPQ ! TPP !
   TPQ @ 1 + u + TMP-PATH-CHECK
   TPQ @ 0 ?do TPP@ i + c@ TPB i + c! loop
   47 TPB TPQ @ + c!
   u 0 ?do a i + c@ TPB TPQ @ + 1 + i + c! loop
   TPB TPQ @ 1 + u + ;
s" TMP-PATH" s" ptr u8 n -- ptr u8 n" TRUST
