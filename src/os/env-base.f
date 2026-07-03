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

: ZBYTE@ ( ptr u8 n -- u8 )
   + c@ ;
s" ZBYTE@" s" ptr u8 n -- u8" TRUST

: ZBYTE! ( u8 ptr u8 n -- )
   + c! ;
s" ZBYTE!" s" u8 ptr u8 n --" TRUST

: ZPTR+ ( ptr u8 n -- ptr u8 )
   + ;
s" ZPTR+" s" ptr u8 n -- ptr u8" TRUST

: ZLEN ( ptr u8 -- n )
   0 begin 2dup ZBYTE@ 0= 0= while 1 + repeat swap drop ;
s" ZLEN" s" ptr u8 -- n" TRUST

: ARGV$ ( i -- a u )
   ARGV dup ZLEN ;
s" ARGV$" s" n -- ptr u8 n" TRUST

: ENV-FALSE ( -- bool )
   0 0= 0= ;
s" ENV-FALSE" s" -- bool" TRUST

variable ENV-Z
variable ENV-A
variable ENV-U

: ENV-Z-FIELD ( -- ptr ptr u8 )
   ENV-Z 0 ptr-field ;

: ENV-A-FIELD ( -- ptr ptr u8 )
   ENV-A 0 ptr-field ;

: ENV-Z@ ( -- ptr u8 )
   ENV-Z-FIELD @ ;

: ENV-A@ ( -- ptr u8 )
   ENV-A-FIELD @ ;

: ENV-Z! ( ptr u8 -- )
   ENV-Z-FIELD ! ;

: ENV-A! ( ptr u8 -- )
   ENV-A-FIELD ! ;

: ENV=? ( ptr u8 ptr u8 n -- bool )
   ENV-U ! ENV-A! ENV-Z!
   ENV-U @ 0 ?do ENV-Z@ i ZBYTE@ ENV-A@ i ZBYTE@ = 0= if unloop ENV-FALSE exit then loop
   ENV-Z@ ENV-U @ ZBYTE@ $3D = ;
s" ENV=?" s" ptr u8 ptr u8 n -- bool" TRUST

TRUSTED: NULL$ ( -- ptr u8 n )
   0 0 ;

variable ENV-QA
variable ENV-QU

: ENV-QA-FIELD ( -- ptr ptr u8 )
   ENV-QA 0 ptr-field ;

: ENV-QA@ ( -- ptr u8 )
   ENV-QA-FIELD @ ;

: ENV-QA! ( ptr u8 -- )
   ENV-QA-FIELD ! ;

: GETENV ( ptr u8 n -- ptr u8 n )
   ENV-QU ! ENV-QA!
   ENVP-BASE 0= if NULL$ exit then
   0 begin dup ENVP 0= 0= while
      dup ENVP ENV-QA@ ENV-QU @ ENV=? if ENVP ENV-QU @ 1 + ZPTR+ dup ZLEN exit then
      1 +
   repeat
   drop NULL$ ;
s" GETENV" s" ptr u8 n -- ptr u8 n" TRUST

$100 constant TMP-PATH-CAP
s" TMP-PATH-CAP" s" -- n" TRUST
create TPB TMP-PATH-CAP allot
variable TPP
variable TPQ
variable TPS
variable TPU

: TPP@ ( -- ptr u8 )
   TPP @ ;
s" TPP@" s" -- ptr u8" TRUST

: TPS-FIELD ( -- ptr ptr u8 )
   TPS 0 ptr-field ;

: TPS@ ( -- ptr u8 )
   TPS-FIELD @ ;

: TPS! ( ptr u8 -- )
   TPS-FIELD ! ;

: TMP-PATH-CHECK ( n -- )
   TMP-PATH-CAP > if s" env: TMP-PATH exceeds buffer" 76 die then ;
s" TMP-PATH-CHECK" s" n --" TRUST

: TMP-PATH-COPY-SRC ( ptr u8 n -- )
   0 ?do dup i ZBYTE@ TPB TPQ @ 1 + i + ZBYTE! loop drop ;
s" TMP-PATH-COPY-SRC" s" ptr u8 n --" TRUST

: TMP-PATH ( ptr u8 n -- ptr u8 n )
   TPU ! TPS!
   s" HB_TMP" GETENV dup 0 = if drop drop s" /tmp" then TPQ ! TPP !
   TPQ @ 1 + TPU @ + TMP-PATH-CHECK
   TPQ @ 0 ?do TPP@ i ZBYTE@ TPB i ZBYTE! loop
   $2F TPB TPQ @ ZBYTE!
   TPS@ TPU @ TMP-PATH-COPY-SRC
   TPB TPQ @ 1 + TPU @ + ;
s" TMP-PATH" s" ptr u8 n -- ptr u8 n" TRUST
