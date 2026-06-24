\ env.f -- process arguments and environment for Linux/aarch64 ELF entry.
\ EM-STARTUP reads argc/argv/envp from the initial process stack and stores
\ the same DATA cells used by the rest of the engine.

data-base constant ENV-DATA
$2D constant ENV-DASH
s" ENV-DATA" s" -- ptr n" TRUST

: ARGC ( -- n )  ENV-DATA ARGC-CELL + @ ;
s" ARGC" s" -- n" TRUST

: ARGV-BASE ( -- ptr n )
   ENV-DATA ARGV-CELL + @ ;
s" ARGV-BASE" s" -- ptr n" TRUST

: ARGV ( i -- z )  8 * ARGV-BASE + @ ;
s" ARGV" s" n -- ptr u8" TRUST

: ENVP-BASE ( -- ptr n )
   ENV-DATA ENVP-CELL + @ ;
s" ENVP-BASE" s" -- ptr n" TRUST

: ENVP ( i -- z )  8 * ENVP-BASE + @ ;
s" ENVP" s" n -- ptr u8" TRUST

: ZLEN ( ptr u8 -- n ) {: z:ptr :}
   0 begin z over + c@ 0= 0= while 1 + repeat ;
s" ZLEN" s" ptr u8 -- n" TRUST

: ARGV$ ( i -- a u )  ARGV dup ZLEN ;
s" ARGV$" s" n -- ptr u8 n" TRUST

: ENV-FALSE ( -- bool ) 0 0= 0= ;
s" ENV-FALSE" s" -- bool" TRUST

: SCRIPT-LOAD-Z? ( ptr u8 -- bool ) {: z:ptr :}
   z c@ ENV-DASH <> IF ENV-FALSE exit THEN
   z 1 + c@ ENV-DASH <> IF ENV-FALSE exit THEN
   z 2 + c@ 108 <> IF ENV-FALSE exit THEN
   z 3 + c@ 111 <> IF ENV-FALSE exit THEN
   z 4 + c@ 97 <> IF ENV-FALSE exit THEN
   z 5 + c@ 100 <> IF ENV-FALSE exit THEN
   z 6 + c@ 0 = ;
s" SCRIPT-LOAD-Z?" s" ptr u8 -- bool" TRUST

: SCRIPT-LOAD? ( -- bool )
   ARGC 1 <= IF ENV-FALSE exit THEN
   1 ARGV SCRIPT-LOAD-Z? ;
s" SCRIPT-LOAD?" s" -- bool" TRUST

: SCRIPT-SEP? ( n -- bool ) {: idx :}
   idx ARGV {: z:ptr :}
   z c@ ENV-DASH <> IF ENV-FALSE exit THEN
   z 1 + c@ ENV-DASH <> IF ENV-FALSE exit THEN
   z 2 + c@ 0 = ;
s" SCRIPT-SEP?" s" n -- bool" TRUST

: SCRIPT-ARG-START ( -- n )
   SCRIPT-LOAD? 0= IF 2 exit THEN
   2 begin dup ARGC < while
      dup SCRIPT-SEP? IF 1 + exit THEN
      1 +
   repeat
   drop ARGC ;
s" SCRIPT-ARG-START" s" -- n" TRUST

: SCRIPT-ARGC ( -- n )
   ARGC SCRIPT-ARG-START -  dup 0 < if drop 0 then ;
s" SCRIPT-ARGC" s" -- n" TRUST

: SCRIPT-ARGV ( i -- z )  SCRIPT-ARG-START + ARGV ;
s" SCRIPT-ARGV" s" n -- ptr u8" TRUST

: SCRIPT-ARGV$ ( i -- a u )  SCRIPT-ARGV dup ZLEN ;
s" SCRIPT-ARGV$" s" n -- ptr u8 n" TRUST

: ENV=? ( ptr u8 ptr u8 n -- bool ) {: z:ptr a:ptr u :}
   u 0 ?do  z i + c@  a i + c@  = 0= IF unloop 0 0= 0= exit THEN  loop
   z u + c@ 61 = ;
s" ENV=?" s" ptr u8 ptr u8 n -- bool" TRUST

: NULL$ ( -- ptr u8 n )
   0 0 ;
s" NULL$" s" -- ptr u8 n" TRUST

: GETENV ( ptr u8 n -- ptr u8 n ) {: a u :}
   ENV-DATA ENVP-CELL + @ 0 = IF NULL$ exit THEN
   0 begin dup ENVP 0= 0= while
      dup ENVP a u ENV=? IF  ENVP u + 1 +  dup ZLEN  exit THEN
      1 + repeat
   drop NULL$ ;
s" GETENV" s" ptr u8 n -- ptr u8 n" TRUST

256 constant TMP-PATH-CAP
create TPB TMP-PATH-CAP allot
variable TPP  variable TPQ
: TPP@ ( -- ptr u8 )
   TPP @ ;
s" TPP@" s" -- ptr u8" TRUST

: TMP-PATH-CHECK ( n -- )
   TMP-PATH-CAP > IF s" env: TMP-PATH exceeds buffer" 76 die THEN ;
s" TMP-PATH-CHECK" s" n --" TRUST

: TMP-PATH ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   s" HB_TMP" GETENV  dup 0 = IF drop drop s" /tmp" THEN  TPQ ! TPP !
   TPQ @ 1 + u + TMP-PATH-CHECK
   TPQ @ 0 ?do  TPP@ i + c@  TPB i + c!  loop
   47 TPB TPQ @ + c!
   u 0 ?do  a i + c@  TPB TPQ @ + 1 + i + c!  loop
   TPB  TPQ @ 1 + u + ;
s" TMP-PATH" s" ptr u8 n -- ptr u8 n" TRUST
