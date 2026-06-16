\ env.f — process arguments and environment. The engine runs under dyld
\ (LC_MAIN): main(argc, argv, envp) arrives in x0-x2 and EM-STARTUP saves them
\ to DATA header cells at entry (snapshot boots re-store the live values).

$340000000 constant ENV-DATA
$3670 constant ARGC-CELL
$3678 constant ARGV-CELL
$3680 constant ENVP-CELL
s" ENV-DATA" s" -- ptr n" TRUST

: ARGC ( -- n )  ENV-DATA ARGC-CELL + @ ;

: ARGV-BASE ENV-DATA ARGV-CELL + @ ;
s" ARGV-BASE" s" -- ptr n" TRUST

: ARGV ( i -- z )  8 * ARGV-BASE + @ ;   \ argv[i], NUL-terminated
s" ARGV" s" n -- ptr u8" TRUST

: ENVP-BASE ENV-DATA ENVP-CELL + @ ;
s" ENVP-BASE" s" -- ptr n" TRUST

: ENVP ( i -- z )  8 * ENVP-BASE + @ ;   \ envp[i], 0 at the end
s" ENVP" s" n -- ptr u8" TRUST

: ZLEN {: z:ptr :}  0 begin z over + c@ 0= 0= while 1 + repeat ;

: ARGV$ ( i -- a u )  ARGV dup ZLEN ;

: SCRIPT-ARGC ( -- n )
   ARGC 2 -  dup 0 < if drop 0 then ;

: SCRIPT-ARGV ( i -- z )  2 + ARGV ;

: SCRIPT-ARGV$ ( i -- a u )  SCRIPT-ARGV dup ZLEN ;

\ does the c-string z start with name a/u followed by '='?
: ENV=? {: z:ptr a:ptr u :}
   u 0 ?do  z i + c@  a i + c@  = 0= IF unloop 0 0= 0= exit THEN  loop
   z u + c@ 61 = ;

: NULL$ 0 0 ;
s" NULL$" s" -- ptr u8 n" TRUST

\ value of $name, or 0 0 when unset (also when no environment was captured —
\ an engine built before the capture existed must still self-rebuild once)
: GETENV {: a u :}
   ENV-DATA ENVP-CELL + @ 0 = IF NULL$ exit THEN
   0 begin dup ENVP 0= 0= while
      dup ENVP a u ENV=? IF  ENVP u + 1 +  dup ZLEN  exit THEN
      1 + repeat
   drop NULL$ ;

\ $HB_TMP/<name> (default /tmp/<name>) — the build drivers' path knob
create TPB 256 allot
variable TPP  variable TPQ
: TPP@ TPP @ ;
s" TPP@" s" -- ptr u8" TRUST

: TMP-PATH {: a:ptr u :}
   s" HB_TMP" GETENV  dup 0 = IF drop drop s" /tmp" THEN  TPQ ! TPP !
   TPQ @ 0 ?do  TPP@ i + c@  TPB i + c!  loop
   47 TPB TPQ @ + c!
   u 0 ?do  a i + c@  TPB TPQ @ + 1 + i + c!  loop
   TPB  TPQ @ 1 + u + ;
