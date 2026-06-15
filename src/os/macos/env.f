\ env.f — process arguments and environment. The engine runs under dyld
\ (LC_MAIN): main(argc, argv, envp) arrives in x0-x2 and EM-STARTUP saves them
\ to DATA header cells at entry (snapshot boots re-store the live values).

$340000000 constant ENV-DATA
$3670 constant ARGC-CELL
$3678 constant ARGV-CELL
$3680 constant ENVP-CELL

: ARGC ( -- n )  ENV-DATA ARGC-CELL + @ ;

: ARGV ( i -- z )  8 * ENV-DATA ARGV-CELL + @ + @ ;   \ argv[i], NUL-terminated

: ENVP ( i -- z )  8 * ENV-DATA ENVP-CELL + @ + @ ;   \ envp[i], 0 at the end

: ZLEN {: z :}  0 begin z over + c@ while 1 + repeat ;

: ARGV$ ( i -- a u )  ARGV dup ZLEN ;

: SCRIPT-ARGC ( -- n )
   ARGC 2 -  dup 0 < if drop 0 then ;

: SCRIPT-ARGV ( i -- z )  2 + ARGV ;

: SCRIPT-ARGV$ ( i -- a u )  SCRIPT-ARGV dup ZLEN ;

\ does the c-string z start with name a/u followed by '='?
: ENV=? {: z a u :}
   u 0 ?do  z i + c@  a i + c@  = 0= IF unloop 0 exit THEN  loop
   z u + c@ 61 = ;

\ value of $name, or 0 0 when unset (also when no environment was captured —
\ an engine built before the capture existed must still self-rebuild once)
: GETENV {: a u :}
   ENV-DATA ENVP-CELL + @ 0 = IF 0 0 exit THEN
   0 begin dup ENVP while
      dup ENVP a u ENV=? IF  ENVP u + 1 +  dup ZLEN  exit THEN
      1 + repeat
   drop 0 0 ;

\ $HB_TMP/<name> (default /tmp/<name>) — the build drivers' path knob
create TPB 256 allot
variable TPP  variable TPQ
: TMP-PATH {: a u :}
   s" HB_TMP" GETENV  dup 0 = IF drop drop s" /tmp" THEN  TPQ ! TPP !
   TPQ @ 0 ?do  TPP @ i + c@  TPB i + c!  loop
   47 TPB TPQ @ + c!
   u 0 ?do  a i + c@  TPB TPQ @ + 1 + i + c!  loop
   TPB  TPQ @ 1 + u + ;
