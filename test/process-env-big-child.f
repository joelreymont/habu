\ process-env-big-child.f - child for the large-inherited-environment test.
\
\ The test spawns this program with HABU_BIG_001..HABU_BIG_600 in its
\ environment. It counts those entries in its own envp, then inherits its whole
\ environment into a grandchild (/usr/bin/env) and counts how many of them the
\ grandchild reports, so the single line it prints - "big-env-child <own>
\ <seen>" - is only "600 600" when a 600-variable environment both arrived and
\ was passed on intact.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

$20000 constant BIG-CAP
$1388 constant BIG-TIMEOUT-MS

create BIG-OUT BIG-CAP allot
create BIG-ERR BIG-CAP allot
variable BIG-I

: BIG-NAME$ ( -- ptr u8 n )
   s" HABU_BIG_" ;

: BIG-U-TYPE ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + emit ;

\ Entries of this process's own envp whose name carries the test's prefix.
: BIG-OWN ( -- n )
   0 0 begin over ENVP 0= 0= while
      over ENVP dup ZLEN BIG-NAME$ STARTS-WITH? if 1 + then
      swap 1 + swap
   repeat nip ;

\ A prefixed entry starts a capture line only at a line boundary, so a value
\ that happens to contain the prefix is not counted.
: BIG-LINE-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   i 0 > if a i 1 - + c@ $0A = 0= if 0 0= 0= exit then then
   a i + u i - BIG-NAME$ STARTS-WITH? ;

: BIG-SEEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   0 BIG-I !
   begin BIG-I @ u < while
      a u BIG-I @ BIG-LINE-AT? if 1 + then
      BIG-I @ 1 + BIG-I !
   repeat ;

: BIG-GRANDCHILD ( -- n )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-DEFAULT-RESET
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/env" >LEN BIG-OUT BIG-CAP >LEN BIG-ERR BIG-CAP >LEN BIG-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} BIG-OUT o LEN>N BIG-SEEN ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 0 ENDOF
   ;MATCH ;

: BIG-MAIN ( -- )
   BIG-OWN {: own:n :}
   BIG-GRANDCHILD {: seen:n :}
   s" big-env-child " type own BIG-U-TYPE s"  " type seen BIG-U-TYPE cr ;

BIG-MAIN
