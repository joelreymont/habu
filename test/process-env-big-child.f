\ process-env-big-child.f - child for the large-inherited-environment test.
\ The parent sets HABU_BIG_001..HABU_BIG_600. This prints "big-env-child <own>
\ <seen>": the prefixed entries in its own envp, and those reported by
\ /usr/bin/env, a grandchild inheriting the whole environment. Only "600 600"
\ shows a 600-variable environment arrived and passed on intact.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f

$20000 constant BIG-CAP

create BIG-OUT BIG-CAP allot
create BIG-ERR BIG-CAP allot
variable BIG-I

: BIG-NAME$ ( -- str )
   s" HABU_BIG_" ;

: BIG-U-TYPE ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR:ZERO + emit ;

: BIG-OWN ( -- n )
   0 0 begin over ENVP 0= 0= while
      over ENVP dup ZLEN >str BIG-NAME$ STR:STARTS-WITH? if 1 + then
      swap 1 + swap
   repeat nip ;

\ A prefixed entry starts a capture line only at a line boundary, so a value
\ that happens to contain the prefix is not counted.
: BIG-LINE-AT? ( str n -- bool ) {: s:str i:n :}
   i 0 > if s STR:DATA i 1 - + c@ $0A = 0= if 0 0= 0= exit then then
   s STR:DATA i + s STR:LEN i - >str BIG-NAME$ STR:STARTS-WITH? ;

: BIG-SEEN ( str -- n ) {: s:str :}
   0
   0 BIG-I !
   begin BIG-I @ s STR:LEN < while
      s BIG-I @ BIG-LINE-AT? if 1 + then
      BIG-I @ 1 + BIG-I !
   repeat ;

: BIG-GRANDCHILD ( -- n )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-DEFAULT-RESET
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/env" BIG-OUT BIG-CAP >LEN BIG-ERR BIG-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} BIG-OUT o LEN>N >str BIG-SEEN ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 0 ENDOF
   ;MATCH ;

: BIG-MAIN ( -- )
   BIG-OWN {: own:n :}
   BIG-GRANDCHILD {: seen:n :}
   s" big-env-child " type own BIG-U-TYPE s"  " type seen BIG-U-TYPE cr ;

BIG-MAIN
