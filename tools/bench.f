\ bench.f — fixed kernels on bin/hb, timed from Habu.

1000000000 constant NS/SEC
1000000 constant NS/MS
256 constant BENCH-PATH-CAP
32 constant BENCH-NUM-CAP
2 constant BENCH-F-SETFD
1 constant BENCH-FD-CLOEXEC
74 constant BENCH-E-IO
75 constant BENCH-E-CAPACITY

create PATH-BUF BENCH-PATH-CAP allot
create NL 1 allot
create NUM-BUF BENCH-NUM-CAP allot

variable IN-R
variable IN-W
variable PID
variable NV
variable NP
variable DT
variable NSPI100
variable SMOKE?

: BENCH-TRUE ( -- bool )
   0 0= ;

: BENCH-FALSE ( -- bool )
   BENCH-TRUE 0= ;

: BENCH-DIE ( ptr u8 n n -- )
   die ;

: BENCH-IO ( ptr u8 n -- )
   BENCH-E-IO BENCH-DIE ;

: BENCH-CAPACITY ( ptr u8 n -- )
   BENCH-E-CAPACITY BENCH-DIE ;

: STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if BENCH-FALSE exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop BENCH-FALSE exit then
      1 +
   repeat drop BENCH-TRUE ;

: BENCH-CHECK-PATH ( n -- )
   BENCH-PATH-CAP 1 - > if s" bench: path too long" BENCH-CAPACITY then ;

: PATHZ ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   u BENCH-CHECK-PATH
   0 begin dup u < while
      dup a + c@ over PATH-BUF + c!
      1 +
   repeat drop
   0 PATH-BUF u + c!
   PATH-BUF ;

: FD-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   fd a u write u <> if s" bench: write failed" BENCH-IO then ;

: FD-WRITE-LN ( n ptr u8 n -- ) {: fd a:ptr u :}
   fd a u FD-WRITE
   fd NL 1 FD-WRITE ;

: CLOEXEC ( n -- ) {: fd :}
   fd BENCH-F-SETFD BENCH-FD-CLOEXEC fcntl 0 <> if
      s" bench: fcntl failed" BENCH-IO
   then ;

: BENCH-PIPE ( -- )
   pipe {: r w rc :}
   rc 0 <> if s" bench: pipe failed" BENCH-IO then
   w IN-W !
   r IN-R ! ;

: UTYPE ( u -- )
   NV !
   NV @ 0= if 48 emit exit then
   0 NP !
   begin NV @ 0 > while
      NV @ 10 mod 48 + NUM-BUF NP @ + c!
      NV @ 10 / NV !
      NP @ 1 + NP !
   repeat
   begin NP @ 0 > while
      NP @ 1 - NP !
      NUM-BUF NP @ + c@ emit
   repeat ;

: PAD3 ( u -- )
   dup 100 < if 48 emit then
   dup 10 < if 48 emit then
   UTYPE ;

: PAD2 ( u -- )
   dup 10 < if 48 emit then
   UTYPE ;

: .SECONDS ( n -- )
   dup NS/SEC / UTYPE
   46 emit
   NS/SEC mod NS/MS / PAD3
   s"  s   " type ;

: .NS/ITER ( n n -- ) {: ns iters :}
   ns 100 * iters / NSPI100 !
   NSPI100 @ 100 / UTYPE
   46 emit
   NSPI100 @ 100 mod PAD2
   s"  ns/iter" type ;

: BENCH-SPAWN-HB ( -- )
   s" bin/hb" PATHZ IN-R @ -1 -1 spawn-io PID !
   PID @ 0 < if s" bench: spawn failed" BENCH-IO then ;

: RUN-HB ( ptr u8 n -- n ) {: prog:ptr pu :}
   BENCH-PIPE
   IN-W @ CLOEXEC
   BENCH-SPAWN-HB
   IN-R @ close
   IN-W @ prog pu FD-WRITE-LN
   IN-W @ close
   PID @ wait-rc ;

: BENCH ( ptr u8 n n ptr u8 n -- ) {: name:ptr nu iters prog:ptr pu :}
   name nu type
   mono-ns DT !
   prog pu RUN-HB dup 0 <> if
      s"  FAILED rc=" type UTYPE cr exit
   then drop
   mono-ns DT @ - DT !
   32 emit
   DT @ .SECONDS
   DT @ iters .NS/ITER
   cr ;

: NORMAL ( -- )
   s" counter   (reg loop)      " 100000000
      s" : K 0 begin 1 + dup 100000000 = until drop ; K" BENCH
   s" do-loop   (frame stack)   " 100000000
      s" : K 100000000 0 do loop ; K" BENCH
   s" local-ref (ldr per use)   " 100000000
      s" : K {: a :} 0 begin a + dup 100000000 < 0= until drop ; 1 K" BENCH
   s" call      (bl/ret + spill)" 10000000
      s" : F 1 + ; : K 0 begin F dup 10000000 = until drop ; K" BENCH
   s" f-accum   (d-reg loop)     " 100000000
      s" : K {: n :} 0.0 0 begin 1 + swap 1.5 f+ swap dup n = until drop f0< ; 100000000 K" BENCH ;

: SMOKE ( -- )
   s" counter   (reg loop)      " 10000
      s" : K 0 begin 1 + dup 10000 = until drop ; K" BENCH
   s" do-loop   (frame stack)   " 10000
      s" : K 10000 0 do loop ; K" BENCH
   s" local-ref (ldr per use)   " 10000
      s" : K {: a :} 0 begin a + dup 10000 < 0= until drop ; 1 K" BENCH
   s" call      (bl/ret + spill)" 1000
      s" : F 1 + ; : K 0 begin F dup 1000 = until drop ; K" BENCH
   s" f-accum   (d-reg loop)     " 10000
      s" : K {: n :} 0.0 0 begin 1 + swap 1.5 f+ swap dup n = until drop f0< ; 10000 K" BENCH ;

: MAIN ( -- )
   10 NL c!
   0 SMOKE? !
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" --smoke" STR= if -1 SMOKE? ! then
   then
   SMOKE? @ if SMOKE else NORMAL then ;

MAIN
