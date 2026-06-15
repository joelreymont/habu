\ bench.f — fixed kernels on bin/hb, timed from Habu.
\ Driver/tooling tier: process spawning, formatting, and benchmark source strings
\ are intentionally unchecked.
0 set-check

1000000000 constant NS/SEC
1000000 constant NS/MS

create PATH-BUF 256 allot
create NL 1 allot
create NUM-BUF 32 allot

variable IN-R
variable IN-W
variable PID
variable NV
variable NP
variable DT
variable NSPI100
variable SMOKE?

: STR= {: a u b v :} ( a u b v -- f )
   u v <> if 0 exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 exit then
      1 +
   repeat drop -1 ;

: PATHZ {: a u :} ( a u -- z )
   0 begin dup u < while
      dup a + c@ over PATH-BUF + c!
      1 +
   repeat drop
   0 PATH-BUF u + c!
   PATH-BUF ;

: FD-WRITE {: fd a u :} ( fd a u -- )
   fd a u write drop ;

: FD-WRITE-LN {: fd a u :} ( fd a u -- )
   fd a u FD-WRITE
   fd NL 1 FD-WRITE ;

: CLOEXEC {: fd :} ( fd -- )
   fd 2 1 fcntl drop ;

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

: .SECONDS ( ns -- )
   dup NS/SEC / UTYPE
   46 emit
   NS/SEC mod NS/MS / PAD3
   s"  s   " type ;

: .NS/ITER {: ns iters :} ( ns iters -- )
   ns 100 * iters / NSPI100 !
   NSPI100 @ 100 / UTYPE
   46 emit
   NSPI100 @ 100 mod PAD2
   s"  ns/iter" type ;

: RUN-HB {: prog pu :} ( prog pu -- rc )
   pipe drop IN-W ! IN-R !
   IN-W @ CLOEXEC
   s" bin/hb" PATHZ IN-R @ -1 -1 spawn-io PID !
   IN-R @ close
   IN-W @ prog pu FD-WRITE-LN
   IN-W @ close
   PID @ wait-rc ;

: BENCH {: name nu iters prog pu :} ( name nu iters prog pu -- )
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
