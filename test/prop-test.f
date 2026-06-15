\ prop-test.f — property-based checker-soundness test, SELF-HOSTED in habu.
\ Generates runnable typed defs, checks them, RUNS the certified ones IN-PROCESS
\ (via `evaluate`), and fails (exit 1) if a certified def's real out-arity differs
\ from its declared ( in -- out ) — a false-cert. No Python, no gforth, no
\ spawning: generator, driver and measurement are all habu, run by bin/habu.
\ See PROP-TESTING.md.  Run:  bin/habu < test/prop-test.f   (exit 1 on a false-cert)
0 set-check                       \ the harness is uncheckable metaprogramming

\ ---- measurement: count residual items above a sentinel (engine has no `depth`)
variable D   -987654321 constant MK
: NAB  0 D ! begin dup MK <> while drop D @ 1+ D ! repeat drop D @ ;
variable VERD                     \ last verdict, set by the check hook
: VH  CHECK! dup VERD ! ;         \ MUST leave the verdict on the stack for the compiler
: ERR@  $340000000 $37D8 + @ ;    \ EVALERR-CELL: 0 = clean, 1 = recovered from an error

\ ---- seeded PRNG (LCG) ----
variable SEED   1 SEED !
: RND  SEED @ 1103515245 * 12345 +  $7FFFFFFF and  dup SEED ! ;
: RND%  RND swap mod ;            \ ( n -- 0..n-1 )

\ ---- byte buffers; loops use per-loop counter VARIABLES (no ?do / no `i`) ----
create BBUF 512 allot   variable BLEN     \ the body
create PBUF 1024 allot  variable PLEN     \ the full def / the runner
variable SA  variable SU  variable BI  variable PJ  variable NJ
: B+  ( a u -- )  SU ! SA !  0 BI !
   begin BI @ SU @ < while  SA @ BI @ + c@  BBUF BLEN @ + c!  BLEN @ 1+ BLEN !  BI @ 1+ BI !  repeat ;
: P+  ( a u -- )  SU ! SA !  0 PJ !
   begin PJ @ SU @ < while  SA @ PJ @ + c@  PBUF PLEN @ + c!  PLEN @ 1+ PLEN !  PJ @ 1+ PJ !  repeat ;
: BD  ( n -- )  48 + BBUF BLEN @ + c!  BLEN @ 1+ BLEN ! ;
variable NI
: PI64  ( n -- )  NI !  0 NJ !  begin NJ @ NI @ < while  s" i64 " P+  NJ @ 1+ NJ !  repeat ;

\ ---- depth-tracked linear body generator over the integer sublanguage ----
variable DEP  variable NIN  variable DOUT  variable K
: STEP  ( -- )   \ append one depth-feasible op to BBUF, update DEP
   5 RND% 0 = IF                          \ 1-in-5: a net-0 STRUCTURAL op (DEP unchanged)
      DEP @ 1 >= IF 3 ELSE 1 THEN RND% K !
      K @ 0 = IF s" 3 0 ?do loop " B+ exit THEN              \ bounded neutral loop
      K @ 1 = IF s" dup 0= if 1+ else 1- then " B+ exit THEN \ balanced branch
               s" >r r> " B+ exit THEN                       \ balanced return stack
   DEP @ 2 >= IF 9 RND% ELSE  DEP @ 1 >= IF 6 RND% ELSE 0 THEN  THEN  K !
   K @ 0 = IF 10 RND% BD s"  " B+  DEP @ 1+ DEP !  exit THEN   \ literal
   K @ 1 = IF s" 1+ " B+      exit THEN
   K @ 2 = IF s" 1- " B+      exit THEN
   K @ 3 = IF s" negate " B+  exit THEN
   K @ 4 = IF s" dup " B+   DEP @ 1+ DEP !  exit THEN
   K @ 5 = IF s" drop " B+  DEP @ 1- DEP !  exit THEN
   K @ 6 = IF s" + " B+     DEP @ 1- DEP !  exit THEN
   K @ 7 = IF s" over " B+  DEP @ 1+ DEP !  exit THEN
            s" nip " B+     DEP @ 1- DEP ! ;
variable STEPS  variable GI
: GEN  ( -- )   \ PBUF := ": G ( i64*NIN -- i64*DOUT ) <body> ;"
   0 BLEN ! 0 PLEN !
   4 RND% NIN !   NIN @ DEP !
   6 RND% 3 + STEPS !
   0 GI ! begin GI @ STEPS @ < while  STEP  GI @ 1+ GI !  repeat   \ build body, compute true DEP
   DEP @ DOUT !                            \ declared = true residual depth ...
   10 RND% 3 < IF                          \ ... perturbed 30% of the time (intended-reject)
      2 RND% IF DOUT @ 1+ ELSE DOUT @ 1- THEN
      dup 0 < IF drop 0 THEN  DOUT ! THEN
   s" : G ( " P+  NIN @ PI64  s" -- " P+  DOUT @ PI64  s" ) " P+
   BBUF BLEN @ P+  s" ; " P+ ;

\ ---- driver: check, then (if certified) run + measure, in-process ----
variable NCERT  variable NFC  variable RJ
: ONE  ( -- )
   GEN
   ['] VH set-check   PBUF PLEN @ evaluate   0 set-check
   VERD @ -1 = IF
      NCERT @ 1+ NCERT !
      0 PLEN ! s" MK " P+  NIN @ NI !  0 RJ !
      begin RJ @ NI @ < while  s" 7 " P+  RJ @ 1+ RJ !  repeat   s" G NAB" P+
      PBUF PLEN @ evaluate                   \ leaves measured on the stack, or traps
      ERR@ 0 = IF
         DOUT @ <> IF  NFC @ 1+ NFC !  s" FALSE-CERT(arity) declared " type DOUT @ . cr  THEN
      ELSE
         NFC @ 1+ NFC !  s" FALSE-CERT(trap) declared " type DOUT @ . cr
      THEN
   THEN ;
variable N  variable RI
: RUN  ( seed count -- )
   N !  SEED !  0 NCERT ! 0 NFC !
   0 RI ! begin RI @ N @ < while  ONE  RI @ 1+ RI !  repeat
   s" prop-test: " type N @ . s" programs, " type
   NCERT @ . s" certified, " type  NFC @ . s" FALSE-CERT(s)" type cr ;

\ self-test: prove the detector has teeth (a sound checker won't hand us a real
\ false-cert, so confirm the arity comparison fires on a fabricated mismatch).
: SELFTEST
   5 DOUT !
   4 DOUT @ <> 0= IF s" prop-test: self-test BROKEN" 1 die THEN
   4 4 <> IF s" prop-test: self-test BROKEN (equal flagged)" 1 die THEN
   s" prop-test: self-test OK (arity comparison fires)" type cr ;

\ leave/exit regression baits: non-neutral leave / divergent exit programs that a
\ SOUND checker rejects. If a regression ever certifies one, its real arity differs
\ from its declared sig -> die. (These are where this session's false-certs lived.)
: BAIT  ( a u -- )   \ MUST NOT certify
   ['] VH set-check  evaluate  0 set-check
   VERD @ -1 = IF s" prop-test: BAIT certified — leave/exit soundness regressed!" 1 die THEN ;
: BAITS
   s" : G ( -- ) 3 0 ?do 99 leave loop ;"            BAIT   \ leave carries an extra value
   s" : G ( i64 -- i64 ) dup 0 < if 0 0 exit then ;" BAIT   \ exit-path arity != fall-through
   s" : G ( -- i64 ) 5 0 ?do leave 9 loop ;"         BAIT   \ leave-point != loop-exit
   s" prop-test: baits OK (non-neutral leave / divergent exit rejected)" type cr ;

\ Fail loudly on any false-cert (`die` exits with the code; IF/THEN are
\ compile-only so this is wrapped in a word). A clean run reaches end-of-input,
\ which exits 0 in batch mode — no `bye` needed (the engine has none).
: FINISH  NFC @ 0 > IF s" prop-test: FALSE-CERT found" 1 die THEN ;
SELFTEST
BAITS
1 250 RUN
FINISH
