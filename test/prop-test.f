\ prop-test.f — property-based checker-soundness test, SELF-HOSTED in habu.
\ Generates runnable typed defs, checks them, RUNS the certified ones IN-PROCESS
\ (via `evaluate`), and fails (exit 1) if a certified def's real out-arity differs
\ from its declared ( in -- out ) — a false-cert. No Python, no gforth, no
\ spawning: generator, driver and measurement are all habu, run by bin/hb.
\ See PROP-TESTING.md.  Run:  bin/hb < test/prop-test.f   (exit 1 on a false-cert)
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
variable TFLAG                             \ sig element type: 0 = i64 (concrete), 1 = n (generic)
: PTY  ( n -- )  NI !  0 NJ !  begin NJ @ NI @ < while  TFLAG @ IF s" n " ELSE s" i64 " THEN P+  NJ @ 1+ NJ !  repeat ;
: PC   ( c -- )  PBUF PLEN @ + c!  PLEN @ 1+ PLEN ! ;

\ ---- depth-tracked generator over the integer sublanguage ----
variable DEP  variable NIN  variable DOUT  variable K
variable LOC?                              \ inputs bound as locals a/b/c at the body top?
variable PERT?                             \ declared out perturbed away from the true depth?
: BLET  ( i -- )  97 + BBUF BLEN @ + c!  BLEN @ 1+ BLEN !  s"  " B+ ;   \ emit letter a+i then space
: STEP  ( -- )   \ append one depth-feasible op to BBUF, update DEP
   5 RND% 0 = IF                          \ 1-in-5: a net-0 STRUCTURAL op (DEP unchanged)
      DEP @ 1 >= IF 4 ELSE 1 THEN RND% K !
      K @ 0 = IF s" 3 0 ?do loop " B+ exit THEN              \ bounded neutral loop
      K @ 1 = IF s" dup 0= if 1+ else 1- then " B+ exit THEN \ balanced branch
      K @ 2 = IF s" >r r> " B+ exit THEN                     \ balanced return stack
               s" [: 1+ ;] execute " B+ exit THEN            \ quotation applied
   DEP @ 2 >= IF 9 RND% ELSE  DEP @ 1 >= IF 6 RND% ELSE 0 THEN  THEN  K !
   K @ 0 = IF                                                  \ push a value: a local ref, or a literal
      LOC? @ IF NIN @ RND% BLET ELSE 10 RND% BD s"  " B+ THEN
      DEP @ 1+ DEP !  exit THEN
   K @ 1 = IF s" 1+ " B+      exit THEN
   K @ 2 = IF s" 1- " B+      exit THEN
   K @ 3 = IF s" negate " B+  exit THEN
   K @ 4 = IF s" dup " B+   DEP @ 1+ DEP !  exit THEN
   K @ 5 = IF s" drop " B+  DEP @ 1- DEP !  exit THEN
   K @ 6 = IF s" + " B+     DEP @ 1- DEP !  exit THEN
   K @ 7 = IF s" over " B+  DEP @ 1+ DEP !  exit THEN
            s" nip " B+     DEP @ 1- DEP ! ;
variable STEPS  variable GI
: GEN-BODY  ( nin uselocals -- )           \ fill BBUF with a body, set NIN and the true residual DEP
   over 0 > and  LOC? !                     \ bind inputs as locals only when there ARE inputs
   NIN !  0 BLEN !
   LOC? @ IF                                \ {: a b c :} prefix binds the NIN inputs as locals
      s" {: " B+  0 GI ! begin GI @ NIN @ < while GI @ BLET GI @ 1+ GI ! repeat  s" :} " B+  0 DEP !
   ELSE NIN @ DEP ! THEN
   6 RND% 3 + STEPS !
   0 GI ! begin GI @ STEPS @ < while  STEP  GI @ 1+ GI !  repeat ;   \ build body, compute true DEP
: HEAD  {: nch din dout :}                  \ PBUF += ": <nch> ( din -- dout ) "  (TFLAG picks i64/n)
   0 PLEN !  s" : " P+  nch PC  s"  ( " P+  din PTY  s" -- " P+  dout PTY  s" ) " P+ ;
: BODY+  ( -- )  BBUF BLEN @ P+  s" ; " P+ ;
: GEN  ( -- )   \ PBUF := ": G ( i64*NIN -- i64*DOUT ) <body> ;"
   0 TFLAG !
   4 RND%  3 RND% 0 =  GEN-BODY            \ 0..3 inputs, bound as locals 1-in-3
   DEP @ DOUT !  0 PERT? !                 \ declared = true residual depth ...
   10 RND% 3 < IF                          \ ... perturbed 30% of the time (intended-reject)
      2 RND% IF DOUT @ 1+ ELSE DOUT @ 1- THEN
      dup 0 < IF drop 0 THEN  DOUT !
      DOUT @ DEP @ <> IF -1 PERT? ! THEN THEN
   71 NIN @ DOUT @ HEAD  BODY+ ;           \ 71 = 'G'

\ ---- driver: check, then run + measure, in-process; forget each program ----
variable NCERT  variable NFC  variable NFR  variable RJ
\ ---- complete checkpoint/rollback: a `: G ;` grows THREE persistent stores —
\ code (CP), the name dict (NDICT) and the checker's certified-signature table
\ (UEND, the USIGS cursor). Per-check transient pools (the term arena, QEN) reset
\ themselves in the checker's NEW; the `:` handler resets the codegen scratch. So a
\ correct forget = restore exactly CP/NDICT/UEND. Without UEND, an unbounded sweep
\ overflows USIGS ("user sigs full") after ~1k certified defs. Two levels (program
\ + shrink variant) so shrinking can roll back inside a program's own checkpoint.
variable CPSAVE  variable NDSAVE  variable UESAVE
variable SCPSV   variable SNDSV   variable SUESV
: MARK    ( -- )  cp@ CPSAVE !  ndict@ NDSAVE !  UEND @ UESAVE ! ;
: FORGET  ( -- )  NDSAVE @ ndict!  CPSAVE @ cp!  UESAVE @ UEND ! ;
: SMARK   ( -- )  cp@ SCPSV !   ndict@ SNDSV !   UEND @ SUESV ! ;
: SFORGET ( -- )  SNDSV @ ndict!  SCPSV @ cp!    SUESV @ UEND ! ;
\ ---- shared measurement: build "MK <nin×7> <nch> NAB", run <nch>, compare arity ----
: RUN1  ( nch nin -- )   \ PBUF := "MK <nin copies of 7> <nch> NAB"
   NI !  0 PLEN ! s" MK " P+  0 RJ ! begin RJ @ NI @ < while  s" 7 " P+  RJ @ 1+ RJ ! repeat
   PC  32 PC  s" NAB" P+ ;
: CHK  ( a u -- )  ['] VH set-check  evaluate  0 set-check ;     \ check one def; VERD := verdict
: MEASURE  {: nch nin expected :}   \ run a CERTIFIED word <nch>; +1 NFC on arity-mismatch or trap
   nch nin RUN1  PBUF PLEN @ evaluate          \ ( -- measured ), or traps and recovers
   ERR@ 0 = IF  expected <> IF  NFC @ 1+ NFC !  s" FALSE-CERT(arity) " type nch emit cr  THEN
   ELSE  NFC @ 1+ NFC !  s" FALSE-CERT(trap) " type nch emit cr  THEN ;

\ ---- metamorphic subsumption: an i64-certified body must also certify under the
\ generic ( n -- n ) sig (n subsumes i64). If it does, run it too (free false-cert
\ coverage); i64-cert but n-reject is a checker inconsistency (logged, non-fatal). ----
variable NSUB  variable NSI
: SUBSUME  ( -- )   \ pre: BBUF/NIN/DOUT is the certified i64 program G
   SMARK  1 TFLAG !  71 NIN @ DOUT @ HEAD  BODY+  0 TFLAG !  PBUF PLEN @ CHK
   VERD @ -1 = IF  NSUB @ 1+ NSUB !  71 NIN @ DOUT @ MEASURE
   ELSE  NSI @ 1+ NSI !  THEN
   SFORGET ;

\ ---- metamorphic render round-trip: render the just-certified body's effect, then
\ re-declare the SAME body with that exact rendered sig — it must re-certify. ----
variable NRT  variable NRI  variable RSA  variable RSU
: ROUNDTRIP  ( -- )   \ pre: G just certified; REND-SIG holds G's rendered effect
   REND-SIG  RSU !  RSA !
   SMARK  0 PLEN ! s" : G ( " P+  RSA @ RSU @ P+  s"  ) " P+  BBUF BLEN @ P+  s" ; " P+  PBUF PLEN @ CHK
   VERD @ -1 = IF  NRT @ 1+ NRT !  71 NIN @ DOUT @ MEASURE
   ELSE  NRI @ 1+ NRI !  THEN
   SFORGET ;

\ ---- metamorphic composition: A:(x--y) and B:(y--z) both certified => ': C A B ;'
\ must certify ( x -- z ) and run-match — chains arities; catches what one body can't. ----
variable NCMP  variable NCI  variable CAI  variable CAO  variable CBO
: COMPOSE  ( -- )
   SMARK  0 TFLAG !
   3 RND% 0 GEN-BODY  NIN @ CAI !  DEP @ CAO !  65 CAI @ CAO @ HEAD  BODY+  PBUF PLEN @ CHK   \ A=65
   VERD @ -1 = IF
      CAO @ 0 GEN-BODY  DEP @ CBO !  66 CAO @ CBO @ HEAD  BODY+  PBUF PLEN @ CHK              \ B=66, in=CAO
      VERD @ -1 = IF
         0 PLEN ! s" : C ( " P+  CAI @ PTY  s" -- " P+  CBO @ PTY  s" ) A B ; " P+  PBUF PLEN @ CHK   \ C=67
         VERD @ -1 = IF  NCMP @ 1+ NCMP !  67 CAI @ CBO @ MEASURE
         ELSE  NCI @ 1+ NCI !  THEN
      THEN
   THEN
   SFORGET ;

\ ---- shrinking: on a FALSE-CERT, delta-debug BBUF down to a minimal body that
\ STILL satisfies a predicate (certify-and-mismatch in real use; "still certifies"
\ in the self-test). Drops one trailing token per step, restoring any drop that
\ breaks the predicate. Token surgery just moves BLEN — the bytes stay put. ----
variable PRED  variable BSAVE
: TRIM-TRAIL ( -- )  begin BLEN @ 0 > BBUF BLEN @ 1- + c@ 32 = and while  BLEN @ 1- BLEN !  repeat ;
: DROP-LAST  ( -- f )   \ remove the last space-delimited token; f = did-remove
   TRIM-TRAIL  BLEN @ 0= IF 0 exit THEN
   begin BLEN @ 0 > BBUF BLEN @ 1- + c@ 32 <> and while  BLEN @ 1- BLEN !  repeat  -1 ;
: REBUILD-G ( -- )  0 TFLAG !  71 NIN @ DOUT @ HEAD  BODY+ ;   \ PBUF := ": G ( NIN -- DOUT ) BBUF ;"
: FCFAIL?  ( -- f )   \ does the current BBUF certify AND run to an arity != DOUT (or trap)?
   SMARK  REBUILD-G  PBUF PLEN @ CHK
   VERD @ -1 = IF  71 NIN @ RUN1  PBUF PLEN @ evaluate
      ERR@ 0 = IF  DOUT @ <>  ELSE  1  THEN
   ELSE  0  THEN  SFORGET ;
: STILLCERT? ( -- f )  SMARK  REBUILD-G  PBUF PLEN @ CHK  VERD @ -1 =  SFORGET ;
: SHRINK  ( pred-xt -- )   \ minimize BBUF keeping (pred) true
   PRED !
   begin
      BLEN @ BSAVE !
      DROP-LAST IF  PRED @ execute IF -1 ELSE  BSAVE @ BLEN !  0 THEN
      ELSE 0 THEN
   while repeat ;

\ ---- driver: base program (false-cert + false-reject), then metamorphic amplifiers ----
variable NFC0
: ONE  ( -- )
   MARK  GEN                                 \ checkpoint, then build ": G ( ... ) <body> ;"
   PBUF PLEN @ CHK
   VERD @ -1 = IF                            \ CERTIFIED: measured out-arity must equal declared
      NCERT @ 1+ NCERT !
      NFC @ NFC0 !  71 NIN @ DOUT @ MEASURE   \ base run-and-compare
      NFC @ NFC0 @ > IF                       \ a FALSE-CERT: shrink to a minimal counterexample & print
         ['] FCFAIL? SHRINK
         s" minimal counterexample: " type  REBUILD-G  PBUF PLEN @ type cr THEN
      ROUNDTRIP  SUBSUME                      \ metamorphic amplifiers on the certified body
   ELSE VERD @ 0 = PERT? @ 0= and IF         \ rejected but NOT perturbed -> the generator declared the
      NFR @ 1+ NFR !                          \ true arity yet the checker rejected it: a FALSE-REJECT
   THEN THEN
   FORGET                                    \ forget G (code + dict entry + recorded sig)
   COMPOSE ;                                  \ independent two-body composition probe
variable N  variable RI
: RUN  ( seed count -- )
   N !  SEED !  0 NCERT ! 0 NFC ! 0 NFR !  0 NSUB ! 0 NSI ! 0 NRT ! 0 NRI ! 0 NCMP ! 0 NCI !
   0 RI ! begin RI @ N @ < while  ONE  RI @ 1+ RI !  repeat
   s" prop-test: " type N @ . s" programs, " type
   NCERT @ . s" certified, " type  NFC @ . s" FALSE-CERT(s), " type
   NFR @ . s" false-reject(s)" type cr
   s" prop-test: metamorphic — " type  NSUB @ . s" subsumption + " type
   NRT @ . s" round-trip + " type  NCMP @ . s" composition runs; " type
   NSI @ NRI @ + NCI @ +  . s" inconsistency(ies)" type cr ;

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

\ shrink self-test: a long certified ( i64 -- i64 ) body must REDUCE under the
\ "still certifies" predicate — proves the delta-debug loop + token surgery work
\ (a sound checker never hands us a real false-cert to shrink, so exercise the
\ machinery on an achievable predicate).
: SELFTEST-SHRINK
   0 BLEN !  s" dup drop 1+ 1- negate 1+ 1- " B+  1 NIN !  1 DOUT !
   BLEN @  ['] STILLCERT? SHRINK  BLEN @  >  0= IF
      s" prop-test: self-test SHRINK BROKEN (no reduction)" 1 die THEN
   s" prop-test: shrink OK (delta-debug reduced to: " type  REBUILD-G  PBUF PLEN @ type s" )" type cr ;

\ Fail loudly on any false-cert (`die` exits with the code; IF/THEN are
\ compile-only so this is wrapped in a word). A clean run reaches end-of-input,
\ which exits 0 in batch mode — no `bye` needed (the engine has none).
: FINISH  NFC @ 0 > IF s" prop-test: FALSE-CERT found" 1 die THEN ;
SELFTEST
SELFTEST-SHRINK
BAITS
1 250 RUN
FINISH
