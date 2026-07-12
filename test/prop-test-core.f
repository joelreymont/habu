\ prop-test-core.f — property-based checker-soundness test, SELF-HOSTED in habu.
\ Generates runnable typed defs, checks them, RUNS the certified ones IN-PROCESS
\ (via `evaluate`), and fails (exit 1) if a certified def's real out-arity differs
\ from its declared ( in -- out ) — a false-cert — or if any metamorphic amplifier
\ (subsumption / render round-trip / composition) reports an inconsistency. No
\ host scripting, no gforth, no spawning: generator, driver and measurement are
\ all habu, run by bin/hb.
\
\ The default run shards the sweep across PROP-SHARD-N forked slots, each a
\ distinct seed running DEFAULT-COUNT iterations, so a single gate phase covers
\ N x DEFAULT-COUNT distinct-seed programs in parallel; one red shard fails the
\ phase. The `bin/hb <seed> <count>` argv override runs one seed serially to
\ reproduce a specific run. Fork wrappers load before the check hook so their
\ own definitions certify under the default checker, not the throw-on-reject
\ prop hook.

require lib/errors.f
require lib/process.f
require lib/process-fork.f

TRUSTED: PROP-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> if 70 throw then ;
TRUSTED: PROP-INSTALL-HOOK ( -- )
   ['] PROP-CHECK-HOOK set-check ;
PROP-INSTALL-HOOK

\ ---- measurement: compare stack depth before and after a certified run
variable BASE  variable MC
TRUSTED: CLEAR-MEAS  ( R n -- n )
   dup MC !  begin MC @ 0 > while  swap drop  MC @ 1- MC !  repeat ;
variable VERD                     \ last verdict, set by the check hook
\ Reads the engine's evaluate-error cell by its NAMED layout constant
\ (EVALERR-CELL, src/habu/layout.f) rather than a hardcoded offset, so a
\ layout change can't silently point this peek at the wrong cell.
TRUSTED: ERR@  ( -- n )
   data-base EVALERR-CELL + @ ; \ EVALERR-CELL: 0 = clean, 1 = recovered from an error

\ ---- seeded PRNG (LCG) ----
500 constant DEFAULT-COUNT

\ Default seed varies per run (mono-ns clock) so the fuzzer explores a fresh
\ space each gate instead of a frozen 250-case regression; RUN-SEED is printed
\ (POS.) and the `bin/hb <seed> <count>` argv override reproduces any run.
: FRESH-SEED ( -- n )
   mono-ns $7FFFFFFF and dup 0= if drop 1 then ;
3 constant LOG-LIMIT
variable SEED   1 SEED !
: RND  ( -- n )
   SEED @ 1103515245 * 12345 +  $7FFFFFFF and  dup SEED ! ;
: RND%  ( n -- n ) {: bound:n :}
   RND bound mod ;

\ ---- byte buffers; loops use per-loop counter VARIABLES (no ?do / no `i`) ----
512 constant BBUF-CAP
1024 constant PBUF-CAP
create BBUF BBUF-CAP allot   variable BLEN     \ the body
create PBUF PBUF-CAP allot  variable PBUF-U   \ the full def / the runner
variable SA  variable SU  variable PJ  variable NJ
variable RUN-SEED  variable N  variable RI
: PROP-B+  ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@  BBUF BLEN @ + c!
      BLEN @ 1+ BLEN !
      1+
   repeat drop ;
: P+  ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@  PBUF PBUF-U @ + c!
      PBUF-U @ 1+ PBUF-U !
      1+
   repeat drop ;
: BD  ( n -- ) {: digit:n :}
   digit 48 + BBUF BLEN @ + c!
   BLEN @ 1+ BLEN ! ;
variable NI
variable TFLAG                             \ sig element type: 0 = i64 (concrete), 1 = n (generic)
: PTY  ( n -- ) {: count:n :}
   0 begin dup count < while
      TFLAG @ if s" n " else s" i64 " then P+
      1+
   repeat drop ;
: PC   ( n -- ) {: c:n :}
   c PBUF PBUF-U @ + c!
   PBUF-U @ 1+ PBUF-U ! ;
: POS. ( -- )  s" seed " type RUN-SEED @ .  s" iteration " type RI @ . ;
: DEF. ( -- )  PBUF PBUF-U @ type cr ;
: BODY. ( -- )  BBUF BLEN @ type cr ;

\ ---- depth-tracked generator over the integer sublanguage ----
variable DEP  variable NIN  variable DOUT  variable K
variable LOCN                              \ fresh mid-body local name counter (z0, z1, ...)
variable LOC?                              \ inputs bound as locals a/b/c at the body top?
variable PERT?                             \ declared out perturbed away from the true depth?
: BLET  ( n -- ) {: idx:n :}
   idx 97 + BBUF BLEN @ + c!
   BLEN @ 1+ BLEN !
   s"  " PROP-B+ ;   \ emit letter a+i then space
\ Emit a mid-body local round-trip `{: zN :} zN` (net-0, needs DEP>=1): binds
\ the data top to a fresh local zN then pushes it back. LOCN keeps names unique
\ within a body (z0, z1, ...), never colliding with input locals a/b/c.
: STEP-LOCAL  ( -- )
   s" {: z" PROP-B+  LOCN @ BD  s"  :} z" PROP-B+  LOCN @ BD  s"  " PROP-B+
   LOCN @ 1+ LOCN ! ;
\ Named structural (net-0) op texts: PROP-STEP emits them and the alphabet
\ self-test asserts each one is generated and certifies, so a class cannot
\ silently fall out of the fuzzer's reach.
: OP-LOOP$   ( -- ptr u8 n )  s" 3 0 ?do loop " ;                \ bounded neutral loop
: OP-LEAVE$  ( -- ptr u8 n )  s" 3 0 ?do leave loop " ;          \ valid leave inside a loop
: OP-STR$    ( -- ptr u8 n )  S\" s\" x\" 2drop " ;              \ string literal, then dropped
: OP-QUOT0$  ( -- ptr u8 n )  s" [: ;] execute " ;               \ empty row-poly quotation
: OP-BRANCH$ ( -- ptr u8 n )  s" dup 0= if 1+ else 1- then " ;   \ balanced branch
: OP-RSTK$   ( -- ptr u8 n )  s" >r r> " ;                       \ balanced return stack
: OP-RCOPY$  ( -- ptr u8 n )  s" >r r@ drop r> " ;               \ return-stack copy via r@
: OP-QUOT1$  ( -- ptr u8 n )  s" [: 1+ ;] execute " ;            \ quotation applied
: PROP-STEP  ( -- )   \ append one depth-feasible op to BBUF, update DEP
   5 RND% 0 = IF                          \ 1-in-5: a net-0 STRUCTURAL op (DEP unchanged)
      DEP @ 1 >= IF 9 ELSE 4 THEN RND% K !     \ DEP>=1: ops 0..8; DEP=0: depth-free 0..3
      K @ 0 = IF OP-LOOP$ PROP-B+ exit THEN
      K @ 1 = IF OP-LEAVE$ PROP-B+ exit THEN
      K @ 2 = IF OP-STR$ PROP-B+ exit THEN
      K @ 3 = IF OP-QUOT0$ PROP-B+ exit THEN
      K @ 4 = IF OP-BRANCH$ PROP-B+ exit THEN
      K @ 5 = IF OP-RSTK$ PROP-B+ exit THEN
      K @ 6 = IF OP-RCOPY$ PROP-B+ exit THEN
      K @ 7 = IF OP-QUOT1$ PROP-B+ exit THEN
               STEP-LOCAL exit THEN                               \ mid-body local round-trip
   DEP @ 2 >= IF 15 RND% ELSE  DEP @ 1 >= IF 6 RND% ELSE 0 THEN  THEN  K !
   K @ 0 = IF                                                  \ push a value: a local ref, or a literal
      LOC? @ 0= 0= IF NIN @ RND% BLET ELSE 10 RND% BD s"  " PROP-B+ THEN
      DEP @ 1+ DEP !  exit THEN
   K @ 1 = IF s" 1+ " PROP-B+      exit THEN
   K @ 2 = IF s" 1- " PROP-B+      exit THEN
   K @ 3 = IF s" negate " PROP-B+  exit THEN
   K @ 4 = IF s" dup " PROP-B+   DEP @ 1+ DEP !  exit THEN
   K @ 5 = IF s" drop " PROP-B+  DEP @ 1- DEP !  exit THEN
   K @ 6 = IF s" + " PROP-B+     DEP @ 1- DEP !  exit THEN
   K @ 7 = IF s" over " PROP-B+  DEP @ 1+ DEP !  exit THEN
   K @ 8 = IF s" nip " PROP-B+   DEP @ 1- DEP !  exit THEN
   K @ 9 = IF s" swap " PROP-B+  exit THEN
   K @ 10 = IF s" - " PROP-B+    DEP @ 1- DEP !  exit THEN
   K @ 11 = IF s" * " PROP-B+    DEP @ 1- DEP !  exit THEN
   K @ 12 = IF s" and " PROP-B+  DEP @ 1- DEP !  exit THEN
   K @ 13 = IF s" or " PROP-B+   DEP @ 1- DEP !  exit THEN
             s" xor " PROP-B+    DEP @ 1- DEP ! ;
variable STEPS  variable GI
: GEN-BODY  ( n n -- ) {: in-count:n use-flag:n :}  \ fill BBUF with a body, set NIN and the true residual DEP
   in-count 0 > if use-flag 0= if 0 else -1 then else 0 then LOC? !
   in-count NIN !  0 BLEN !  0 LOCN !
   LOC? @ 0= 0= IF                          \ {: a b c :} prefix binds the NIN inputs as locals
      s" {: " PROP-B+  0 GI ! begin GI @ NIN @ < while GI @ BLET GI @ 1+ GI ! repeat  s" :} " PROP-B+  0 DEP !
   ELSE NIN @ DEP ! THEN
   6 RND% 3 + STEPS !
   0 GI ! begin GI @ STEPS @ < while  PROP-STEP  GI @ 1+ GI !  repeat ;   \ build body, compute true DEP
: HEAD  ( n n n -- ) {: name-ch:n in-arity:n out-arity:n :}  \ PBUF += ": <nch> ( din -- dout ) "  (TFLAG picks i64/n)
   0 PBUF-U !  s" : " P+  name-ch PC  s"  ( " P+  in-arity PTY  s" -- " P+  out-arity PTY  s" ) " P+ ;
: BODY+  ( -- )  BBUF BLEN @ P+  s" ; " P+ ;
: GEN  ( -- )   \ PBUF := ": G ( i64*NIN -- i64*DOUT ) <body> ;"
   0 TFLAG !
   4 RND%  3 RND% 0 = if -1 else 0 then GEN-BODY  \ 0..3 inputs, bound as locals 1-in-3
   DEP @ DOUT !  0 PERT? !                 \ declared = true residual depth ...
   10 RND% 3 < IF                          \ ... perturbed 30% of the time (intended-reject)
      2 RND% 0= 0= IF DOUT @ 1+ ELSE DOUT @ 1- THEN
      dup 0 < IF drop 0 THEN  DOUT !
      DOUT @ DEP @ <> IF -1 PERT? ! THEN THEN
   71 NIN @ DOUT @ HEAD  BODY+ ;           \ 71 = 'G'

\ ---- driver: check, then run + measure, in-process; forget each program ----
variable NCERT  variable PROP-NFC  variable NFR  variable RJ
variable LAST-MEAS  variable LAST-TRAP
variable FC-KIND  variable FC-EXP  variable FC-MEAS
\ ---- complete checkpoint/rollback: a `: G ;` grows persistent code, dictionary,
\ certified signatures, and no-return metadata. Rejected candidates are checked
\ without compiling; accepted candidates are compiled with the hook off after
\ CHECK! has already certified their effect. Two levels (program + shrink variant)
\ let shrinking roll back inside a program's own checkpoint.
variable CPSAVE  variable NDSAVE  variable UESAVE
variable SCPSV   variable SNDSV   variable SUESV
variable CHKCPSV variable CHKNDSV variable CHKUESV
TRUSTED: MARK    ( -- )  cp@ CPSAVE !  ndict@ NDSAVE !  UEND @ UESAVE ! ;
TRUSTED: FORGET  ( -- )  NDSAVE @ ndict!  CPSAVE @ cp!  UESAVE @ UEND !  UTERM! ;
TRUSTED: SMARK   ( -- )  cp@ SCPSV !   ndict@ SNDSV !   UEND @ SUESV ! ;
TRUSTED: SFORGET ( -- )  SNDSV @ ndict!  SCPSV @ cp!    SUESV @ UEND !  UTERM! ;
TRUSTED: CHK-MARK ( -- ) cp@ CHKCPSV ! ndict@ CHKNDSV ! UEND @ CHKUESV ! ;
TRUSTED: CHK-FORGET ( -- ) CHKNDSV @ ndict! CHKCPSV @ cp! CHKUESV @ UEND ! UTERM! ;
\ ---- shared measurement: build "depth BASE ! <nin×7> <nch> depth BASE @ - CLEAR-MEAS" ----
: RUN1  ( n n -- ) {: name-ch:n in-arity:n :}
   0 PBUF-U ! s" depth BASE ! " P+
   0 RJ ! begin RJ @ in-arity < while  s" 7 " P+  RJ @ 1+ RJ ! repeat
   name-ch PC  32 PC  s" depth BASE @ - CLEAR-MEAS" P+ ;
TRUSTED: CHK-HOOK ( ptr u8 n -- n )
   CHECK! dup VERD ! drop -1 ;
\ Differential boundary: certification already happened via CHECK! in CHK;
\ the compile stage runs unchecked so the fuzzer measures the candidate's
\ true runtime arity without re-entering the hook. Queued owner:
\ habu-seal-set-check-b3676b33 (test set-check behind the friend latch).
TRUSTED: CHK-COMPILE-CERT ( ptr u8 n -- )
   0 set-check
   evaluate
   ['] PROP-CHECK-HOOK set-check ;
: CHK-BODY$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a 2 + u 4 - ;
TRUSTED: CHK  ( ptr u8 n -- )
   CHK-MARK
   0 VERD !
   2dup CHK-BODY$ CHECK! VERD !
   VERD @ -1 = IF
      CHK-COMPILE-CERT
   ELSE
      2drop
      CHK-FORGET
   THEN ;
TRUSTED: RUN-MEAS  ( n n -- )   \ execute a word and set LAST-MEAS/LAST-TRAP
   0 LAST-TRAP !  RUN1  PBUF PBUF-U @ evaluate
   ERR@ 0 = IF
      dup 0< IF  drop -1 LAST-TRAP !  ELSE  LAST-MEAS !  THEN
   ELSE  -1 LAST-TRAP !  THEN ;
: FC-SET-ARITY ( n n -- )
   FC-MEAS !  FC-EXP !  1 FC-KIND !  PROP-NFC @ 1+ PROP-NFC ! ;
: FC-SET-TRAP ( n -- )
   FC-EXP !  2 FC-KIND !  PROP-NFC @ 1+ PROP-NFC ! ;
: FC-LINE  ( n -- )
   s" prop-test: FALSE-CERT " type POS.
   s" word " type emit s"  " type
   FC-KIND @ 1 = IF
      s" expected " type FC-EXP @ .  s" measured " type FC-MEAS @ . cr
   ELSE
      s" expected " type FC-EXP @ .  s" trap during measurement" type cr
   THEN ;
: MEASURE  ( n n n -- ) {: name-ch:n in-arity:n expected:n :}   \ run a CERTIFIED word <nch>; +1 NFC on arity-mismatch or trap
   name-ch in-arity RUN-MEAS
   LAST-TRAP @ IF  expected FC-SET-TRAP  name-ch FC-LINE
   ELSE  LAST-MEAS @ expected <> IF  expected LAST-MEAS @ FC-SET-ARITY  name-ch FC-LINE  THEN THEN ;
\ Shards suppress the (capped) metamorphic inconsistency logs so N parallel
\ children do not interleave them on the shared stdout; false-cert lines
\ (FC-LINE) stay unconditional because a false-cert fails the phase. The logs
\ are capped/quiet but the finding is FATAL either way: the NSI/NRI/NCI
\ counters fail the run at the summary (FINISH) and fail the shard (SHARD-CHILD)
\ — an inconsistency-reporting property tester that exits 0 is error masking.
variable SWEEP-QUIET
: LOG-META  ( ptr u8 n -- ) {: a:ptr u:n :}
   SWEEP-QUIET @ if exit then
   s" prop-test: metamorphic " type a u type s"  inconsistency: " type POS. cr
   s" variant: " type PBUF PBUF-U @ type cr
   s" body: " type BODY. ;

\ ---- metamorphic subsumption: an i64-certified body must also certify under the
\ generic ( n -- n ) sig (n subsumes i64). If it does, run it too (free false-cert
\ coverage); i64-cert but n-reject is a checker inconsistency (fatal at FINISH). ----
variable NSUB  variable NSI
: SUBSUME  ( -- )   \ pre: BBUF/NIN/DOUT is the certified i64 program G
   SMARK  1 TFLAG !  83 NIN @ DOUT @ HEAD  BODY+  0 TFLAG !  PBUF PBUF-U @ CHK
   VERD @ -1 = IF  NSUB @ 1+ NSUB !  83 NIN @ DOUT @ MEASURE
   ELSE  NSI @ LOG-LIMIT < IF s" subsumption" LOG-META THEN  NSI @ 1+ NSI !  THEN
   SFORGET ;

\ ---- metamorphic render round-trip: render the just-certified body's effect, then
\ re-declare the SAME body with that exact rendered sig — it must re-certify. ----
variable NRT  variable NRI  variable RSA  variable RSU
TRUSTED: REND-SIG$ ( -- ptr u8 n )
   REND-SIG ;
: ROUNDTRIP  ( -- )   \ pre: G just certified; REND-SIG holds G's rendered effect
   REND-SIG$  RSU !  RSA !
   SMARK  0 PBUF-U ! s" : R ( " P+  RSA @ RSU @ P+  s"  ) " P+  BBUF BLEN @ P+  s" ; " P+  PBUF PBUF-U @ CHK
   VERD @ -1 = IF  NRT @ 1+ NRT !  82 NIN @ DOUT @ MEASURE
   ELSE  NRI @ LOG-LIMIT < IF s" round-trip" LOG-META THEN  NRI @ 1+ NRI !  THEN
   SFORGET ;

\ ---- metamorphic composition: A:(x--y) and B:(y--z) both certified => ': C A B ;'
\ must certify ( x -- z ) and run-match — chains arities; catches what one body can't. ----
variable NCMP  variable NCI  variable CAI  variable CAO  variable CBO
: COMPOSE  ( -- )
   SMARK  0 TFLAG !
   3 RND% 0 GEN-BODY  NIN @ CAI !  DEP @ CAO !  88 CAI @ CAO @ HEAD  BODY+  PBUF PBUF-U @ CHK   \ X=88
   VERD @ -1 = IF
      CAO @ 0 GEN-BODY  DEP @ CBO !  89 CAO @ CBO @ HEAD  BODY+  PBUF PBUF-U @ CHK              \ Y=89, in=CAO
      VERD @ -1 = IF
         0 PBUF-U ! s" : Z ( " P+  CAI @ PTY  s" -- " P+  CBO @ PTY  s" ) X Y ; " P+  PBUF PBUF-U @ CHK   \ Z=90
         VERD @ -1 = IF  NCMP @ 1+ NCMP !  90 CAI @ CBO @ MEASURE
         ELSE  NCI @ LOG-LIMIT < IF s" composition" LOG-META THEN  NCI @ 1+ NCI !  THEN
      THEN
   THEN
   SFORGET ;

\ ---- shrinking: on a FALSE-CERT, delta-debug BBUF down to a minimal body that
\ STILL satisfies a predicate (certify-and-mismatch in real use; "still certifies"
\ in the self-test). Drops one trailing token per step, restoring any drop that
\ breaks the predicate. Token surgery just moves BLEN — the bytes stay put. ----
variable BSAVE
: TRIM-TRAIL ( -- )  begin BLEN @ 0 > BBUF BLEN @ 1- + c@ 32 = and while  BLEN @ 1- BLEN !  repeat ;
: DROP-LAST  ( -- bool )   \ remove the last space-delimited token; f = did-remove
   TRIM-TRAIL  BLEN @ 0= IF 0 0= 0= exit THEN
   begin BLEN @ 0 > BBUF BLEN @ 1- + c@ 32 <> and while  BLEN @ 1- BLEN !  repeat  0 0= ;
: REBUILD-G ( -- )  0 TFLAG !  71 NIN @ DOUT @ HEAD  BODY+ ;   \ PBUF := ": G ( NIN -- DOUT ) BBUF ;"
: FCFAIL?  ( -- bool )   \ does the current BBUF certify AND run to an arity != DOUT (or trap)?
   SMARK  REBUILD-G  PBUF PBUF-U @ CHK
   VERD @ -1 = IF  71 NIN @ RUN-MEAS
      LAST-TRAP @ 0= 0= IF  0 0=  ELSE  LAST-MEAS @ DOUT @ <>  THEN
   ELSE  0 0= 0=  THEN  SFORGET ;
: STILLCERT? ( -- bool )  SMARK  REBUILD-G  PBUF PBUF-U @ CHK  VERD @ -1 =  SFORGET ;
\ Differential boundary: deliberately compiles a checker-REJECTED body to
\ confirm a false reject - unchecked compile is the point. Queued owner:
\ habu-seal-set-check-b3676b33 (test set-check behind the friend latch).
TRUSTED: CONFIRM-FR? ( -- bool )   \ compile unchecked, run, and prove the rejected true-sig body matches
   SMARK  0 set-check  PBUF PBUF-U @ evaluate  ['] PROP-CHECK-HOOK set-check
   ERR@ 0 = IF  71 NIN @ RUN-MEAS
      LAST-TRAP @ IF  0  ELSE  LAST-MEAS @ DOUT @ =  THEN
   ELSE  0  THEN  SFORGET ;
: LOG-FR ( -- )
   s" prop-test: false-reject confirmed: " type POS. cr
   s" definition: " type REBUILD-G DEF. ;
\ typed-local-lint: allow-bare-local - pred keeps the predicate quotation from the stack signature.
: SHRINK  ( R [ -- bool ] -- R ) {: pred :}  \ minimize BBUF keeping (pred) true
   begin
      BLEN @ BSAVE !
      DROP-LAST if
         pred execute if 0 0= else BSAVE @ BLEN ! 0 0= 0= then
      else
         0 0= 0=
      then
   while repeat ;

\ ---- driver: base program (false-cert + false-reject), then metamorphic amplifiers ----
variable NFC0
: ONE  ( -- )
   MARK  GEN                                 \ checkpoint, then build ": G ( ... ) <body> ;"
   PBUF PBUF-U @ CHK
   VERD @ -1 = IF                            \ CERTIFIED: measured out-arity must equal declared
      NCERT @ 1+ NCERT !
      PROP-NFC @ NFC0 !  71 NIN @ DOUT @ MEASURE   \ base run-and-compare
      PROP-NFC @ NFC0 @ > IF                       \ a FALSE-CERT: shrink to a minimal counterexample & print
         s" original: " type  REBUILD-G  DEF.
         [: FCFAIL? ;] SHRINK
         s" minimized counterexample: " type  REBUILD-G  DEF. THEN
      ROUNDTRIP  SUBSUME                      \ metamorphic amplifiers on the certified body
   ELSE VERD @ 0 = PERT? @ 0= and IF         \ rejected but NOT perturbed -> the generator declared the
      CONFIRM-FR? IF                          \ true arity and execution agree: a FALSE-REJECT
         NFR @ LOG-LIMIT < IF LOG-FR THEN
         NFR @ 1+ NFR !
      THEN
   THEN THEN
   FORGET                                    \ forget G (code + dict entry + recorded sig)
   COMPOSE ;                                  \ independent two-body composition probe
: RUN-CORE  ( n n -- )   \ generate + measure across N programs; prints only findings
   N !  dup RUN-SEED !  SEED !  0 NCERT ! 0 PROP-NFC ! 0 NFR !  0 NSUB ! 0 NSI ! 0 NRT ! 0 NRI ! 0 NCMP ! 0 NCI !
   0 RI ! begin RI @ N @ < while  ONE  RI @ 1+ RI !  repeat ;
: RUN  ( n n -- )   \ RUN-CORE plus the per-run summary (serial repro path)
   RUN-CORE
   s" prop-test: " type N @ . s" programs, " type
   NCERT @ . s" certified, " type  PROP-NFC @ . s" FALSE-CERT(s), " type
   NFR @ . s" false-reject(s)" type cr
   s" prop-test: metamorphic — " type  NSUB @ . s" subsumption + " type
   NRT @ . s" round-trip + " type  NCMP @ . s" composition runs; " type
   NSI @ NRI @ + NCI @ +  . s" inconsistency(ies)" type cr ;

\ self-test: prove the detector has teeth (a sound checker won't hand us a real
\ false-cert, so confirm the arity comparison fires on a fabricated mismatch).
: SELFTEST ( -- )
   5 DOUT !
   4 DOUT @ <> 0= IF s" prop-test: self-test BROKEN" 1 die THEN
   4 4 <> IF s" prop-test: self-test BROKEN (equal flagged)" 1 die THEN
   s" prop-test: self-test OK (arity comparison fires)" type cr ;

\ regression baits: programs that a SOUND checker rejects. If a regression ever
\ certifies one, either arity or type/signature soundness regressed.
: BAIT  ( ptr u8 n -- )   \ MUST NOT certify
   CHK
   VERD @ -1 = IF s" prop-test: BAIT certified - checker soundness regressed!" 1 die THEN ;
: BAITS ( -- )
   s" : G ( -- ) 3 0 ?do 99 leave loop ;"            BAIT   \ leave carries an extra value
   s" : G ( i64 -- i64 ) dup 0 < if 0 0 exit then ;" BAIT   \ exit-path arity != fall-through
   s" : G ( -- i64 ) 5 0 ?do leave 9 loop ;"         BAIT   \ leave-point != loop-exit
   s" : G ( i64 -- i64 ) 0= ;"                       BAIT   \ bool must not refine to concrete i64
   s" : G ( i64 ) drop ;"                            BAIT   \ malformed sig: missing --
   s" : G ( [ -- ) ;"                                BAIT   \ malformed quotation sig
   s" prop-test: baits OK (control/type/signature regressions rejected)" type cr ;

\ alphabet self-test: every structural op class must (a) CERTIFY as a minimal
\ ( i64 -- i64 ) body - the checker accepts the class - and (b) be GENERATED by
\ the fuzzer within a capped deterministic-seed sweep - the class is reachable.
\ A renumbered K table or a narrowed RND% bound that silently dropped a class
\ from the explored space dies here instead of shrinking coverage unnoticed.
9 constant ALPHA-N
400 constant ALPHA-CAP
create ALPHA-SEEN ALPHA-N allot
variable PH-I  variable PH-J  variable PH-HIT
variable ALPHA-I  variable ALPHA-J  variable ALPHA-TRIES
: PROP-HAS? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   v 0= IF 0 0= exit THEN
   0 PH-I !
   begin PH-I @ v + u <= while
      -1 PH-HIT !  0 PH-J !
      begin PH-J @ v < while
         a PH-I @ PH-J @ + + c@  b PH-J @ + c@ <> IF 0 PH-HIT !  v PH-J ! ELSE PH-J @ 1+ PH-J ! THEN
      repeat
      PH-HIT @ IF 0 0= exit THEN
      PH-I @ 1+ PH-I !
   repeat 0 0= 0= ;
: ALPHA-DET$ ( n -- ptr u8 n ) {: i:n :}   \ the substring PROP-STEP emits for class i
   i 0 = IF OP-LOOP$ exit THEN
   i 1 = IF OP-LEAVE$ exit THEN
   i 2 = IF OP-STR$ exit THEN
   i 3 = IF OP-QUOT0$ exit THEN
   i 4 = IF OP-BRANCH$ exit THEN
   i 5 = IF OP-RSTK$ exit THEN
   i 6 = IF OP-RCOPY$ exit THEN
   i 7 = IF OP-QUOT1$ exit THEN
   s" {: z" ;
: ALPHA-CERT$ ( n -- ptr u8 n ) {: i:n :}  \ a minimal certifiable instance of class i
   i 8 = IF s" {: z0 :} z0 " exit THEN
   i ALPHA-DET$ ;
: ALPHA-CERT1 ( n -- ) {: i:n :}   \ ": G ( i64 -- i64 ) <class-op> ;" must certify
   SMARK
   0 BLEN !  i ALPHA-CERT$ PROP-B+
   0 LOC? !  0 TFLAG !  1 NIN !  1 DOUT !
   71 1 1 HEAD  BODY+
   PBUF PBUF-U @ CHK
   VERD @ -1 <> IF
      s" prop-test: alphabet class REJECTED: " type i ALPHA-CERT$ type cr
      s" prop-test: alphabet self-test FAILED (class no longer certifies)" 1 die THEN
   SFORGET ;
: ALPHA-RESET ( -- )
   0 ALPHA-J ! begin ALPHA-J @ ALPHA-N < while
      0 ALPHA-SEEN ALPHA-J @ + c!  ALPHA-J @ 1+ ALPHA-J !
   repeat ;
: ALPHA-SCAN ( -- )   \ mark every class whose op text appears in the generated BBUF
   0 ALPHA-J ! begin ALPHA-J @ ALPHA-N < while
      BBUF BLEN @ ALPHA-J @ ALPHA-DET$ PROP-HAS? IF 1 ALPHA-SEEN ALPHA-J @ + c! THEN
      ALPHA-J @ 1+ ALPHA-J !
   repeat ;
: ALPHA-MISSING ( -- n )   \ first unseen class, or -1 when all seen
   0 ALPHA-J ! begin ALPHA-J @ ALPHA-N < while
      ALPHA-SEEN ALPHA-J @ + c@ 0= IF ALPHA-J @ exit THEN
      ALPHA-J @ 1+ ALPHA-J !
   repeat -1 ;
: SELFTEST-ALPHABET ( -- )
   0 ALPHA-I ! begin ALPHA-I @ ALPHA-N < while
      ALPHA-I @ ALPHA-CERT1  ALPHA-I @ 1+ ALPHA-I !
   repeat
   12345 SEED !  ALPHA-RESET  0 ALPHA-TRIES !
   begin ALPHA-MISSING -1 <> ALPHA-TRIES @ ALPHA-CAP < and while
      GEN  ALPHA-SCAN  ALPHA-TRIES @ 1+ ALPHA-TRIES !
   repeat
   ALPHA-MISSING -1 <> IF
      s" prop-test: alphabet class NEVER GENERATED within cap: " type ALPHA-MISSING ALPHA-DET$ type cr
      s" prop-test: alphabet self-test FAILED (class unreachable)" 1 die THEN
   s" prop-test: alphabet OK (" type ALPHA-N . s" op classes generated + certified)" type cr ;

\ shrink self-test: a long certified ( i64 -- i64 ) body must REDUCE under the
\ "still certifies" predicate — proves the delta-debug loop + token surgery work
\ (a sound checker never hands us a real false-cert to shrink, so exercise the
\ machinery on an achievable predicate).
: SELFTEST-SHRINK ( -- )
   0 BLEN !  s" dup drop 1+ 1- negate 1+ 1- " PROP-B+  1 NIN !  1 DOUT !
   BLEN @  [: STILLCERT? ;] SHRINK  BLEN @  >  0= IF
      s" prop-test: self-test SHRINK BROKEN (no reduction)" 1 die THEN
   s" prop-test: shrink OK (delta-debug reduced to: " type  REBUILD-G  PBUF PBUF-U @ type s" )" type cr ;

\ Fail loudly on any false-cert (`die` exits with the code; IF/THEN are
\ compile-only so this is wrapped in a word). A clean run reaches end-of-input,
\ which exits 0 in batch mode — no `bye` needed (the engine has none).
: NMETA ( -- n )  NSI @ NRI @ + NCI @ + ;
: FINISH
   PROP-NFC @ 0 > IF s" prop-test: FALSE-CERT found" 1 die THEN
   NMETA 0 > IF s" prop-test: METAMORPHIC-INCONSISTENCY found" 1 die THEN ;
variable ARG-N  variable ARG-I  variable ARG-L
: ARG>U?  ( ptr u8 -- n bool ) {: z:ptr :}   \ parse a non-empty decimal argv c-string
   z ZLEN ARG-L !  ARG-L @ 0= IF 0 0 0= 0= exit THEN
   0 ARG-N !  0 ARG-I !
   begin ARG-I @ ARG-L @ < while
      z ARG-I @ + c@  dup 48 < over 57 > or IF drop 0 0 0= 0= exit THEN
      48 -  ARG-N @ 10 * +  ARG-N !
      ARG-I @ 1+ ARG-I !
   repeat  ARG-N @ 0 0= ;
: USAGE ( -- )  s" prop-test: usage: bin/hb [seed count] < test/prop-test.f" 64 die ;
: ARG-U ( n -- n )  ARGV ARG>U? 0= IF drop USAGE THEN ;
\ ============================================================================
\ Primitive-effect axiom differential census (dot habu-primitive-effect-axiom).
\ The PES table in src/core/checker.f is the single, minimal typing trust root:
\ every checked primitive's stack effect is one axiom row. This walks the LIVE
\ table, classifies each axiom, and for every executable axiom builds a runner,
\ executes it, and asserts the measured out-arity equals the declared dout — a
\ per-axiom differential test. Non-executable axioms (syscalls, control, parser
\ literals, defining words, engine/checker introspection, emitters, atomics,
\ FFI) cannot be safely run in-process with dummy operands; their arity is
\ pinned instead by the native self-rebuild + behavioral gate. Every PES row
\ must be classified: an unclassified name fails the census, so a new axiom
\ cannot land without a difftest classification. See docs/effects.md
\ "Primitive axiom set".
\ ============================================================================

\ ---- trusted leaves: read the live PES axiom table + evaluate a runner -------
TRUSTED: AX-COUNT ( -- n )  #PE @ ;
TRUSTED: AX-NAME$ ( n -- ptr u8 n ptr u8 n )
   PE-SYM@ dup SYM-PKG$ rot SYM-NAME$ ;
TRUSTED: AX-STK ( n -- n )   \ node offset -> count of EN-PUSH nodes down to the base row
   0 swap
   begin dup E-PTR EN.TAG @ EN-PUSH = while swap 1+ swap E-PTR EN.B @ repeat drop ;
TRUSTED: AX-ARITY ( n -- n n )   \ pe-idx -> declared ( din dout )
   PE-EFF@ E-PTR dup ER.DIN @ AX-STK swap ER.DOUT @ AX-STK ;
TRUSTED: AXEVAL ( -- n )  PBUF PBUF-U @ evaluate ;

\ ---- category codes ----------------------------------------------------------
0 constant AX-NOEXEC   1 constant AX-GEN   2 constant AX-MEM   3 constant AX-FLOAT
4 constant AX-UNKNOWN

\ ---- census state ------------------------------------------------------------
variable AX-DIN-V   variable AX-DOUT-V
variable AX-N-EXEC  variable AX-N-NOEXEC  variable AX-BAD  variable AX-UNCLASS
variable AX-I       variable AX-LI        variable AX-TS
64 constant AXNAME-CAP
create AXNAME-BUF AXNAME-CAP allot   variable AXNAME-U
64 constant AXBUF-CAP
create AXBUF AXBUF-CAP allot

\ ---- small checked string helpers -------------------------------------------
: AX-STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: AX-STARTS? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v < if 0 0= 0= exit then
   0 begin dup v < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: AX-HAS-QUOTE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ 34 = if drop 0 0= exit then
      1+
   repeat drop 0 0= 0= ;

\ whole-word membership in a space-delimited list
: AX-LIST-HAS? ( ptr u8 n ptr u8 n -- bool ) {: na:ptr nu:n la:ptr lu:n :}
   0 AX-LI !
   begin AX-LI @ lu < while
      begin AX-LI @ lu < la AX-LI @ + c@ 32 = and while AX-LI @ 1+ AX-LI ! repeat
      AX-LI @ AX-TS !
      begin AX-LI @ lu < la AX-LI @ + c@ 32 <> and while AX-LI @ 1+ AX-LI ! repeat
      AX-LI @ AX-TS @ > if
         la AX-TS @ +  AX-LI @ AX-TS @ -  na nu AX-STR= if 0 0= exit then
      then
   repeat 0 0= 0= ;

\ ---- classification lists (folded lowercase, as stored in the symbol table) --
: AX-GEN-LIST ( -- ptr u8 n )
   s"  dup drop swap over nip tuck rot -rot 2dup 2drop 2swap 2over + - * and or xor 1+ 1- negate invert 0= 0< = < > <> <= >= / mod /mod abs min max lshift rshift cells cell+ chars char+ depth here rbase cp@ dbase@ check@ ndict@ data-base get-current epoch-seconds mono-ns script-argc . u. emit cr space s>f wf-n@ locw-hw-n@ tfam-n@ sumv-n@ pf-n@ tf-str-u@ tf-pk-n@ schema-n@ schema-root-n@ wf-wide? wf-needs-p2? wf-w-at lower-cert:magic lower-cert:version lower-cert:header-cells lower-cert:magic-cell lower-cert:version-cell lower-cert:total-bytes-cell lower-cert:needs-cell lower-cert:wf-count-cell lower-cert:bind-count-cell lower-cert:fetch-count-cell lower-cert:fetch-data-cells-cell lower-cert:wf-cells lower-cert:fetch-cells lower-cert:check-cells lower-cert:guard-cells lower-cert:fetch-flag lower-cert:store-flag lower-cert:body-len-cell lower-cert:body-hash-cell lower-cert:fnv-offset lower-cert:fnv-prime lower-cert:cell-count " ;
: AX-MEM-LIST ( -- ptr u8 n )
   s"  @ ! ptr-field +! c@ c! count rd32 core-str= core-str=ci tfam-ctor-word? type " ;
: AX-FLOAT-LIST ( -- ptr u8 n )
   s"  f+ f- f* f/ fnegate fabs fsqrt f< f> f= f0< f0= f>s " ;
: AX-NOEXEC-A ( -- ptr u8 n )
   s"  open read ioctl mmap path0 open-rd access unlink rename chmod symlink readlink mkdir rmdir stat64 lstat64 getdirentries64 pipe dup2 fcntl poll kill setpgid write close " ;
: AX-NOEXEC-B ( -- ptr u8 n )
   s"  fence run-in-stack .s allot , c, script-argv$ throw die fork wait-rc wait-status patch32 snap-rebase prof-on prof-report cp! ndict! set-current wordlist search-wl parse-name pathz check-candidate! ['] char [char] create variable constant f. atomic@ atomic! atomic-add atomic-cas ffi-call-bounded " ;

\ Checker-substrate introspection that cannot take dummy operands, plus the seal
\ watermark capture. The indexed accessors fail closed with `76 die` on an
\ out-of-range index (WF-ROW@ checker.f, TF-REC@ type-family.f), so a dummy `7`
\ kills the census process whenever the live table is shorter; seal-capture
\ (native BSEALCAP) rewrites the sealed friend-band ndict watermark, mutating
\ live seal state mid-process like cp!/ndict!. Their arity is pinned by the
\ native self-rebuild + behavioral gate. The zero-arg high-water readers
\ (wf-n@ tfam-n@ sumv-n@ pf-n@ tf-str-u@ tf-pk-n@ schema-n@ schema-root-n@) are pure
\ variable reads and stay difftested in AX-GEN-LIST, matching ndict@/cp@.
\ wf-wide?/wf-needs-p2? (zero-arg scans) and wf-w-at (indexed with a total
\ 1-default, never dies) are likewise difftested in AX-GEN-LIST;
\ layout-valid-desc-cell is checker-only metadata with no runtime dictionary
\ word, so it is censused but cannot execute. locw-hw@ carries the same
\ 76-die index guard as wf-tokix@ (a dummy seq past LOCSEQ dies), and the
\ pass-2 live-table words (p2-carve-w / p2-live-w@ / p2-live-cum@ /
\ p2-locseq-reset, checker.f) read or mutate live pass-2 compile scratch
\ (P2SEQ/P2LW), so they all sit here. wide-mark stamps DNAME-WIDE on the
\ newest published dict record inside an mprotect bracket (a live dictionary
\ mutation, seal-capture class), and rec-wide-publish consumes the checker's
\ RECW latch and may call it — neither can run under census dummies.
\ prot-wid-add mutates the sealed friend-band protected-WID registry (a
\ seal-capture-class live seal mutation) and its overflow path exits the
\ process (NR-EXIT-GROUP rc 84), so it can never take a dummy operand. The
\ owner-wid predicates observe the mutable protected owner registry; generic
\ census stacks cannot supply a meaningful registry state, so they are
\ state-dependent noexec axioms rather than pure generated cases.
\ tfam-ctor-word? is a pure registry-read predicate and stays difftested in
\ AX-MEM-LIST (empty census registry -> false, one flag out).
: AX-NOEXEC-C ( -- ptr u8 n )
   s"  seal-capture seal-friend prot-wid-add owner-wid-preflight? owner-wid-public? owner-wid-private? owner-wid? wf-tokix@ wf-off@ wf-pos@ wf-fam@ wf-width@ wf-term@ wf-flags@ tfam-width@ locw-hw@ p2-carve-w p2-live-w@ p2-live-cum@ p2-locseq-reset wide-mark rec-wide-publish layout-valid-desc-cell tfam-name$ tfam-arity@ tfam-kind@ tfam-public? tfam-derive-eq? tfam-derive-hash? tfam-var-start@ tfam-var-count@ sumv-name$ sumv-ctor-pkg$ lower-cert:cell@ lower-cert:bytes " ;

: AX-CAT ( ptr u8 n -- n )
   2dup AX-HAS-QUOTE? if 2drop AX-NOEXEC exit then
   2dup s" spawn" AX-STARTS? if 2drop AX-NOEXEC exit then
   2dup s" checker-" AX-STARTS? if 2drop AX-NOEXEC exit then
   2dup s" checker-cert:" AX-STARTS? if 2drop AX-NOEXEC exit then
   2dup s" lower-cert-hook:" AX-STARTS? if 2drop AX-NOEXEC exit then
   2dup s" diag-" AX-STARTS? if 2drop AX-NOEXEC exit then
   2dup s" ffi-" AX-STARTS? if 2drop AX-NOEXEC exit then
   2dup AX-GEN-LIST AX-LIST-HAS? if 2drop AX-GEN exit then
   2dup AX-MEM-LIST AX-LIST-HAS? if 2drop AX-MEM exit then
   2dup AX-FLOAT-LIST AX-LIST-HAS? if 2drop AX-FLOAT exit then
   2dup AX-NOEXEC-A AX-LIST-HAS? if 2drop AX-NOEXEC exit then
   2dup AX-NOEXEC-B AX-LIST-HAS? if 2drop AX-NOEXEC exit then
   2dup AX-NOEXEC-C AX-LIST-HAS? if 2drop AX-NOEXEC exit then
   2drop AX-UNKNOWN ;

\ ---- name buffer -------------------------------------------------------------
: AX-NAME-SAVE ( ptr u8 n -- ) {: a:ptr u:n :}
   u AXNAME-U !
   0 begin dup u < while
      dup a + c@ over AXNAME-BUF + c!
      1+
   repeat drop ;
package AXIOM-NAME
public

: SAVE-SYMBOL ( ptr u8 n ptr u8 n -- ) {: pkg:ptr pkgu:n name:ptr nameu:n :}
   pkgu nameu + pkgu 0 > if 1 + then
   AXNAME-CAP > if s" prim-axiom: qualified name too long" 76 die then
   0 AXNAME-U !
   pkgu 0 > if
      pkg pkgu AX-NAME-SAVE
      58 AXNAME-BUF AXNAME-U @ + c!
      AXNAME-U @ 1 + AXNAME-U !
   then
   0 begin dup nameu < while
      dup name + c@ AXNAME-BUF AXNAME-U @ + c!
      AXNAME-U @ 1 + AXNAME-U !
      1+
   repeat drop ;

;package
: AXNAME$ ( -- ptr u8 n )  AXNAME-BUF AXNAME-U @ ;

\ ---- MEM operand recipes (real AXBUF; LDR/STR tolerate the buffer) -----------
: AX-MEM-OPS ( ptr u8 n -- ptr u8 n )
   2dup s" @" AX-STR= if 2drop s" AXBUF " exit then
   2dup s" c@" AX-STR= if 2drop s" AXBUF " exit then
   2dup s" count" AX-STR= if 2drop s" AXBUF " exit then
   2dup s" rd32" AX-STR= if 2drop s" AXBUF " exit then
   2dup s" !" AX-STR= if 2drop s" 7 AXBUF " exit then
   2dup s" +!" AX-STR= if 2drop s" 7 AXBUF " exit then
   2dup s" c!" AX-STR= if 2drop s" 7 AXBUF " exit then
   2dup s" ptr-field" AX-STR= if 2drop s" AXBUF 0 " exit then
   2dup s" type" AX-STR= if 2drop s" AXBUF 0 " exit then
   2dup s" core-str=" AX-STR= if 2drop s" AXBUF 3 AXBUF 3 " exit then
   2dup s" core-str=ci" AX-STR= if 2drop s" AXBUF 3 AXBUF 3 " exit then
   2dup s" tfam-ctor-word?" AX-STR= if 2drop s" AXBUF 3 " exit then
   2drop s"  " ;

\ ---- runner builders: PBUF := "depth BASE ! <ops> <name> depth BASE @ - CLEAR-MEAS"
: AX-BUILD ( ptr u8 n ptr u8 n -- )   \ ( name-a name-u ops-a ops-u )
   0 PBUF-U !
   s" depth BASE ! " P+
   P+                       \ ops (top pair)
   s"  " P+
   P+                       \ name (remaining pair)
   s"  depth BASE @ - CLEAR-MEAS" P+ ;

: AX-BUILD-REP ( ptr u8 n n -- )   \ ( name-a name-u din ); repeat "7 " din times
   {: din:n :}
   0 PBUF-U !
   s" depth BASE ! " P+
   0 begin dup din < while s" 7 " P+ 1+ repeat drop
   P+                       \ name
   s"  depth BASE @ - CLEAR-MEAS" P+ ;

: AX-BUILD-FREP ( ptr u8 n n -- )   \ ( name-a name-u din ); repeat "1 s>f " din times
   {: din:n :}
   0 PBUF-U !
   s" depth BASE ! " P+
   0 begin dup din < while s" 1 s>f " P+ 1+ repeat drop
   P+
   s"  depth BASE @ - CLEAR-MEAS" P+ ;

\ ---- measure (eval PBUF; set LAST-MEAS/LAST-TRAP) ----------------------------
: AX-MEASURE ( -- )
   0 LAST-TRAP !
   AXEVAL
   ERR@ 0 = if
      dup 0 < if drop -1 LAST-TRAP ! else LAST-MEAS ! then
   else drop -1 LAST-TRAP ! then ;

: AX-BAD-REPORT ( -- )
   s" prim-axiom: MISMATCH " type AXNAME$ type
   s"  declared-dout " type AX-DOUT-V @ .
   LAST-TRAP @ if s" measured-trap" type else s" measured " type LAST-MEAS @ . then cr ;

: AX-CHECK ( -- )   \ PBUF built; run + compare measured to AX-DOUT-V; bump counters
   AX-MEASURE
   AX-N-EXEC @ 1+ AX-N-EXEC !
   LAST-TRAP @ if AX-BAD-REPORT AX-BAD @ 1+ AX-BAD ! exit then
   LAST-MEAS @ AX-DOUT-V @ <> if AX-BAD-REPORT AX-BAD @ 1+ AX-BAD ! then ;

: AX-RUN-MEM ( ptr u8 n -- )
   2dup AX-MEM-OPS AX-BUILD AX-CHECK ;

: AX-DISPATCH ( ptr u8 n n -- ) {: cat:n :}
   cat AX-GEN = if AX-DIN-V @ AX-BUILD-REP AX-CHECK exit then
   cat AX-FLOAT = if AX-DIN-V @ AX-BUILD-FREP AX-CHECK exit then
   cat AX-MEM = if AX-RUN-MEM exit then
   cat AX-NOEXEC = if 2drop AX-N-NOEXEC @ 1+ AX-N-NOEXEC ! exit then
   s" prim-axiom: UNCLASSIFIED " type type cr
   AX-UNCLASS @ 1+ AX-UNCLASS ! ;

: AX-ROW ( n -- ) {: i:n :}
   i AX-ARITY AX-DOUT-V ! AX-DIN-V !
   i AX-NAME$ AXIOM-NAME:SAVE-SYMBOL
   AXNAME$ 2dup AX-CAT AX-DISPATCH ;

: AX-CENSUS ( -- )
   0 AX-N-EXEC ! 0 AX-N-NOEXEC ! 0 AX-BAD ! 0 AX-UNCLASS !
   0 AX-I !
   begin AX-I @ AX-COUNT < while AX-I @ AX-ROW AX-I @ 1+ AX-I ! repeat
   s" prim-axiom: " type AX-COUNT . s" axioms (" type
   AX-N-EXEC @ . s" difftested, " type AX-N-NOEXEC @ . s" noexec); " type
   AX-UNCLASS @ . s" unclassified, " type AX-BAD @ . s" mismatch(es)" type cr
   AX-UNCLASS @ AX-BAD @ + 0 > if s" prim-axiom: AXIOM CENSUS FAILED" 1 die then
   s" prim-axiom: census OK (every PES axiom classified; executable axioms difftested)" type cr ;

\ self-test with teeth: a fabricated wrong declared-dout must be detected
: AX-SELFTEST ( -- )
   1 AX-DIN-V !  5 AX-DOUT-V !  0 AX-BAD !
   s" dup" AX-NAME-SAVE
   AXNAME$ AX-DIN-V @ AX-BUILD-REP AX-CHECK
   AX-BAD @ 0 = if s" prim-axiom: self-test BROKEN (lying axiom not detected)" 1 die then
   s" prim-axiom: self-test OK (difftest detects a lying axiom)" type cr ;

: PROP-RUN ( n n -- )
   SELFTEST
   SELFTEST-SHRINK
   BAITS
   SELFTEST-ALPHABET
   AX-SELFTEST
   AX-CENSUS
   RUN
   FINISH ;

\ ---- seed-sweep: shard the sweep across PROP-SHARD-N forked slots, each a
\ distinct seed for DEFAULT-COUNT iterations, so one gate phase covers
\ N x DEFAULT-COUNT distinct-seed programs in parallel. Self-tests + baits run
\ once in the parent; children run silently (SWEEP-QUIET) and die 1 on a
\ false-cert or metamorphic inconsistency. One red shard fails the phase.
\ Distinct seeds come from a per-run
\ base spread by a 32-bit golden-ratio step so no two slots share an LCG walk. ----
8 constant PROP-SHARD-N
$9E3779B1 constant PROP-SHARD-STEP
create PROP-SHARD-PIDS PROP-SHARD-N cells allot
variable SWEEP-BASE  variable SWEEP-RED  variable SWEEP-I

: SHARD-SEED ( n -- n ) {: i:n :}
   SWEEP-BASE @ i PROP-SHARD-STEP * + $7FFFFFFF and dup 0= if drop 1 then ;
: SHARD-PID! ( pid n -- ) {: p:pid i:n :}
   p PID>N i cells PROP-SHARD-PIDS + ! ;
: SHARD-PID@ ( n -- pid ) {: i:n :}
   i cells PROP-SHARD-PIDS + @ >PID ;
1 constant PROP-O-WRONLY   \ O_WRONLY (macOS/Linux)
\ Point a descriptor at /dev/null (best effort): a shard checks hundreds of
\ intentionally-rejected fuzz programs, and the checker's per-reject
\ diagnostics on stderr (x N shards) would overflow the gate's bounded stderr
\ capture. A false-cert reports on stdout (FC-LINE) and via the shard's
\ nonzero exit, so muting stderr is safe.
: MUTE-FD ( n -- ) {: dst:n :}
   s" /dev/null" >LEN PROC-PATHZ PROP-O-WRONLY 0 open {: fd:n :}
   fd 0 < if exit then
   fd dst dup2 drop
   fd close ;
: SHARD-MUTE-STDERR ( -- )
   2 MUTE-FD ;
\ The named failure line goes to STDOUT (fd 2 is muted in shards; `die` writes
\ stderr, so its message would vanish) and carries the shard seed so any red is
\ reproducible serially via `bin/hb <seed> <count>`.
: SHARD-FAIL ( ptr u8 n -- )
   type s"  in shard seed " type RUN-SEED @ .
   s" " 1 die ;
\ Fault-injection seam for the sweep red-path self-test ONLY: when set, a shard
\ dies red before running any iteration, so SELFTEST-SWEEP-RED can prove one
\ red shard fails the whole sweep at the cost of the forks alone. Never set
\ outside that self-test's probe child.
variable SHARD-FAULT
: SHARD-CHILD ( n -- )   \ never returns: run this shard's seed, then exit 0 / 1
   SHARD-MUTE-STDERR
   SHARD-FAULT @ IF s" prop-test: FAULT-INJECT" SHARD-FAIL THEN
   SHARD-SEED DEFAULT-COUNT RUN-CORE
   PROP-NFC @ 0 > IF s" prop-test: FALSE-CERT" SHARD-FAIL THEN
   NMETA 0 > IF s" prop-test: METAMORPHIC-INCONSISTENCY" SHARD-FAIL THEN
   s" " 0 die ;
: SHARD-FORK ( n -- ) {: i:n :}
   PROC-FORK-RAW {: p:pid :}
   p PID>N 0= IF i SHARD-CHILD THEN   \ child diverges (dies), only the parent falls through
   p i SHARD-PID! ;
: SHARD-JOIN ( n -- ) {: i:n :}
   i SHARD-PID@ PROC-WAIT-RC MATCH result ok OF drop ENDOF err OF drop -1 SWEEP-RED ! ENDOF ;MATCH ;
: SWEEP  ( n -- )   \ base seed -> fork all shards, join all, fail on any red
   SWEEP-BASE !  0 SWEEP-RED !  -1 SWEEP-QUIET !
   0 SWEEP-I ! begin SWEEP-I @ PROP-SHARD-N < while  SWEEP-I @ SHARD-FORK  SWEEP-I @ 1+ SWEEP-I ! repeat
   0 SWEEP-I ! begin SWEEP-I @ PROP-SHARD-N < while  SWEEP-I @ SHARD-JOIN  SWEEP-I @ 1+ SWEEP-I ! repeat
   SWEEP-RED @ IF s" prop-test: sweep FAILED (a shard reported above: FALSE-CERT or METAMORPHIC-INCONSISTENCY)" 1 die THEN
   s" prop-test: sweep OK — " type PROP-SHARD-N . s" shards x " type DEFAULT-COUNT . s" iters, distinct seeds" type cr ;

\ shard-seed self-test: distinct per-slot streams. The golden-ratio step is
\ odd, so i*STEP mod 2^31 is injective over the shard range, and the LCG's odd
\ multiplier makes each step a bijection on the state space - distinct seeds
\ can never merge into one walk. Pin pairwise distinctness and the 31-bit
\ nonzero clamp across representative bases, including one where base + STEP
\ wraps to exactly 0 and exercises the 0->1 clamp.
variable SS-I  variable SS-J  variable SS-BAD
: SHARD-SEEDS-CHECK ( n -- ) {: base:n :}
   base SWEEP-BASE !
   0 SS-I ! begin SS-I @ PROP-SHARD-N < while
      SS-I @ SHARD-SEED {: si:n :}
      si 1 < si $7FFFFFFF > or IF -1 SS-BAD ! THEN
      0 SS-J ! begin SS-J @ SS-I @ < while
         SS-J @ SHARD-SEED si = IF -1 SS-BAD ! THEN
         SS-J @ 1+ SS-J !
      repeat
      SS-I @ 1+ SS-I !
   repeat ;
: SELFTEST-SHARD-SEEDS ( -- )
   0 SS-BAD !
   1 SHARD-SEEDS-CHECK
   $7FFFFFFF SHARD-SEEDS-CHECK
   $61C8864F SHARD-SEEDS-CHECK
   SS-BAD @ IF s" prop-test: shard-seed self-test FAILED (duplicate or out-of-range slot seed)" 1 die THEN
   s" prop-test: shard-seeds OK (distinct per-slot seed streams)" type cr ;

\ sweep red-path self-test: one red shard must fail the whole sweep. The probe
\ child mutes its own stdout+stderr, arms the fault seam, and runs a sweep in
\ which every shard dies red before its first iteration - so the probe costs
\ only the forks. The parent asserts the probe exits 1 (SWEEP's red die); a
\ probe exiting 0 means shard reds no longer propagate and this dies.
: SWEEP-RED-CHILD ( -- )   \ never returns
   1 MUTE-FD  2 MUTE-FD
   -1 SHARD-FAULT !
   7 SWEEP
   s" " 0 die ;
: SELFTEST-SWEEP-RED ( -- )
   PROC-FORK-RAW {: p:pid :}
   p PID>N 0= IF SWEEP-RED-CHILD THEN
   p PROC-WAIT-RC MATCH result ok OF ENDOF err OF ENDOF ;MATCH 1 <> IF   \ completion code; a red shard must exit 1
      s" prop-test: sweep-red self-test FAILED (a red shard did not fail the sweep)" 1 die THEN
   s" prop-test: sweep-red OK (one red shard fails the sweep)" type cr ;

: PROP-RUN-DEFAULT ( -- )   \ sharded sweep: self-tests + axiom census once, then N slots
   SELFTEST
   SELFTEST-SHRINK
   BAITS
   SELFTEST-ALPHABET
   SELFTEST-SHARD-SEEDS
   SELFTEST-SWEEP-RED
   AX-SELFTEST
   AX-CENSUS
   FRESH-SEED SWEEP ;

: PROP-MAIN ( -- )
   ARGC 1 = IF  PROP-RUN-DEFAULT
   ELSE ARGC 3 = IF  1 ARG-U  2 ARG-U  PROP-RUN
   ELSE  USAGE  THEN THEN ;
