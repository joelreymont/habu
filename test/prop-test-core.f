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
require lib/string.f
require lib/fs.f
require lib/process.f
require lib/process-fork.f

TRUSTED: PROP-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> if 70 throw then ;
TRUSTED: PROP-INSTALL-HOOK ( -- )
   LOWER-CERT-HOOK:INSTALL
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
   PROP-INSTALL-HOOK ;
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
   SMARK  0 set-check  PBUF PBUF-U @ evaluate  PROP-INSTALL-HOOK
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
\ table and requires that each row own exactly one audited proof recipe in the
\ ledger at the end of this file. Each recipe restates the row's identity
\ (defining package, primitive name, and per-slot typed operands) and one proof
\ kind. For an executable row the census first compiles a checked candidate from
\ the recipe's exact typed operands, then runs the primitive in-process guarded
\ by a value-provenance canary below its operands, and asserts the measured
\ out-arity equals the declared dout — so a primitive that reaches below its
\ declared inputs fails even when its final depth happens to match. A
\ non-executable row (syscall, control, parser literal, defining word,
\ engine/checker introspection, emitter, atomic, FFI) carries an explicit
\ fail-closed noexec recipe instead; its arity is pinned by the native
\ self-rebuild + behavioral gate. A missing, duplicate, or stale recipe, or an
\ identity/arity/type mutation, fails the census with the row identity, so no
\ axiom can land or drift without an audited recipe. See docs/effects.md
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

\ ---- census state ------------------------------------------------------------
variable AX-DIN-V   variable AX-DOUT-V
variable AX-N-EXEC  variable AX-N-NOEXEC  variable AX-BAD  variable AX-UNCLASS
variable AX-I
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

\ ---- proof recipes replace the old classification lists (folded lowercase) --
\ Every row's proof kind now comes from its audited recipe in the ledger at the
\ end of this file, cross-checked against the live table by the AXR package
\ below. The former AX-CAT name/prefix allowlists are gone: a new primitive is
\ classified only by adding its recipe, never by falling through a substring
\ heuristic.
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

\ ---- audited per-row proof recipes ------------------------------------------
\ One recipe per live PES row lives in the `\ AXR ...` ledger at the end of this
\ file (data, not code). The AXR package parses that ledger from this source,
\ binds each recipe to its live row by slot index, cross-checks identity + arity
\ against the live table, and — for an executable row — compiles a checked
\ candidate from the recipe's exact typed operands. A missing, duplicate, or
\ stale recipe, or an identity/arity/type mutation, fails the census naming the
\ row. CANARY / CANARY-VERIFY are the runtime value-provenance guard the runner
\ builders (below) plant beneath every operand row.
package AXR
public

0 constant NOEXEC   1 constant GEN   2 constant MEM   3 constant FLOAT
$4D3C2B1A constant CANARY            \ provenance guard planted below the operands

\ Runner tail: the runner leaves ( guard measured ); if the primitive left the
\ guard intact the measured out-depth stands, otherwise it reads -1 (a trap the
\ census reports), so reaching below the declared inputs is caught.
: CANARY-VERIFY ( n n -- n ) {: guard:n measured:n :}
   guard CANARY = if measured else -1 then ;

private

384 constant MAX                     \ live rows (315) + headroom
64 constant TOKEN-MAX                \ widest recipe line's token count
$40000 constant SRC-CAP              \ >= this source file, ledger included
9 constant CH-TAB   10 constant CH-LF   13 constant CH-CR   32 constant CH-SP
65 constant NOEXEC-PROBE             \ a known noexec slot for the stale-noexec teeth test

create SRC SRC-CAP allot   variable SRC-U
create OFF MAX cells allot   create LEN MAX cells allot
create KIND MAX cells allot  create DIN MAX cells allot  create DOUT MAX cells allot
create SEEN MAX allot
create TOK-O TOKEN-MAX cells allot   create TOK-U TOKEN-MAX cells allot
variable TOK-N   variable ROWS
variable POS   variable TPOS   variable START   variable IDX   variable TS

: CELL-AT ( n ptr a -- ptr a ) {: i:n a:ptr :}  i cells a + ;
: BYTE-AT ( n ptr u8 -- ptr u8 ) {: i:n a:ptr :}  i a + ;
: OFF@ ( n -- n )   OFF CELL-AT @ ;
: LEN@ ( n -- n )   LEN CELL-AT @ ;
: K@ ( n -- n )     KIND CELL-AT @ ;
: DIN@ ( n -- n )   DIN CELL-AT @ ;
: DOUT@ ( n -- n )  DOUT CELL-AT @ ;
: SEEN@ ( n -- n )  SEEN BYTE-AT c@ ;
: SEEN! ( n n -- ) {: value:n i:n :}  value i SEEN BYTE-AT c! ;
: ROW$ ( n -- ptr u8 n ) {: i:n :}  SRC i OFF@ + i LEN@ ;

: BAD ( -- )  s" prim-recipe: malformed recipe row" 1 die ;
: DUP-BAD ( n -- )   \ ( slot ) duplicate or out-of-range recipe slot
   s" prim-recipe: duplicate or out-of-range recipe slot " type . cr
   s" prim-recipe: proof failed" 1 die ;
: WS? ( n -- bool )
   dup CH-SP = if drop 0 0= exit then
   dup CH-TAB = if drop 0 0= exit then
   CH-CR = ;

\ ---- tokenizer over one recipe line -----------------------------------------
: T@ ( n -- ptr u8 n ) {: i:n :}
   i TOK-N @ >= if BAD then
   SRC i TOK-O CELL-AT @ + i TOK-U CELL-AT @ ;
: TOKEN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   TOK-N @ TOKEN-MAX >= if BAD then
   a SRC - TOK-N @ TOK-O CELL-AT !
   u TOK-N @ TOK-U CELL-AT !
   TOK-N @ 1+ TOK-N ! ;
: SPLIT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 TOK-N !  0 TPOS !
   begin TPOS @ u < while
      begin TPOS @ u < a TPOS @ + c@ WS? and while TPOS @ 1+ TPOS ! repeat
      TPOS @ TS !
      begin TPOS @ u < a TPOS @ + c@ WS? 0= and while TPOS @ 1+ TPOS ! repeat
      TPOS @ TS @ > if a TS @ + TPOS @ TS @ - TOKEN+ then
   repeat ;

: TOKEN-N ( n -- n )   \ ledger token index -> its decimal value (BAD if not numeric)
   T@ STR>NUMBER? MATCH option
     none OF BAD ENDOF
     some OF ENDOF
   ;MATCH ;
: VALID-KIND? ( n -- bool )
   dup NOEXEC < if drop 0 0= 0= exit then  FLOAT <= ;
: CLAIM? ( n -- bool ) {: i:n :}   \ mark slot i seen; false if out of range or already claimed
   i 0 < i MAX >= or if 0 0= 0= exit then
   i SEEN@ 0= 0= if 0 0= 0= exit then
   -1 i SEEN!  0 0= ;

\ ---- ledger ingest ----------------------------------------------------------
: ADD-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SPLIT
   TOK-N @ 11 < if BAD then
   0 T@ s" \" AX-STR= 0= if BAD then
   1 T@ s" AXR" AX-STR= 0= if BAD then
   2 TOKEN-N {: i:n :}
   i CLAIM? 0= if i DUP-BAD then
   3 TOKEN-N dup VALID-KIND? 0= if BAD then i KIND CELL-AT !
   4 TOKEN-N i DIN CELL-AT !
   5 TOKEN-N i DOUT CELL-AT !
   6 T@ {: id:ptr idu:n :}
   id SRC - i OFF CELL-AT !
   a u + id - i LEN CELL-AT !
   ROWS @ 1+ ROWS ! ;
: RECIPE-LINE? ( ptr u8 n -- bool )  s" \ AXR " AX-STARTS? ;
: MAYBE-ADD ( ptr u8 n -- )
   2dup RECIPE-LINE? if ADD-LINE else 2drop then ;
: RESET ( -- )
   0 ROWS !
   0 begin dup MAX < while
      0 over SEEN!  0 over OFF CELL-AT !  0 over LEN CELL-AT !
      1+
   repeat drop ;
: LOAD-SOURCE ( -- )
   RESET
   s" test/prop-test-core.f" SRC SRC-CAP READ-ALL SRC-U !
   0 START !  0 POS !
   begin POS @ SRC-U @ < while
      SRC POS @ + c@ CH-LF = if
         SRC START @ + POS @ START @ - MAYBE-ADD
         POS @ 1+ START !
      then
      POS @ 1+ POS !
   repeat
   START @ SRC-U @ < if SRC START @ + SRC-U @ START @ - MAYBE-ADD then ;

\ ---- identity + arity cross-check against the live row ----------------------
: PKG-OK? ( ptr u8 n ptr u8 n -- bool ) {: exp:ptr eu:n live:ptr lu:n :}
   exp eu s" -" AX-STR= if lu 0= exit then  exp eu live lu AX-STR= ;
: ID-ARITY ( n -- n n ) {: i:n :}   \ count pe-in / pe-out tokens in row i's identity span
   0 0
   4 IDX !
   begin IDX @ 1+ TOK-N @ < while
      IDX @ 1+ T@ s" pe-in" AX-STR= if swap 1+ swap
      else IDX @ 1+ T@ s" pe-out" AX-STR= if 1+
      else 2drop BAD then then
      IDX @ 2 + IDX !
   repeat ;
: ROW-OK? ( n -- bool ) {: i:n :}
   i ROW$ SPLIT
   TOK-N @ 4 < if 0 0= 0= exit then
   0 T@ s" prim" AX-STR= 0= 0 T@ s" pprim" AX-STR= 0= and if 0 0= 0= exit then
   i AX-NAME$ {: lp:ptr lpu:n ln:ptr lnu:n :}
   1 T@ lp lpu PKG-OK? 0= if 0 0= 0= exit then
   2 T@ ln lnu AX-STR= 0= if 0 0= 0= exit then
   i ID-ARITY {: din:n dout:n :}
   din i DIN@ <> dout i DOUT@ <> or if 0 0= 0= exit then
   i AX-ARITY {: ldin:n ldout:n :}
   ldin din = ldout dout = and ;
: COVERED? ( -- bool )   \ every live row claimed exactly once and identity-sound
   ROWS @ AX-COUNT <> if 0 0= 0= exit then
   0 begin dup AX-COUNT < while
      dup SEEN@ 0= if drop 0 0= 0= exit then
      dup ROW-OK? 0= if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;
: REPORT-RECIPE ( n -- ) {: i:n :}   \ echo row i's full ledger recipe line
   s" prim-recipe: recipe " type  i ROW$ type cr ;

\ ---- executable candidate: compile the recipe's exact typed operands ---------
: TYPE+ ( ptr u8 n -- )   \ append the checked type token for one pe-* slot type
   2dup s" pe-a" AX-STR= if 2drop s" a " P+ exit then
   2dup s" pe-b" AX-STR= if 2drop s" b " P+ exit then
   2dup s" pe-c" AX-STR= if 2drop s" c " P+ exit then
   2dup s" pe-d" AX-STR= if 2drop s" d " P+ exit then
   2dup s" pe-f" AX-STR= if 2drop s" bool " P+ exit then
   2dup s" pe-n" AX-STR= if 2drop s" n " P+ exit then
   2dup s" pe-r" AX-STR= if 2drop s" r " P+ exit then
   2dup s" pe-u8" AX-STR= if 2drop s" u8 " P+ exit then
   2dup s" pe-ptr-a" AX-STR= if 2drop s" ptr a " P+ exit then
   2dup s" pe-ptr-a-raw" AX-STR= if 2drop s" ptr a " P+ exit then
   2dup s" pe-ptr-b" AX-STR= if 2drop s" ptr b " P+ exit then
   2dup s" pe-ptr-c" AX-STR= if 2drop s" ptr c " P+ exit then
   2dup s" pe-ptr-d" AX-STR= if 2drop s" ptr d " P+ exit then
   2dup s" pe-ptr-e" AX-STR= if 2drop s" ptr e " P+ exit then
   2dup s" pe-ptr-n" AX-STR= if 2drop s" ptr n " P+ exit then
   2dup s" pe-ptr-u8" AX-STR= if 2drop s" ptr u8 " P+ exit then
   2dup s" pe-ptr-ptr-b" AX-STR= if 2drop s" ptr ptr b " P+ exit then
   2drop BAD ;
: SIDE+ ( ptr u8 n -- ) {: dir:ptr du:n :}   \ emit the types of slots on side dir
   4 IDX !
   begin IDX @ 1+ TOK-N @ < while
      IDX @ 1+ T@ dir du AX-STR= if IDX @ T@ TYPE+ then
      IDX @ 2 + IDX !
   repeat ;
: BUILD-CANDIDATE ( n -- )   \ PBUF := ": C ( <in-types> -- <out-types> ) <name> ;"
   ROW$ SPLIT
   0 PBUF-U !
   s" : AX-RECIPE-CANDIDATE ( " P+
   s" pe-in" SIDE+
   s" -- " P+
   s" pe-out" SIDE+
   s" ) " P+
   AXNAME$ P+
   s"  ;" P+ ;
: CHECK-EXEC? ( n -- bool )   \ does the recipe's typed candidate certify?
   SMARK  BUILD-CANDIDATE  PBUF PBUF-U @ CHK  VERD @ -1 =  SFORGET ;

\ ---- naming failure path: name the row (reason + live + ledger identity) ----
: FAIL ( n ptr u8 n -- ) {: i:n a:ptr u:n :}   \ ( row-i reason ) name reason, live + ledger identity, die
   s" prim-recipe: " type a u type
   s"  at live row " type i .  s" name " type AXNAME$ type cr
   i SEEN@ 0= 0= if i REPORT-RECIPE then
   s" prim-recipe: proof failed" 1 die ;
: VERIFY-COVERAGE ( -- )   \ name the first live row lacking a sound recipe (count / missing / identity / arity)
   ROWS @ AX-COUNT <> if
      s" prim-recipe: recipe count " type ROWS @ .  s" does not match live axiom count " type AX-COUNT . cr
      s" prim-recipe: proof failed" 1 die
   then
   0 begin dup AX-COUNT < while
      dup AX-NAME$ AXIOM-NAME:SAVE-SYMBOL
      dup SEEN@ 0= if dup s" no proof recipe for this live row" FAIL then
      dup ROW-OK? 0= if dup s" recipe identity or arity mutation" FAIL then
      1+
   repeat drop ;

\ ---- teeth: prove missing / duplicate / mutation / stale-noexec all reject ---
: STALE-NOEXEC-REJECTS? ( -- bool )   \ point a noexec recipe at another row's identity; must reject
   NOEXEC-PROBE K@ NOEXEC <> if 0 0= 0= exit then
   NOEXEC-PROBE OFF@ NOEXEC-PROBE LEN@ {: saved-o:n saved-u:n :}
   0 OFF@ NOEXEC-PROBE OFF CELL-AT !
   0 LEN@ NOEXEC-PROBE LEN CELL-AT !
   NOEXEC-PROBE ROW-OK? 0= {: rejected:bool :}
   saved-o NOEXEC-PROBE OFF CELL-AT !
   saved-u NOEXEC-PROBE LEN CELL-AT !
   rejected ;
: SELFTEST ( -- )
   VERIFY-COVERAGE                       \ names the offending row if the real ledger is broken
   0 CLAIM? if s" prim-recipe: duplicate slot accepted" 1 die then
   0 0 SEEN!
   COVERED? if s" prim-recipe: missing slot accepted" 1 die then
   -1 0 SEEN!
   0 DIN@ 1+ 0 DIN CELL-AT !
   0 ROW-OK? if s" prim-recipe: arity mutation accepted" 1 die then
   0 DIN@ 1- 0 DIN CELL-AT !
   STALE-NOEXEC-REJECTS? 0= if s" prim-recipe: stale noexec identity accepted" 1 die then
   COVERED? 0= if s" prim-recipe: self-test did not restore ledger" 1 die then ;

public

: LOAD ( -- )   \ parse the ledger + verify coverage + run the teeth self-test
   LOAD-SOURCE  SELFTEST ;
: PROOF-KIND ( n -- n )  K@ ;
: PROVE ( n -- ) {: i:n :}   \ per live row: recipe present, identity/arity sound, typed candidate certifies
   i SEEN@ 0= if i s" no proof recipe for this live row" FAIL then
   i ROW-OK? 0= if i s" recipe identity or arity mutation" FAIL then
   i K@ NOEXEC <> if
      i CHECK-EXEC? 0= if i s" typed operand/effect recipe rejected" FAIL then
   then ;

;package

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

\ ---- runner builders: plant AXR:CANARY beneath the operand row, then measure
\ the produced depth. The trailing AXR:CANARY-VERIFY reads -1 (a trap) when the
\ primitive reached below its declared inputs and clobbered the guard, so a
\ value-provenance violation fails even when the final depth happens to match.
\ PBUF := "AXR:CANARY depth BASE ! <ops> <name> depth BASE @ - CLEAR-MEAS AXR:CANARY-VERIFY"
: AX-BUILD ( ptr u8 n ptr u8 n -- )   \ ( name-a name-u ops-a ops-u )
   0 PBUF-U !
   s" AXR:CANARY depth BASE ! " P+
   P+                       \ ops (top pair)
   s"  " P+
   P+                       \ name (remaining pair)
   s"  depth BASE @ - CLEAR-MEAS AXR:CANARY-VERIFY" P+ ;

: AX-BUILD-REP ( ptr u8 n n -- )   \ ( name-a name-u din ); repeat "7 " din times
   {: din:n :}
   0 PBUF-U !
   s" AXR:CANARY depth BASE ! " P+
   0 begin dup din < while s" 7 " P+ 1+ repeat drop
   P+                       \ name
   s"  depth BASE @ - CLEAR-MEAS AXR:CANARY-VERIFY" P+ ;

: AX-BUILD-FREP ( ptr u8 n n -- )   \ ( name-a name-u din ); repeat "1 s>f " din times
   {: din:n :}
   0 PBUF-U !
   s" AXR:CANARY depth BASE ! " P+
   0 begin dup din < while s" 1 s>f " P+ 1+ repeat drop
   P+
   s"  depth BASE @ - CLEAR-MEAS AXR:CANARY-VERIFY" P+ ;

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

: AX-DISPATCH ( ptr u8 n n -- ) {: kind:n :}
   kind AXR:GEN = if AX-DIN-V @ AX-BUILD-REP AX-CHECK exit then
   kind AXR:FLOAT = if AX-DIN-V @ AX-BUILD-FREP AX-CHECK exit then
   kind AXR:MEM = if AX-RUN-MEM exit then
   kind AXR:NOEXEC = if 2drop AX-N-NOEXEC @ 1+ AX-N-NOEXEC ! exit then
   s" prim-axiom: UNCLASSIFIED " type type cr
   AX-UNCLASS @ 1+ AX-UNCLASS ! ;

: AX-ROW ( n -- ) {: i:n :}
   i AX-ARITY AX-DOUT-V ! AX-DIN-V !
   i AX-NAME$ AXIOM-NAME:SAVE-SYMBOL
   i AXR:PROVE
   AXNAME$ i AXR:PROOF-KIND AX-DISPATCH ;

: AX-CENSUS ( -- )
   0 AX-N-EXEC ! 0 AX-N-NOEXEC ! 0 AX-BAD ! 0 AX-UNCLASS !
   AXR:LOAD
   s" prim-axiom: recipe self-test OK (missing, duplicate, arity mutation, and stale noexec reject)" type cr
   0 AX-I !
   begin AX-I @ AX-COUNT < while AX-I @ AX-ROW AX-I @ 1+ AX-I ! repeat
   s" prim-axiom: " type AX-COUNT . s" axioms (" type
   AX-N-EXEC @ . s" difftested, " type AX-N-NOEXEC @ . s" noexec); " type
   AX-UNCLASS @ . s" unclassified, " type AX-BAD @ . s" mismatch(es)" type cr
   AX-UNCLASS @ AX-BAD @ + 0 > if s" prim-axiom: AXIOM CENSUS FAILED" 1 die then
   s" prim-axiom: census OK (one proof recipe per PES row; executable rows typed and difftested)" type cr ;

\ self-test with teeth: a fabricated wrong declared-dout must be detected
: AX-SELFTEST ( -- )
   1 AX-DIN-V !  5 AX-DOUT-V !  0 AX-BAD !
   s" dup" AX-NAME-SAVE
   AXNAME$ AX-DIN-V @ AX-BUILD-REP AX-CHECK
   AX-BAD @ 0 = if s" prim-axiom: self-test BROKEN (lying axiom not detected)" 1 die then
   s" prim-axiom: self-test OK (difftest detects a lying axiom)" type cr ;

\ canary self-test with teeth: the value-provenance guard AXR:CANARY-VERIFY is a
\ rejection path with no other coverage, so exercise it directly. A fabricated
\ runner reaches one cell below its single declared operand and clobbers the
\ guard, yet keeps the final depth right (`nip` drops the operand AND the guard,
\ then a replacement is pushed) — the census must read that as a trap. A second
\ honest runner (`1+` over one operand) must NOT trap, so the guard cannot pass
\ by always failing. If AXR:CANARY-VERIFY ever regressed to always-pass, the
\ clobber leg dies here.
: AX-SELFTEST-CANARY ( -- )
   0 PBUF-U !
   s" AXR:CANARY depth BASE ! 7 nip 7 depth BASE @ - CLEAR-MEAS AXR:CANARY-VERIFY" P+
   AX-MEASURE
   LAST-TRAP @ 0= if s" prim-axiom: canary self-test BROKEN (below-input clobber not trapped)" 1 die then
   0 PBUF-U !
   s" AXR:CANARY depth BASE ! 7 1+ depth BASE @ - CLEAR-MEAS AXR:CANARY-VERIFY" P+
   AX-MEASURE
   LAST-TRAP @ if s" prim-axiom: canary self-test BROKEN (honest runner falsely trapped)" 1 die then
   s" prim-axiom: canary self-test OK (below-input clobber trapped, honest run clean)" type cr ;

: PROP-RUN ( n n -- )
   SELFTEST
   SELFTEST-SHRINK
   BAITS
   SELFTEST-ALPHABET
   AX-SELFTEST
   AX-SELFTEST-CANARY
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
   PROC-FORK:RAW {: p:pid :}
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
   PROC-FORK:RAW {: p:pid :}
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
   AX-SELFTEST-CANARY
   AX-CENSUS
   FRESH-SEED SWEEP ;

: PROP-MAIN ( -- )
   ARGC 1 = IF  PROP-RUN-DEFAULT
   ELSE ARGC 3 = IF  1 ARG-U  2 ARG-U  PROP-RUN
   ELSE  USAGE  THEN THEN ;

\ ---- audited primitive proof recipe ledger (one row per live PES axiom) ------
\ Data, not code. The AXR package above parses these lines from this source. One
\ line per live PES slot, in live-table order:
\   "\ AXR <slot> <kind> <din> <dout> <identity>"
\ where <kind> is 0 noexec / 1 gen / 2 mem / 3 float and <identity> is the
\ canonical PEINV row identity: "<prim|pprim> <package-or-dash> <name>
\ <flags-or-dash> <pe-TYPE pe-in ...  pe-TYPE pe-out ...>". Regenerate the
\ identity + order from:
\   bin/hb --load tools/primitive-effect-inventory.f -- manifest
\ and set each row's <kind> to its proof class. The census cross-checks every
\ recipe against the live table and fails, naming the row, on any drift.
\
\ ---- why the hard rows carry the kind they do (audit for kind assignment) ----
\ Most noexec (kind 0) rows are self-evident syscalls, control words, parser
\ literals, or defining words. The rows below are the subtle ones a future
\ reviewer will second-guess, so the reasoning that used to live beside the old
\ name-lists is recorded here.
\ Checker-substrate introspection cannot take dummy operands, and the seal
\ watermark capture rewrites live seal state. The indexed accessors fail closed
\ with `76 die` on an out-of-range index (WF-ROW@ in checker.f, TF-REC@ in
\ type-family.f), so a dummy `7` would kill the census process whenever the live
\ table is shorter; seal-capture (native BSEALCAP) rewrites the sealed
\ friend-band ndict watermark, mutating live seal state mid-process like
\ cp!/ndict!. Their arity is pinned instead by the native self-rebuild fixpoint
\ plus the behavioral gate, so they carry a noexec (kind 0) recipe.
\ The zero-arg state readers (wf-n@, tfam-n@, sumv-n@, tf-str-u@, tf-pk-n@,
\ lay-n@, schema-n@, schema-root-n@, checker-scope-depth, and
\ TYPE-FIELD:TX-DEPTH) are pure variable reads and STAY difftested as generic
\ (kind 1) rows, matching ndict@/cp@. prot-wid-room is likewise a read-only
\ capacity calculation over the protected-WID count. wf-wide? and
\ wf-needs-p2? (zero-arg scans) and wf-w-at (indexed with a total 1-default that
\ never dies) are likewise generic (kind 1). locw-hw@ fails closed with a 76-die
\ index guard of its own (a dummy sequence past LOCSEQ dies), so it stays noexec,
\ and the pass-2 live-table words (p2-carve-w, p2-live-w@, p2-live-cum@,
\ p2-locseq-reset in checker.f) read or mutate live pass-2 compile scratch
\ (P2SEQ/P2LW), so all of those are noexec.
\ wide-mark stamps DNAME-WIDE on the newest published dict record inside an
\ mprotect bracket (a live dictionary mutation, seal-capture class), and
\ rec-wide-publish consumes the checker's RECW latch and may call it — neither
\ can run under census dummies. rec-min-in@ drains the checker's RECMI publish
\ latch the same way (dot habu-habu-certified-words-84e84eaf): a census
\ execution would desync the latch the engine publish tail consumes for the
\ DNAME-MIN-IN record poke. prot-wid-add mutates the sealed friend-band
\ protected-WID registry (a seal-capture-class live seal mutation) and its
\ overflow path exits the process (NR-EXIT-GROUP rc 84), so it can never take a
\ dummy operand. drain-pretrust (dot habu-engine-pre-trust-77410827) replays the
\ pending pre-trust defer registrations into the LIVE checker registry (a trust
\ usig row plus a checker-defer row per pending slot) — a checker-substrate
\ mutation of the trust/seal-capture class; a census execution would re-drain an
\ already-empty table at best and inject duplicate registry rows at worst.
\ checker-scope-finalize releases the live top rollback frame and finalizes every
\ extension registry; executing it at census depth zero would underflow and
\ mutate the checker transaction substrate, so its identity and arity are pinned
\ as noexec.
\ The read-only owner-wid predicates are total over numeric dummy operands and
\ STAY difftested as generic (kind 1) rows against the valid cold-empty registry.
\ tfam-ctor-word? is a pure registry-read predicate and STAYS difftested as an
\ owned-memory (kind 2) row (empty census registry -> false, one flag out).
\ trust records a live checker signature from its string operands (a checker
\ substrate mutation; UNSAFE-TOK? bans it inside checked bodies anyway);
\ check/check! spawn a full checker run over their string operand (a dummy
\ pointer would be lexed); typefamily/sumtype/enum/product are top-level parser
\ words that consume their own block tokens up to the ;NAME closer, so a census
\ runner has no input to feed them; layout-buffer likewise parses its own name +
\ type tokens after its count operand, and typed-buffer/typed-variable/
\ defer-layout-buffer are the same generative definer class (they parse their own
\ name + type after an optional count operand; UNSAFE-TOK? bans them inside
\ checked bodies). ldefer-bind is the deferred column's shared runtime binder: it
\ allots + writes cells at caller-supplied data-base offsets, so a census run over
\ random operands would store through arbitrary addresses - not soundly executable.
\ Their axioms keep them checker-known so the seal-time internal-word marking
\ pass leaves them top-level executable (dot habu-hb-crash-bare-c5be6634).
\ ptx-barrier! is the same class as trust: it resolves its string operand to a
\ symbol and mutates that word's control flags (a checker-registry side effect a
\ census execution over random operands cannot exercise soundly); UNSAFE-TOK?
\ bans it inside checked bodies, and its axiom keeps it top-level executable so
\ library source can declare an explicit barrier. cast-pend! arms the checker's
\ one-shot cast-certification window with a name (ptr,len) that a later
\ CORE-STR=CI reads: a census execution over a random operand would arm the
\ window with a garbage pointer + length and leave the checker mid-armed — a
\ checker-substrate mutation of the same class as trust.
\ ----------------------------------------------------------------------------
\ AXR 0 1 1 2 prim - dup - pe-a pe-in pe-a pe-out pe-a pe-out
\ AXR 1 1 1 0 prim - drop - pe-a pe-in
\ AXR 2 1 2 2 prim - swap - pe-a pe-in pe-b pe-in pe-b pe-out pe-a pe-out
\ AXR 3 1 2 3 prim - over - pe-a pe-in pe-b pe-in pe-a pe-out pe-b pe-out pe-a pe-out
\ AXR 4 1 2 1 prim - nip - pe-a pe-in pe-b pe-in pe-b pe-out
\ AXR 5 1 2 3 prim - tuck - pe-a pe-in pe-b pe-in pe-b pe-out pe-a pe-out pe-b pe-out
\ AXR 6 1 3 3 prim - rot - pe-a pe-in pe-b pe-in pe-c pe-in pe-b pe-out pe-c pe-out pe-a pe-out
\ AXR 7 1 3 3 prim - -rot - pe-a pe-in pe-b pe-in pe-c pe-in pe-c pe-out pe-a pe-out pe-b pe-out
\ AXR 8 1 2 4 prim - 2dup - pe-a pe-in pe-b pe-in pe-a pe-out pe-b pe-out pe-a pe-out pe-b pe-out
\ AXR 9 1 2 0 prim - 2drop - pe-a pe-in pe-b pe-in
\ AXR 10 1 4 4 prim - 2swap - pe-a pe-in pe-b pe-in pe-c pe-in pe-d pe-in pe-c pe-out pe-d pe-out pe-a pe-out pe-b pe-out
\ AXR 11 1 4 6 prim - 2over - pe-a pe-in pe-b pe-in pe-c pe-in pe-d pe-in pe-a pe-out pe-b pe-out pe-c pe-out pe-d pe-out pe-a pe-out pe-b pe-out
\ AXR 12 1 2 1 prim - + - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 13 1 2 1 prim - + - pe-ptr-a pe-in pe-n pe-in pe-ptr-a pe-out
\ AXR 14 1 2 1 prim - + - pe-n pe-in pe-ptr-a pe-in pe-ptr-a pe-out
\ AXR 15 1 2 1 prim - - - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 16 1 2 1 prim - - - pe-ptr-a pe-in pe-n pe-in pe-ptr-a pe-out
\ AXR 17 1 2 1 prim - - - pe-ptr-a pe-in pe-ptr-a pe-in pe-n pe-out
\ AXR 18 1 2 1 prim - * - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 19 1 2 1 prim - and - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 20 1 2 1 prim - and - pe-f pe-in pe-f pe-in pe-f pe-out
\ AXR 21 1 2 1 prim - or - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 22 1 2 1 prim - or - pe-f pe-in pe-f pe-in pe-f pe-out
\ AXR 23 1 2 1 prim - xor - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 24 1 2 1 prim - xor - pe-f pe-in pe-f pe-in pe-f pe-out
\ AXR 25 1 1 1 prim - 1+ - pe-n pe-in pe-n pe-out
\ AXR 26 1 1 1 prim - 1+ - pe-ptr-a pe-in pe-ptr-a pe-out
\ AXR 27 1 1 1 prim - 1- - pe-n pe-in pe-n pe-out
\ AXR 28 1 1 1 prim - 1- - pe-ptr-a pe-in pe-ptr-a pe-out
\ AXR 29 1 1 1 prim - negate - pe-n pe-in pe-n pe-out
\ AXR 30 1 1 1 prim - invert - pe-n pe-in pe-n pe-out
\ AXR 31 1 1 1 prim - 0= - pe-a pe-in pe-f pe-out
\ AXR 32 1 1 1 prim - 0< - pe-n pe-in pe-f pe-out
\ AXR 33 1 2 1 prim - = - pe-n pe-in pe-n pe-in pe-f pe-out
\ AXR 34 1 2 1 prim - = - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
\ AXR 35 1 2 1 prim - < - pe-n pe-in pe-n pe-in pe-f pe-out
\ AXR 36 1 2 1 prim - < - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
\ AXR 37 1 2 1 prim - > - pe-n pe-in pe-n pe-in pe-f pe-out
\ AXR 38 1 2 1 prim - > - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
\ AXR 39 1 2 1 prim - <> - pe-n pe-in pe-n pe-in pe-f pe-out
\ AXR 40 1 2 1 prim - <> - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
\ AXR 41 1 2 1 prim - <= - pe-n pe-in pe-n pe-in pe-f pe-out
\ AXR 42 1 2 1 prim - <= - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
\ AXR 43 1 2 1 prim - >= - pe-n pe-in pe-n pe-in pe-f pe-out
\ AXR 44 1 2 1 prim - >= - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
\ AXR 45 1 2 1 prim - / - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 46 1 2 1 prim - mod - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 47 1 2 2 prim - /mod - pe-n pe-in pe-n pe-in pe-n pe-out pe-n pe-out
\ AXR 48 1 1 1 prim - abs - pe-n pe-in pe-n pe-out
\ AXR 49 1 2 1 prim - min - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 50 1 2 1 prim - max - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 51 1 2 1 prim - lshift - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 52 1 2 1 prim - rshift - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 53 1 1 1 prim - cells - pe-n pe-in pe-n pe-out
\ AXR 54 1 1 1 prim - cell+ - pe-ptr-a pe-in pe-ptr-a pe-out
\ AXR 55 1 1 1 prim - cell+ - pe-n pe-in pe-n pe-out
\ AXR 56 1 1 1 prim - chars - pe-n pe-in pe-n pe-out
\ AXR 57 1 1 1 prim - char+ - pe-ptr-a pe-in pe-ptr-a pe-out
\ AXR 58 1 1 1 prim - char+ - pe-n pe-in pe-n pe-out
\ AXR 59 2 1 1 prim - @ - pe-ptr-a pe-in pe-a pe-out
\ AXR 60 2 2 0 prim - ! - pe-a pe-in pe-ptr-a pe-in
\ AXR 61 2 2 1 prim - ptr-field - pe-ptr-a pe-in pe-n pe-in pe-ptr-ptr-b pe-out
\ AXR 62 2 2 0 prim - +! - pe-n pe-in pe-ptr-n pe-in
\ AXR 63 2 1 1 prim - c@ - pe-ptr-u8 pe-in pe-u8 pe-out
\ AXR 64 2 2 0 prim - c! - pe-u8 pe-in pe-ptr-u8 pe-in
\ AXR 65 0 1 1 prim - atomic@ - pe-ptr-a pe-in pe-a pe-out
\ AXR 66 0 2 0 prim - atomic! - pe-a pe-in pe-ptr-a pe-in
\ AXR 67 0 2 1 prim - atomic-add - pe-n pe-in pe-ptr-n pe-in pe-n pe-out
\ AXR 68 0 3 1 prim - atomic-cas - pe-a pe-in pe-a pe-in pe-ptr-a pe-in pe-a pe-out
\ AXR 69 0 0 0 prim - fence - -
\ AXR 70 0 3 0 prim - run-in-stack - pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 71 2 1 2 prim - count - pe-ptr-u8 pe-in pe-ptr-u8 pe-out pe-n pe-out
\ AXR 72 1 1 0 prim - . - pe-n pe-in
\ AXR 73 0 0 0 prim - .s - -
\ AXR 74 1 0 1 prim - depth - pe-n pe-out
\ AXR 75 1 0 1 prim - here - pe-ptr-a-raw pe-out
\ AXR 76 0 1 0 prim - allot - pe-n pe-in
\ AXR 77 0 1 0 prim - , - pe-n pe-in
\ AXR 78 0 1 0 prim - c, - pe-n pe-in
\ AXR 79 2 2 0 prim - type - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 80 1 0 1 prim - script-argc - pe-n pe-out
\ AXR 81 0 1 2 prim - script-argv$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
\ AXR 82 0 1 0 prim - throw - pe-n pe-in
\ AXR 83 0 3 0 prim - die - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in
\ AXR 84 0 3 1 prim - open - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 85 0 3 1 prim - read - pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 86 0 3 1 prim - ioctl - pe-n pe-in pe-n pe-in pe-ptr-a pe-in pe-n pe-out
\ AXR 87 0 6 1 prim - mmap - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 88 0 2 1 prim - path0 - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-out
\ AXR 89 0 1 1 prim - open-rd - pe-ptr-u8 pe-in pe-n pe-out
\ AXR 90 0 2 1 prim - access - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 91 0 1 1 prim - unlink - pe-ptr-u8 pe-in pe-n pe-out
\ AXR 92 0 2 1 prim - rename - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-out
\ AXR 93 0 2 1 prim - chmod - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 94 0 2 1 prim - symlink - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-out
\ AXR 95 0 3 1 prim - readlink - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 96 0 2 1 prim - mkdir - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 97 0 1 1 prim - rmdir - pe-ptr-u8 pe-in pe-n pe-out
\ AXR 98 0 2 1 prim - stat64 - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-out
\ AXR 99 0 2 1 prim - lstat64 - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-out
\ AXR 100 0 4 1 prim - getdirentries64 - pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-ptr-n pe-in pe-n pe-out
\ AXR 101 0 0 3 prim - pipe - pe-n pe-out pe-n pe-out pe-n pe-out
\ AXR 102 0 2 1 prim - dup2 - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 103 0 3 1 prim - fcntl - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 104 0 3 1 prim - poll - pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 105 0 2 1 prim - kill - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 106 0 2 1 prim - setpgid - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 107 0 4 1 prim - spawn-io - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 108 0 5 1 prim - spawn-argv-io - pe-ptr-u8 pe-in pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 109 0 6 1 prim - spawn-argv-env-io - pe-ptr-u8 pe-in pe-ptr-a pe-in pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 110 0 7 1 prim - spawn-argv-env-cwd-io - pe-ptr-u8 pe-in pe-ptr-a pe-in pe-ptr-a pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 111 0 0 1 prim - fork - pe-n pe-out
\ AXR 112 0 1 1 prim - wait-rc - pe-n pe-in pe-n pe-out
\ AXR 113 0 1 1 prim - wait-status - pe-n pe-in pe-n pe-out
\ AXR 114 0 2 0 prim - patch32 trusted-only pe-n pe-in pe-n pe-in
\ AXR 115 0 6 0 prim - snap-rebase - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in
\ AXR 116 0 3 1 prim - write - pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 117 0 1 0 prim - close - pe-n pe-in
\ AXR 118 0 1 1 prim - close-rc - pe-n pe-in pe-n pe-out
\ AXR 119 1 0 1 prim - epoch-seconds - pe-n pe-out
\ AXR 120 1 0 1 prim - mono-ns - pe-n pe-out
\ AXR 121 0 1 0 prim - prof-on - pe-n pe-in
\ AXR 122 0 0 0 prim - prof-report - -
\ AXR 123 1 0 1 prim - rbase - pe-n pe-out
\ AXR 124 1 0 1 prim - cp@ - pe-n pe-out
\ AXR 125 0 1 0 prim - cp! - pe-n pe-in
\ AXR 126 1 0 1 prim - dbase@ - pe-n pe-out
\ AXR 127 1 0 1 prim - check@ - pe-n pe-out
\ AXR 128 1 0 1 prim - ndict@ - pe-n pe-out
\ AXR 129 0 1 0 prim - ndict! - pe-n pe-in
\ AXR 130 0 0 0 prim - seal-capture - -
\ AXR 131 0 0 0 prim - seal-friend - -
\ AXR 132 0 0 0 prim - drain-pretrust - -
\ AXR 133 1 0 1 prim - data-base - pe-ptr-a pe-out
\ AXR 134 0 1 0 prim - prot-wid-add - pe-n pe-in
\ AXR 135 1 0 1 prim - prot-wid-room - pe-n pe-out
\ AXR 136 1 3 1 prim - owner-wid-preflight? - pe-n pe-in pe-n pe-in pe-n pe-in pe-f pe-out
\ AXR 137 1 1 1 prim - owner-wid-public? - pe-n pe-in pe-f pe-out
\ AXR 138 1 1 1 prim - owner-wid-private? - pe-n pe-in pe-f pe-out
\ AXR 139 1 1 1 prim - owner-wid? - pe-n pe-in pe-f pe-out
\ AXR 140 2 2 1 prim - tfam-ctor-word? - pe-ptr-u8 pe-in pe-n pe-in pe-f pe-out
\ AXR 141 0 0 1 prim - wordlist - pe-n pe-out
\ AXR 142 1 0 1 prim - get-current - pe-n pe-out
\ AXR 143 0 1 0 prim - set-current - pe-n pe-in
\ AXR 144 0 3 1 prim - search-wl - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 145 0 0 2 prim - parse-name - pe-ptr-u8 pe-out pe-n pe-out
\ AXR 146 2 4 1 prim - core-str= - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-f pe-out
\ AXR 147 2 4 1 prim - core-str=ci - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-f pe-out
\ AXR 148 0 3 0 prim - pathz - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in
\ AXR 149 0 2 1 prim - path0 - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-out
\ AXR 150 2 1 1 prim - rd32 - pe-ptr-u8 pe-in pe-n pe-out
\ AXR 151 0 2 0 prim - diag-file! - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 152 0 3 0 prim - diag-origin! - pe-n pe-in pe-n pe-in pe-n pe-in
\ AXR 153 0 1 0 prim - diag-json! - pe-f pe-in
\ AXR 154 0 2 0 prim - diag-buffer! - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 155 0 0 0 prim - diag-buffer-off - -
\ AXR 156 0 0 2 prim - diag-buffer$ - pe-ptr-u8 pe-out pe-n pe-out
\ AXR 157 0 0 0 prim - checker-scope-start - -
\ AXR 158 0 0 0 prim - checker-scope-finalize - -
\ AXR 159 0 0 0 prim - checker-scope-done - -
\ AXR 160 1 0 1 prim - checker-scope-depth - pe-n pe-out
\ AXR 161 0 2 1 prim - check-candidate! - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 162 0 2 1 prim - check - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 163 0 2 1 prim - check! - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 164 0 0 0 prim - checker-candidate-scope-start - -
\ AXR 165 0 0 0 prim - checker-candidate-scope-done - -
\ AXR 166 0 2 0 prim - checker-usigs-truncate-from - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 167 0 2 0 prim - checker-usigs-truncate-from-raw - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 168 0 2 0 prim - checker-undefine - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 169 0 2 0 prim - checker-undefine-guard - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 170 0 2 0 prim - checker-export - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 171 0 0 1 prim - checker-package-active? - pe-f pe-out
\ AXR 172 0 2 0 prim - checker-deflinear - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 173 0 4 0 prim - checker-defrecord - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 174 0 4 0 prim - checker-deffamily - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 175 0 4 0 prim - checker-defsum - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 176 0 4 0 prim - checker-defsum-noend - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 177 0 4 0 prim - checker-defenum - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 178 0 4 0 prim - checker-defproduct - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 179 0 2 3 prim - checker-layout-info - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out pe-n pe-out pe-f pe-out
\ AXR 180 0 2 2 prim - checker-storage-info - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out pe-f pe-out
\ AXR 181 0 6 0 prim - checker-deflayout-buffer - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 182 0 6 0 prim - checker-deftyped-buffer - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 183 0 4 0 prim - checker-deftyped-variable - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 184 0 2 0 prim - checker-lbuf-name-guard - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 185 0 2 1 prim - checker-defined? - pe-ptr-u8 pe-in pe-n pe-in pe-f pe-out
\ AXR 186 0 2 0 prim - cast-pend! - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 187 0 4 0 prim - trust - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
\ AXR 188 0 2 0 prim - ptx-barrier! - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 189 1 0 1 prim - tfam-n@ - pe-n pe-out
\ AXR 190 0 1 1 prim - tfam-width@ - pe-n pe-in pe-n pe-out
\ AXR 191 0 1 2 prim - tfam-name$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
\ AXR 192 0 1 1 prim - tfam-arity@ - pe-n pe-in pe-n pe-out
\ AXR 193 1 0 1 prim - tfam-decl-param-count - pe-n pe-out
\ AXR 194 1 1 2 prim - tfam-decl-param>char - pe-n pe-in pe-n pe-out pe-f pe-out
\ AXR 195 1 1 2 prim - tfam-decl-char>param - pe-n pe-in pe-n pe-out pe-f pe-out
\ AXR 196 0 1 1 prim - tfam-kind@ - pe-n pe-in pe-n pe-out
\ AXR 197 0 1 1 prim - tfam-public? - pe-n pe-in pe-f pe-out
\ AXR 198 0 1 1 prim - tfam-derive-eq? - pe-n pe-in pe-f pe-out
\ AXR 199 0 1 1 prim - tfam-derive-hash? - pe-n pe-in pe-f pe-out
\ AXR 200 0 1 1 prim - tfam-var-start@ - pe-n pe-in pe-n pe-out
\ AXR 201 0 1 1 prim - tfam-var-count@ - pe-n pe-in pe-n pe-out
\ AXR 202 0 1 2 prim - sumv-name$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
\ AXR 203 0 1 2 prim - sumv-ctor-pkg$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
\ AXR 204 0 0 1 pprim type-field count - pe-n pe-out
\ AXR 205 1 0 1 pprim type-field tx-depth - pe-n pe-out
\ AXR 206 0 0 1 pprim type-field no-variant - pe-n pe-out
\ AXR 207 0 4 2 pprim type-field find - pe-n pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out pe-f pe-out
\ AXR 208 0 3 2 pprim type-field each - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out pe-f pe-out
\ AXR 209 0 1 1 pprim type-field family@ - pe-n pe-in pe-n pe-out
\ AXR 210 0 1 1 pprim type-field variant@ - pe-n pe-in pe-n pe-out
\ AXR 211 0 1 2 pprim type-field name$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
\ AXR 212 0 1 1 pprim type-field schema@ - pe-n pe-in pe-n pe-out
\ AXR 213 0 1 1 pprim type-field slot@ - pe-n pe-in pe-n pe-out
\ AXR 214 0 1 1 pprim type-field cells@ - pe-n pe-in pe-n pe-out
\ AXR 215 0 1 1 pprim type-field byte-off@ - pe-n pe-in pe-n pe-out
\ AXR 216 0 1 1 pprim type-field bytes@ - pe-n pe-in pe-n pe-out
\ AXR 217 0 1 1 pprim type-field align@ - pe-n pe-in pe-n pe-out
\ AXR 218 0 1 1 pprim type-field flags@ - pe-n pe-in pe-n pe-out
\ AXR 219 1 0 1 prim - wf-n@ - pe-n pe-out
\ AXR 220 0 1 1 prim - wf-off@ - pe-n pe-in pe-n pe-out
\ AXR 221 0 1 1 prim - wf-pos@ - pe-n pe-in pe-n pe-out
\ AXR 222 0 1 1 prim - wf-fam@ - pe-n pe-in pe-n pe-out
\ AXR 223 0 1 1 prim - wf-width@ - pe-n pe-in pe-n pe-out
\ AXR 224 0 1 1 prim - wf-term@ - pe-n pe-in pe-n pe-out
\ AXR 225 0 1 1 prim - wf-flags@ - pe-n pe-in pe-n pe-out
\ AXR 226 1 0 1 prim - wf-wide? - pe-f pe-out
\ AXR 227 1 0 1 prim - wf-needs-p2? - pe-f pe-out
\ AXR 228 1 2 1 prim - wf-w-at - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 229 0 0 0 prim - wide-mark - -
\ AXR 230 0 0 0 prim - rec-wide-publish - -
\ AXR 231 0 0 1 prim - rec-min-in@ - pe-n pe-out
\ AXR 232 0 1 1 prim - locw-hw@ - pe-n pe-in pe-n pe-out
\ AXR 233 1 0 1 prim - locw-hw-n@ - pe-n pe-out
\ AXR 234 1 0 1 pprim lower-cert magic - pe-n pe-out
\ AXR 235 1 0 1 pprim lower-cert version - pe-n pe-out
\ AXR 236 1 0 1 pprim lower-cert header-cells - pe-n pe-out
\ AXR 237 1 0 1 pprim lower-cert magic-cell - pe-n pe-out
\ AXR 238 1 0 1 pprim lower-cert version-cell - pe-n pe-out
\ AXR 239 1 0 1 pprim lower-cert total-bytes-cell - pe-n pe-out
\ AXR 240 1 0 1 pprim lower-cert needs-cell - pe-n pe-out
\ AXR 241 1 0 1 pprim lower-cert wf-count-cell - pe-n pe-out
\ AXR 242 1 0 1 pprim lower-cert bind-count-cell - pe-n pe-out
\ AXR 243 1 0 1 pprim lower-cert fetch-count-cell - pe-n pe-out
\ AXR 244 1 0 1 pprim lower-cert fetch-data-cells-cell - pe-n pe-out
\ AXR 245 1 0 1 pprim lower-cert wf-cells - pe-n pe-out
\ AXR 246 1 0 1 pprim lower-cert fetch-cells - pe-n pe-out
\ AXR 247 1 0 1 pprim lower-cert check-cells - pe-n pe-out
\ AXR 248 1 0 1 pprim lower-cert guard-cells - pe-n pe-out
\ AXR 249 1 0 1 pprim lower-cert fetch-flag - pe-n pe-out
\ AXR 250 1 0 1 pprim lower-cert store-flag - pe-n pe-out
\ AXR 251 1 0 1 pprim lower-cert xpad-flag - pe-n pe-out
\ AXR 252 1 0 1 pprim lower-cert body-len-cell - pe-n pe-out
\ AXR 253 1 0 1 pprim lower-cert body-hash-cell - pe-n pe-out
\ AXR 254 1 0 1 pprim lower-cert fnv-offset - pe-n pe-out
\ AXR 255 1 0 1 pprim lower-cert fnv-prime - pe-n pe-out
\ AXR 256 1 0 1 pprim lower-cert cell-count - pe-n pe-out
\ AXR 257 0 1 1 pprim lower-cert cell@ - pe-n pe-in pe-n pe-out
\ AXR 258 0 0 2 pprim lower-cert bytes trusted-only pe-ptr-u8 pe-out pe-n pe-out
\ AXR 259 0 2 1 pprim lower-cert-hook hook - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 260 0 1 0 pprim checker-cert install - pe-n pe-in
\ AXR 261 0 3 0 pprim checker-cert produce - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in
\ AXR 262 0 0 0 prim - p2-locseq-reset - -
\ AXR 263 0 1 1 prim - p2-carve-w - pe-n pe-in pe-n pe-out
\ AXR 264 0 1 1 prim - p2-live-w@ - pe-n pe-in pe-n pe-out
\ AXR 265 0 1 1 prim - p2-live-cum@ - pe-n pe-in pe-n pe-out
\ AXR 266 1 0 1 prim - sumv-n@ - pe-n pe-out
\ AXR 267 1 0 1 prim - tf-str-u@ - pe-n pe-out
\ AXR 268 1 0 1 prim - tf-pk-n@ - pe-n pe-out
\ AXR 269 1 0 1 prim - lay-n@ - pe-n pe-out
\ AXR 270 1 0 1 prim - schema-n@ - pe-n pe-out
\ AXR 271 1 0 1 prim - schema-root-n@ - pe-n pe-out
\ AXR 272 0 2 0 prim - checker-defer - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 273 0 2 0 prim - checker-package - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 274 0 2 0 prim - checker-using - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 275 0 0 0 prim - checker-public - -
\ AXR 276 0 0 0 prim - checker-private - -
\ AXR 277 0 0 0 prim - checker-end-package - -
\ AXR 278 0 3 1 prim - ffi-call trusted-only pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 279 0 3 1 prim - ffi-call-n trusted-only pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 280 0 4 1 prim - ffi-call-bounded trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 281 0 7 1 prim - ffi-call-abi-bounded trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-ptr-c pe-in pe-ptr-d pe-in pe-ptr-e pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 282 0 7 1 prim - ffi-call-abi-r-bounded trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-ptr-c pe-in pe-ptr-d pe-in pe-ptr-e pe-in pe-n pe-in pe-n pe-in pe-r pe-out
\ AXR 283 0 7 1 prim - ffi-call-abi trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-ptr-c pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 284 0 7 1 prim - ffi-call-abi-r trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-ptr-c pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-r pe-out
\ AXR 285 3 2 1 prim - f+ - pe-r pe-in pe-r pe-in pe-r pe-out
\ AXR 286 3 2 1 prim - f- - pe-r pe-in pe-r pe-in pe-r pe-out
\ AXR 287 3 2 1 prim - f* - pe-r pe-in pe-r pe-in pe-r pe-out
\ AXR 288 3 2 1 prim - f/ - pe-r pe-in pe-r pe-in pe-r pe-out
\ AXR 289 3 1 1 prim - fnegate - pe-r pe-in pe-r pe-out
\ AXR 290 3 1 1 prim - fabs - pe-r pe-in pe-r pe-out
\ AXR 291 3 1 1 prim - fsqrt - pe-r pe-in pe-r pe-out
\ AXR 292 3 2 1 prim - f< - pe-r pe-in pe-r pe-in pe-f pe-out
\ AXR 293 3 2 1 prim - f> - pe-r pe-in pe-r pe-in pe-f pe-out
\ AXR 294 3 2 1 prim - f= - pe-r pe-in pe-r pe-in pe-f pe-out
\ AXR 295 3 1 1 prim - f0< - pe-r pe-in pe-f pe-out
\ AXR 296 3 1 1 prim - f0= - pe-r pe-in pe-f pe-out
\ AXR 297 1 1 1 prim - s>f - pe-n pe-in pe-r pe-out
\ AXR 298 3 1 1 prim - f>s - pe-r pe-in pe-n pe-out
\ AXR 299 0 1 0 prim - f. - pe-r pe-in
\ AXR 300 0 0 2 prim - s" - pe-ptr-u8 pe-out pe-n pe-out
\ AXR 301 0 0 1 prim - c" - pe-ptr-u8 pe-out
\ AXR 302 0 0 0 prim - ." - -
\ AXR 303 0 0 2 prim - s\" - pe-ptr-u8 pe-out pe-n pe-out
\ AXR 304 0 0 1 prim - c\" - pe-ptr-u8 pe-out
\ AXR 305 0 0 0 prim - .\" - -
\ AXR 306 0 0 1 prim - ['] - pe-n pe-out
\ AXR 307 0 0 1 prim - char - pe-n pe-out
\ AXR 308 0 0 1 prim - [char] - pe-n pe-out
\ AXR 309 1 1 0 prim - emit - pe-n pe-in
\ AXR 310 1 0 0 prim - cr - -
\ AXR 311 1 0 0 prim - space - -
\ AXR 312 1 1 0 prim - u. - pe-n pe-in
\ AXR 313 0 0 1 prim - create - pe-ptr-a pe-out
\ AXR 314 0 0 1 prim - variable - pe-ptr-a pe-out
\ AXR 315 0 0 1 prim - constant - pe-a pe-out
\ AXR 316 0 0 1 prim - getpid - pe-n pe-out
\ AXR 317 0 1 1 prim - proc-watch-open - pe-n pe-in pe-n pe-out
\ AXR 318 0 2 1 prim - kill-errno - pe-n pe-in pe-n pe-in pe-n pe-out
\ AXR 319 0 3 1 prim - execve - pe-ptr-u8 pe-in pe-ptr-a pe-in pe-ptr-a pe-in pe-n pe-out
\ AXR 320 0 2 1 prim - munmap - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
\ AXR 321 0 2 0 prim - ext-mark-free-tail - pe-ptr-u8 pe-in pe-n pe-in
\ AXR 322 0 0 0 prim - typefamily - -
\ AXR 323 0 0 0 prim - sumtype - -
\ AXR 324 0 0 0 prim - enum - -
\ AXR 325 0 0 0 prim - product - -
\ AXR 326 0 1 0 prim - layout-buffer - pe-n pe-in
\ AXR 327 0 5 0 prim - ldefer-bind - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in
\ AXR 328 0 5 0 prim - ldefer-grow - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in
\ AXR 329 0 0 0 prim - defer-layout-buffer - -
\ AXR 330 0 1 0 prim - typed-buffer - pe-n pe-in
\ AXR 331 0 0 0 prim - typed-variable - -
