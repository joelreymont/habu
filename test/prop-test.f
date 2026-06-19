\ prop-test.f — property-based checker-soundness test, SELF-HOSTED in habu.
\ Generates runnable typed defs, checks them, RUNS the certified ones IN-PROCESS
\ (via `evaluate`), and fails (exit 1) if a certified def's real out-arity differs
\ from its declared ( in -- out ) — a false-cert. No host scripting, no gforth, no
\ spawning: generator, driver and measurement are all habu, run by bin/hb.
\ See PROP-TESTING.md.  Run:  bin/hb < test/prop-test.f   (exit 1 on a false-cert)
\ Optional sweep override: bin/hb 123 1000 < test/prop-test.f

TRUSTED: PROP-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! ;
' PROP-CHECK-HOOK set-check

\ ---- measurement: compare stack depth before and after a certified run
variable BASE  variable MC
TRUSTED: CLEAR-MEAS  ( R n -- n )
   dup MC !  begin MC @ 0 > while  swap drop  MC @ 1- MC !  repeat ;
variable VERD                     \ last verdict, set by the check hook
TRUSTED: VH  ( ptr u8 n -- n )
   CHECK! dup VERD ! ;            \ MUST leave the verdict on the stack for the compiler
TRUSTED: ERR@  ( -- n )
   $340000000 $37D8 + @ ;         \ EVALERR-CELL: 0 = clean, 1 = recovered from an error

\ ---- seeded PRNG (LCG) ----
1 constant DEFAULT-SEED
250 constant DEFAULT-COUNT
3 constant LOG-LIMIT
variable SEED   1 SEED !
: RND  ( -- n )
   SEED @ 1103515245 * 12345 +  $7FFFFFFF and  dup SEED ! ;
: RND%  ( n -- n ) {: bound :}
   RND bound mod ;

\ ---- byte buffers; loops use per-loop counter VARIABLES (no ?do / no `i`) ----
512 constant BBUF-CAP
1024 constant PBUF-CAP
create BBUF BBUF-CAP allot   variable BLEN     \ the body
create PBUF PBUF-CAP allot  variable PLEN     \ the full def / the runner
variable SA  variable SU  variable BI  variable PJ  variable NJ
variable RUN-SEED  variable N  variable RI
: B+  ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@  BBUF BLEN @ + c!
      BLEN @ 1+ BLEN !
      1+
   repeat drop ;
: P+  ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@  PBUF PLEN @ + c!
      PLEN @ 1+ PLEN !
      1+
   repeat drop ;
: BD  ( n -- ) {: digit :}
   digit 48 + BBUF BLEN @ + c!
   BLEN @ 1+ BLEN ! ;
variable NI
variable TFLAG                             \ sig element type: 0 = i64 (concrete), 1 = n (generic)
: PTY  ( n -- ) {: count :}
   0 begin dup count < while
      TFLAG @ if s" n " else s" i64 " then P+
      1+
   repeat drop ;
: PC   ( n -- ) {: c :}
   c PBUF PLEN @ + c!
   PLEN @ 1+ PLEN ! ;
: POS. ( -- )  s" seed " type RUN-SEED @ .  s" iteration " type RI @ . ;
: DEF. ( -- )  PBUF PLEN @ type cr ;
: BODY. ( -- )  BBUF BLEN @ type cr ;

\ ---- depth-tracked generator over the integer sublanguage ----
variable DEP  variable NIN  variable DOUT  variable K
variable LOC?                              \ inputs bound as locals a/b/c at the body top?
variable PERT?                             \ declared out perturbed away from the true depth?
: BLET  ( n -- ) {: idx :}
   idx 97 + BBUF BLEN @ + c!
   BLEN @ 1+ BLEN !
   s"  " B+ ;   \ emit letter a+i then space
: STEP  ( -- )   \ append one depth-feasible op to BBUF, update DEP
   5 RND% 0 = IF                          \ 1-in-5: a net-0 STRUCTURAL op (DEP unchanged)
      DEP @ 1 >= IF 4 ELSE 1 THEN RND% K !
      K @ 0 = IF s" 3 0 ?do loop " B+ exit THEN              \ bounded neutral loop
      K @ 1 = IF s" dup 0= if 1+ else 1- then " B+ exit THEN \ balanced branch
      K @ 2 = IF s" >r r> " B+ exit THEN                     \ balanced return stack
               s" [: 1+ ;] execute " B+ exit THEN            \ quotation applied
   DEP @ 2 >= IF 15 RND% ELSE  DEP @ 1 >= IF 6 RND% ELSE 0 THEN  THEN  K !
   K @ 0 = IF                                                  \ push a value: a local ref, or a literal
      LOC? @ 0= 0= IF NIN @ RND% BLET ELSE 10 RND% BD s"  " B+ THEN
      DEP @ 1+ DEP !  exit THEN
   K @ 1 = IF s" 1+ " B+      exit THEN
   K @ 2 = IF s" 1- " B+      exit THEN
   K @ 3 = IF s" negate " B+  exit THEN
   K @ 4 = IF s" dup " B+   DEP @ 1+ DEP !  exit THEN
   K @ 5 = IF s" drop " B+  DEP @ 1- DEP !  exit THEN
   K @ 6 = IF s" + " B+     DEP @ 1- DEP !  exit THEN
   K @ 7 = IF s" over " B+  DEP @ 1+ DEP !  exit THEN
   K @ 8 = IF s" nip " B+   DEP @ 1- DEP !  exit THEN
   K @ 9 = IF s" swap " B+  exit THEN
   K @ 10 = IF s" - " B+    DEP @ 1- DEP !  exit THEN
   K @ 11 = IF s" * " B+    DEP @ 1- DEP !  exit THEN
   K @ 12 = IF s" and " B+  DEP @ 1- DEP !  exit THEN
   K @ 13 = IF s" or " B+   DEP @ 1- DEP !  exit THEN
             s" xor " B+    DEP @ 1- DEP ! ;
variable STEPS  variable GI
: GEN-BODY  ( n n -- ) {: in-count use-flag :}  \ fill BBUF with a body, set NIN and the true residual DEP
   in-count 0 > if use-flag 0= if 0 else -1 then else 0 then LOC? !
   in-count NIN !  0 BLEN !
   LOC? @ 0= 0= IF                          \ {: a b c :} prefix binds the NIN inputs as locals
      s" {: " B+  0 GI ! begin GI @ NIN @ < while GI @ BLET GI @ 1+ GI ! repeat  s" :} " B+  0 DEP !
   ELSE NIN @ DEP ! THEN
   6 RND% 3 + STEPS !
   0 GI ! begin GI @ STEPS @ < while  STEP  GI @ 1+ GI !  repeat ;   \ build body, compute true DEP
: HEAD  ( n n n -- ) {: name-ch in-arity out-arity :}  \ PBUF += ": <nch> ( din -- dout ) "  (TFLAG picks i64/n)
   0 PLEN !  s" : " P+  name-ch PC  s"  ( " P+  in-arity PTY  s" -- " P+  out-arity PTY  s" ) " P+ ;
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
variable NCERT  variable NFC  variable NFR  variable RJ
variable LAST-MEAS  variable LAST-TRAP
variable FC-KIND  variable FC-EXP  variable FC-MEAS
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
\ ---- shared measurement: build "depth BASE ! <nin×7> <nch> depth BASE @ - CLEAR-MEAS" ----
: RUN1  ( n n -- ) {: name-ch in-arity :}
   0 PLEN ! s" depth BASE ! " P+
   0 RJ ! begin RJ @ in-arity < while  s" 7 " P+  RJ @ 1+ RJ ! repeat
   name-ch PC  32 PC  s" depth BASE @ - CLEAR-MEAS" P+ ;
TRUSTED: CHK  ( ptr u8 n -- )
   ['] VH set-check  evaluate  ['] PROP-CHECK-HOOK set-check ;     \ check one def; VERD := verdict
TRUSTED: RUN-MEAS  ( n n -- )   \ execute a word and set LAST-MEAS/LAST-TRAP
   0 LAST-TRAP !  RUN1  PBUF PLEN @ evaluate
   ERR@ 0 = IF
      dup 0< IF  drop -1 LAST-TRAP !  ELSE  LAST-MEAS !  THEN
   ELSE  -1 LAST-TRAP !  THEN ;
: FC-SET-ARITY ( n n -- )
   FC-MEAS !  FC-EXP !  1 FC-KIND !  NFC @ 1+ NFC ! ;
: FC-SET-TRAP ( n -- )
   FC-EXP !  2 FC-KIND !  NFC @ 1+ NFC ! ;
: FC-LINE  ( n -- )
   s" prop-test: FALSE-CERT " type POS.
   s" word " type emit s"  " type
   FC-KIND @ 1 = IF
      s" expected " type FC-EXP @ .  s" measured " type FC-MEAS @ . cr
   ELSE
      s" expected " type FC-EXP @ .  s" trap during measurement" type cr
   THEN ;
: MEASURE  ( n n n -- ) {: name-ch in-arity expected :}   \ run a CERTIFIED word <nch>; +1 NFC on arity-mismatch or trap
   name-ch in-arity RUN-MEAS
   LAST-TRAP @ IF  expected FC-SET-TRAP  name-ch FC-LINE
   ELSE  LAST-MEAS @ expected <> IF  expected LAST-MEAS @ FC-SET-ARITY  name-ch FC-LINE  THEN THEN ;
: LOG-META  ( ptr u8 n -- ) {: a:ptr u :}
   s" prop-test: metamorphic " type a u type s"  inconsistency: " type POS. cr
   s" variant: " type PBUF PLEN @ type cr
   s" body: " type BODY. ;

\ ---- metamorphic subsumption: an i64-certified body must also certify under the
\ generic ( n -- n ) sig (n subsumes i64). If it does, run it too (free false-cert
\ coverage); i64-cert but n-reject is a checker inconsistency (logged, non-fatal). ----
variable NSUB  variable NSI
: SUBSUME  ( -- )   \ pre: BBUF/NIN/DOUT is the certified i64 program G
   SMARK  1 TFLAG !  71 NIN @ DOUT @ HEAD  BODY+  0 TFLAG !  PBUF PLEN @ CHK
   VERD @ -1 = IF  NSUB @ 1+ NSUB !  71 NIN @ DOUT @ MEASURE
   ELSE  NSI @ LOG-LIMIT < IF s" subsumption" LOG-META THEN  NSI @ 1+ NSI !  THEN
   SFORGET ;

\ ---- metamorphic render round-trip: render the just-certified body's effect, then
\ re-declare the SAME body with that exact rendered sig — it must re-certify. ----
variable NRT  variable NRI  variable RSA  variable RSU
: ROUNDTRIP  ( -- )   \ pre: G just certified; REND-SIG holds G's rendered effect
   REND-SIG  RSU !  RSA !
   SMARK  0 PLEN ! s" : G ( " P+  RSA @ RSU @ P+  s"  ) " P+  BBUF BLEN @ P+  s" ; " P+  PBUF PLEN @ CHK
   VERD @ -1 = IF  NRT @ 1+ NRT !  71 NIN @ DOUT @ MEASURE
   ELSE  NRI @ LOG-LIMIT < IF s" round-trip" LOG-META THEN  NRI @ 1+ NRI !  THEN
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
         ELSE  NCI @ LOG-LIMIT < IF s" composition" LOG-META THEN  NCI @ 1+ NCI !  THEN
      THEN
   THEN
   SFORGET ;

\ ---- shrinking: on a FALSE-CERT, delta-debug BBUF down to a minimal body that
\ STILL satisfies a predicate (certify-and-mismatch in real use; "still certifies"
\ in the self-test). Drops one trailing token per step, restoring any drop that
\ breaks the predicate. Token surgery just moves BLEN — the bytes stay put. ----
variable PRED  variable BSAVE
: TRIM-TRAIL ( -- )  begin BLEN @ 0 > BBUF BLEN @ 1- + c@ 32 = and while  BLEN @ 1- BLEN !  repeat ;
: DROP-LAST  ( -- bool )   \ remove the last space-delimited token; f = did-remove
   TRIM-TRAIL  BLEN @ 0= IF 0 0= 0= exit THEN
   begin BLEN @ 0 > BBUF BLEN @ 1- + c@ 32 <> and while  BLEN @ 1- BLEN !  repeat  0 0= ;
: REBUILD-G ( -- )  0 TFLAG !  71 NIN @ DOUT @ HEAD  BODY+ ;   \ PBUF := ": G ( NIN -- DOUT ) BBUF ;"
: FCFAIL?  ( -- bool )   \ does the current BBUF certify AND run to an arity != DOUT (or trap)?
   SMARK  REBUILD-G  PBUF PLEN @ CHK
   VERD @ -1 = IF  71 NIN @ RUN-MEAS
      LAST-TRAP @ 0= 0= IF  0 0=  ELSE  LAST-MEAS @ DOUT @ <>  THEN
   ELSE  0 0= 0=  THEN  SFORGET ;
: STILLCERT? ( -- bool )  SMARK  REBUILD-G  PBUF PLEN @ CHK  VERD @ -1 =  SFORGET ;
TRUSTED: CONFIRM-FR? ( -- bool )   \ compile unchecked, run, and prove the rejected true-sig body matches
   SMARK  0 set-check  PBUF PLEN @ evaluate  ['] PROP-CHECK-HOOK set-check
   ERR@ 0 = IF  71 NIN @ RUN-MEAS
      LAST-TRAP @ IF  0  ELSE  LAST-MEAS @ DOUT @ =  THEN
   ELSE  0  THEN  SFORGET ;
: LOG-FR ( -- )
   s" prop-test: false-reject confirmed: " type POS. cr
   s" definition: " type REBUILD-G DEF. ;
TRUSTED: SHRINK  ( R [ -- bool ] -- R )   \ minimize BBUF keeping (pred) true
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
: RUN  ( n n -- )
   N !  dup RUN-SEED !  SEED !  0 NCERT ! 0 NFC ! 0 NFR !  0 NSUB ! 0 NSI ! 0 NRT ! 0 NRI ! 0 NCMP ! 0 NCI !
   0 RI ! begin RI @ N @ < while  ONE  RI @ 1+ RI !  repeat
   s" prop-test: " type N @ . s" programs, " type
   NCERT @ . s" certified, " type  NFC @ . s" FALSE-CERT(s), " type
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
TRUSTED: BAIT  ( ptr u8 n -- )   \ MUST NOT certify
   ['] VH set-check  evaluate  ['] PROP-CHECK-HOOK set-check
   VERD @ -1 = IF s" prop-test: BAIT certified - checker soundness regressed!" 1 die THEN ;
: BAITS ( -- )
   s" : G ( -- ) 3 0 ?do 99 leave loop ;"            BAIT   \ leave carries an extra value
   s" : G ( i64 -- i64 ) dup 0 < if 0 0 exit then ;" BAIT   \ exit-path arity != fall-through
   s" : G ( -- i64 ) 5 0 ?do leave 9 loop ;"         BAIT   \ leave-point != loop-exit
   s" : G ( i64 -- i64 ) 0= ;"                       BAIT   \ bool must not refine to concrete i64
   s" : G ( i64 ) drop ;"                            BAIT   \ malformed sig: missing --
   s" : G ( [ -- ) ;"                                BAIT   \ malformed quotation sig
   s" prop-test: baits OK (control/type/signature regressions rejected)" type cr ;

\ shrink self-test: a long certified ( i64 -- i64 ) body must REDUCE under the
\ "still certifies" predicate — proves the delta-debug loop + token surgery work
\ (a sound checker never hands us a real false-cert to shrink, so exercise the
\ machinery on an achievable predicate).
: SELFTEST-SHRINK ( -- )
   0 BLEN !  s" dup drop 1+ 1- negate 1+ 1- " B+  1 NIN !  1 DOUT !
   BLEN @  [: STILLCERT? ;] SHRINK  BLEN @  >  0= IF
      s" prop-test: self-test SHRINK BROKEN (no reduction)" 1 die THEN
   s" prop-test: shrink OK (delta-debug reduced to: " type  REBUILD-G  PBUF PLEN @ type s" )" type cr ;

\ Fail loudly on any false-cert (`die` exits with the code; IF/THEN are
\ compile-only so this is wrapped in a word). A clean run reaches end-of-input,
\ which exits 0 in batch mode — no `bye` needed (the engine has none).
: FINISH  NFC @ 0 > IF s" prop-test: FALSE-CERT found" 1 die THEN ;
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
: ARG-U ( n -- n )  ARGV ARG>U? 0= IF drop USAGE 0 THEN ;
: PROP-MAIN ( -- )
   SELFTEST
   SELFTEST-SHRINK
   BAITS
   ARGC 1 = IF  DEFAULT-SEED DEFAULT-COUNT RUN
   ELSE ARGC 3 = IF  1 ARG-U  2 ARG-U  RUN
   ELSE  USAGE  THEN THEN
   FINISH ;
PROP-MAIN
