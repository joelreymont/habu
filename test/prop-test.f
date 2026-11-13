\ prop-test.f - property-based checker soundness in Habu: generates typed
\ definitions, checks them, runs the certified ones in process (`evaluate`) and
\ exits 1 when a certified definition's out-arity differs from its declaration
\ (a false-cert) or a metamorphic amplifier (subsumption, composition) reports
\ an inconsistency.
\ Run: bin/hb --load test/prop-test.f
\ bin/hb --load test/prop-test.f -- <seed> <count> runs one seed serially to
\ reproduce a run.
\
\ Shrinking: on a false-cert, delta-debug BBUF down to a minimal body that still
\ satisfies a predicate (certify-and-mismatch in real use; "still certifies" in
\ the self-test). Drops one trailing token per step, restoring any drop that
\ breaks the predicate. Token surgery moves BLEN only; the bytes stay put.
\
\ Primitive semantic properties compile typed candidates through the production
\ checker, execute the real operations on valid operands, and compare runtime
\ output depth while preserving a guard below the declared inputs.
\
\ Seed sweep: PROP-SHARD-N forked slots, each a distinct seed for DEFAULT-COUNT
\ iterations. Self-tests and baits run once in the parent; children run silently
\ (SWEEP-QUIET) and die 1 on a false-cert or metamorphic inconsistency. One red
\ shard fails the phase. Seeds come from a per-run base spread by a 32-bit
\ golden-ratio step, so no two slots share an LCG walk. The golden-ratio step is
\ odd, so i*STEP mod 2^31 is injective over the shard range, and the LCG's odd
\ multiplier makes each step a bijection on the state space.

require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/fmt.f
require src/lib/test/eval.f

package PROP-TEST
private

variable VERD                     \ CHK's last verdict: -1 certified, 0 refused

variable BASE  variable MC
\ Drains an arbitrary residual row while preserving its measured depth.
TRUSTED: CLEAR-MEAS  ( R n -- n )
   dup MC !  begin MC @ 0 > while  swap drop  MC @ 1- MC !  repeat ;

$4D3C2B1A constant CANARY

: VERIFY ( n n -- n ) {: guard:n measured:n :}
   guard CANARY = if measured else -1 then ;

2000 constant DEFAULT-COUNT

\ The default seed varies per run, so each gate explores a fresh space.
: FRESH-SEED ( -- n )
   mono-ns $7FFFFFFF and dup 0= if drop 1 then ;
3 constant LOG-LIMIT
variable SEED   1 SEED !
: RND  ( -- n )
   SEED @ 1103515245 * 12345 +  $7FFFFFFF and  dup SEED ! ;
: RND%  ( n -- n ) {: bound:n :}
   RND bound mod ;

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
: POS. ( -- )  s" seed " type RUN-SEED @ FMT:.INT  s"  iteration " type RI @ FMT:.INT ;
: DEF. ( -- )  PBUF PBUF-U @ type cr ;
: BODY. ( -- )  BBUF BLEN @ type cr ;

variable DEP  variable NIN  variable DOUT  variable K
variable LOCN                              \ fresh mid-body local name counter (z0, z1, ...)
variable LOC?                              \ inputs bound as locals a/b/c at the body top?
variable PERT?                             \ declared out perturbed away from the true depth?
: BLET  ( n -- ) {: idx:n :}
   idx 97 + BBUF BLEN @ + c!
   BLEN @ 1+ BLEN !
   s"  " PROP-B+ ;
\ Emit a mid-body local round-trip `{: zN :} zN` (net-0, needs DEP>=1).
: STEP-LOCAL  ( -- )
   s" {: z" PROP-B+  LOCN @ BD  s"  :} z" PROP-B+  LOCN @ BD  s"  " PROP-B+
   LOCN @ 1+ LOCN ! ;
\ Named structural (net-0) op texts: PROP-STEP emits them and the alphabet
\ self-test asserts each one is generated and certifies.
: OP-LOOP$   ( -- ptr u8 n )  s" 3 0 ?do loop " ;
: OP-LEAVE$  ( -- ptr u8 n )  s" 3 0 ?do leave loop " ;
: OP-STR$    ( -- ptr u8 n )  S\" s\" x\" 2drop " ;
: OP-QUOT0$  ( -- ptr u8 n )  s" [: ;] execute " ;
: OP-BRANCH$ ( -- ptr u8 n )  s" dup 0= if 1+ else 1- then " ;
: OP-RSTK$   ( -- ptr u8 n )  s" >r r> " ;
: OP-RCOPY$  ( -- ptr u8 n )  s" >r r@ drop r> " ;
: OP-QUOT1$  ( -- ptr u8 n )  s" [: 1+ ;] execute " ;
: PROP-STEP  ( -- )   \ append one depth-feasible op to BBUF, update DEP
   5 RND% 0 = IF
      DEP @ 1 >= IF 9 ELSE 4 THEN RND% K !     \ DEP>=1: ops 0..8; DEP=0: depth-free 0..3
      K @ 0 = IF OP-LOOP$ PROP-B+ exit THEN
      K @ 1 = IF OP-LEAVE$ PROP-B+ exit THEN
      K @ 2 = IF OP-STR$ PROP-B+ exit THEN
      K @ 3 = IF OP-QUOT0$ PROP-B+ exit THEN
      K @ 4 = IF OP-BRANCH$ PROP-B+ exit THEN
      K @ 5 = IF OP-RSTK$ PROP-B+ exit THEN
      K @ 6 = IF OP-RCOPY$ PROP-B+ exit THEN
      K @ 7 = IF OP-QUOT1$ PROP-B+ exit THEN
               STEP-LOCAL exit THEN
   DEP @ 2 >= IF 15 RND% ELSE  DEP @ 1 >= IF 6 RND% ELSE 0 THEN  THEN  K !
   K @ 0 = IF
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
   LOC? @ 0= 0= IF
      s" {: " PROP-B+  0 GI ! begin GI @ NIN @ < while GI @ BLET GI @ 1+ GI ! repeat  s" :} " PROP-B+  0 DEP !
   ELSE NIN @ DEP ! THEN
   6 RND% 3 + STEPS !
   0 GI ! begin GI @ STEPS @ < while  PROP-STEP  GI @ 1+ GI !  repeat ;
: HEAD  ( n n n -- ) {: name-ch:n in-arity:n out-arity:n :}
   0 PBUF-U !  s" : " P+  name-ch PC  s"  ( " P+  in-arity PTY  s" -- " P+  out-arity PTY  s" ) " P+ ;
: BODY+  ( -- )  BBUF BLEN @ P+  s" ; " P+ ;
: GEN  ( -- )   \ PBUF := ": G ( i64*NIN -- i64*DOUT ) <body> ;"
   0 TFLAG !
   4 RND%  3 RND% 0 = if -1 else 0 then GEN-BODY
   DEP @ DOUT !  0 PERT? !
   10 RND% 3 < IF
      2 RND% 0= 0= IF DOUT @ 1+ ELSE DOUT @ 1- THEN
      dup 0 < IF drop 0 THEN  DOUT !
      DOUT @ DEP @ <> IF -1 PERT? ! THEN THEN
   71 NIN @ DOUT @ HEAD  BODY+ ;           \ 71 = 'G'

variable NCERT  variable PROP-NFC  variable NFR  variable RJ
variable LAST-MEAS  variable LAST-TRAP
variable FC-KIND  variable FC-EXP  variable FC-MEAS
TYPED-VARIABLE HELD [ -- ]
1001 constant UNDO-RC

public

\ Public only so TRIAL's closed text can reach it.
: RUN-HELD ( -- )
   HELD @ execute  UNDO-RC throw ;

private

\ The body runs in a closed text that always throws, so the engine forgets
\ every word the body defined.
: TRIAL ( [ -- ] -- )
   HELD !
   [: s" PROP-TEST:RUN-HELD" evaluate-closed ;] catch
   dup UNDO-RC = IF  drop  ELSE  throw  THEN ;

PTR-VARIABLE SRC-A
variable SRC-U

: SRC$ ( -- ptr u8 n )
   SRC-A @ SRC-U @ ;

: COMPILE ( -- )
   SRC$ evaluate-closed ;

\ COMPILE-TRUSTED's `TRUSTED` before a PBUF candidate, or MEAS-PBUF's wrapper
\ around a PBUF text, the longest at 55 bytes.
PBUF-CAP 55 + constant TBUF-CAP
TBUF-CAP BUFFER: TBUF
variable TBUF-U

: TBUF+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   TBUF-U @ u + TBUF-CAP > IF  E-STR-CAPACITY throw  THEN
   a TBUF TBUF-U @ + u BYTE-COPY
   TBUF-U @ u + TBUF-U ! ;

\ A program reaches a refused candidate's body only through TRUSTED:, so
\ COMPILE-TRUSTED compiles `TRUSTED` and its `: G ( ... ) <body> ;` unenforced.
: COMPILE-TRUSTED ( ptr u8 n -- n )
   0 TBUF-U !  s" TRUSTED" TBUF+  TBUF+
   TBUF TBUF-U @ SRC-U !  SRC-A !
   ['] COMPILE catch ;

\ One measurement line, checked at top level from depth 0:
\ "<nin×7> <nch> depth CLEAR-MEAS MEAS-OUT !".
: RUN1  ( n n -- ) {: name-ch:n in-arity:n :}
   0 PBUF-U !
   0 RJ ! begin RJ @ in-arity < while  s" 7 " P+  RJ @ 1+ RJ ! repeat
   name-ch PC  s"  depth CLEAR-MEAS MEAS-OUT !" P+ ;
16384 constant CHK-DIAG-CAP
CHK-DIAG-CAP BUFFER: CHK-DIAG
\ A refusal is rc 70 with the checker's diagnostic in CHK-DIAG; any other
\ nonzero rc, an engine refusal after certification among them, is rethrown.
: CHK  ( ptr u8 n -- )
   SRC-U !
   SRC-A !
   CHK-DIAG CHK-DIAG-CAP DIAG-BUFFER!
   ['] COMPILE catch  DIAG-BUFFER$ nip {: rc:n diag-u:n :}
   DIAG-BUFFER-OFF
   rc 0= IF  -1 VERD !  EXIT  THEN
   rc CHECKER-REJECT-RC <>  diag-u 0=  or IF  rc throw  THEN
   0 VERD ! ;
variable MEAS-OUT
\ A measurement text's throw code, a read below its floor among them, or a
\ negative MEAS-OUT is a trap; else MEAS-OUT is the measured depth.
: MEAS-RC ( n -- )
   0 LAST-TRAP !
   0= 0= IF  -1 LAST-TRAP !  EXIT  THEN
   MEAS-OUT @ dup 0< IF  drop -1 LAST-TRAP !  ELSE  LAST-MEAS !  THEN ;
\ A CANARY text reads the guard under CLEAR-MEAS's row, which no checked row
\ types, so it runs as the body of a TRUSTED: definition it defines and calls.
: MEAS-PBUF ( -- )
   0 TBUF-U !
   s" TRUSTED: PROP-MEASURE ( -- ) " TBUF+  PBUF PBUF-U @ TBUF+
   s"  MEAS-OUT ! ; PROP-MEASURE" TBUF+
   [: TBUF TBUF-U @ TEST-EVAL:RC  MEAS-RC ;] TRIAL ;
: RUN-MEAS  ( n n -- )   \ execute a word and set LAST-MEAS/LAST-TRAP
   RUN1  PBUF PBUF-U @ TEST-EVAL:RC  MEAS-RC ;
: FC-SET-ARITY ( n n -- )
   FC-MEAS !  FC-EXP !  1 FC-KIND !  PROP-NFC @ 1+ PROP-NFC ! ;
: FC-SET-TRAP ( n -- )
   FC-EXP !  2 FC-KIND !  PROP-NFC @ 1+ PROP-NFC ! ;
: FC-LINE  ( n -- )
   s" prop-test: FALSE-CERT " type POS.
   s"  word " type emit s"  " type
   FC-KIND @ 1 = IF
      s" expected " type FC-EXP @ FMT:.INT  s"  measured " type FC-MEAS @ FMT:.INT cr
   ELSE
      s" expected " type FC-EXP @ FMT:.INT  s"  trap during measurement" type cr
   THEN ;
: MEASURE  ( n n n -- ) {: name-ch:n in-arity:n expected:n :}   \ run a CERTIFIED word <nch>; +1 NFC on arity-mismatch or trap
   name-ch in-arity RUN-MEAS
   LAST-TRAP @ IF  expected FC-SET-TRAP  name-ch FC-LINE
   ELSE  LAST-MEAS @ expected <> IF  expected LAST-MEAS @ FC-SET-ARITY  name-ch FC-LINE  THEN THEN ;
\ Shards suppress the (capped) metamorphic inconsistency logs so N parallel
\ children do not interleave them on the shared stdout; false-cert lines
\ (FC-LINE) stay unconditional because a false-cert fails the phase. The logs
\ are capped/quiet but the finding is fatal either way: the NSI/NCI
\ counters fail the run at the summary (FINISH) and fail the shard (SHARD-CHILD).
variable SWEEP-QUIET
: LOG-META  ( ptr u8 n -- ) {: a:ptr u:n :}
   SWEEP-QUIET @ if exit then
   s" prop-test: metamorphic " type a u type s"  inconsistency: " type POS. cr
   s" variant: " type PBUF PBUF-U @ type cr
   s" body: " type BODY. ;

\ Metamorphic subsumption: an i64-certified body must also certify under the
\ generic ( n -- n ) sig (n subsumes i64). If it does, run it too (free
\ false-cert coverage); i64-cert but n-reject is a checker inconsistency (fatal
\ at FINISH).
variable NSUB
variable NSI
: SUBSUME  ( -- )   \ pre: BBUF/NIN/DOUT is the certified i64 program G
   [: 1 TFLAG !  83 NIN @ DOUT @ HEAD  BODY+  0 TFLAG !  PBUF PBUF-U @ CHK
   VERD @ -1 = IF  NSUB @ 1+ NSUB !  83 NIN @ DOUT @ MEASURE
   ELSE  NSI @ LOG-LIMIT < IF s" subsumption" LOG-META THEN  NSI @ 1+ NSI !  THEN
   ;] TRIAL ;

\ Metamorphic composition: A:(x--y) and B:(y--z) both certified => ': C A B ;'
\ must certify ( x -- z ) and run-match; chains arities one body cannot reach.
variable NCMP
variable NCI
variable CAI  variable CAO  variable CBO
: COMPOSE  ( -- )
   [: 0 TFLAG !
   3 RND% 0 GEN-BODY  NIN @ CAI !  DEP @ CAO !  88 CAI @ CAO @ HEAD  BODY+  PBUF PBUF-U @ CHK   \ X=88
   VERD @ -1 = IF
      CAO @ 0 GEN-BODY  DEP @ CBO !  89 CAO @ CBO @ HEAD  BODY+  PBUF PBUF-U @ CHK              \ Y=89
      VERD @ -1 = IF
         0 PBUF-U ! s" : Z ( " P+  CAI @ PTY  s" -- " P+  CBO @ PTY  s" ) X Y ; " P+  PBUF PBUF-U @ CHK   \ Z=90
         VERD @ -1 = IF  NCMP @ 1+ NCMP !  90 CAI @ CBO @ MEASURE
         ELSE  NCI @ LOG-LIMIT < IF s" composition" LOG-META THEN  NCI @ 1+ NCI !  THEN
      THEN
   THEN
   ;] TRIAL ;

variable BSAVE
: LAST-SPACE? ( -- bool )
   BLEN @ 0= IF 0 0= 0= exit THEN
   BBUF BLEN @ 1- + c@ 32 = ;
: TRIM-TRAIL ( -- )  begin LAST-SPACE? while  BLEN @ 1- BLEN !  repeat ;
: DROP-LAST  ( -- bool )   \ remove the last space-delimited token; f = did-remove
   TRIM-TRAIL  BLEN @ 0= IF 0 0= 0= exit THEN
   begin BLEN @ 0 > LAST-SPACE? 0= and while  BLEN @ 1- BLEN !  repeat  0 0= ;
: REBUILD-G ( -- )  0 TFLAG !  71 NIN @ DOUT @ HEAD  BODY+ ;   \ PBUF := ": G ( NIN -- DOUT ) BBUF ;"
\ A predicate run under TRIAL leaves its answer here: a trial returns nothing.
TYPED-VARIABLE TRIAL-FLAG bool
: FCFAIL?  ( -- bool )   \ does the current BBUF certify AND run to an arity != DOUT (or trap)?
   [: REBUILD-G  PBUF PBUF-U @ CHK
   VERD @ -1 = IF  71 NIN @ RUN-MEAS
      LAST-TRAP @ 0= 0= IF  0 0=  ELSE  LAST-MEAS @ DOUT @ <>  THEN
   ELSE  0 0= 0=  THEN  TRIAL-FLAG ! ;] TRIAL  TRIAL-FLAG @ ;
: STILLCERT? ( -- bool )  [: REBUILD-G  PBUF PBUF-U @ CHK  VERD @ -1 =  TRIAL-FLAG ! ;] TRIAL  TRIAL-FLAG @ ;
: CONFIRM-FR? ( -- bool )
   [: PBUF PBUF-U @ COMPILE-TRUSTED
   0 = IF  71 NIN @ RUN-MEAS
      LAST-TRAP @ IF  0 0= 0=  ELSE  LAST-MEAS @ DOUT @ =  THEN
   ELSE  0 0= 0=  THEN  TRIAL-FLAG ! ;] TRIAL  TRIAL-FLAG @ ;
: LOG-FR ( -- )
   s" prop-test: false-reject confirmed: " type POS. cr
   s" definition: " type REBUILD-G DEF. ;
: SHRINK  ( R [ -- bool ] -- R ) {: pred :}
   begin
      BLEN @ BSAVE !
      DROP-LAST if
         pred execute if 0 0= else BSAVE @ BLEN ! 0 0= 0= then
      else
         0 0= 0=
      then
   while repeat ;

variable NFC0
: CHECK-G  ( -- )
   GEN
   PBUF PBUF-U @ CHK
   VERD @ -1 = IF                            \ CERTIFIED: measured out-arity must equal declared
      NCERT @ 1+ NCERT !
      PROP-NFC @ NFC0 !  71 NIN @ DOUT @ MEASURE
      PROP-NFC @ NFC0 @ > IF                       \ a FALSE-CERT: shrink to a minimal counterexample & print
         s" original: " type  REBUILD-G  DEF.
         [: FCFAIL? ;] SHRINK
         s" minimized counterexample: " type  REBUILD-G  DEF. THEN
      SUBSUME
   ELSE VERD @ 0 = PERT? @ 0= and IF         \ rejected but NOT perturbed -> the generator declared the
      CONFIRM-FR? IF                          \ true arity and execution agree: a FALSE-REJECT
         NFR @ LOG-LIMIT < IF LOG-FR THEN
         NFR @ 1+ NFR !
      THEN
   THEN THEN ;
: ONE  ( -- )
   ['] CHECK-G TRIAL
   COMPOSE ;
: RUN-CORE  ( n n -- )   \ prints only findings
   N !  dup RUN-SEED !  SEED !  0 NCERT ! 0 PROP-NFC ! 0 NFR !  0 NSUB ! 0 NSI ! 0 NCMP ! 0 NCI !
   0 RI ! begin RI @ N @ < while  ONE  RI @ 1+ RI !  repeat ;
: REPRO  ( n n -- )
   RUN-CORE
   s" prop-test: " type N @ FMT:.INT s"  programs, " type
   NCERT @ FMT:.INT s"  certified, " type  PROP-NFC @ FMT:.INT s"  FALSE-CERT(s), " type
   NFR @ FMT:.INT s"  false-reject(s)" type cr
   s" prop-test: metamorphic — " type  NSUB @ FMT:.INT s"  subsumption + " type
   NCMP @ FMT:.INT s"  composition runs; " type
   NSI @ NCI @ +  FMT:.INT s"  inconsistency(ies)" type cr ;

: BAIT  ( ptr u8 n -- )
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
variable CERT-I
: ALPHA-CERT1 ( n -- )
   CERT-I !
   [: 0 BLEN !  CERT-I @ ALPHA-CERT$ PROP-B+
   0 LOC? !  0 TFLAG !  1 NIN !  1 DOUT !
   71 1 1 HEAD  BODY+
   PBUF PBUF-U @ CHK
   VERD @ -1 <> IF
      s" prop-test: alphabet class REJECTED: " type CERT-I @ ALPHA-CERT$ type cr
      s" prop-test: alphabet self-test FAILED (class no longer certifies)" 1 die THEN
   ;] TRIAL ;
: ALPHA-RESET ( -- )
   0 ALPHA-J ! begin ALPHA-J @ ALPHA-N < while
      0 ALPHA-SEEN ALPHA-J @ + c!  ALPHA-J @ 1+ ALPHA-J !
   repeat ;
: ALPHA-SCAN ( -- )
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
   s" prop-test: alphabet OK (" type ALPHA-N FMT:.INT s"  op classes generated + certified)" type cr ;

\ A sound checker never hands us a real false-cert to shrink, so exercise the
\ machinery on an achievable predicate.
: SELFTEST-SHRINK ( -- )
   0 BLEN !  s" dup drop 1+ 1- negate 1+ 1- " PROP-B+  1 NIN !  1 DOUT !
   BLEN @  [: STILLCERT? ;] SHRINK  BLEN @  >  0= IF
      s" prop-test: self-test SHRINK BROKEN (no reduction)" 1 die THEN
   s" prop-test: shrink OK (delta-debug reduced to: " type  REBUILD-G  PBUF PBUF-U @ type s" )" type cr ;

\ No generated seed reaches CONFIRM-FR?, so this drives it: a bool into `+` is
\ refused yet leaves the declared one cell.
: SELFTEST-FR ( -- )
   0 BLEN !  s" 0 0= + " PROP-B+  1 NIN !  1 DOUT !  REBUILD-G
   PBUF PBUF-U @ CHK  VERD @ -1 = IF
      s" prop-test: self-test FR canary CERTIFIED (no rejected body to confirm)" 1 die THEN
   CONFIRM-FR? 0= IF
      s" prop-test: self-test FALSE-REJECT ORACLE BROKEN (rejected body did not confirm)" 1 die THEN
   s" prop-test: false-reject oracle OK (a rejected body runs to its declared arity)" type cr ;

\ A clean run reaches end of input, which exits 0 in batch mode.
: NMETA ( -- n )  NSI @ NCI @ + ;
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
: USAGE ( -- )  s" prop-test: usage: bin/hb --load test/prop-test.f [-- seed count]" 64 die ;
: ARG-U ( n -- n )  ARGV ARG>U? 0= IF drop USAGE THEN ;

: SCRIPT-ARG-U ( n -- n )
   SCRIPT-ARGV$ STR>NUMBER? MATCH option
     none OF USAGE ENDOF
     some OF ENDOF
   ;MATCH ;

create BUF 64 allot

: SELFTEST-GUARD ( -- )
   0 PBUF-U !
   s" CANARY depth BASE ! 7 nip 7 depth BASE @ - CLEAR-MEAS VERIFY" P+
   MEAS-PBUF
   LAST-TRAP @ 0= if s" prop-test: below-input guard self-test did not detect a clobber" 1 die then
   0 PBUF-U !
   s" CANARY depth BASE ! 7 1+ depth BASE @ - CLEAR-MEAS VERIFY" P+
   MEAS-PBUF
   LAST-TRAP @ LAST-MEAS @ 1 <> or if
      s" prop-test: below-input guard self-test rejected an intact guard" 1 die
   then ;

: OUT-OK? ( -- bool )
   LAST-TRAP @ 0= LAST-MEAS @ DOUT @ = and ;

: SELFTEST-DEPTH ( -- )
   0 PBUF-U !
   s" CANARY depth BASE ! 7 dup depth BASE @ - CLEAR-MEAS VERIFY" P+
   MEAS-PBUF
   5 DOUT ! OUT-OK? if s" prop-test: output-depth self-test accepted a mismatch" 1 die then
   2 DOUT ! OUT-OK? 0= if s" prop-test: output-depth self-test rejected an exact match" 1 die then ;

: BUILD-CHECK-CASE-RUNNER ( ptr u8 n -- ) {: ops:ptr opsu:n :}
   0 PBUF-U !
   s" CANARY depth BASE ! " P+
   ops opsu P+
   s" PROP-PC depth BASE @ - CLEAR-MEAS VERIFY" P+ ;

PTR-VARIABLE CASE-DEF  variable CASE-DEFU
PTR-VARIABLE CASE-OPS  variable CASE-OPSU
variable CASE-EXP
: CHECK-CASE ( ptr u8 n ptr u8 n n -- )
   CASE-EXP !  CASE-OPSU !  CASE-OPS !  CASE-DEFU !  CASE-DEF !
   [: CASE-DEF @ CASE-DEFU @ CHK
   VERD @ -1 <> if
      s" prop-test: primitive candidate rejected: " type CASE-DEF @ CASE-DEFU @ type cr
      s" prop-test: primitive semantic case failed" 1 die
   then
   CASE-EXP @ DOUT !
   CASE-OPS @ CASE-OPSU @ BUILD-CHECK-CASE-RUNNER
   MEAS-PBUF
   OUT-OK? 0= if
      s" prop-test: primitive runtime mismatch: " type CASE-DEF @ CASE-DEFU @ type cr
      LAST-TRAP @ if s" prop-test: primitive reached below its declared inputs" 1 die then
      s" prop-test: primitive runtime output depth differs from its typed candidate" 1 die
   then
   ;] TRIAL ;

: GENERIC-CASES ( -- )
   s" : PROP-PC ( a -- a a ) dup ; " s" 7 " 2 CHECK-CASE
   s" : PROP-PC ( a -- ) drop ; " s" 7 " 0 CHECK-CASE
   s" : PROP-PC ( a b -- b a ) swap ; " s" 7 7 " 2 CHECK-CASE
   s" : PROP-PC ( a b -- a b a ) over ; " s" 7 7 " 3 CHECK-CASE
   s" : PROP-PC ( a b -- b ) nip ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( a b -- b a b ) tuck ; " s" 7 7 " 3 CHECK-CASE
   s" : PROP-PC ( a b c -- b c a ) rot ; " s" 7 7 7 " 3 CHECK-CASE
   s" : PROP-PC ( a b c -- c a b ) -rot ; " s" 7 7 7 " 3 CHECK-CASE
   s" : PROP-PC ( a b -- a b a b ) 2dup ; " s" 7 7 " 4 CHECK-CASE
   s" : PROP-PC ( a b -- ) 2drop ; " s" 7 7 " 0 CHECK-CASE
   s" : PROP-PC ( a b c d -- c d a b ) 2swap ; " s" 7 7 7 7 " 4 CHECK-CASE
   s" : PROP-PC ( a b c d -- a b c d a b ) 2over ; " s" 7 7 7 7 " 6 CHECK-CASE
   s" : PROP-PC ( n n -- n ) + ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a n -- ptr a ) + ; " s" BUF 7 " 1 CHECK-CASE
   s" : PROP-PC ( n ptr a -- ptr a ) + ; " s" 7 BUF " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) - ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a n -- ptr a ) - ; " s" BUF 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a ptr a -- n ) - ; " s" BUF BUF " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) * ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) and ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( bool bool -- bool ) and ; " s" 0 0 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) or ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( bool bool -- bool ) or ; " s" 0 0 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) xor ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( bool bool -- bool ) xor ; " s" 0 0 " 1 CHECK-CASE
   s" : PROP-PC ( n -- n ) 1+ ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a -- ptr a ) 1+ ; " s" BUF " 1 CHECK-CASE
   s" : PROP-PC ( n -- n ) 1- ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a -- ptr a ) 1- ; " s" BUF " 1 CHECK-CASE
   s" : PROP-PC ( n -- n ) negate ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( n -- n ) invert ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( a -- bool ) 0= ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( n -- bool ) 0< ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- bool ) = ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a ptr a -- bool ) = ; " s" BUF BUF " 1 CHECK-CASE
   s" : PROP-PC ( n n -- bool ) < ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a ptr a -- bool ) < ; " s" BUF BUF " 1 CHECK-CASE
   s" : PROP-PC ( n n -- bool ) > ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a ptr a -- bool ) > ; " s" BUF BUF " 1 CHECK-CASE
   s" : PROP-PC ( n n -- bool ) <> ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a ptr a -- bool ) <> ; " s" BUF BUF " 1 CHECK-CASE
   s" : PROP-PC ( n n -- bool ) <= ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a ptr a -- bool ) <= ; " s" BUF BUF " 1 CHECK-CASE
   s" : PROP-PC ( n n -- bool ) >= ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a ptr a -- bool ) >= ; " s" BUF BUF " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) / ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) mod ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n n ) /mod ; " s" 7 7 " 2 CHECK-CASE
   s" : PROP-PC ( n -- n ) abs ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) min ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) max ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) lshift ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) rshift ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( n -- n ) cells ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a -- ptr a ) cell+ ; " s" BUF " 1 CHECK-CASE
   s" : PROP-PC ( n -- n ) cell+ ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( n -- n ) chars ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( ptr a -- ptr a ) char+ ; " s" BUF " 1 CHECK-CASE
   s" : PROP-PC ( n -- n ) char+ ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( n -- ) . ; " s" 7 " 0 CHECK-CASE
   s" : PROP-PC ( -- n ) depth ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- ptr n ) here ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) script-argc ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) epoch-seconds ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) mono-ns ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) rbase ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) cp@ ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) dbase@ ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) ndict@ ; " s" " 1 CHECK-CASE
   \ data-base's pointee is a base address, which no definition may publish
   \ under a type variable; the case is about its arity, so it names a pointee.
   s" : PROP-PC ( -- ptr n ) data-base ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) prot-wid-room ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) get-current ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) checker-scope-depth ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) wf-n@ ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( n n -- n ) wf-w-at ; " s" 7 7 " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) locw-hw-n@ ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( -- n ) schema-reg:schema-root-n@ ; " s" " 1 CHECK-CASE
   s" : PROP-PC ( n -- r ) s>f ; " s" 7 " 1 CHECK-CASE
   s" : PROP-PC ( n -- ) emit ; " s" 7 " 0 CHECK-CASE
   s" : PROP-PC ( -- ) cr ; " s" " 0 CHECK-CASE
   s" : PROP-PC ( -- ) space ; " s" " 0 CHECK-CASE
   s" : PROP-PC ( n -- ) u. ; " s" 7 " 0 CHECK-CASE ;

: MEMORY-CASES ( -- )
   s" : PROP-PC ( ptr a -- a ) @ ; " s" BUF " 1 CHECK-CASE
   s" : PROP-PC ( a ptr a -- ) ! ; " s" 7 BUF " 0 CHECK-CASE
   s" : PROP-PC ( ptr a n -- ptr ptr b ) ptr-field ; " s" BUF 0 " 1 CHECK-CASE
   s" : PROP-PC ( n ptr n -- ) +! ; " s" 7 BUF " 0 CHECK-CASE
   s" : PROP-PC ( ptr u8 -- u8 ) c@ ; " s" BUF " 1 CHECK-CASE
   s" : PROP-PC ( u8 ptr u8 -- ) c! ; " s" 7 BUF " 0 CHECK-CASE
   s" : PROP-PC ( ptr u8 -- ptr u8 n ) count ; " s" BUF " 2 CHECK-CASE
   s" : PROP-PC ( ptr u8 n -- ) type ; " s" BUF 0 " 0 CHECK-CASE
   s" : PROP-PC ( ptr u8 n -- bool ) tfam-ctor-word? ; " s" BUF 3 " 1 CHECK-CASE
   s" : PROP-PC ( ptr u8 n ptr u8 n -- bool ) core-str= ; " s" BUF 3 BUF 3 " 1 CHECK-CASE
   s" : PROP-PC ( ptr u8 n ptr u8 n -- bool ) core-str=ci ; " s" BUF 3 BUF 3 " 1 CHECK-CASE
   s" : PROP-PC ( ptr u8 -- n ) rd32 ; " s" BUF " 1 CHECK-CASE ;

: FLOAT-CASES ( -- )
   s" : PROP-PC ( r r -- r ) f+ ; " s" 1 s>f 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r r -- r ) f- ; " s" 1 s>f 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r r -- r ) f* ; " s" 1 s>f 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r r -- r ) f/ ; " s" 1 s>f 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r -- r ) fnegate ; " s" 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r -- r ) fabs ; " s" 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r -- r ) fsqrt ; " s" 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r r -- bool ) f< ; " s" 1 s>f 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r r -- bool ) f> ; " s" 1 s>f 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r r -- bool ) f= ; " s" 1 s>f 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r -- bool ) f0< ; " s" 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r -- bool ) f0= ; " s" 1 s>f " 1 CHECK-CASE
   s" : PROP-PC ( r -- n ) f>s ; " s" 1 s>f " 1 CHECK-CASE ;

: PRIM-RUN ( -- )
   SELFTEST-GUARD
   SELFTEST-DEPTH
   GENERIC-CASES
   MEMORY-CASES
   FLOAT-CASES
   s" prop-test: primitive semantics OK (typed candidates, runtime depth, below-input guard); canary self-test OK" type cr ;

: PROP-RUN ( n n -- )
   SELFTEST-SHRINK
   BAITS
   SELFTEST-ALPHABET
   SELFTEST-FR
   REPRO
   FINISH ;

\ The gate runs this row in one pool slot beside other rows, so the sweep keeps
\ two shards: a wider fan-out covers nothing more and takes the other rows' CPU.
2 constant PROP-SHARD-N
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
\ Point a descriptor at /dev/null (best effort): a shard checks thousands of
\ intentionally-rejected fuzz programs, and the checker's per-reject diagnostics
\ on stderr would overflow the gate's bounded stderr capture. A false-cert
\ reports on stdout (FC-LINE) and via the shard's nonzero exit, so muting
\ stderr is safe.
: MUTE-FD ( n -- )
   {: dst:n :}
   s" /dev/null" FS:>PATHZ PROP-O-WRONLY 0 open {: fd:n :}
   fd 0 < if exit then
   fd dst dup2 drop
   fd close ;
: SHARD-MUTE-STDERR ( -- )
   2 MUTE-FD ;
\ The named failure line goes to stdout (fd 2 is muted in shards; `die` writes
\ stderr, so its message would vanish) and carries the shard seed so any red is
\ reproducible serially.
: SHARD-FAIL ( ptr u8 n -- )
   type s"  in shard seed " type RUN-SEED @ .
   s" " 1 die ;
\ Fault-injection seam for the sweep red-path self-test only. Its selected
\ shard fails; every other shard exits cleanly without an iteration. A negative
\ slot disables the seam.
variable SHARD-FAULT
-1 SHARD-FAULT !
: SHARD-CHILD ( n -- )   \ never returns: run this shard's seed, then exit 0 / 1
   SHARD-MUTE-STDERR
   SHARD-FAULT @ 0 >= IF
      dup SHARD-FAULT @ = IF s" prop-test: FAULT-INJECT" SHARD-FAIL THEN
      s" " 0 die
   THEN
   SHARD-SEED DEFAULT-COUNT RUN-CORE
   PROP-NFC @ 0 > IF s" prop-test: FALSE-CERT" SHARD-FAIL THEN
   NMETA 0 > IF s" prop-test: METAMORPHIC-INCONSISTENCY" SHARD-FAIL THEN
   s" " 0 die ;
: SHARD-FORK ( n -- ) {: i:n :}
   PROC-FORK:RAW {: p:pid :}
   p PID>N 0= IF i SHARD-CHILD THEN   \ child diverges (dies), only the parent falls through
   p i SHARD-PID! ;
: SHARD-JOIN ( n -- ) {: i:n :}
   i SHARD-PID@ PROCESS:WAIT PROCESS:STATUS>RC RC>N 0<> IF
      SWEEP-RED @ 1+ SWEEP-RED ! THEN ;
: SWEEP  ( n -- )
   SWEEP-BASE !  0 SWEEP-RED !  -1 SWEEP-QUIET !
   0 SWEEP-I ! begin SWEEP-I @ PROP-SHARD-N < while  SWEEP-I @ SHARD-FORK  SWEEP-I @ 1+ SWEEP-I ! repeat
   0 SWEEP-I ! begin SWEEP-I @ PROP-SHARD-N < while  SWEEP-I @ SHARD-JOIN  SWEEP-I @ 1+ SWEEP-I ! repeat
   SWEEP-RED @ IF s" prop-test: sweep FAILED (a shard reported above: FALSE-CERT or METAMORPHIC-INCONSISTENCY)" SWEEP-RED @ die THEN
   s" prop-test: sweep OK — " type PROP-SHARD-N FMT:.INT s"  shards x " type DEFAULT-COUNT FMT:.INT s"  iters, distinct seeds" type cr ;

\ Pin pairwise distinctness and the 31-bit nonzero clamp across representative
\ bases, including one where base + STEP wraps to exactly 0 and exercises the
\ 0->1 clamp.
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

\ The sweep exits with its red-shard count, so an all-red sweep cannot satisfy
\ this test.
: SWEEP-RED-CHILD ( -- )   \ never returns
   1 MUTE-FD  2 MUTE-FD
   PROP-SHARD-N 2 / SHARD-FAULT !
   7 SWEEP
   s" " 0 die ;
: SELFTEST-SWEEP-RED ( -- )
   PROC-FORK:RAW {: p:pid :}
   p PID>N 0= IF SWEEP-RED-CHILD THEN
   p PROCESS:WAIT PROCESS:STATUS>RC RC>N 1 <> IF
      s" prop-test: sweep-red self-test FAILED (a red shard did not fail the sweep)" 1 die THEN
   s" prop-test: sweep-red OK (one red shard fails the sweep)" type cr ;

: RUN-DEFAULT ( -- )
   SELFTEST-SHRINK
   BAITS
   SELFTEST-ALPHABET
   SELFTEST-FR
   PRIM-RUN
   SELFTEST-SHARD-SEEDS
   SELFTEST-SWEEP-RED
   FRESH-SEED SWEEP ;

: MAIN ( -- )
   SCRIPT-SOURCE? IF
      SCRIPT-ARGC 0 = IF RUN-DEFAULT
      ELSE SCRIPT-ARGC 2 = IF
         PRIM-RUN 0 SCRIPT-ARG-U 1 SCRIPT-ARG-U PROP-RUN
      ELSE USAGE THEN THEN
      exit
   THEN
   ARGC 1 = IF  RUN-DEFAULT
   ELSE ARGC 3 = IF  PRIM-RUN  1 ARG-U  2 ARG-U  PROP-RUN
   ELSE  USAGE  THEN THEN ;

MAIN

;package
