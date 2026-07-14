\ maki/store-rehydrate-test.f - typed store-row rehydration acceptance (sub-dot 6
\ v2-typestate-store-rehydrate, dot habu-v2-typestate-store-57afdc0a).
\ MODEL-CAD-V2-PLAN.md § R7 Design Addendum, sub-dot at :1953-1961, decoder note :1824-1826.
\
\ Three acceptances:
\ (a) a valid persisted evidence row rehydrates to its TYPED fields (positive control
\     asserting the recovered verdicts + golden leg + licensed precision);
\ (b) a malformed/forged persisted row REJECTS with a named throw - one negative per
\     failure class (bad field count, unknown field kind/label, out-of-domain verdict,
\     out-of-domain precision), never a silent skip;
\ (c) round-trip golden: persist -> rehydrate -> re-render/re-persist is byte-identical
\     (the committed goldens RT-DEV$ / RT-HOST$), for both the device and host golden legs.
\
\ Plus the store-seal remainder of the promotion lane: the durable schedule writer
\ SK-PUT-DURABLE is now package-MAKI-private, so a cross-package / qualified
\ MAKI:SK-PUT-DURABLE no longer resolves (verdict 1), while the paired read control
\ MAKI:SCHED-GET still certifies (-1) through the same qualification - a real seal, not a
\ broken qualifier or an unloaded word (per LESSONS; promote-test.f threat model).
\ CHECK-QUIET-CANDIDATE! returns -1 certified / 0 type-reject / 1 unresolvable.

require lib/test.f
require test/checker-assert.f
require maki/store-rehydrate.f
require maki/store-replay.f       \ loads the now-sealed SK-PUT-DURABLE (seal regression below)

T-RESET

\ ==== store-seal regression (top-level / cross-package qualification) ==============
s" SB-SK-PUT-DURABLE ( ptr u8 n n -- ) MAKI:SK-PUT-DURABLE"
   CHECK-QUIET-CANDIDATE! 1 T=
s" SB-OK-SCHED-GET ( ptr u8 n -- n bool ) MAKI:SCHED-GET"
   CHECK-QUIET-CANDIDATE! -1 T=

package MAKI

\ ---- committed round-trip goldens (device + host golden legs) --------------------
: RT-DEV$  ( -- ptr u8 n )  s" certify=pass|golden=device-fail:tf32|gradcheck=not-run|profile=pass" ;
: RT-HOST$ ( -- ptr u8 n )  s" certify=pass|golden=pass|gradcheck=not-run|profile=not-run" ;

\ ---- re-render a decoded value through the sealed wire owner (no wire duplication) -
: RT-REPERSIST ( ptr u8 n ptr u8 n -- )   \ ( re-key suffix -- ) decode -> project -> EVID-PUT-G
   EVID-ROW-DECODE
   {: ka:ptr ku:n cv:verdict leg:EVID:golden-leg gv:verdict gp:EVID:prec-class grv:verdict pv:verdict :}
   ka ku
   cv EVID-VERDICT>CODE  gv EVID-VERDICT>CODE  grv EVID-VERDICT>CODE  pv EVID-VERDICT>CODE
   leg EVID-LEG>DEV?  gp EVID-PREC>CODE
   EVID-PUT-G ;

\ ---- (a) positive control: a device row rehydrates to its typed fields -----------
: RT-POS ( -- )
   STORE-RESET
   s" rk" V-PASS V-FAIL V-NOTRUN V-PASS true PREC-TF32 EVID-PUT-G
   s" rk" EVID-GET TTRUE                 \ persisted row is present
   EVID-ROW-DECODE
   {: cv:verdict leg:EVID:golden-leg gv:verdict gp:EVID:prec-class grv:verdict pv:verdict :}
   cv  EVID-VERDICT>CODE V-PASS    T=    \ certify verdict
   leg EVID-LEG>DEV?     TTRUE           \ golden leg = device
   gv  EVID-VERDICT>CODE V-FAIL    T=    \ golden verdict
   gp  EVID-PREC>CODE    PREC-TF32 T=    \ licensed precision = tf32
   grv EVID-VERDICT>CODE V-NOTRUN  T=    \ gradcheck verdict
   pv  EVID-VERDICT>CODE V-PASS    T= ;  \ profile verdict

\ ---- (b) one fixture per rehydrate failure class (each must throw a named code) ---
: RT-BADFIELDS  ( -- )  s" certify=pass|golden=pass|gradcheck=not-run"                              EVID-ROW-DECODE 2drop 2drop 2drop ;
: RT-BADLABEL   ( -- )  s" cert=pass|golden=pass|gradcheck=not-run|profile=not-run"                 EVID-ROW-DECODE 2drop 2drop 2drop ;
: RT-BADVERDICT ( -- )  s" certify=maybe|golden=pass|gradcheck=not-run|profile=not-run"             EVID-ROW-DECODE 2drop 2drop 2drop ;
: RT-BADPREC    ( -- )  s" certify=pass|golden=device-pass:fp16|gradcheck=not-run|profile=not-run"  EVID-ROW-DECODE 2drop 2drop 2drop ;

\ ---- (c) round-trip golden: persist -> rehydrate -> re-persist is byte-identical --
: RT-ROUND-DEV ( -- )
   STORE-RESET
   s" rt-a" V-PASS V-FAIL V-NOTRUN V-PASS true PREC-TF32 EVID-PUT-G
   s" rt-a" EVID-GET TTRUE  RT-DEV$ T$=            \ persisted row == the committed golden
   s" rt-b"  s" rt-a" EVID-GET drop  RT-REPERSIST  \ rehydrate rt-a, re-render under rt-b
   s" rt-b" EVID-GET TTRUE  RT-DEV$ T$= ;          \ re-persisted row == the golden (identity)
: RT-ROUND-HOST ( -- )
   STORE-RESET
   s" rh-a" V-PASS V-PASS V-NOTRUN V-NOTRUN EVID-PUT
   s" rh-a" EVID-GET TTRUE  RT-HOST$ T$=
   s" rh-b"  s" rh-a" EVID-GET drop  RT-REPERSIST
   s" rh-b" EVID-GET TTRUE  RT-HOST$ T$= ;

\ ==== run =========================================================================
RT-POS
' RT-BADFIELDS  E-EVID-ROW-FIELDS  TTHROWS
' RT-BADLABEL   E-EVID-ROW-LABEL   TTHROWS
' RT-BADVERDICT E-EVID-ROW-VERDICT TTHROWS
' RT-BADPREC    E-EVID-ROW-PREC    TTHROWS
RT-ROUND-DEV
RT-ROUND-HOST

STORE-RESET
;package

T-REPORT
