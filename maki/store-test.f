\ maki/store-test.f - checked tests for the CAD artifact store (dot cad-5).
\ Root resolve + create, per-class append/query roundtrips (latest row wins),
\ whole-key-prefix matching over keys that themselves contain "|", the missing-store
\ read (not-found, no throw), and every named fail-closed throw (empty/newline key,
\ newline field, bad class, bad verdict, row capacity).
\ Writes only under the store root; STORE-RESET keeps the tree from leaking.

require lib/test.f
require lib/string.f
require maki/precision.f
require maki/store.f

package MAKI

\ ---- fixtures: a newline-bearing key/value and an over-cap value -------------
create NL-BUF 3 allot
$61 NL-BUF c!  $0A NL-BUF 1+ c!  $62 NL-BUF 2 + c!    \ "a<LF>b"
: NL$ ( -- ptr u8 n )  NL-BUF 3 ;

1100 constant BIGV-U
create BIGV BIGV-U allot
: FILL-BIGV ( -- )  BIGV-U 0 ?do  $78 BIGV i + c!  loop ;   \ 'x' * 1100 (> STORE-ROW-CAP)
FILL-BIGV

\ ---- fail-closed probes -----------------------------------------------------
: TRY-EMPTYKEY   ( -- )  s" " 0 SCHED-PUT ;
: TRY-NLKEY      ( -- )  NL$ 0 SCHED-PUT ;
: TRY-NLFIELD    ( -- )  s" k" s" op" s" f" NL$ CALIB-PUT ;
: TRY-BADCLASS   ( -- )  CLS-N STORE-CLASS-PATH 2drop ;
: TRY-BADVERDICT ( -- )  s" ek" V-N V-PASS V-PASS V-PASS EVID-PUT ;
: TRY-BADPREC    ( -- )  s" ek" V-PASS V-PASS V-PASS V-PASS true PREC-N EVID-PUT-G ;
: TRY-BADPF      ( -- )  s" pk" s" sig" PF-N s" r" PROFIT-PUT ;
: TRY-CAP        ( -- )  s" tbl" s" op" s" fld" BIGV BIGV-U CALIB-PUT ;

T-RESET

\ ---- root resolves and is created as a directory ----------------------------
STORE-ROOT$ nip 0 > TTRUE
STORE-ROOT+ DIR? TTRUE

\ ---- schedules: miss -> hit -> latest wins ---------------------------------
STORE-RESET
s" k1" SCHED-GET nip TFALSE
s" k1" SCHED-GET drop -1 T=
s" k1" 3 SCHED-PUT
s" k1" SCHED-GET nip TTRUE
s" k1" SCHED-GET drop 3 T=
s" k1" 8 SCHED-PUT
s" k1" SCHED-GET drop 8 T=            \ append-only: latest row wins

\ a §7.4-shaped key (internal "|") matches on the whole key, not a false split, and
\ a distinct same-arity key with a differing trailing field does not cross-match.
s" a|b|c|d|e|f|g|h" 5 SCHED-PUT
s" a|b|c|d|e|f|g|h" SCHED-GET drop 5 T=
s" a|b|c|d|e|f|g|h2" SCHED-GET nip TFALSE     \ distinct key, differs before the delim
s" x|b|c|d|e|f|g|h"  SCHED-GET nip TFALSE     \ distinct key, differs in the first field

\ ---- measurement history ---------------------------------------------------
s" mk" 4 250 MEAS-PUT
s" mk" MEAS-GET TTRUE 250 T= 4 T=
s" mk" 7 180 MEAS-PUT
s" mk" MEAS-GET TTRUE 180 T= 7 T=
s" nomeas" MEAS-GET TFALSE -1 T= -1 T=

\ ---- evidence rows (per-gate verdicts) -------------------------------------
s" ek" V-PASS V-PASS V-NOTRUN V-NOTRUN EVID-PUT
s" ek" EVID-GET TTRUE
s" certify=pass|golden=pass|gradcheck=not-run|profile=not-run" T$=
s" noev" EVID-GET nip TFALSE

\ a device golden leg records the licensed precision it was judged under (8.1 lever 5)
s" ekd" V-PASS V-PASS V-NOTRUN V-NOTRUN true PREC-F32 EVID-PUT-G
s" ekd" EVID-GET TTRUE
s" certify=pass|golden=device-pass:f32|gradcheck=not-run|profile=not-run" T$=
s" ekt" V-PASS V-FAIL V-NOTRUN V-NOTRUN true PREC-TF32 EVID-PUT-G
s" ekt" EVID-GET TTRUE
s" certify=pass|golden=device-fail:tf32|gradcheck=not-run|profile=not-run" T$=

\ ---- profitability facts (section 5.7) -------------------------------------
s" pk" s" 431E24867468A764" PF-REGRESSION s" measured-regression" PROFIT-PUT
s" pk" PROFIT-GET TTRUE
s" 431E24867468A764|regression|measured-regression" T$=
s" pk2" s" DEADBEEF" PF-PROFITABLE s" faster-fused" PROFIT-PUT
s" pk2" PROFIT-GET TTRUE  s" DEADBEEF|profitable|faster-fused" T$=
s" nopk" PROFIT-GET nip TFALSE

\ ---- calibration tables (section 9) ----------------------------------------
s" occupancy" s" gemm-tf32" s" regs-per-thread" s" 168" CALIB-PUT
s" occupancy" s" gemm-tf32" s" regs-per-thread" CALIB-GET TTRUE s" 168" T$=
s" occupancy" s" gemm-tf32" s" regs-per-thread" s" 200" CALIB-PUT     \ update
s" occupancy" s" gemm-tf32" s" regs-per-thread" CALIB-GET TTRUE s" 200" T$=
s" occupancy" s" gemm-tf32" s" no-field" CALIB-GET nip TFALSE

\ ---- fail-closed throws -----------------------------------------------------
' TRY-EMPTYKEY   E-STORE-KEY     TTHROWS
' TRY-NLKEY      E-STORE-KEY     TTHROWS
' TRY-NLFIELD    E-STORE-FIELD   TTHROWS
' TRY-BADCLASS   E-STORE-CLASS   TTHROWS
' TRY-BADVERDICT E-STORE-VERDICT TTHROWS
' TRY-BADPREC    E-STORE-PREC    TTHROWS
' TRY-BADPF      E-STORE-VERDICT TTHROWS
' TRY-CAP        E-STORE-FULL    TTHROWS

\ ---- missing store reads as not-found (no throw) ---------------------------
STORE-RESET
s" k1" SCHED-GET nip TFALSE
s" mk" MEAS-GET TFALSE -1 T= -1 T=
s" ek" EVID-GET nip TFALSE

STORE-RESET
T-REPORT

end-package
