\ maki/store.f - the CAD artifact store: on-disk layout + row schemas (dot cad-5).
\
\ CAD-PLAN section 13. The single host-side, file-backed store for the Model CAD
\ pipeline: schedules (key -> selected candidate), measurement history, evidence
\ rows (per-gate verdicts), fusion profitability facts (section 5.7), and
\ calibration tables (section 9). Content-addressed by the section 7.4 key that
\ maki/sched-key.f (SK-KEY$) renders. This is a NEW store, DISTINCT from the AOT
\ build-image cache (tools/hb-build-lib.f + lib/content-key.f), which is keyed by
\ source digest, not by region/shape/dtype/layout/target. We imitate that cache's
\ root/dir discipline but never reuse it.
\
\ Layout (append-only v1): one line-oriented file per record class under the store
\ root - schedules.rows, measurements.rows, evidence.rows, profitability.rows,
\ calibration.rows. Root: the HABU_CAD_STORE env var when set, else tmp/cad-store
\ under the workspace root (regenerable, gitignored, never committed).
\
\ Row format: "<key>|<class-specific fields>", one row per line (the report
\ machine-render discipline - agent-parseable by splitting on the first "|" past
\ the key). The section 7.4 key itself contains "|" separators, so a query matches
\ the WHOLE key followed by one "|" delimiter - internal pipes never cause a false
\ match. Query is a linear scan; the LATEST matching row wins (append-only: an update
\ is a new appended row, older rows are shadowed).
\
\ Key contract (caller invariant): a query key is a COMPLETE key, never a proper
\ pipe-aligned prefix of a stored key. Section 7.4 keys are fixed 8-field, so no key
\ is a pipe-prefix of another; calibration lookups always pass the full table|op|field
\ prefix. Match is therefore exact-key. A partial key would match a longer key's
\ prefix and yield that longer row's tail - a caller-contract violation, not masked.
\
\   schedules.rows      <key>|<selected-candidate-index>
\   measurements.rows   <key>|<candidate-index>|<median-ns>
\   evidence.rows       <key>|certify=<v>|golden=[device-]<v>[:<prec>]|gradcheck=<v>|profile=<v>
\                       (a device golden leg carries the licensed precision it was judged
\                       under, e.g. golden=device-pass:tf32; host legs stay plain golden=<v>)
\   profitability.rows  <key>|<region-sig>|<verdict>|<reason>
\   calibration.rows    <table>|<op-or-family>|<field>|<value>
\
\ The evidence verdict text (STORE-V$) and profitability verdict text (PF-NAME) are
\ this file's ON-DISK encoding, deliberately owned here so the wire format stays
\ stable even if maki/report.f changes its human-facing verdict render.
\
\ Consumers: schedules is LIVE end to end - PROMOTE (maki/cad.f) records the region-0
\ selection through SK-PUT-DURABLE (maki/store-replay.f: hot table + schedules.rows in
\ one step) and TILE/TUNE rehydrate the table once per process (REPLAY-ENSURE) and
\ replay the stored selection by key, same-process and fresh-process. The other
\ classes await their consumers: profitability feeds the section 5.7 planner rollback,
\ calibration feeds section 9 self-calibration, and measurement history feeds
\ cad-6-tune / cad-7 regression detection - schema + append + query + tests only.
\
\ Fail closed: IO errors from lib/fs / lib/fs-mutate propagate (never swallowed); an
\ empty key or a newline in the key/any field is a named throw; an oversized row or
\ store file is a named throw; a bad class id or verdict tag is a named throw. A
\ read of a store that does not exist yet is legitimately "no rows" (not an error).
\ maki -> habu only; store owns -5090..-5099.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require maki/report.f
require maki/precision.f

-5090 constant E-STORE-KEY      \ empty key or key contains a newline
-5091 constant E-STORE-FIELD    \ a field / row contains a framing newline
-5092 constant E-STORE-FULL     \ row or store file exceeds its buffer capacity
-5093 constant E-STORE-VERDICT  \ verdict tag out of range for evidence / profitability
-5094 constant E-STORE-CLASS    \ record-class id out of range
-5095 constant E-STORE-ROW      \ malformed stored row on parse (non-numeric / missing pipe)
-5096 constant E-STORE-ROOT     \ resolved store root path empty or over the path cap
-5097 constant E-STORE-PREC     \ precision tag out of range for a device golden leg

package MAKI
public

\ ---- record classes (one append-only file per class) -----------------------
0 constant CLS-SCHED
1 constant CLS-MEAS
2 constant CLS-EVID
3 constant CLS-PROFIT
4 constant CLS-CALIB
5 constant CLS-N

\ ---- profitability verdicts (section 5.7: profitable vs measured regression) --
0 constant PF-PROFITABLE
1 constant PF-REGRESSION
2 constant PF-N

private

1024   constant STORE-ROW-CAP       \ one built row (key + fields + trailing NL)
1024   constant STORE-RESULT-CAP     \ one queried suffix, copied stable
$40000 constant STORE-READ-CAP       \ whole-class read buffer (append-only file cap)
$7C    constant STORE-PIPE           \ '|'
$0A    constant STORE-NL             \ newline
$2D    constant STORE-MINUS          \ '-'
$30    constant STORE-ZERO           \ '0'

create STORE-ROOT-BUF   FS-PATH-CAP allot     variable STORE-ROOT-U
create STORE-PATH-BUF   FS-PATH-CAP allot     variable STORE-PATH-U
create STORE-ROW        STORE-ROW-CAP allot    variable STORE-ROW-U
create STORE-RESULT     STORE-RESULT-CAP allot variable STORE-RESULT-U
create STORE-READ       STORE-READ-CAP allot   variable STORE-QU        \ read buffer + its length
create STORE-KEYBUF     STORE-ROW-CAP allot    variable STORE-KEYBUF-U  \ copied match-key (ptr-stable)
variable STORE-Q-FOUND                                                   \ -1 once a match was seen

\ ---- class -> filename (fail closed on a bad class id) ----------------------
: CLS-FILE$ ( n -- ptr u8 n )
   case
      CLS-SCHED  of s" schedules.rows"     endof
      CLS-MEAS   of s" measurements.rows"  endof
      CLS-EVID   of s" evidence.rows"      endof
      CLS-PROFIT of s" profitability.rows" endof
      CLS-CALIB  of s" calibration.rows"   endof
      E-STORE-CLASS throw
   endcase ;

\ ---- verdict -> on-disk text (this file's stable wire encoding) -------------
: STORE-V$ ( n -- ptr u8 n )
   case
      V-PASS   of s" pass"    endof
      V-FAIL   of s" fail"    endof
      V-NOTRUN of s" not-run" endof
      E-STORE-VERDICT throw
   endcase ;

: PF-NAME ( n -- ptr u8 n )
   case
      PF-PROFITABLE of s" profitable" endof
      PF-REGRESSION of s" regression" endof
      E-STORE-VERDICT throw
   endcase ;

\ precision -> on-disk text (the store owns its wire encoding; ids from maki/precision.f)
: STORE-P$ ( n -- ptr u8 n )
   case
      PREC-F32  of s" f32"  endof
      PREC-TF32 of s" tf32" endof
      E-STORE-PREC throw
   endcase ;

\ ---- store root (HABU_CAD_STORE or tmp/cad-store) + class file path ---------
: STORE-DEFAULT$ ( -- ptr u8 n )  s" tmp/cad-store" ;

: STORE-ENV$ ( -- ptr u8 n )
   s" HABU_CAD_STORE" GETENV dup 0 > if exit then
   2drop STORE-DEFAULT$ ;

: STORE-ROOT-RESOLVE ( -- )
   STORE-ENV$ {: a:ptr u:n :}
   u 0 <= if E-STORE-ROOT throw then
   u FS-PATH-CAP > if E-STORE-ROOT throw then
   a STORE-ROOT-BUF u BYTE-COPY
   u STORE-ROOT-U ! ;

: STORE-ROOT$ ( -- ptr u8 n )
   STORE-ROOT-RESOLVE  STORE-ROOT-BUF STORE-ROOT-U @ ;

: STORE-ENSURE ( -- )  STORE-ROOT$ MAKE-DIRS ;

: STORE-CLASS-PATH ( n -- ptr u8 n ) {: cls:n :}
   STORE-ROOT$ cls CLS-FILE$ STORE-PATH-BUF JOIN-PATH STORE-PATH-U !
   STORE-PATH-BUF STORE-PATH-U @ ;

\ ---- row builder (private buffer; never touches the shared SB) --------------
: SROW-RESET ( -- )  0 STORE-ROW-U ! ;

: SROW-CK ( n -- ) {: k:n :}
   STORE-ROW-U @ k + STORE-ROW-CAP > if E-STORE-FULL throw then ;

: SROW+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u SROW-CK
   a STORE-ROW STORE-ROW-U @ + u BYTE-COPY
   STORE-ROW-U @ u + STORE-ROW-U ! ;

: SROW-C+ ( n -- ) {: c:n :}
   1 SROW-CK
   c STORE-ROW STORE-ROW-U @ + c!
   STORE-ROW-U @ 1+ STORE-ROW-U ! ;

: SROW-PIPE ( -- )  STORE-PIPE SROW-C+ ;
: SROW-NL   ( -- )  STORE-NL   SROW-C+ ;

: SROW-U ( n -- ) {: n:n :}                      \ n >= 0, base-10 digits
   n 10 >= if n 10 / recurse then
   n 10 mod STORE-ZERO + SROW-C+ ;

: SROW-N ( n -- ) {: n:n :}                       \ signed
   n 0 < if STORE-MINUS SROW-C+ 0 n - SROW-U exit then
   n SROW-U ;

\ ---- key validation (non-empty, newline-free framing) ----------------------
: STORE-CK-KEY ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-STORE-KEY throw then
   a u STORE-NL INDEX-OF MATCH option
     none OF ENDOF
     some OF drop E-STORE-KEY throw ENDOF
   ;MATCH ;

\ ---- append the built row (validated) to a class file ----------------------
: STORE-ROW-VALIDATE ( -- )                       \ content + exactly one trailing NL
   STORE-ROW-U @ 2 < if E-STORE-FIELD throw then
   STORE-ROW STORE-ROW-U @ 1- + c@ STORE-NL <> if E-STORE-FIELD throw then
   STORE-ROW STORE-ROW-U @ 1- STORE-NL INDEX-OF MATCH option
     none OF ENDOF
     some OF drop E-STORE-FIELD throw ENDOF
   ;MATCH ;

: STORE-APPEND ( n -- ) {: cls:n :}
   STORE-ROW-VALIDATE
   STORE-ENSURE
   cls STORE-CLASS-PATH  STORE-ROW STORE-ROW-U @  APPEND-FILE ;

\ ---- read a whole class file (missing file -> empty, not an error) ----------
: STORE-READ-CLASS ( n -- ptr u8 n ) {: cls:n :}
   cls STORE-CLASS-PATH {: pa:ptr pu:n :}
   pa pu FILE? 0= if STORE-READ 0 exit then
   pa pu FILE-SIZE STORE-READ-CAP > if E-STORE-FULL throw then
   pa pu STORE-READ STORE-READ-CAP READ-ALL {: got:n :}
   STORE-READ got ;

\ ---- line scan + whole-key-prefix match (latest row wins) -------------------
: STORE-LINE-END ( ptr u8 n n -- n ) {: a:ptr u:n off:n :}
   off begin dup u < while
      dup a + c@ STORE-NL = if exit then
      1+
   repeat ;

: STORE-MATCH? ( ptr u8 n ptr u8 n -- ptr u8 n bool ) {: la:ptr lu:n ka:ptr ku:n :}
   lu ku 1+ < if la 0 false exit then
   la ku ka ku STR= 0= if la 0 false exit then
   la ku + c@ STORE-PIPE <> if la 0 false exit then
   la ku 1+ +  lu ku 1+ -  true ;

: STORE-RESULT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}
   u STORE-RESULT-CAP > if E-STORE-FULL throw then
   a STORE-RESULT u BYTE-COPY  u STORE-RESULT-U ! ;

\ Base pointers are the fixed STORE-READ / STORE-KEYBUF create buffers (ptr-typed);
\ only lengths live in variables. A match copies its suffix over STORE-RESULT, so the
\ LAST matching row survives (append-only latest-wins) with no stored pointer.
: STORE-LINE-AT ( n -- n ) {: off:n :}
   STORE-READ STORE-QU @ off STORE-LINE-END {: ed:n :}
   ed off > if
      STORE-READ off +  ed off -  STORE-KEYBUF STORE-KEYBUF-U @ STORE-MATCH? if
         STORE-RESULT-COPY  -1 STORE-Q-FOUND !
      else 2drop then
   then
   ed 1+ ;

: STORE-SCAN ( -- )
   0 STORE-Q-FOUND !
   0 begin dup STORE-QU @ < while STORE-LINE-AT repeat drop ;

: STORE-QUERY ( n ptr u8 n -- ptr u8 n bool ) {: cls:n ka:ptr ku:n :}
   ku STORE-ROW-CAP > if E-STORE-FULL throw then
   ka STORE-KEYBUF ku BYTE-COPY  ku STORE-KEYBUF-U !
   cls STORE-READ-CLASS nip  STORE-QU !                  \ length only; base is STORE-READ
   STORE-SCAN
   STORE-Q-FOUND @ 0= if STORE-RESULT 0 false exit then
   STORE-RESULT STORE-RESULT-U @ true ;

\ ---- suffix helpers (split on the first / last pipe) ------------------------
: STORE-SPLIT-PIPE ( ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr u:n :}
   a u STORE-PIPE INDEX-OF MATCH option
     none OF E-STORE-ROW throw ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: i:n :}
   a i  a i 1+ +  u i 1+ - ;

: STORE-LAST-PIPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   u begin dup 0 > while
      1- dup a + c@ STORE-PIPE = if exit then
   repeat drop -1 ;

: STORE-PARSE-INT ( ptr u8 n -- n )
   STR>NUMBER?
   MATCH option none OF E-STORE-ROW throw ENDOF some OF ENDOF ;MATCH ;

public

\ ---- root + reset ----------------------------------------------------------
\ STORE-ROOT$ resolves and creates the store root, returning its path.
: STORE-ROOT+ ( -- ptr u8 n )  STORE-ENSURE STORE-ROOT$ ;

\ STORE-RESET is test-only: it truncates the whole store tree under the root so a
\ test leaks no rows. A missing store is a no-op.
: STORE-RESET ( -- )
   STORE-ROOT$ {: a:ptr u:n :}
   u 0 > if a u EXISTS? if a u REMOVE-TREE then then ;

\ ---- schedules -------------------------------------------------------------
: SCHED-PUT ( ptr u8 n n -- ) {: ka:ptr ku:n sel:n :}
   ka ku STORE-CK-KEY
   SROW-RESET  ka ku SROW+  SROW-PIPE  sel SROW-N  SROW-NL
   CLS-SCHED STORE-APPEND ;

: SCHED-GET ( ptr u8 n -- n bool ) {: ka:ptr ku:n :}
   CLS-SCHED ka ku STORE-QUERY 0= if 2drop -1 false exit then
   STORE-PARSE-INT true ;

\ SCHED-LOAD replays every schedules row (in file order, latest wins) to a caller
\ quotation - the last "|" splits <key> from <selected-candidate>. Used by the
\ replay-table backing (maki/store-replay.f) to rehydrate the hot in-memory table.
: SCHED-LINE ( ptr u8 n n [ ptr u8 n n -- ] -- n ) {: ba:ptr bu:n off:n q :} \ typed-local-lint: allow-bare-local - q is the schedule-row callback quotation
   ba bu off STORE-LINE-END {: ed:n :}
   ed off > if
      ba off +  ed off -  {: la:ptr lu:n :}
      la lu STORE-LAST-PIPE {: pi:n :}
      pi 0 < if E-STORE-ROW throw then
      la pi
      la pi 1+ +  lu pi 1+ -  STORE-PARSE-INT
      q execute
   then
   ed 1+ ;

: SCHED-LOAD ( [ ptr u8 n n -- ] -- ) {: q :} \ typed-local-lint: allow-bare-local - q is the schedule-row callback quotation
   CLS-SCHED STORE-READ-CLASS {: ba:ptr bu:n :}
   0 begin dup bu < while
      >r ba bu r> q SCHED-LINE
   repeat drop ;

\ ---- measurement history ---------------------------------------------------
: MEAS-PUT ( ptr u8 n n n -- ) {: ka:ptr ku:n cand:n med:n :}
   ka ku STORE-CK-KEY
   SROW-RESET  ka ku SROW+  SROW-PIPE  cand SROW-N  SROW-PIPE  med SROW-N  SROW-NL
   CLS-MEAS STORE-APPEND ;

: MEAS-GET ( ptr u8 n -- n n bool ) {: ka:ptr ku:n :}
   CLS-MEAS ka ku STORE-QUERY 0= if 2drop -1 -1 false exit then
   STORE-SPLIT-PIPE {: ca:ptr cu:n ma:ptr mu:n :}
   ca cu STORE-PARSE-INT  ma mu STORE-PARSE-INT  true ;

\ ---- evidence (per-gate verdicts) ------------------------------------------
\ the golden field records WHICH leg produced the verdict AND, for a device leg, the
\ LICENSED PRECISION it was judged under (CAD-PLAN 8.1 lever 5): a device model golden
\ (slice 5) writes "golden=device-<v>:<prec>" so a promoted artifact carries both the
\ proof the device leg ran and the precision row that licensed it; the host
\ self-consistency / artifact legs write the plain "golden=<v>" (no precision axis).
: EVID-GOLDEN+ ( n bool n -- ) {: g:n gdev?:bool prec:n :}
   s" golden=" SROW+
   gdev? if s" device-" SROW+ then
   g STORE-V$ SROW+
   gdev? if $3A SROW-C+ prec STORE-P$ SROW+ then ;   \ ":" + licensed precision
: EVID-PUT-G ( ptr u8 n n n n n bool n -- ) {: ka:ptr ku:n c:n g:n gc:n p:n gdev?:bool prec:n :}
   ka ku STORE-CK-KEY
   SROW-RESET
   ka ku SROW+  SROW-PIPE
   s" certify="   SROW+  c  STORE-V$ SROW+  SROW-PIPE
   g gdev? prec EVID-GOLDEN+  SROW-PIPE
   s" gradcheck=" SROW+  gc STORE-V$ SROW+  SROW-PIPE
   s" profile="   SROW+  p  STORE-V$ SROW+
   SROW-NL
   CLS-EVID STORE-APPEND ;
: EVID-PUT ( ptr u8 n n n n n -- ) {: ka:ptr ku:n c:n g:n gc:n p:n :}
   ka ku c g gc p false PREC-F32 EVID-PUT-G ;

: EVID-GET ( ptr u8 n -- ptr u8 n bool ) {: ka:ptr ku:n :}
   CLS-EVID ka ku STORE-QUERY ;

\ ---- fusion profitability facts (section 5.7) ------------------------------
: PROFIT-PUT ( ptr u8 n ptr u8 n n ptr u8 n -- )
   {: ka:ptr ku:n ra:ptr ru:n v:n na:ptr nu:n :}
   ka ku STORE-CK-KEY
   SROW-RESET
   ka ku SROW+  SROW-PIPE
   ra ru SROW+  SROW-PIPE
   v PF-NAME SROW+  SROW-PIPE
   na nu SROW+
   SROW-NL
   CLS-PROFIT STORE-APPEND ;

: PROFIT-GET ( ptr u8 n -- ptr u8 n bool ) {: ka:ptr ku:n :}
   CLS-PROFIT ka ku STORE-QUERY ;

\ ---- calibration tables (section 9) ----------------------------------------
: CALIB-PREFIX ( ptr u8 n ptr u8 n ptr u8 n -- )   \ STORE-ROW = "table|op|field"
   {: ta:ptr tu:n oa:ptr ou:n fa:ptr fu:n :}
   SROW-RESET  ta tu SROW+  SROW-PIPE  oa ou SROW+  SROW-PIPE  fa fu SROW+ ;

: CALIB-PUT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: ta:ptr tu:n oa:ptr ou:n fa:ptr fu:n va:ptr vu:n :}
   ta tu STORE-CK-KEY
   ta tu oa ou fa fu CALIB-PREFIX
   SROW-PIPE  va vu SROW+  SROW-NL
   CLS-CALIB STORE-APPEND ;

: CALIB-GET ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: ta:ptr tu:n oa:ptr ou:n fa:ptr fu:n :}
   ta tu oa ou fa fu CALIB-PREFIX
   CLS-CALIB  STORE-ROW STORE-ROW-U @  STORE-QUERY ;

;package
