\ run-verdict.f - full-gate PERF-VERDICT retry driver (impure orchestration).
\
\ This is the runner-side wiring (dot habu-integrate-robust-verdict-7f26769e) for
\ the PURE policy in test/perf-verdict.f. The policy judges numbers; this file
\ drives the frozen retry rule over REAL attempts:
\
\   - MEASURE is an injectable seam ( n -- att ): production binds it (run-lib.f)
\     to build attempt 1 from in-process measurements and attempts 2/3 from fresh
\     subprocess gate re-runs; tests bind a fake that returns injected timings.
\   - RUN runs attempt 1; an initial pass stops at one attempt; an initial
\     marginal runs EXACTLY two more fresh attempts and aggregates 2-of-3; any
\     hard fail / inadmissible / SHA mismatch fails closed. Never more than three
\     attempts (E-TRV-ATTEMPTS proves no recursion).
\   - The machine attempt line (emitted by a --gate-attempt worker, PA-EMIT) and
\     its parser (PA-PARSE) are the deterministic cross-process evidence channel;
\     fresh/empty are supplied by the PARENT (root owner), the other eight fields
\     by the worker.
\   - Correctness and performance are rendered as SEPARATE fields.
\
\ Load after test/perf-verdict.f.

require test/perf-verdict.f

\ Named throw codes for the retry driver (unclaimed -4600..-4699 block; the
\ sibling policy owns -4500..-4599). Kept global so run-lib.f can name them.
-4600 constant E-TRV-FIRST
-4699 constant E-TRV-LAST
-4600 constant E-TRV-ATTEMPTS   \ more than three attempts => recursion bug (fail loud)

3 constant TRV-MAX-ATTEMPTS
$09 constant TRV-TAB
$0A constant TRV-LF
$3D constant TRV-EQ
1000000 constant TRV-BAD-MS     \ a self-evidently over-budget elapsed for a fail-closed attempt

package TR-VERDICT

variable ATTEMPTS

: FALSE-BOOL ( -- bool )
   0 0= 0= ;

: TRUE-BOOL ( -- bool )
   0 0= ;

\ ---- injectable attempt seam ----------------------------------------------
public
defer MEASURE ( n -- PERF-VERDICT:att )   \ measure one fresh attempt by index (1..3)
private

: COUNT-ATTEMPT ( -- )
   ATTEMPTS @ 1+ dup ATTEMPTS !
   TRV-MAX-ATTEMPTS > if E-TRV-ATTEMPTS throw then ;

: MEASURE-COUNTED ( n -- PERF-VERDICT:att )
   COUNT-ATTEMPT MEASURE ;

\ ---- rendering -------------------------------------------------------------
: EMIT-LINE ( PERF-VERDICT:att -- )
   PERF-VERDICT:ATTEMPT-LINE type cr ;

: TF$ ( bool -- ptr u8 n )
   if s" t" else s" f" then ;

: FINAL-ROW ( PERF-VERDICT:att ptr u8 n -- ) {: name:ptr nameu:n :}
   \ typed-local-lint: allow-bare-local - a keeps the PERF-VERDICT:att role from the signature.
   {: a :}
   s" perf-verdict: performance=" type name nameu type
   s"  correctness=" type a PERF-VERDICT:CORRECT? TF$ type
   s"  attempts=" type ATTEMPTS @ .U cr ;

\ ---- the retry driver ------------------------------------------------------
\ Marginal-but-admissible attempt 1: run exactly two more fresh attempts and
\ aggregate 2-of-3. Any other outcome resolves at one attempt.
\ typed-local-lint: allow-bare-local - a1 keeps the PERF-VERDICT:att role from the signature.
: RUN-MARGINAL ( PERF-VERDICT:att -- bool ) {: a1 :}
   \ typed-local-lint: allow-bare-local - a2 keeps the PERF-VERDICT:att role.
   2 MEASURE-COUNTED {: a2 :}
   a2 EMIT-LINE
   \ typed-local-lint: allow-bare-local - a3 keeps the PERF-VERDICT:att role.
   3 MEASURE-COUNTED {: a3 :}
   a3 EMIT-LINE
   a1 a2 a3 PERF-VERDICT:TWO-OF-THREE? if
      a1 s" marginal-pass" FINAL-ROW TRUE-BOOL exit
   then
   a1 s" marginal-fail" FINAL-ROW FALSE-BOOL ;

public

: RESET ( -- )
   0 ATTEMPTS ! ;

: ATTEMPTS@ ( -- n )
   ATTEMPTS @ ;

\ Render every attempt + a final verdict row; return true iff performance passes.
: RUN ( -- bool )
   RESET
   \ typed-local-lint: allow-bare-local - a1 keeps the PERF-VERDICT:att role from the seam signature.
   1 MEASURE-COUNTED {: a1 :}
   a1 EMIT-LINE
   a1 PERF-VERDICT:INITIAL-PASS? if a1 s" pass" FINAL-ROW TRUE-BOOL exit then
   a1 PERF-VERDICT:ADMISSIBLE? 0= if a1 s" inadmissible" FINAL-ROW FALSE-BOOL exit then
   a1 PERF-VERDICT:HARD-FAIL? if a1 s" hard-fail" FINAL-ROW FALSE-BOOL exit then
   a1 RUN-MARGINAL ;

\ ---- machine attempt-line channel (worker <-> parent) ----------------------
\ Worker emits eight fields; the parent supplies fresh/empty (it owns the roots).
private

: SB-FLD ( ptr u8 n n -- ) {: v:n :}          \ append TAB key '=' value into the SB
   TRV-TAB SB-APPEND-C SB-APPEND TRV-EQ SB-APPEND-C v SB-U ;

: SB-BOOL-FLD ( ptr u8 n bool -- )
   if 1 else 0 then SB-FLD ;

public

\ Build the deterministic machine attempt line (worker emits, parent parses).
\ PA-EMIT and PA-PARSE share this exact format.
: PA-LINE$ ( n n n n n bool bool bool -- ptr u8 n )   \ elapsed budget pre post sha correct control cache
   {: e:n b:n pr:n po:n sh:n co:bool ct:bool ca:bool :}
   SB-RESET
   s" perf-attempt" SB-APPEND
   s" elapsed" e SB-FLD
   s" budget" b SB-FLD
   s" pre" pr SB-FLD
   s" post" po SB-FLD
   s" sha" sh SB-FLD
   s" correct" co SB-BOOL-FLD
   s" control" ct SB-BOOL-FLD
   s" cache" ca SB-BOOL-FLD
   SB$ ;

: PA-EMIT ( n n n n n bool bool bool -- )
   PA-LINE$ type cr ;

private

\ A canonical fail-closed attempt: hard-fail band AND inadmissible (sha=0). Used
\ when a worker produced no parseable evidence.
: PA-INVALID ( bool -- PERF-VERDICT:att ) {: em:bool :}
   TRV-BAD-MS 1 1 1 0 FALSE-BOOL FALSE-BOOL FALSE-BOOL em FALSE-BOOL
   PERF-VERDICT:ATTEMPT ;

: NEXT-TAB ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}   \ index of next TAB at/after start, else u
   start begin dup u < while
      dup a + c@ TRV-TAB = if exit then
      1+
   repeat ;

: LINE-LEN ( ptr u8 n -- n ) {: a:ptr u:n :}             \ length up to first LF, else u
   0 begin dup u < while
      dup a + c@ TRV-LF = if exit then
      1+
   repeat ;

: FIELD-AT? ( ptr u8 n ptr u8 n -- n bool ) {: fa:ptr fu:n ka:ptr ku:n :}
   fa fu ka ku STARTS-WITH? 0= if 0 FALSE-BOOL exit then
   fu ku 1+ < if 0 FALSE-BOOL exit then
   fa ku BYTE+ c@ TRV-EQ <> if 0 FALSE-BOOL exit then
   fa ku 1+ BYTE+ fu ku 1+ - STR>NUMBER? MATCH option
     none OF 0 FALSE-BOOL ENDOF
     some OF TRUE-BOOL ENDOF
   ;MATCH ;

variable PA-POS
: PA-VAL ( ptr u8 n ptr u8 n -- n bool ) {: la:ptr lu:n ka:ptr ku:n :}
   0 PA-POS !
   begin PA-POS @ lu < while
      la lu PA-POS @ NEXT-TAB {: fend:n :}
      la PA-POS @ BYTE+  fend PA-POS @ -  ka ku FIELD-AT? if TRUE-BOOL exit then drop
      fend 1+ PA-POS !
   repeat
   0 FALSE-BOOL ;

variable PL-POS
: PA-LINE? ( ptr u8 n -- ptr u8 n bool ) {: a:ptr u:n :}    \ locate the perf-attempt line
   0 PL-POS !
   begin PL-POS @ u < while
      a PL-POS @ BYTE+ u PL-POS @ - {: ra:ptr ru:n :}
      ra ru s" perf-attempt" STARTS-WITH? if
         ra  ra ru LINE-LEN  TRUE-BOOL exit
      then
      begin PL-POS @ u < a PL-POS @ BYTE+ c@ TRV-LF <> and while
         PL-POS @ 1+ PL-POS !
      repeat
      PL-POS @ 1+ PL-POS !
   repeat
   a 0 FALSE-BOOL ;

public

\ Build an attempt from a captured worker stdout blob. Missing evidence or a
\ partial line yields a fail-closed attempt (parent supplies empty?).
: PA-PARSE ( ptr u8 n bool -- PERF-VERDICT:att ) {: em:bool :}
   PA-LINE? 0= if 2drop em PA-INVALID exit then
   {: la:ptr lu:n :}
   la lu s" elapsed" PA-VAL 0= if drop em PA-INVALID exit then {: e:n :}
   la lu s" budget"  PA-VAL 0= if drop em PA-INVALID exit then {: b:n :}
   la lu s" pre"     PA-VAL 0= if drop em PA-INVALID exit then {: pr:n :}
   la lu s" post"    PA-VAL 0= if drop em PA-INVALID exit then {: po:n :}
   la lu s" sha"     PA-VAL 0= if drop em PA-INVALID exit then {: sh:n :}
   la lu s" correct" PA-VAL 0= if drop em PA-INVALID exit then 0<> {: co:bool :}
   la lu s" control" PA-VAL 0= if drop em PA-INVALID exit then 0<> {: ct:bool :}
   la lu s" cache"   PA-VAL 0= if drop em PA-INVALID exit then 0<> {: ca:bool :}
   b 0 <= if em PA-INVALID exit then
   pr 0 <= if em PA-INVALID exit then
   po 0 <= if em PA-INVALID exit then
   e b pr po sh co ct TRUE-BOOL em ca PERF-VERDICT:ATTEMPT ;

;package
