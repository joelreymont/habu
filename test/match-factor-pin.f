\ match-factor-pin.f - persistent pin for the factored native MATCH/construct
\ compiler stencils (dot habu-pin-factored-match-08a85f14).
\
\ Commit ed6207a00ef7 ("Factor repeated MATCH compiler stencils") replaced three
\ byte-identical inline engine sequences in EM-COMPILE-ADT-MODE (src/habu/habu2.f)
\ with BL/B-shared subroutines LADTPUSHTOK / LMFRTOP / LADTDIE, a -408 B
\ compiler-text win. That landing shipped NO focused regression: the required
\ byte-identical emitted-USER-code and diagnostic-path proof was not persistent.
\ This fixture is that proof. It is emitted-user-code + diagnostic coverage; the
\ compiler-text SIZE shrink is ratcheted separately by the CODELEN row in
\ test/gate-size-attribution-test.f (owned by the size lane), not here.
\
\ Before/after evidence (recorded once at authoring, spark linux-arm64): the
\ pinned bytes and every diagnostic below were captured from an engine built at
\ ed6207a00ef7's PARENT (pre-factoring) and at the factored fixpoint; all were
\ byte-for-byte and character-for-character identical. The pinned constants here
\ are that shared value, so a future emit/diagnostic drift fails this gate with
\ no historical engine needed.
\
\ Coverage (each an assertion below):
\   payload construction        PIN con-ok / con-err emitted bytes (a wide-variant
\                               constructor emits the same single tag push)
\   MATCH success               RT round-trips execute to the pinned results
\   wide payloads               PIN match-wide (multi-cell arm), RT wide round-trip
\   tag mismatch                DIAG bad-tag: rc 85 + "hb: bad mfp tag"
\   underdepth                  DIAG under: rc 70 + "hb: interpret stack underdepth: PWN"
\   package wordlist restoration PKG: match+construct inside a package; the
\                               private word is gone after ;package (E-UNDEFINED)
\   write-xor-execute discipline RT/PKG runs prove the LPROT RW/RX flips in the
\                               shared legs are exact (a botched flip faults, not
\                               exits 0) and the bad-tag die (inline emit under
\                               RW, then run) executes
\   factored failure branches   DIAG con/match unknown family|variant, expected-of:
\                               every LADTDIE caller dies at its own token, rc 70
\   ahead-of-time compilation   the pinned bytes ARE what an AOT image bakes (the
\                               factored routines are compiler text, never copied
\                               into the user image); end-to-end AOT MATCH round
\                               trips are gated by test/gate-aot-positive-lib.f
\                               (GAP-FETCH / GEMT), which this pin backstops
\   fixpoint bootstrap          every case runs under the self-rebuilt fixpoint
\                               bin/hb (SUBJECT forks re-invoke it), so the
\                               factored compiler in the fixpoint image is what
\                               emits the pinned bytes
\   shared-call overhead        BENCH times ADT compile throughput in a fork;
\                               the authoring A/B (factored vs pre-factoring, 500
\                               match+construct compiles x15) was a null result:
\                               factored median 212.2 ms vs 213.3 ms, inside the
\                               ~1% run-to-run band. The committed BENCH is the
\                               forward throughput ratchet.
\
\ Runs as an isolated fork (test/gate-stdlib-inline-lib.f GSI-TAIL-PROCESS
\ GSI-FORK-INCLUDE). Negatives/positives run in disposable SUBJECT forks so a
\ die never touches the runner.
\
\ Standalone (requires resolve every dependency; run BY THE ENGINE over stdin):
\   bin/hb < test/match-factor-pin.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/memory.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/subject.f

package MATCH-FACTOR-PIN

$4000 constant CAP
30000 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U

\ --- SUBJECT capture ------------------------------------------------------
\ Run one source in a disposable fork of the running (fixpoint) engine, keeping
\ the captured stdout/stderr byte spans for the assertions.

: CAP-LENS ( len len -- )
   LEN>N ERR-U !
   LEN>N OUT-U ! ;

: OUT$ ( -- ptr u8 n )   OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )   ERR ERR-U @ ;

\ Run one source in a disposable fork, assert its exit code, and keep the
\ captured stdout/stderr spans. T-OUTCOME-EXITED= consumes the top outcome, so
\ the two capture lengths (out then err, per lib/process capture order) sit ready
\ for CAP-LENS. A layout outcome cannot be bound to a local, hence this order.
: RUN! ( ptr u8 n n -- ) {: src:ptr srcu:n rc:n :}
   src srcu OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   rc T-OUTCOME-EXITED=
   CAP-LENS ;

\ --- assertion shapes -----------------------------------------------------
\ Positive: exit 0, empty stderr, stdout equals the pinned bytes.
: POS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n src:ptr srcu:n want:ptr wantu:n :}
   label labelu T-LABEL   src srcu 0 RUN!
   label labelu T-LABEL   ERR$ s" " T$=
   label labelu T-LABEL   OUT$ want wantu T$= ;

\ Assert one pinned hex line is present in the last capture's stdout (the whole
\ per-word byte string appears verbatim). Runs against the single PIN-PROG dump
\ already captured by RUN!, so all representative words share one fork.
: PIN= ( ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n hex:ptr hexu:n :}
   label labelu T-LABEL   OUT$ hex hexu CONTAINS? TTRUE ;

\ Negative: exact exit code + a stderr diagnostic substring; the armed-line
\ stdout (if any) is pinned exactly so a fail-open (silent accept) trips too.
: NEG ( ptr u8 n ptr u8 n n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n src:ptr srcu:n rc:n out:ptr outu:n diag:ptr diagu:n :}
   label labelu T-LABEL   src srcu rc RUN!
   label labelu T-LABEL   OUT$ out outu T$=
   label labelu T-LABEL   ERR$ diag diagu CONTAINS? TTRUE ;

\ --- PIN: exact emitted-user-byte fixtures --------------------------------
\ Each source defines representative construct/match words and dumps the exact
\ bytes each emits (cp@ span -> hex). CB@ is the sole raw read boundary (the
\ mmap'd code region is not a checked object); the family name is fixed so the
\ bad-tag die message bytes are stable. The dumped hex below is the pin.
\
\ Disassembly of the pinned words (decoded with src/arch/arm64/disasm.f;
\ operands are register/imm indices):
\   con-ok   sub sp,#16; str x30,[sp]; movz x16,#0(tag); str x16,[x19];
\            add x19,#8; ldr x30,[sp]; add sp,#16; ret            (32 B)
\   con-err  identical but movz x16,#1(err tag)                   (32 B)
\   match-2  ldur x9,[x19,#-8]; cmp x9,#0; b.ne +.. ; sub x19,#8;
\            <arm ok: add>; ...; cmp x9,#1; b.ne ..; <arm err: neg>;
\            b .. ; inline bad-tag die "hb: bad mfp tag\n" write(2)+exit85 (168 B)
\   match-3  three cmp #tag / b.ne dispatch arms + shared bad-tag die (164 B)
\ The bad-tag die block is the C-DIE-BAD-TAG inline emission reached on a
\ scrutinee whose tag matches no arm; it is emitted per MATCH, not shared.
\
\ The dump program is assembled once so every representative word shares the
\ reader/formatter; SHOW prints one word's emitted bytes as an uppercase-hex
\ line. Each expected line below is CONTAINS?-asserted, so the full per-word
\ byte string must appear verbatim in the dump.
create PIN-PROG 4096 allot
variable PIN-PROG-U

: PP+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a PIN-PROG PIN-PROG-U @ + u BYTE-COPY
   PIN-PROG-U @ u + PIN-PROG-U ! ;

: PIN-PROG! ( -- )
   0 PIN-PROG-U !
   s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n" PP+
   s\" SUMTYPE mfpw 0\n  VARIANT small n ;VARIANT\n  VARIANT big ptr u8 n n ;VARIANT\n;SUMTYPE\n" PP+
   s\" SUMTYPE mfpt 0\n  VARIANT one ;VARIANT\n  VARIANT two ;VARIANT\n  VARIANT three ;VARIANT\n;SUMTYPE\n" PP+
   s\" variable A\n" PP+
   s\" TRUSTED: CB@ ( n -- n ) {: a:n :} a {: p:ptr :} p c@ ;\n" PP+
   s\" : HN ( n -- ) $F and dup 10 < if 48 + else 55 + then emit ;\n" PP+
   s\" : HB ( n -- ) {: v:n :} v 4 rshift HN v HN ;\n" PP+
   s\" : MARK ( -- ) cp@ A ! ;\n" PP+
   s\" : SHOW ( -- ) cp@ {: e:n :} A @ begin dup e < while dup CB@ HB 1 + repeat drop cr ;\n" PP+
   s\" MARK : W-CON ( n -- mfp ) construct mfp ok ; SHOW\n" PP+
   s\" MARK : W-CONE ( n -- mfp ) construct mfp err ; SHOW\n" PP+
   s\" MARK : W-MATCH ( mfp -- n ) MATCH mfp ok OF 1 + ENDOF err OF negate ENDOF ;MATCH ; SHOW\n" PP+
   s\" MARK : W-MATCHW ( mfpw -- n ) MATCH mfpw small OF 100 * ENDOF big OF nip nip ENDOF ;MATCH ; SHOW\n" PP+
   s\" MARK : W-MATCH3 ( mfpt -- n ) MATCH mfpt one OF 1 ENDOF two OF 2 ENDOF three OF 3 ENDOF ;MATCH ; SHOW\n" PP+ ;

: PIN-CASES ( -- )
   PIN-PROG!
   \ one fork: emit every representative word once, then pin each line's bytes.
   s" pin/dump" T-LABEL   PIN-PROG PIN-PROG-U @ 0 RUN!
   s" pin/stderr" T-LABEL   ERR$ s" " T$=
   \ payload construction: scalar sum, both tags (a wide-variant constructor
   \ emits the identical single tag push, so con-wide adds no distinct bytes).
   s" pin/con-ok"
      s" FF4300D1FE0300F9100080D2700200F973220091FE0340F9FF430091C0035FD6" PIN=
   s" pin/con-err"
      s" FF4300D1FE0300F9300080D2700200F973220091FE0340F9FF430091C0035FD6" PIN=
   \ MATCH success (compile): scalar 2-variant eliminator.
   s" pin/match-2"
      s" FF4300D1FE0300F969825FF83F0100F1A1010054732200D1300080D2700200F973220091732200D16A0240F9732200D1690240F929010A8B690200F973220091170000143F0500F101010054732200D1732200D1690240F9E90309CB690200F9732200910E0000140500001468623A20626164206D6670207461670A400080D261FFFF10020280D2080880D2010000D4A00A80D2C80B80D2010000D4FE0340F9FF430091C0035FD6" PIN=
   \ wide payloads: the ptr u8 n n arm's multi-cell refinement lowering.
   s" pin/match-wide"
      s" FF4300D1FE0300F969825FF83F0100F1A1010054736200D1900C80D2700200F973220091732200D16A0240F9732200D1690240F9297D0A9B690200F9732200911D0000143F0500F1A1010054732200D1732200D1690240F9732200D1690200F973220091732200D1690240F9732200D1690200F9732200910F0000140600001468623A20626164206D667077207461670A000000400080D241FFFF10220280D2080880D2010000D4A00A80D2C80B80D2010000D4FE0340F9FF430091C0035FD6" PIN=
   \ three-variant enum-shaped dispatch (three cmp/b.ne arms).
   s" pin/match-3"
      s" FF4300D1FE0300F969825FF83F0100F1C1000054732200D1300080D2700200F9732200911D0000143F0500F1C1000054732200D1500080D2700200F973220091160000143F0900F1C1000054732200D1700080D2700200F9732200910F0000140600001468623A20626164206D667074207461670A000000400080D241FFFF10220280D2080880D2010000D4A00A80D2C80B80D2010000D4FE0340F9FF430091C0035FD6" PIN= ;

\ --- RT: MATCH success + write-xor-execute discipline ---------------------
\ Round-trips construct then MATCH inside one word and print the result. A run
\ to exit 0 with the pinned stdout proves the shared legs' RW/RX flips are exact
\ (a wrong LPROT state SIGBUSes on the checker-bridge call or faults the run).
\ Scalar and wide round-trips share one fork.

: RT-CASES ( -- )
   s" rt/roundtrip"
      s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: RT ( n -- n ) construct mfp ok MATCH mfp ok OF 1 + ENDOF err OF negate ENDOF ;MATCH ;\n: RTE ( n -- n ) construct mfp err MATCH mfp ok OF 1 + ENDOF err OF negate ENDOF ;MATCH ;\nSUMTYPE mfpw 0\n  VARIANT small n ;VARIANT\n  VARIANT big ptr u8 n n ;VARIANT\n;SUMTYPE\n: RW ( ptr u8 n n -- n ) construct mfpw big MATCH mfpw small OF 100 * ENDOF big OF nip nip ENDOF ;MATCH ;\n41 RT . 7 RTE . s\" zz\" 3 9 RW . cr\ns\" ok\" type cr\n"
      s\" 42\n-7\n9\n\nok\n" POS ;

\ --- PKG: package wordlist restoration -------------------------------------
\ A package owns its family + a private word, uses construct+MATCH inside, and
\ exposes a public runner. After ;package the private word must be gone: the
\ shared-leg BL/RET traffic and LBCAP token capture must not corrupt the
\ compile-time wordlist. The public runner returns 45 (4 + PRIV 41); the bare
\ PRIV reference after ;package dies E-UNDEFINED at exit 70.

: PKG-CASE ( -- )
   s" pkg/restore"
      s\" package MFPK\nSUMTYPE pk 0\n  VARIANT ok n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: PRIV ( -- n ) 41 ;\n: RP ( n -- n ) construct pk ok MATCH pk ok OF PRIV + ENDOF err OF PRIV - ENDOF ;MATCH ;\npublic\n: RUN ( -- n ) 4 RP ;\n;package\nMFPK:RUN . cr\ns\" armed\" type cr\nPRIV . cr\n"
      70 s\" 45\n\narmed\n" s" E-UNDEFINED: PRIV" NEG ;

\ --- DIAG: every factored failure branch dies at its own token -------------
\ construct legs (LADTDIE via B): unknown family / variant, rc 70.
\ match legs (LADTDIE via B): unknown family / variant / expected-of, rc 70.
\ runtime tag mismatch (C-DIE-BAD-TAG inline): rc 85.
\ certified-word interpret underdepth: rc 70 (routes through the same fail path
\ this fixture pins the ADT legs feed).

: DIAG-CASES ( -- )
   s" diag/con-fam"
      s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( n -- n ) construct nofam ok ;\n"
      70 s" " s" hb: construct: unknown family: nofam" NEG
   s" diag/con-var"
      s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( n -- mfp ) construct mfp novar ;\n"
      70 s" " s" hb: construct: unknown variant: novar" NEG
   s" diag/match-fam"
      s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( mfp -- n ) MATCH nofam ok OF ENDOF ;MATCH ;\n"
      70 s" " s" hb: match: unknown family: nofam" NEG
   s" diag/match-var"
      s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( mfp -- n ) MATCH mfp novar OF ENDOF ;MATCH ;\n"
      70 s" " s" hb: match: unknown variant: novar" NEG
   s" diag/match-of"
      s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: X ( mfp -- n ) MATCH mfp ok NOTOF ENDOF ;MATCH ;\n"
      70 s" " s" hb: match: expected of: NOTOF" NEG
   s" diag/bad-tag"
      s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: PW ( mfp -- n ) MATCH mfp ok OF ENDOF err OF ENDOF ;MATCH ;\nTRUSTED: FORGE ( n -- mfp ) 99 ;\n: BAD ( -- n ) 7 FORGE PW ;\ns\" armed\" type cr\nBAD . cr\n"
      85 s\" armed\n" s" hb: bad mfp tag" NEG
   s" diag/underdepth"
      s\" SUMTYPE mfp 0\n  VARIANT ok  n ;VARIANT\n  VARIANT err n ;VARIANT\n;SUMTYPE\n: PW ( mfp -- n ) MATCH mfp ok OF ENDOF err OF ENDOF ;MATCH ;\n: PWN ( n -- n ) construct mfp ok PW ;\ns\" armed\" type cr\nPWN . cr\n"
      70 s\" armed\n" s" hb: interpret stack underdepth: PWN" NEG ;

\ --- BENCH: ADT compile throughput ----------------------------------------
\ Measures the factored shared-call path under real compile load. Runs in a
\ disposable SUBJECT fork whose program generates 150 MATCH+construct word pairs
\ and times one `evaluate` over the whole batch (the child's evaluate boundary
\ is a TRUSTED word inside the forked source string, so it is not a top-level
\ trust site and needs no inventory row; the parent stays checked). The child
\ prints the compile time and ratchets it against a load-scaled ceiling
\ (TEST-BUDGET:PERF-MS, inherited from the fixture's lib/test) so a
\ compile-throughput regression (a shared routine turned expensive, per-arm
\ emission ballooned) trips without flaking on box saturation; the parent
\ asserts the child exited 0 and echoes its recorded number into the gate log.
\ The authoring factored-vs-unfactored A/B was a null result (see header); this
\ is the forward guard.

create BENCH-PROG 2048 allot
variable BENCH-PROG-U

: BP+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a BENCH-PROG BENCH-PROG-U @ + u BYTE-COPY
   BENCH-PROG-U @ u + BENCH-PROG-U ! ;

: BENCH-PROG! ( -- )
   0 BENCH-PROG-U !
   s\" SUMTYPE bres 0\n  VARIANT v0 n ;VARIANT\n  VARIANT v1 n ;VARIANT\n  VARIANT v2 n ;VARIANT\n  VARIANT v3 n ;VARIANT\n  VARIANT v4 n ;VARIANT\n  VARIANT v5 n ;VARIANT\n  VARIANT v6 n ;VARIANT\n  VARIANT v7 n ;VARIANT\n;SUMTYPE\n" BP+
   s\" $100000 constant SCAP  create SRC SCAP allot  variable SU  variable T0\n" BP+
   s\" TRUSTED: EVL ( ptr u8 n -- ) evaluate ;\n" BP+
   s\" : C+ ( n -- ) {: c:n :} SU @ 1+ SCAP > if 1 throw then c SRC SU @ + c! SU @ 1+ SU ! ;\n" BP+
   s\" : S+ ( ptr u8 n -- ) {: a:ptr u:n :} SU @ u + SCAP > if 1 throw then a SRC SU @ + u BYTE-COPY SU @ u + SU ! ;\n" BP+
   s\" : N+ ( n -- ) {: v:n :} v 10 >= if v 10 / RECURSE then v 10 mod 48 + C+ ;\n" BP+
   s\" : GEN ( n -- ) {: i:n :} s\" : BW\" S+ i N+ s\"  ( bres -- n ) MATCH bres v0 OF ENDOF v1 OF ENDOF v2 OF ENDOF v3 OF ENDOF v4 OF ENDOF v5 OF ENDOF v6 OF ENDOF v7 OF ENDOF ;MATCH ; \" S+ s\" : BC\" S+ i N+ s\"  ( n -- bres ) construct bres v3 ; \" S+ ;\n" BP+
   s\" : BUILD ( n -- ) {: n:n :} 0 SU ! 0 begin dup n < while dup GEN 1+ repeat drop ;\n" BP+
   s\" : RUN-BENCH ( -- ) 150 BUILD\n" BP+
   s\"    mono-ns T0 !  SRC SU @ EVL  mono-ns T0 @ - PROC-NS-PER-MS / {: ms:n :}\n" BP+
   s\"    s\" bench: 150 ADT word compiles in \" type ms . s\"  ms\" type cr\n" BP+
   s\"    ms 6000 TEST-BUDGET:PERF-MS > if s\" bench: ADT compile over budget\" 1 die then ;\n" BP+
   s\" RUN-BENCH\n" BP+
   s\" s\" ok\" type cr\n" BP+ ;

: BENCH ( -- )
   BENCH-PROG!
   s" bench/throughput" T-LABEL
   BENCH-PROG BENCH-PROG-U @ 0 RUN!
   OUT$ type ;

public

: RUN ( -- )
   T-RESET
   PIN-CASES
   RT-CASES
   PKG-CASE
   DIAG-CASES
   BENCH
   T-REPORT
   s" match-factor-pin: ok" type cr ;

;package

MATCH-FACTOR-PIN:RUN
