\ match-factor-pin.f - construct/MATCH execution and diagnostic regressions.
\ Runs real programs in disposable engine processes and checks their results.
\ Run: bin/hb --load test/match-factor-pin.f

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
\ Positive: exit 0, empty stderr, stdout equals the expected output.
: POS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n src:ptr srcu:n want:ptr wantu:n :}
   label labelu T-LABEL   src srcu 0 RUN!
   label labelu T-LABEL   ERR$ s" " T$=
   label labelu T-LABEL   OUT$ want wantu T$= ;

\ Negative: exact exit code + a stderr diagnostic substring; the armed-line
\ stdout (if any) is pinned exactly so a fail-open (silent accept) trips too.
: NEG ( ptr u8 n ptr u8 n n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n src:ptr srcu:n rc:n out:ptr outu:n diag:ptr diagu:n :}
   label labelu T-LABEL   src srcu rc RUN!
   label labelu T-LABEL   OUT$ out outu T$=
   label labelu T-LABEL   ERR$ diag diagu CONTAINS? TTRUE ;

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

public

: RUN ( -- )
   T-RESET
   RT-CASES
   PKG-CASE
   DIAG-CASES
   T-REPORT
   s" match-factor-pin: ok" type cr ;

;package

MATCH-FACTOR-PIN:RUN
