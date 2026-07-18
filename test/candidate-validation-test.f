\ candidate-validation-test.f - structural batching and boundary regression.

require lib/test.f
require lib/test/src-shape.f

package CANDIDATE-VALIDATION-TEST

: PATH-PIN ( ptr u8 n -- )
   1 SHAPE:COUNT= ;

: CASES ( -- )
   s" test/candidate-validation.f" SHAPE:LOAD
   S\" s\q test/" 32 SHAPE:COUNT=
   s" test/engine-suite.f" PATH-PIN
   s" test/type-family-suite.f" PATH-PIN
   s" test/type-family-rollback-suite.f" PATH-PIN
   s" test/type-decl-suite.f" PATH-PIN
   s" test/type-nominal-suite.f" PATH-PIN
   s" test/type-nominal-dup-bad.f" PATH-PIN
   s" test/value-nominal-suite.f" PATH-PIN
   s" test/value-nominal-dup-bad.f" PATH-PIN
   s" test/type-ctor-suite.f" PATH-PIN
   s" test/type-linear-suite.f" PATH-PIN
   s" test/type-match-suite.f" PATH-PIN
   s" test/type-layout-lower-pending.f" PATH-PIN
   s" test/layout-buffer.f" PATH-PIN
   s" test/layout-buffer-forge.f" PATH-PIN
   s" test/layout-buffer-depth.f" PATH-PIN
   s" test/layout-valid-w1-bad.f" PATH-PIN
   s" test/layout-valid-product-bad.f" PATH-PIN
   s" test/layout-valid-guards.f" PATH-PIN
   s" test/layout-valid-growth.f" PATH-PIN
   s" test/layout-valid-active-bad.f" PATH-PIN
   s" test/layout-valid-root-bad.f" PATH-PIN
   s" test/layout-valid-hook-forge.f" PATH-PIN
   s" test/layout-valid-walk-forge.f" PATH-PIN
   s" test/layout-valid-desc-forge.f" PATH-PIN
   s" test/wide-store-seal.f" PATH-PIN
   s" test/protection-span.f" PATH-PIN
   s" test/top-row-hook-test.f" PATH-PIN
   s" test/lower-txn-protection.f" PATH-PIN
   s" test/lower-txn-large.f" PATH-PIN
   s" test/type-export-suite.f" PATH-PIN
   s" test/bootstrap-wide-memory-src.f" PATH-PIN
   s" test/candidate-validation-test.f" PATH-PIN ;

: WORKER ( -- )
   s" $40000 constant SRC-CAP" SHAPE:MUST-HAVE
   s" $8000 constant IO-CAP" SHAPE:MUST-HAVE
   s" 120000 constant TIMEOUT-MS" SHAPE:MUST-HAVE
   s" TIMEOUT-MS >MS SUBJECT:RUN STORE" SHAPE:MUST-HAVE
   s" RUN-ARGV" SHAPE:MUST-LACK
   s" ENGINE-KEY$ TARGET-HEX HEX-LEN STR=" SHAPE:MUST-HAVE
   s" SRC SRC-U @ SRC-HEX HASH" SHAPE:MUST-HAVE
   s" OUT OUT-U @ OUT-HEX HASH" SHAPE:MUST-HAVE
   s" ERR ERR-U @ ERR-HEX HASH" SHAPE:MUST-HAVE
   s" ERR-U @ 0= path pathu" SHAPE:MUST-HAVE
   s" needle needleu ERR-HAS? path pathu" SHAPE:MUST-HAVE
   S\" 0 SCRIPT-ARGV$ s\q shared\q STR= if RUN-SHARED exit then" SHAPE:MUST-HAVE
   S\" 0 SCRIPT-ARGV$ s\q top-row\q STR= if RUN-TOP-ROW exit then" SHAPE:MUST-HAVE
   s" manifest" SHAPE:MUST-HAVE
   s" ENUM case-kind positive diagnostic negative ;ENUM" SHAPE:MUST-HAVE
   s" kind MATCH case-kind" SHAPE:MUST-HAVE
   S\" s\q test/top-row-hook-test.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" 1 SHAPE:COUNT= ;

: DIRECT-PIN ( ptr u8 n -- )
   1 SHAPE:COUNT= ;

: MANIFESTS ( -- )
   s" construct case-kind positive" 19 SHAPE:COUNT=
   s" construct case-kind diagnostic" 3 SHAPE:COUNT=
   s" construct case-kind negative" 10 SHAPE:COUNT=
   S\" s\q test/type-family-suite.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-family-rollback-suite.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-decl-suite.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-nominal-suite.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-nominal-dup-bad.f\q construct case-kind negative 70\n      s\q TYPE-NOMINAL-DUP-ARMED\q s\q bad or duplicate signature type\q RUN-CASE" DIRECT-PIN
   S\" s\q test/value-nominal-suite.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/value-nominal-dup-bad.f\q construct case-kind negative 67\n      s\q VALUE-NOMINAL-DUP-ARMED\q s\q duplicate family\q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-linear-suite.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-match-suite.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-layout-lower-pending.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-buffer.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/engine-suite.f\q construct case-kind diagnostic 0 s\q \q s\q habu: in mea1:\q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-ctor-suite.f\q construct case-kind diagnostic 0 s\q \q s\q duplicate family\q RUN-CASE" DIRECT-PIN
   S\" s\q test/type-export-suite.f\q construct case-kind diagnostic 0 s\q \q s\q habu: in xpu3:\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-buffer-forge.f\q construct case-kind negative 70\n      s\q LAYOUT-BUFFER-FORGE-ARMED\q s\q E-UNDEFINED: LBUF-PEND!\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-w1-bad.f\q construct case-kind negative 85\n      s\q LAYOUT-VALID-ARMED\q s\q hb: bad layout tag\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-product-bad.f\q construct case-kind negative 85\n      s\q LAYOUT-VALID-ARMED\q s\q hb: bad layout tag\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-active-bad.f\q construct case-kind negative 85\n      s\q LAYOUT-VALID-ACTIVE-ARMED\q s\q hb: bad layout tag\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-root-bad.f\q construct case-kind negative 85\n      s\q LAYOUT-VALID-ROOT-ARMED\q s\q hb: bad layout tag\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-hook-forge.f\q construct case-kind negative 70\n      s\q LAYOUT-VALID-FORGE-ARMED\q s\q E-UNDEFINED: LAYOUT-VALID-RECORD-XT\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-walk-forge.f\q construct case-kind negative 70\n      s\q LAYOUT-VALID-FORGE-ARMED\q s\q E-UNDEFINED: VP-TERM-WALK-XT\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-desc-forge.f\q construct case-kind negative 70\n      s\q LAYOUT-VALID-FORGE-ARMED\q s\q E-UNDEFINED: LAYOUT-VALID-DESC-XT\q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-buffer-depth.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-guards.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/layout-valid-growth.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/wide-store-seal.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/protection-span.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/lower-txn-protection.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/lower-txn-large.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/bootstrap-wide-memory-src.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q test/candidate-validation-test.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" DIRECT-PIN
   S\" s\q shared\q MANIFEST\n   SHARED-CASES" SHAPE:MUST-HAVE
   S\" s\q top-row\q MANIFEST\n   TOP-ROW-CASE" SHAPE:MUST-HAVE ;

: RUNNER ( -- )
   s" test/gate-validation-worker.f" SHAPE:LOAD
   S\" candidate candidateu s\q shared\q s\q candidate validation shared\q RUN-WORKER" SHAPE:MUST-HAVE
   S\" s\q bin/hb\q s\q shared\q s\q baseline validation shared\q RUN-WORKER" SHAPE:MUST-HAVE
   S\" candidate candidateu s\q top-row\q s\q candidate validation top-row\q RUN-WORKER" SHAPE:MUST-HAVE
   S\" s\q bin/hb\q s\q top-row\q" SHAPE:MUST-LACK
   s" CHECK-BASELINE" 2 SHAPE:COUNT=
   s" CANDIDATE-EVIDENCE CANDIDATE-EVIDENCE-U @ type" SHAPE:MUST-HAVE ;

: BOUNDARIES ( -- )
   s" test/top-row-hook-test.f" SHAPE:LOAD
   S\" s\q 1 set-top-check\q TRH-BARE$ TRH-RUN-LOAD" DIRECT-PIN
   S\" s\q data-base 8 + set-top-check\q TRH-BARE$ TRH-RUN-LOAD" DIRECT-PIN
   S\" s\q set-top-check\q TRH-BARE$ TRH-RUN-LOAD" DIRECT-PIN
   s" MISSING-HOOK$ TRH-RUN-LOAD" DIRECT-PIN
   s" CUSTOM-HOOK$ TRH-RUN-LOAD" DIRECT-PIN
   s" TRH-CHILD-HOOK$ TRH-RUN-LOAD" DIRECT-PIN
   S\" s\q 1 set-top-check\q TRH-BARE$ TRH-RUN-STDIN" DIRECT-PIN
   s" TRH-CHILD-HOOK$ TRH-RUN-STDIN" DIRECT-PIN
   s" TRH-PREFLIGHT-REPLACE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-PREFLIGHT-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-SEAL-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-SNAP-SEAL-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-BELOW-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-ABOVE-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-PREFLIGHT-REINSTALL$ TRH-RUN-SUBJECT" DIRECT-PIN ;

public

: TEST ( -- )
   T-RESET
   CASES
   WORKER
   MANIFESTS
   RUNNER
   BOUNDARIES
   T-REPORT
   s" candidate-validation-test: ok" type cr ;

;package

CANDIDATE-VALIDATION-TEST:TEST
