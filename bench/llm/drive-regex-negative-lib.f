\ drive-regex-negative-lib.f - native stdlib regex negative driver.
\
\ Load after bench/llm/drive-stdlib-lib.f and bench/llm/drive-stdlib-live.f.

: DRX-SYNTAX-TASK? ( -- bool )
   DS-NAME$ s" RX-BAD-PATTERN" STR= ;

: DRX-CAPACITY-TASK? ( -- bool )
   DS-NAME$ s" RX-CAPACITY" STR= ;

: DRX-CODE$ ( -- ptr u8 n )
   DRX-SYNTAX-TASK? if s" code E-RX-SYNTAX" exit then
   DRX-CAPACITY-TASK? if s" code E-RX-CAPACITY" exit then
   s" code E-RX-SYNTAX" ;

: DRX-ERROR-CONST$ ( -- ptr u8 n )
   DRX-SYNTAX-TASK? if s" E-RX-SYNTAX" exit then
   DRX-CAPACITY-TASK? if s" E-RX-CAPACITY" exit then
   s" E-RX-SYNTAX" ;

: DRX-BUILD-TESTS ( -- )
   DS-TEST-RESET
   s" : DRX-NEG-MAIN ( -- )" DS-TEST-LN
   s"    [: " DS-TEST+
   DS-NAME$ DS-TEST+
   s"  ;] catch" DS-TEST-LN
   DRX-CODE$ DRX-ERROR-CONST$ DS-NEGATIVE-TEST-TAIL
   s" DRX-NEG-MAIN" DS-TEST-LN ;

: DRX-FINISH-CHECK-REJECT ( -- )
   DRX-CODE$ DS-LR-NEGATIVE ;

: DRX-FINISH-TESTS ( -- )
   DRX-CODE$ DS-LR-NEGATIVE ;

: DRX-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DS-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DRX-BUILD-TESTS
   DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL
   DS-CAND-VALID? 0= if DS-INVALID-CANDIDATE exit then
   DS-RUN-CHECK
   DS-RC @ 0 <> if DRX-FINISH-CHECK-REJECT exit then
   DS-RUN-TESTS
   DRX-FINISH-TESTS ;

: DRX-RUN-MODEL ( -- )
   DS-PREPARE
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   MRUN-TOKENS @ DS-TOKENS !
   MRUN-RC @ 0= 0= if DS-MODEL-ERROR exit then
   MRUN-TEXT$ DRX-EVALUATE-TEXT ;

: DRX-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   textu DS-OUT-CAP > if E-DS-CAPACITY throw then
   text DS-OUT-BUF textu BYTE-COPY
   textu DS-OUT-U !
   DS-PREPARE
   0 DS-TOKENS !
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DRX-EVALUATE-TEXT ;

: DRX-MAIN ( -- )
   DS-CONFIG
   DRX-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
