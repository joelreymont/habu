\ drive-regex-negative-test.f - focused tests for regex negative driver.

: DRXT-CONFIG-COMMON ( -- )
   DTH-MODELS$ MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" -- error" DS-SIG!
   s" regex" DS-CATEGORY!
   s" code E-RX-SYNTAX" DS-TESTS!
   s" regex negative fixture" DS-SPEC!
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   8 DS-TASK-ORDER !
   2 DS-K !
   1 DS-MAX-REPAIRS ! ;

: DRXT-CONFIG-SYNTAX ( -- )
   DRXT-CONFIG-COMMON
   98 DS-ID !
   s" RX-BAD-PATTERN" DS-NAME!
   s" code E-RX-SYNTAX" DS-TESTS! ;

: DRXT-CONFIG-CAPACITY ( -- )
   DRXT-CONFIG-COMMON
   99 DS-ID !
   s" RX-CAPACITY" DS-NAME!
   s" code E-RX-CAPACITY" DS-TESTS! ;

: DRXT-SYNTAX$ ( -- ptr u8 n )
   DTH-SRC-RESET
   s" create RX-PAT 42 c, create RX-TXT 97 c, create RX-OUT 32 allot variable RX-LEN : RX-OUT-PTR ( -- ptr u8 ) RX-OUT ; " DTH-SRC+
   s" : RX-BAD-PATTERN ( -- ) RX-PAT 1 >LEN RX-OUT-PTR 32 >LEN RX-COMPILE LEN>N RX-LEN ! RX-TXT 1 >LEN RX-OUT-PTR RX-LEN @ >LEN RX-MATCH? drop " DTH-SRC+
   DTH-SRC-END ;

: DRXT-CAPACITY$ ( -- ptr u8 n )
   DTH-SRC-RESET
   s" create RX-PAT 97 c, 98 c, 99 c, create RX-OUT 1 allot : RX-OUT-PTR ( -- ptr u8 ) RX-OUT ; " DTH-SRC+
   s" : RX-CAPACITY ( -- ) RX-PAT 3 >LEN RX-OUT-PTR 0 >LEN RX-COMPILE drop " DTH-SRC+
   DTH-SRC-END ;

: DRXT-SILENT$ ( -- ptr u8 n )
   DTH-SRC-RESET
   s" create RX-PAT 97 c, create RX-OUT 32 allot : RX-OUT-PTR ( -- ptr u8 ) RX-OUT ; " DTH-SRC+
   s" : RX-BAD-PATTERN ( -- ) RX-PAT 1 >LEN RX-OUT-PTR 32 >LEN RX-COMPILE drop " DTH-SRC+
   DTH-SRC-END ;

: DRXT-WRONG$ ( -- ptr u8 n )
   DTH-SRC-RESET
   s" create RX-PAT 97 c, 98 c, 99 c, create RX-OUT 1 allot : RX-OUT-PTR ( -- ptr u8 ) RX-OUT ; " DTH-SRC+
   s" : RX-BAD-PATTERN ( -- ) RX-PAT 3 >LEN RX-OUT-PTR 0 >LEN RX-COMPILE drop " DTH-SRC+
   DTH-SRC-END ;

: DRXT-ASSERT-REJECT ( ptr u8 n -- )
   LR-OUTCOME$ s" reject" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ 0 T=
   DTH-ROW-HAS ;

: DRXT-ASSERT-FAIL ( ptr u8 n -- )
   LR-OUTCOME$ s" fail" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ 0 T=
   DTH-ROW-HAS ;

: DRXT-TEST-SYNTAX ( -- )
   DRXT-CONFIG-SYNTAX
   DRXT-SYNTAX$ DRX-RUN-TEXT
   s" code E-RX-SYNTAX" DRXT-ASSERT-REJECT
   CLEANUP-RUN ;

: DRXT-TEST-CAPACITY ( -- )
   DRXT-CONFIG-CAPACITY
   DRXT-CAPACITY$ DRX-RUN-TEXT
   s" code E-RX-CAPACITY" DRXT-ASSERT-REJECT
   CLEANUP-RUN ;

: DRXT-TEST-SILENT ( -- )
   DRXT-CONFIG-SYNTAX
   DRXT-SILENT$ DRX-RUN-TEXT
   s" silent success" DRXT-ASSERT-FAIL
   CLEANUP-RUN ;

: DRXT-TEST-WRONG ( -- )
   DRXT-CONFIG-SYNTAX
   DRXT-WRONG$ DRX-RUN-TEXT
   s" wrong error code" DRXT-ASSERT-FAIL
   CLEANUP-RUN ;

: DRXT-MAIN ( -- )
   T-RESET
   DRXT-TEST-SYNTAX
   DRXT-TEST-CAPACITY
   DRXT-TEST-SILENT
   DRXT-TEST-WRONG
   T-REPORT
   s" drive-regex-negative-test: ok" type cr ;

DRXT-MAIN
