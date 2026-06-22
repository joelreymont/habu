\ diagnostic-stats-check-test.f - checker fixture for diagnostic stats reducer.
\
\ Load after lib/test.f, lib/json-write.f, bench/llm/manifest.f,
\ bench/llm/diagnostic-json-check-stub.f, and bench/llm/diagnostic-stats.f.
\ Runtime JSON field parsing is covered by diagnostic-stats-test.f with the real
\ JSON parser; this fixture keeps the new reducer body checked without loading
\ tools/json.f's catch-based recovery boundary.

8192 constant DGSCT-BUF-CAP
44 constant DGSCT-COMMA
91 constant DGSCT-LBRACK
93 constant DGSCT-RBRACK

create DGSCT-EXP-BUF DGSCT-BUF-CAP allot

variable DGSCT-EXP-U

: DGSCT-EXP-ROOM ( n -- ) {: add :}
   add 0 < if E-BM-FIELD throw then
   add DGSCT-BUF-CAP DGSCT-EXP-U @ - > if E-BM-FIELD throw then ;

: DGSCT-EXP-APPEND ( ptr u8 n -- ) {: a:ptr u :}
   u DGSCT-EXP-ROOM
   a DGSCT-EXP-BUF DGSCT-EXP-U @ + u BYTE-COPY
   DGSCT-EXP-U @ u + DGSCT-EXP-U ! ;

: DGSCT-EXP-C ( n -- ) {: c :}
   1 DGSCT-EXP-ROOM
   c DGSCT-EXP-BUF DGSCT-EXP-U @ + c!
   DGSCT-EXP-U @ 1+ DGSCT-EXP-U ! ;

: DGSCT-EXP$ ( -- ptr u8 n )
   DGSCT-EXP-BUF DGSCT-EXP-U @ ;

: DGSCT-EVENTS$ ( -- ptr u8 n )
   s" 1	fix_type
1	fix_type
2	add_producer
3	custom_class
2	custom_class
4	zeta
4	alpha
" ;

: DGSCT-BAD-ROUND$ ( -- ptr u8 n )
   s" nope	fix_type
" ;

: DGSCT-BAD-FIELDS$ ( -- ptr u8 n )
   s" 1
" ;

: DGSCT-EXP-APPEND-JW ( -- )
   JW$ DGSCT-EXP-APPEND ;

: DGSCT-EXP-STAT ( ptr u8 n n bool n n -- )
   {: cls:ptr clsu diag success iter delta :}
   JW-RESET
   JW-OBJECT-START
   s" repair_class" cls clsu JW-FIELD-S
   JW-COMMA s" diagnostic_count" diag JW-FIELD-U
   JW-COMMA s" repair_success" success JW-FIELD-BOOL
   JW-COMMA s" repair_iterations" iter JW-FIELD-U
   JW-COMMA s" token_delta" delta JW-FIELD-U
   JW-OBJECT-END
   DGSCT-EXP-APPEND-JW ;

: DGSCT-EXPECTED-STATS! ( -- )
   0 DGSCT-EXP-U !
   DGSCT-LBRACK DGSCT-EXP-C
   s" add_producer" 1 DGS-TRUE 1 7 DGSCT-EXP-STAT
   DGSCT-COMMA DGSCT-EXP-C
   s" fix_type" 2 DGS-TRUE 1 7 DGSCT-EXP-STAT
   DGSCT-COMMA DGSCT-EXP-C
   s" alpha" 1 DGS-TRUE 1 7 DGSCT-EXP-STAT
   DGSCT-COMMA DGSCT-EXP-C
   s" custom_class" 2 DGS-TRUE 2 7 DGSCT-EXP-STAT
   DGSCT-COMMA DGSCT-EXP-C
   s" zeta" 1 DGS-TRUE 1 7 DGSCT-EXP-STAT
   DGSCT-RBRACK DGSCT-EXP-C ;

: DGSCT-EXPECT-BAD-ROUND ( -- )
   DGSCT-BAD-ROUND$ DGS-FALSE 0 DGS-REPAIR-STATS$ 2drop ;

: DGSCT-EXPECT-BAD-FIELDS ( -- )
   DGSCT-BAD-FIELDS$ DGS-FALSE 0 DGS-REPAIR-STATS$ 2drop ;

: DGSCT-EXPECT-STATS ( -- )
   DGSCT-EXPECTED-STATS!
   DGSCT-EVENTS$ DGS-TRUE 7 DGS-REPAIR-STATS$ DGSCT-EXP$ T$=
   s" " DGS-TRUE 0 DGS-REPAIR-STATS$ s" []" T$= ;

: DGSCT-MAIN ( -- )
   T-RESET
   DGSCT-EXPECT-STATS
   [: DGSCT-EXPECT-BAD-ROUND ;] E-BM-FIELD TTHROWSQ
   [: DGSCT-EXPECT-BAD-FIELDS ;] E-BM-SCHEMA TTHROWSQ
   T-REPORT
   s" diagnostic-stats-check-test: ok" type cr ;

DGSCT-MAIN
