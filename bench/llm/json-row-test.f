\ json-row-test.f - focused tests for bench/llm/json-row.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/json-write.f bench/llm/fixture-text.f bench/llm/json-row.f
\ bench/llm/json-row-test.f

: BQT-BUF$ ( -- ptr u8 n )
   BQ-OUT BQ-OUT-LEN @ ;

: BQT-TEST-SAME ( -- )
   s" string" s" string" BQ-SAME? TTRUE
   s" string" s" file" BQ-SAME? TFALSE
   s" string" s" strings" BQ-SAME? TFALSE ;

: BQT-TEST-HEX ( -- )
   0 BQ-HEX 48 T=
   9 BQ-HEX 57 T=
   10 BQ-HEX 65 T=
   15 BQ-HEX 70 T= ;

: BQT-TEST-U00 ( -- )
   BQ-RESET
   1 BQ-U00
   BQT-BUF$ s" \u0001" T$= ;

: BQT-TEST-STRING ( -- )
   BQ-RESET
   BFT-JSON-ESCAPE-SAMPLE$ {: a:ptr u :}
   a u BQ-STRING
   BQT-BUF$ a u BFT-JSON-STRING$ T$= ;

: BQT-TEST-PLAIN ( -- )
   BQ-RESET
   s" alpha" {: a:ptr u :}
   a u BQ-STRING
   BQT-BUF$ a u BFT-JSON-STRING$ T$= ;

: BQT-RUN ( -- )
   T-RESET
   BQT-TEST-SAME
   BQT-TEST-HEX
   BQT-TEST-U00
   BQT-TEST-STRING
   BQT-TEST-PLAIN
   T-REPORT ;

BQT-RUN
