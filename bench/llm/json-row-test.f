\ json-row-test.f - focused tests for bench/llm/json-row.f.
\ Run: cat lib/errors.f lib/test.f bench/llm/json-row.f bench/llm/json-row-test.f | bin/hb

create BQT-IN
97 c, 34 c, 98 c, 92 c, 99 c, 8 c, 12 c, 10 c, 13 c, 9 c, 0 c, 1 c, 127 c,
13 constant BQT-IN-U

create BQT-WANT
34 c,
97 c,
92 c, 34 c,
98 c,
92 c, 92 c,
99 c,
92 c, 98 c,
92 c, 102 c,
92 c, 110 c,
92 c, 114 c,
92 c, 116 c,
92 c, 117 c, 48 c, 48 c, 48 c, 48 c,
92 c, 117 c, 48 c, 48 c, 48 c, 49 c,
127 c,
34 c,
32 constant BQT-WANT-U

create BQT-PLAIN-WANT
34 c, 97 c, 108 c, 112 c, 104 c, 97 c, 34 c,
7 constant BQT-PLAIN-WANT-U

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
   BQT-IN BQT-IN-U BQ-STRING
   BQT-BUF$ BQT-WANT BQT-WANT-U T$= ;

: BQT-TEST-PLAIN ( -- )
   BQ-RESET
   s" alpha" BQ-STRING
   BQT-BUF$ BQT-PLAIN-WANT BQT-PLAIN-WANT-U T$= ;

: BQT-RUN ( -- )
   T-RESET
   BQT-TEST-SAME
   BQT-TEST-HEX
   BQT-TEST-U00
   BQT-TEST-STRING
   BQT-TEST-PLAIN
   T-REPORT ;

BQT-RUN
