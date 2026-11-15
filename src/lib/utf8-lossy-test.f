\ utf8-lossy-test.f - UTF8-LOSSY:REPAIR against Rust's String::from_utf8_lossy:
\ each expected span is that function's output over the same input.

require src/lib/test.f
require src/lib/arena.f
require src/lib/utf8-lossy.f

package UTF8-LOSSY-TEST

: ASSERT-REPAIR ( ^u8 n ^u8 n -- )
   {: in:^ inlen:n want:^ wantlen:n :}
   ARENA:NEW {: a:^ :}
   in inlen a UTF8-LOSSY:REPAIR {: out:^ outlen:n :}
   out outlen want wantlen T$=
   out in <> TTRUE
   a ARENA:RELEASE ;

: ASSERT-UNCOPIED ( ^u8 n -- ) {: in:^ inlen:n :}
   ARENA:NEW {: a:^ :}
   in inlen a UTF8-LOSSY:REPAIR {: out:^ outlen:n :}
   out in = TTRUE
   outlen inlen T=
   a ARENA:RELEASE ;

\ Unicode 3.9, Table 3-8: each maximal subpart is one U+FFFD.
: TABLE-3-8 ( -- )
   S\" \x61\xF1\x80\x80\xE1\x80\xC2\x62\x80\x63\x80\xBF\x64"
   S\" \x61\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\x62\xEF\xBF\xBD\x63\xEF\xBF\xBD\xEF\xBF\xBD\x64"
   ASSERT-REPAIR ;

\ A truncated sequence is one U+FFFD, and the byte after it is kept.
: TRUNCATED ( -- )
   S\" \xC2" S\" \xEF\xBF\xBD" ASSERT-REPAIR
   S\" \xC2\x41" S\" \xEF\xBF\xBD\x41" ASSERT-REPAIR
   S\" \xE2\x82" S\" \xEF\xBF\xBD" ASSERT-REPAIR
   S\" \xE2\x82\x41" S\" \xEF\xBF\xBD\x41" ASSERT-REPAIR
   S\" \xF0\x9F\x98" S\" \xEF\xBF\xBD" ASSERT-REPAIR
   S\" \xF0\x9F\x98\x41" S\" \xEF\xBF\xBD\x41" ASSERT-REPAIR ;

\ No prefix of a well-formed sequence: each byte is its own U+FFFD.
: OVERLONG ( -- )
   S\" \xC0\xAF" S\" \xEF\xBF\xBD\xEF\xBF\xBD" ASSERT-REPAIR
   S\" \xE0\x80\xAF"
   S\" \xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD" ASSERT-REPAIR ;

: SURROGATE ( -- )
   S\" \xED\xA0\x80"
   S\" \xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD" ASSERT-REPAIR ;

: OUT-OF-RANGE ( -- )
   S\" \xF4\x90\x80\x80"
   S\" \xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD" ASSERT-REPAIR
   S\" \xF5\xF6\xF7\xF8\xF9\xFA\xFB\xFC\xFD\xFE\xFF"
   S\" \xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD"
   ASSERT-REPAIR ;

: LONE-CONTINUATION ( -- )
   S\" \x80" S\" \xEF\xBF\xBD" ASSERT-REPAIR
   S\" \x41\x80\x42" S\" \x41\xEF\xBF\xBD\x42" ASSERT-REPAIR ;

\ Well-formed multibyte sequences around a bad byte are copied whole.
: COPIES-SCALARS ( -- )
   S\" \xC3\xA9\xFF\xF0\x9F\x98\x80"
   S\" \xC3\xA9\xEF\xBF\xBD\xF0\x9F\x98\x80" ASSERT-REPAIR ;

: VALID-UNCOPIED ( -- )
   S\" \x41\xC3\xA9\xF0\x9F\x98\x80" ASSERT-UNCOPIED ;

: NEGATIVE-LENGTH ( -- )
   ARENA:NEW {: a:^ :}
   [: S" A" drop -1 a UTF8-LOSSY:REPAIR 2drop ;] E-STR-BOUNDS TTHROWSQ
   a ARENA:RELEASE ;

public

: RUN ( -- )
   T-RESET
   TABLE-3-8 TRUNCATED OVERLONG SURROGATE OUT-OF-RANGE
   LONE-CONTINUATION COPIES-SCALARS VALID-UNCOPIED NEGATIVE-LENGTH ;

;package

UTF8-LOSSY-TEST:RUN
T-REPORT
