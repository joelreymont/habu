\ gate-hb-build-repl.f - checked runner for hb-build --repl checks.
\
\ Load after test/gate-build-common.f.

variable GHR-SIZE

: GHR-PATHS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu out:ptr outu report:ptr reportu :}
   src srcu GB-SRC!
   out outu GB-OUT!
   report reportu GB-REPORT! ;

: GHR-SOURCE-OK ( -- )
   GE-SRC-RESET
   s" : SQ ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" EXPORT SQ" GE-SRC-LINE
   s" 9 SQ . CR" GE-SRC-LINE ;

: GHR-SOURCE-BAD ( -- )
   GE-SRC-RESET
   s" : RBAD ( i64 -- i64 ) 0= ;" GE-SRC-LINE
   s" EXPORT RBAD" GE-SRC-LINE ;

: GHR-BUILD-REPL-CAPTURE ( -- )
   s" --repl" PROC-ARGV+
   GB-HB-BUILD-ARGS
   GB-HB-BUILD-CAPTURE ;

: GHR-BUILD-REPL-OK ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-WRITE-SRC
   GB-BUILD-ARGV
   GHR-BUILD-REPL-CAPTURE
   label labelu GE-EXPECT-OK
   GB-OUT$ FILE? 0= if label labelu GE-FAIL then ;

: GHR-BUILD-REPL-NZ ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-WRITE-SRC
   GB-BUILD-ARGV
   GHR-BUILD-REPL-CAPTURE
   label labelu GE-EXPECT-NONZERO ;

: GHR-RUN-EXPECT ( -- )
   SB-RESET s" 81" GE-OUT-LINE GE-SB-LF
   SB$ s" hb-build --repl output" GB-RUN-EXPECT ;

: GHR-IMGDUMP ( -- )
   GE-HB-RESET
   s" tools/imgdump.f" PROC-ARGV+
   GB-OUT$ PROC-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" imgdump generated engine" GE-EXPECT-OK
   s" + " s" imgdump missing seed dict" GE-EXPECT-OUT-HAS ;

: GHR-GOOD ( -- )
   s" hb-rt.f" s" hb-rt" s" hb-rt-dict" GHR-PATHS
   GHR-SOURCE-OK
   s" hb-build --repl" GHR-BUILD-REPL-OK
   GB-OUT$ FILE-SIZE GHR-SIZE !
   GHR-RUN-EXPECT
   GHR-IMGDUMP ;

: GHR-BAD ( -- )
   s" hb-rt-bad.f" s" hb-rt-bad" s" hb-rt-bad.err" GHR-PATHS
   GHR-SOURCE-BAD
   s" hb-build --repl bool-as-i64 false cert" GHR-BUILD-REPL-NZ
   s" expected: i64" s" hb-build --repl diagnostic expected type" GE-EXPECT-ERR-HAS
   s" actual: bool" s" hb-build --repl diagnostic actual type" GE-EXPECT-ERR-HAS ;

: GHR-RUN ( -- )
   s" hb-gate-build-repl" GT-START
   GHR-GOOD
   GHR-BAD
   GT-CLEANUP
   s" PASS: hb-build --repl verifies user defs (" type
   GHR-SIZE @ GB-U.
   s"  B, engine + library)" type cr
   s" PASS: native hb-build REPL gate phase" type cr ;

GHR-RUN
