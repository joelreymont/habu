\ codesign.f - checked executable promotion and ad-hoc signing helpers.
\
\ Load after lib/errors.f, lib/fs.f, lib/fs-mutate.f, lib/process.f,
\ and lib/process-argv.f.

4096 constant CODESIGN-OUT-CAP
4096 constant CODESIGN-ERR-CAP
10000 constant CODESIGN-TIMEOUT-MS

create CODESIGN-OUT CODESIGN-OUT-CAP allot
create CODESIGN-ERR CODESIGN-ERR-CAP allot

: CODESIGN-TOOL ( -- ptr u8 n )
   s" /usr/bin/codesign" ;

: CODESIGN-RC0 ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: CODESIGN-EXPECT-TOOL ( -- )
   CODESIGN-TOOL FILE? 0= if E-BUILD-COMMAND throw then ;

: CODESIGN-EXPECT-FILE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-PATH throw then
   a u FILE? 0= if E-BUILD-PATH throw then ;

: CODESIGN-EXPECT-EXECUTABLE ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-EXPECT-FILE
   a u EXECUTABLE? 0= if E-BUILD-PATH throw then ;

: CODESIGN-RUN ( -- n )
   CODESIGN-EXPECT-TOOL
   CODESIGN-TOOL >LEN
   CODESIGN-OUT CODESIGN-OUT-CAP >LEN
   CODESIGN-ERR CODESIGN-ERR-CAP >LEN
   CODESIGN-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   {: outu erru rc :}
   outu drop erru drop rc RC>N ;

: CODESIGN-VERIFY-RC ( ptr u8 n -- n ) {: a:ptr u :}
   a u CODESIGN-EXPECT-EXECUTABLE
   PROC-ARGV-RESET
   s" -v"  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+
   CODESIGN-RUN ;

: CODESIGN-VERIFY ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-VERIFY-RC CODESIGN-RC0 ;

: CODESIGN-FORCE ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-EXPECT-FILE
   PROC-ARGV-RESET
   s" -s"  >LEN PROC-ARGV+
   s" -"  >LEN PROC-ARGV+
   s" --force"  >LEN PROC-ARGV+
   a u  >LEN PROC-ARGV+
   CODESIGN-RUN CODESIGN-RC0 ;

: CODESIGN-ENSURE ( ptr u8 n -- ) {: a:ptr u :}
   a u CODESIGN-VERIFY-RC 0= if exit then
   a u CODESIGN-FORCE
   a u CODESIGN-VERIFY ;

: PROMOTE-EXECUTABLE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu CODESIGN-EXPECT-FILE
   dstu 0 <= if E-BUILD-PATH throw then
   src srcu CHMOD-X
   src srcu dst dstu RENAME-FILE
   dst dstu CODESIGN-EXPECT-FILE ;

: PROMOTE-SIGNED-EXECUTABLE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu CODESIGN-FORCE
   src srcu dst dstu PROMOTE-EXECUTABLE
   dst dstu CODESIGN-VERIFY ;
