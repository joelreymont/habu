\ seed.f - checked native seed installation helpers.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, src/core/sha256.f,
\ and lib/codesign.f.

64 constant SEED-SHA256-HEX-U
10000 constant SEED-SMOKE-TIMEOUT-MS
1200000 constant SEED-BUILD-TIMEOUT-MS
4096 constant SEED-SMOKE-OUT-CAP
4096 constant SEED-SMOKE-ERR-CAP
32768 constant SEED-BUILD-OUT-CAP
32768 constant SEED-BUILD-ERR-CAP
64 constant SEED-USAGE-RC
66 constant SEED-NOINPUT-RC
47 constant SEED-SLASH
12 constant SEED-SMOKE-IN-U

create SEED-SHA-HEX SEED-SHA256-HEX-U allot
create SEED-TMP-PATH FS-PATH-CAP allot
create SEED-SMOKE-OUT SEED-SMOKE-OUT-CAP allot
create SEED-SMOKE-ERR SEED-SMOKE-ERR-CAP allot
create SEED-BUILD-OUT SEED-BUILD-OUT-CAP allot
create SEED-BUILD-ERR SEED-BUILD-ERR-CAP allot
create SEED-SMOKE-IN
   52 c, 49 c, 32 c, 49 c, 32 c, 43 c, 32 c, 46 c,
   32 c, 99 c, 114 c, 10 c,

variable SEED-LAST

: SEED-TRUE ( -- bool )
   0 0= ;

: SEED-FALSE ( -- bool )
   0 0= 0= ;

: SEED-RC0 ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: SEED-EXPECT-FILE ( ptr u8 n -- ) {: a:ptr u :}
   u 0 <= if E-BUILD-PATH throw then
   a u FILE? 0= if E-BUILD-PATH throw then ;

: SEED-HEX-DIGIT? ( n -- bool ) {: c :}
   c 48 >= c 57 <= and if SEED-TRUE exit then
   c 97 >= c 102 <= and if SEED-TRUE exit then
   c 65 >= c 70 <= and if SEED-TRUE exit then
   SEED-FALSE ;

: SEED-HEX64? ( ptr u8 n -- bool ) {: a:ptr u :}
   u SEED-SHA256-HEX-U <> if SEED-FALSE exit then
   0 begin dup u < while
      dup a + c@ SEED-HEX-DIGIT? 0= if drop SEED-FALSE exit then
      1+
   repeat drop
   SEED-TRUE ;

: SEED-LOWER-HEX ( n -- n ) {: c :}
   c 65 >= c 70 <= and if c 32 + exit then
   c ;

: SEED-HEX= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if SEED-FALSE exit then
   0 begin dup u < while
      dup a + c@ SEED-LOWER-HEX
      over b + c@ SEED-LOWER-HEX <> if drop SEED-FALSE exit then
      1+
   repeat drop
   SEED-TRUE ;

: SEED-SHA-RC0 ( n -- )
   dup 0= if drop exit then
   SHA-E-OPEN = if E-BUILD-PATH throw then
   E-FS-IO throw ;

: SEED-VERIFY-SHA256 ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu expect:ptr expectu :}
   expect expectu SEED-HEX64? 0= if E-BUILD-SOURCE throw then
   path pathu SEED-SHA-HEX SHA256-FILE-HEX SEED-SHA-RC0
   SEED-SHA-HEX SEED-SHA256-HEX-U expect expectu SEED-HEX= 0= if
      E-BUILD-SOURCE throw
   then ;

: SEED-VERIFY-OPTIONAL-SHA256 ( ptr u8 n -- ) {: path:ptr pathu :}
   s" HABU_SEED_SHA256" GETENV {: expect:ptr expectu :}
   expectu 0= if exit then
   path pathu expect expectu SEED-VERIFY-SHA256 ;

: SEED-LAST-SLASH ( ptr u8 n -- n ) {: a:ptr u :}
   -1 SEED-LAST !
   0 begin dup u < while
      dup a + c@ SEED-SLASH = if dup SEED-LAST ! then
      1+
   repeat drop
   SEED-LAST @ ;

: SEED-MAKE-PARENT-DIRS ( ptr u8 n -- ) {: a:ptr u :}
   a u SEED-LAST-SLASH {: idx :}
   idx 0 > if a idx MAKE-DIRS then ;

: SEED-TMP$ ( ptr u8 n -- ptr u8 n )
   s" .seed-tmp" SEED-TMP-PATH FS-MUT-SUFFIX-PATH
   SEED-TMP-PATH swap ;

: SEED-INSTALL ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu SEED-EXPECT-FILE
   dstu 0 <= if E-BUILD-PATH throw then
   dst dstu SEED-MAKE-PARENT-DIRS
   dst dstu SEED-TMP$ {: tmp:ptr tmpu :}
   tmp tmpu EXISTS? if tmp tmpu REMOVE-FILE then
   src srcu tmp tmpu COPY-FILE-STREAM
   tmp tmpu CHMOD-X
   tmp tmpu dst dstu PROMOTE-EXECUTABLE
   dst dstu CODESIGN-ENSURE ;

: SEED-SMOKE-OUT-OK? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 4 <> if SEED-FALSE exit then
   a c@ 52 <> if SEED-FALSE exit then
   a 1 + c@ 50 <> if SEED-FALSE exit then
   a 2 + c@ 10 <> if SEED-FALSE exit then
   a 3 + c@ 10 <> if SEED-FALSE exit then
   SEED-TRUE ;

: SEED-EXPECT-NO-STDERR ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: SEED-SMOKE ( ptr u8 n -- ) {: hb:ptr hbu :}
   PROC-ARGV-RESET
   hb hbu SEED-SMOKE-IN SEED-SMOKE-IN-U
   SEED-SMOKE-OUT SEED-SMOKE-OUT-CAP
   SEED-SMOKE-ERR SEED-SMOKE-ERR-CAP
   SEED-SMOKE-TIMEOUT-MS RUN-ARGV-STDIN-CAPTURE
   {: outu erru rc :}
   rc SEED-RC0
   erru SEED-EXPECT-NO-STDERR
   SEED-SMOKE-OUT outu SEED-SMOKE-OUT-OK? 0= if E-BUILD-STATUS throw then ;

: SEED-PREPARE-BUILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/process-env.f" PROC-ARGV+
   s" lib/build.f" PROC-ARGV+
   s" tools/build-fixpoint.f" PROC-ARGV+
   s" tools/build-fixpoint-main.f" PROC-ARGV+
   s" --" PROC-ARGV+
   s" install" PROC-ARGV+ ;

: SEED-RUN-BUILD-FIXPOINT ( ptr u8 n -- ) {: hb:ptr hbu :}
   SEED-PREPARE-BUILD-ARGV
   hb hbu
   SEED-BUILD-OUT SEED-BUILD-OUT-CAP
   SEED-BUILD-ERR SEED-BUILD-ERR-CAP
   SEED-BUILD-TIMEOUT-MS RUN-ARGV-CAPTURE SEED-RC0
   2drop ;

: SEED-INSTALL-SMOKE-BUILD ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu dst:ptr dstu :}
   src srcu SEED-VERIFY-OPTIONAL-SHA256
   src srcu dst dstu SEED-INSTALL
   dst dstu SEED-SMOKE
   dst dstu SEED-RUN-BUILD-FIXPOINT ;

: SEED-USAGE ( -- )
   s" usage: hb --load ... tools/seed.f tools/seed-main.f -- /path/to/hb-seed" SEED-USAGE-RC die ;

: SEED-MAIN ( -- )
   SCRIPT-ARGC 1 <> if SEED-USAGE then
   0 SCRIPT-ARGV$ 2dup FILE? 0= if s" seed: not a file" SEED-NOINPUT-RC die then
   s" bin/hb" SEED-INSTALL-SMOKE-BUILD
   s" seed OK: trusted seed rebuilt current bin/hb" type cr ;
