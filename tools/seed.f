\ seed.f - checked native seed installation helpers: validate against
\ HABU_SEED_SHA256 when set, install it as bin/hb, smoke it, then rebuild
\ bin/hb from current source with `tools/hb-build.f -- --engine -o bin/hb`.

require src/lib/error.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/codesign.f
require src/core/sha256.f
require src/core/exit-status.f

64 constant SEED-SHA256-HEX-U
10000 constant SEED-SMOKE-TIMEOUT-MS
\ An engine build takes 5 minutes at most.
300000 constant SEED-BUILD-TIMEOUT-MS
4096 constant SEED-SMOKE-OUT-CAP
4096 constant SEED-SMOKE-ERR-CAP
32768 constant SEED-BUILD-OUT-CAP
32768 constant SEED-BUILD-ERR-CAP
47 constant SEED-SLASH
12 constant SEED-SMOKE-IN-U

create SEED-SHA-HEX SEED-SHA256-HEX-U allot
create SEED-FSHA-CTX SHA256-FILE-CTX-BYTES allot
create SEED-SMOKE-OUT SEED-SMOKE-OUT-CAP allot
create SEED-SMOKE-ERR SEED-SMOKE-ERR-CAP allot
create SEED-BUILD-OUT SEED-BUILD-OUT-CAP allot
create SEED-BUILD-ERR SEED-BUILD-ERR-CAP allot
create SEED-LF 10 c,
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

: SEED-EXPECT-FILE ( str -- ) {: s:str :}
   s STR:LEN 0 <= if E-BUILD-PATH throw then
   s FS:FILE? 0= if E-BUILD-PATH throw then ;

: SEED-HEX-DIGIT? ( n -- bool ) {: c :}
   c 48 >= c 57 <= and if SEED-TRUE exit then
   c 97 >= c 102 <= and if SEED-TRUE exit then
   c 65 >= c 70 <= and if SEED-TRUE exit then
   SEED-FALSE ;

: SEED-HEX64? ( str -- bool ) {: s:str :}
   s STR:LEN SEED-SHA256-HEX-U <> if SEED-FALSE exit then
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ SEED-HEX-DIGIT? 0= if drop SEED-FALSE exit then
      1+
   repeat drop
   SEED-TRUE ;

: SEED-LOWER-HEX ( n -- n ) {: c :}
   c 65 >= c 70 <= and if c 32 + exit then
   c ;

: SEED-HEX= ( str str -- bool ) {: a:str b:str :}
   a STR:LEN b STR:LEN <> if SEED-FALSE exit then
   0 begin dup a STR:LEN < while
      dup a STR:DATA + c@ SEED-LOWER-HEX
      over b STR:DATA + c@ SEED-LOWER-HEX <> if drop SEED-FALSE exit then
      1+
   repeat drop
   SEED-TRUE ;

: SEED-SHA-RC0 ( n -- )
   dup 0= if drop exit then
   SHA-E-OPEN = if E-BUILD-PATH throw then
   E-FS-IO throw ;

: SEED-VERIFY-SHA256 ( str str -- ) {: path:str expect:str :}
   expect SEED-HEX64? 0= if E-BUILD-SOURCE throw then
   SEED-FSHA-CTX path SEED-SHA-HEX SHA256-FILE-HEX-IN SEED-SHA-RC0
   SEED-SHA-HEX SEED-SHA256-HEX-U >str expect SEED-HEX= 0= if
      E-BUILD-SOURCE throw
   then ;

: SEED-VERIFY-OPTIONAL-SHA256 ( str -- ) {: path:str :}
   s" HABU_SEED_SHA256" GETENV {: expect:str :}
   expect STR:LEN 0= if exit then
   path expect SEED-VERIFY-SHA256 ;

: SEED-LAST-SLASH ( str -- n ) {: s:str :}
   -1 SEED-LAST !
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ SEED-SLASH = if dup SEED-LAST ! then
      1+
   repeat drop
   SEED-LAST @ ;

: SEED-MAKE-PARENT-DIRS ( str -- ) {: s:str :}
   s SEED-LAST-SLASH {: idx :}
   idx 0 > if s STR:DATA idx >str FS:MAKE-DIRS then ;

: SEED-FILL ( str str -- str )
   {: src:str tmp:str :}
   src tmp FS:COPY-FILE-STREAM
   tmp FS:CHMOD-X
   src ;

: SEED-INSTALL ( str str -- ) {: src:str dst:str :}
   src SEED-EXPECT-FILE
   dst STR:LEN 0 <= if E-BUILD-PATH throw then
   dst SEED-MAKE-PARENT-DIRS
   src dst [: SEED-FILL ;] FS:REPLACE-STAGED drop
   dst CODESIGN:VERIFY ;

: SEED-SMOKE-OUT-OK? ( str -- bool ) {: s:str :}
   s STR:LEN 4 <> if SEED-FALSE exit then
   s STR:DATA c@ 52 <> if SEED-FALSE exit then
   s STR:DATA 1 + c@ 50 <> if SEED-FALSE exit then
   s STR:DATA 2 + c@ 10 <> if SEED-FALSE exit then
   s STR:DATA 3 + c@ 10 <> if SEED-FALSE exit then
   SEED-TRUE ;

: SEED-EXPECT-NO-STDERR ( n -- )
   0 <> if E-BUILD-STATUS throw then ;

: SEED-OUT ( str -- )
   dup STR:LEN 0 <= if drop exit then
   1 swap write drop ;

: SEED-ERR ( str -- )
   dup STR:LEN 0 <= if drop exit then
   2 swap write drop ;

: SEED-REPLAY-BUILD-OUTPUT ( n n -- ) {: outu erru :}
   SEED-BUILD-OUT outu >str SEED-OUT
   SEED-BUILD-ERR erru >str SEED-ERR ;

: SEED-SMOKE-CHECK ( len len rc -- ) {: outu:len erru:len rc:rc :}
   rc RC>N SEED-RC0
   erru LEN>N SEED-EXPECT-NO-STDERR
   SEED-SMOKE-OUT outu LEN>N >str
   SEED-SMOKE-OUT-OK? 0= if E-BUILD-STATUS throw then ;

: SEED-SMOKE ( str -- ) {: hb:str :}
   PROC-ARGV-RESET
   hb SEED-SMOKE-IN SEED-SMOKE-IN-U >str
   SEED-SMOKE-OUT SEED-SMOKE-OUT-CAP >LEN
   SEED-SMOKE-ERR SEED-SMOKE-ERR-CAP >LEN
   SEED-SMOKE-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC SEED-SMOKE-CHECK ENDOF
     err OF PCAP-FAILED:UNMAKE  SEED-SMOKE-CHECK ENDOF
   ;MATCH ;

: SEED-ARGV+ ( str -- )
   PROC-ARGV+ ;

: SEED-PREPARE-BUILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load"  PROC-ARGV+
   s" tools/hb-build.f"  PROC-ARGV+
   s" --"  PROC-ARGV+
   s" --engine"  PROC-ARGV+
   s" -o"  PROC-ARGV+
   s" bin/hb"  PROC-ARGV+ ;

: SEED-RUN-BUILD ( str -- ) {: hb:str :}
   SEED-PREPARE-BUILD-ARGV
   hb
   SEED-BUILD-OUT SEED-BUILD-OUT-CAP >LEN
   SEED-BUILD-ERR SEED-BUILD-ERR-CAP >LEN
   SEED-BUILD-TIMEOUT-MS >MS RUN-ARGV-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop ENDOF
     err OF PCAP-FAILED:UNMAKE  drop {: outu:len erru:len :}
             outu LEN>N erru LEN>N SEED-REPLAY-BUILD-OUTPUT
             s" seed: hb-build failed" SEED-ERR
             SEED-LF 1 >str SEED-ERR
             E-BUILD-STATUS throw ENDOF
   ;MATCH ;

: SEED-INSTALL-SMOKE-BUILD ( str str -- ) {: src:str dst:str :}
   src SEED-VERIFY-OPTIONAL-SHA256
   src dst SEED-INSTALL
   dst SEED-SMOKE
   dst SEED-RUN-BUILD ;

: SEED-USAGE ( -- )
   s" usage: hb --load tools/seed-main.f -- /path/to/hb-seed" EXIT-STATUS:USAGE die ;

: SEED-MAIN ( -- )
   SCRIPT-ARGC 1 <> if SEED-USAGE then
   0 SCRIPT-ARGV$ dup FS:FILE? 0= if s" seed: not a file" EXIT-STATUS:NOINPUT die then
   s" bin/hb" SEED-INSTALL-SMOKE-BUILD
   s" seed OK: trusted seed rebuilt current bin/hb" type cr ;
