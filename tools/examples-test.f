\ examples-test.f - checked fixture for examples/*.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/examples-test.f

65536 constant EXT-COPY-CAP
8192 constant EXT-CAPTURE-CAP
10000 constant EXT-TIMEOUT-MS

variable EXT-TEMP-U
variable EXT-FILES-U

create EXT-TEMP-BUF FS-PATH-CAP allot
create EXT-FILES-BUF FS-PATH-CAP allot
create EXT-PATH-BUF FS-PATH-CAP allot
create EXT-COPY-BUF EXT-COPY-CAP allot
create EXT-OUT EXT-CAPTURE-CAP allot
create EXT-ERR EXT-CAPTURE-CAP allot

: EXT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: EXT-TEMP ( -- ptr u8 n )
   EXT-TEMP-BUF EXT-TEMP-U @ ;

: EXT-FILES ( -- ptr u8 n )
   EXT-FILES-BUF EXT-FILES-U @ ;

: EXT-JOIN$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: pa:ptr pu na:ptr nu :}
   pa pu na nu EXT-PATH-BUF JOIN-PATH {: u :}
   EXT-PATH-BUF u ;

: EXT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: EXT-LF$ ( -- ptr u8 n )
   SB-RESET
   STR-LF SB-APPEND-C
   SB$ ;

: EXT-OK$ ( -- ptr u8 n )
   SB-RESET
   s" test: ok" SB-APPEND
   STR-LF SB-APPEND-C
   SB$ ;

: EXT-CLEAR-BUNDLE ( ptr u8 n -- )
   EXT-EMPTY$ WRITE-ALL ;

: EXT-REQUIRE-FILE ( ptr u8 n -- )
   FILE? TTRUE ;

: EXT-ADD-SOURCE ( ptr u8 n ptr u8 n -- ) {: bundle:ptr bundleu src:ptr srcu :}
   src srcu EXT-REQUIRE-FILE
   src srcu EXT-COPY-BUF EXT-COPY-CAP READ-ALL {: u :}
   bundle bundleu EXT-COPY-BUF u APPEND-FILE ;

: EXT-ADD-SOURCE-LF ( ptr u8 n ptr u8 n -- ) {: bundle:ptr bundleu src:ptr srcu :}
   bundle bundleu src srcu EXT-ADD-SOURCE
   bundle bundleu EXT-LF$ APPEND-FILE ;

: EXT-MAKE-FILES-DIR ( ptr u8 n -- )
   EXT-FILES 2swap EXT-JOIN$ MAKE-DIR ;

: EXT-WRITE-EMPTY-FILE ( ptr u8 n -- )
   EXT-FILES 2swap EXT-JOIN$ EXT-EMPTY$ WRITE-ALL ;

: EXT-PREPARE-ROOT ( -- )
   CLEANUP-RESET
   s" habu-examples" TMPDIR-MKDIR {: a:ptr u :}
   a u EXT-TEMP-BUF EXT-TEMP-U EXT-COPY!
   EXT-TEMP CLEANUP-TREE+
   EXT-TEMP s" files" EXT-FILES-BUF JOIN-PATH EXT-FILES-U !
   EXT-FILES MAKE-DIR ;

: EXT-PREPARE-DIRS ( -- )
   s" src" EXT-MAKE-FILES-DIR
   s" docs" EXT-MAKE-FILES-DIR
   s" build" EXT-MAKE-FILES-DIR
   s" .git" EXT-MAKE-FILES-DIR
   s" .jj" EXT-MAKE-FILES-DIR
   s" .dots" EXT-MAKE-FILES-DIR ;

: EXT-PREPARE-FILES ( -- )
   s" src/main.f" EXT-WRITE-EMPTY-FILE
   s" src/util.f" EXT-WRITE-EMPTY-FILE
   s" docs/readme.txt" EXT-WRITE-EMPTY-FILE
   s" build/app.bin" EXT-WRITE-EMPTY-FILE
   s" .git/ignored.f" EXT-WRITE-EMPTY-FILE
   s" .jj/ignored.txt" EXT-WRITE-EMPTY-FILE
   s" .dots/ignored.f" EXT-WRITE-EMPTY-FILE ;

: EXT-PREPARE ( -- )
   EXT-PREPARE-ROOT
   EXT-PREPARE-DIRS
   EXT-PREPARE-FILES ;

: EXT-BUNDLE-ARRAY ( -- ptr u8 n )
   EXT-TEMP s" array.f" EXT-JOIN$ {: b:ptr u :}
   b u EXT-CLEAR-BUNDLE
   b u s" lib/errors.f" EXT-ADD-SOURCE-LF
   b u s" lib/test.f" EXT-ADD-SOURCE-LF
   b u s" src/core/combinators.f" EXT-ADD-SOURCE-LF
   b u s" lib/array.f" EXT-ADD-SOURCE-LF
   b u s" examples/array.f" EXT-ADD-SOURCE
   b u ;

: EXT-BUNDLE-STRING-REGEX ( -- ptr u8 n )
   EXT-TEMP s" string-regex.f" EXT-JOIN$ {: b:ptr u :}
   b u EXT-CLEAR-BUNDLE
   b u s" lib/errors.f" EXT-ADD-SOURCE-LF
   b u s" lib/string.f" EXT-ADD-SOURCE-LF
   b u s" lib/test.f" EXT-ADD-SOURCE-LF
   b u s" lib/regex.f" EXT-ADD-SOURCE-LF
   b u s" examples/string-regex.f" EXT-ADD-SOURCE
   b u ;

: EXT-BUNDLE-FILE-MAP ( -- ptr u8 n )
   EXT-TEMP s" file-map.f" EXT-JOIN$ {: b:ptr u :}
   b u EXT-CLEAR-BUNDLE
   b u s" lib/errors.f" EXT-ADD-SOURCE-LF
   b u s" lib/string.f" EXT-ADD-SOURCE-LF
   b u s" lib/test.f" EXT-ADD-SOURCE-LF
   b u s" lib/fs.f" EXT-ADD-SOURCE-LF
   b u s" lib/map.f" EXT-ADD-SOURCE-LF
   b u s" examples/file-map.f" EXT-ADD-SOURCE
   b u ;

: EXT-BUNDLE-PROPERTY ( -- ptr u8 n )
   EXT-TEMP s" property-test.f" EXT-JOIN$ {: b:ptr u :}
   b u EXT-CLEAR-BUNDLE
   b u s" lib/errors.f" EXT-ADD-SOURCE-LF
   b u s" lib/string.f" EXT-ADD-SOURCE-LF
   b u s" lib/test.f" EXT-ADD-SOURCE-LF
   b u s" lib/property.f" EXT-ADD-SOURCE-LF
   b u s" examples/property-test.f" EXT-ADD-SOURCE
   b u ;

: EXT-BUNDLE-BUILD-SCRIPT ( -- ptr u8 n )
   EXT-TEMP s" build-script.f" EXT-JOIN$ {: b:ptr u :}
   b u EXT-CLEAR-BUNDLE
   b u s" lib/errors.f" EXT-ADD-SOURCE-LF
   b u s" lib/string.f" EXT-ADD-SOURCE-LF
   b u s" lib/test.f" EXT-ADD-SOURCE-LF
   b u s" lib/fs.f" EXT-ADD-SOURCE-LF
   b u s" lib/argv.f" EXT-ADD-SOURCE-LF
   b u s" examples/build-script.f" EXT-ADD-SOURCE
   b u ;

: EXT-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: EXT-RUN-HB ( -- n n n n )
   s" bin/hb"  >LEN EXT-OUT EXT-CAPTURE-CAP >LEN
   EXT-ERR EXT-CAPTURE-CAP >LEN EXT-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME EXT-CAPTURE>N ;

: EXT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: EXT-ARGV-BUNDLE ( ptr u8 n -- )
   PROC-ARGV-RESET
   s" --load" EXT-ARG+
   EXT-ARG+
   s" --" EXT-ARG+ ;

: EXT-ASSERT-OK ( n n n n -- )
   {: outu erru kind code :}
   kind PROC-OUTCOME-EXIT T=
   code 0 T=
   EXT-ERR erru EXT-EMPTY$ T$=
   EXT-OUT outu EXT-OK$ T$= ;

: EXT-RUN-PLAIN ( ptr u8 n -- )
   EXT-ARGV-BUNDLE
   EXT-RUN-HB EXT-ASSERT-OK ;

: EXT-RUN-FILE-MAP ( ptr u8 n -- )
   EXT-ARGV-BUNDLE
   EXT-FILES EXT-ARG+
   EXT-RUN-HB EXT-ASSERT-OK ;

: EXT-RUN-BUILD-SCRIPT ( ptr u8 n -- )
   EXT-ARGV-BUNDLE
   s" --json" EXT-ARG+
   s" -o" EXT-ARG+
   EXT-TEMP s" app.hb" EXT-JOIN$ EXT-ARG+
   s" examples/array.f" EXT-ARG+
   EXT-RUN-HB EXT-ASSERT-OK ;

: EXT-TEST-ARRAY ( -- )
   EXT-BUNDLE-ARRAY EXT-RUN-PLAIN ;

: EXT-TEST-STRING-REGEX ( -- )
   EXT-BUNDLE-STRING-REGEX EXT-RUN-PLAIN ;

: EXT-TEST-FILE-MAP ( -- )
   EXT-BUNDLE-FILE-MAP EXT-RUN-FILE-MAP ;

: EXT-TEST-PROPERTY ( -- )
   EXT-BUNDLE-PROPERTY EXT-RUN-PLAIN ;

: EXT-TEST-BUILD-SCRIPT ( -- )
   EXT-BUNDLE-BUILD-SCRIPT EXT-RUN-BUILD-SCRIPT ;

: EXT-MAIN ( -- )
   T-RESET
   EXT-PREPARE
   EXT-TEST-ARRAY
   EXT-TEST-STRING-REGEX
   EXT-TEST-FILE-MAP
   EXT-TEST-PROPERTY
   EXT-TEST-BUILD-SCRIPT
   CLEANUP-RUN
   EXT-TEMP EXISTS? TFALSE
   T-REPORT
   s" examples-test: ok" type cr ;

EXT-MAIN
