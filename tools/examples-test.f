\ examples-test.f - checked fixture for examples/*.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\ tools/examples-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/test/outcome.f
require lib/process-argv.f
require lib/process-env.f

65536 constant EXT-COPY-CAP
8192 constant EXT-CAPTURE-CAP
10000 constant EXT-TIMEOUT-MS

variable EXT-TEMP-U
variable EXT-FILES-U
variable EXT-TEMP-A
variable EXT-FILES-A
variable EXT-PATH-A
variable EXT-COPY-A
variable EXT-OUT-A
variable EXT-ERR-A

: EXT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: EXT-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: EXT-PTR-U8@ ( ptr a -- ptr u8 )
   EXT-PTR-U8-FIELD @ ;

: EXT-PTR-U8! ( ptr u8 ptr a -- )
   EXT-PTR-U8-FIELD ! ;

: EXT-ALLOC-BUF ( n ptr a -- ptr u8 ) {: cap slot:ptr :}
   slot EXT-PTR-U8@ 0= if
      cap MEM-ALLOC-BYTES drop slot EXT-PTR-U8!
   then
   slot EXT-PTR-U8@ ;

: EXT-TEMP-BUF ( -- ptr u8 )
   FS-PATH-CAP EXT-TEMP-A EXT-ALLOC-BUF ;

: EXT-FILES-BUF ( -- ptr u8 )
   FS-PATH-CAP EXT-FILES-A EXT-ALLOC-BUF ;

: EXT-PATH-BUF ( -- ptr u8 )
   FS-PATH-CAP EXT-PATH-A EXT-ALLOC-BUF ;

: EXT-COPY-BUF ( -- ptr u8 )
   EXT-COPY-CAP EXT-COPY-A EXT-ALLOC-BUF ;

: EXT-OUT ( -- ptr u8 )
   EXT-CAPTURE-CAP EXT-OUT-A EXT-ALLOC-BUF ;

: EXT-ERR ( -- ptr u8 )
   EXT-CAPTURE-CAP EXT-ERR-A EXT-ALLOC-BUF ;

: EXT-TEMP ( -- ptr u8 n )
   EXT-TEMP-BUF EXT-TEMP-U @ ;

: EXT-FILES ( -- ptr u8 n )
   EXT-FILES-BUF EXT-FILES-U @ ;

: EXT-JOIN$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: pa:ptr pu na:ptr nu :}
   pa pu na nu EXT-PATH-BUF JOIN-PATH {: u :}
   EXT-PATH-BUF u ;

: EXT-PURE ( -- ptr u8 n )
   EXT-TEMP s" pure.f" EXT-JOIN$ ;

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

: EXT-OK3$ ( -- ptr u8 n )
   SB-RESET
   s" test: ok" SB-APPEND STR-LF SB-APPEND-C
   s" test: ok" SB-APPEND STR-LF SB-APPEND-C
   s" test: ok" SB-APPEND STR-LF SB-APPEND-C
   SB$ ;

: EXT-CLEAR-BUNDLE ( ptr u8 n -- )
   EXT-EMPTY$ WRITE-ALL ;

: EXT-REQUIRE-FILE ( ptr u8 n -- )
   FILE? TTRUE ;

: EXT-ADD-SOURCE ( ptr u8 n ptr u8 n -- ) {: bundle:ptr bundleu src:ptr srcu :}
   src srcu EXT-REQUIRE-FILE
   src srcu EXT-COPY-BUF EXT-COPY-CAP READ-ALL {: u :}
   bundle bundleu EXT-COPY-BUF u APPEND-FILE ;

: EXT-PROVIDED$ ( ptr u8 n -- ptr u8 n ) {: src:ptr srcu:n :}
   SB-RESET
   s" s" SB-APPEND 34 SB-APPEND-C 32 SB-APPEND-C
   src srcu SB-APPEND
   34 SB-APPEND-C 32 SB-APPEND-C
   s" provided" SB-APPEND
   STR-LF SB-APPEND-C
   SB$ ;

: EXT-ADD-PROVIDED ( ptr u8 n ptr u8 n -- )
   {: bundle:ptr bundleu:n src:ptr srcu:n :}
   bundle bundleu src srcu EXT-PROVIDED$ APPEND-FILE ;

: EXT-ADD-SOURCE-LF ( ptr u8 n ptr u8 n -- ) {: bundle:ptr bundleu src:ptr srcu :}
   bundle bundleu src srcu EXT-ADD-PROVIDED
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

: EXT-BUNDLE-PURE ( -- ptr u8 n )
   EXT-PURE {: b:ptr u:n :}
   b u EXT-CLEAR-BUNDLE
   b u s" lib/errors.f" EXT-ADD-SOURCE-LF
   b u s" lib/string.f" EXT-ADD-SOURCE-LF
   b u s" lib/test.f" EXT-ADD-SOURCE-LF
   b u s" lib/array.f" EXT-ADD-SOURCE-LF
   b u s" lib/regex.f" EXT-ADD-SOURCE-LF
   b u s" lib/property.f" EXT-ADD-SOURCE-LF
   b u s" examples/array.f" EXT-ADD-SOURCE-LF
   b u s" examples/string-regex.f" EXT-ADD-SOURCE-LF
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

: EXT-RUN-HB ( -- len len outcome )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" then >LEN
   EXT-OUT EXT-CAPTURE-CAP >LEN
   EXT-ERR EXT-CAPTURE-CAP >LEN EXT-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME ;

: EXT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: EXT-ARGV-BUNDLE ( ptr u8 n -- )
   PROC-ARGV-RESET
   s" --load" EXT-ARG+
   EXT-ARG+
   s" --" EXT-ARG+ ;

: EXT-ASSERT-OK ( len len outcome -- )
   0 T-OUTCOME-EXITED= LEN>N {: erru:n :} LEN>N {: outu:n :}
   EXT-ERR erru EXT-EMPTY$ T$=
   EXT-OUT outu EXT-OK$ T$= ;

: EXT-ASSERT-OK3 ( len len outcome -- )
   0 T-OUTCOME-EXITED= LEN>N {: erru:n :} LEN>N {: outu:n :}
   EXT-ERR erru EXT-EMPTY$ T$=
   EXT-OUT outu EXT-OK3$ T$= ;

: EXT-RUN-PURE ( -- )
   EXT-BUNDLE-PURE EXT-ARGV-BUNDLE
   EXT-RUN-HB EXT-ASSERT-OK3 ;

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

: EXT-TEST-FILE-MAP ( -- )
   EXT-BUNDLE-FILE-MAP EXT-RUN-FILE-MAP ;

: EXT-TEST-BUILD-SCRIPT ( -- )
   EXT-BUNDLE-BUILD-SCRIPT EXT-RUN-BUILD-SCRIPT ;

: EXT-MAIN ( -- )
   T-RESET
   EXT-PREPARE
   EXT-RUN-PURE
   EXT-TEST-FILE-MAP
   EXT-TEST-BUILD-SCRIPT
   CLEANUP-RUN
   EXT-TEMP EXISTS? TFALSE
   T-REPORT
   s" examples-test: ok" type cr ;

EXT-MAIN
