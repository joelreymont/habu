\ seed-test.f - focused tests for checked seed recovery helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\ src/core/sha256.f lib/codesign.f tools/seed.f tools/seed-test.f

create SET-ROOT FS-PATH-CAP allot
create SET-BIN FS-PATH-CAP allot
create SET-HB FS-PATH-CAP allot
create SET-SEED FS-PATH-CAP allot
create SET-TEXT FS-PATH-CAP allot
create SET-BAD FS-PATH-CAP allot
create SET-SCRIPT FS-PATH-CAP allot
create SET-FAIL-SCRIPT FS-PATH-CAP allot
create SET-HEX SEED-SHA256-HEX-U allot
8192 constant SET-CAP
create SET-OUT SET-CAP allot
create SET-ERR SET-CAP allot

variable SET-ROOT-U
variable SET-BIN-U
variable SET-HB-U
variable SET-SEED-U
variable SET-TEXT-U
variable SET-BAD-U
variable SET-SCRIPT-U
variable SET-FAIL-SCRIPT-U

: SET-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: SET-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: SET-ROOT$ ( -- ptr u8 n )
   SET-ROOT SET-ROOT-U @ ;

: SET-BIN$ ( -- ptr u8 n )
   SET-BIN SET-BIN-U @ ;

: SET-HB$ ( -- ptr u8 n )
   SET-HB SET-HB-U @ ;

: SET-SEED$ ( -- ptr u8 n )
   SET-SEED SET-SEED-U @ ;

: SET-TEXT$ ( -- ptr u8 n )
   SET-TEXT SET-TEXT-U @ ;

: SET-BAD$ ( -- ptr u8 n )
   SET-BAD SET-BAD-U @ ;

: SET-SCRIPT$ ( -- ptr u8 n )
   SET-SCRIPT SET-SCRIPT-U @ ;

: SET-FAIL-SCRIPT$ ( -- ptr u8 n )
   SET-FAIL-SCRIPT SET-FAIL-SCRIPT-U @ ;

: SET-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-seed-test" TMPDIR-MKDIR SET-ROOT SET-ROOT-U SET-COPY!
   SET-ROOT$ CLEANUP-TREE+
   SET-ROOT$ s" bin" SET-BIN SET-BIN-U SET-PATH!
   SET-ROOT$ s" hb-seed" SET-SEED SET-SEED-U SET-PATH!
   SET-BIN$ s" hb" SET-HB SET-HB-U SET-PATH!
   SET-ROOT$ s" text.txt" SET-TEXT SET-TEXT-U SET-PATH!
   SET-ROOT$ s" bad-hash.txt" SET-BAD SET-BAD-U SET-PATH!
   SET-ROOT$ s" unsigned-script" SET-SCRIPT SET-SCRIPT-U SET-PATH!
   SET-ROOT$ s" seed-build-fail.f" SET-FAIL-SCRIPT SET-FAIL-SCRIPT-U SET-PATH! ;

: SET-X? ( ptr u8 n -- bool )
   STAT-MODE FS-MUT-MODE-EXEC and FS-MUT-MODE-EXEC = ;

: SET-COPY-BIN-HB ( ptr u8 n -- ) {: dst:ptr dstu :}
   s" bin/hb" dst dstu COPY-FILE-STREAM ;

: SET-TEST-HEX-SHAPE ( -- )
   s" e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" SEED-HEX64? TTRUE
   s" E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855" SEED-HEX64? TTRUE
   s" e3b0" SEED-HEX64? TFALSE
   s" z3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" SEED-HEX64? TFALSE ;

: SET-TEST-SHA ( -- )
   SET-TEXT$ s" abc" WRITE-ALL
   SET-TEXT$ s" ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" SEED-VERIFY-SHA256
   SET-TEXT$ s" BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD" SEED-VERIFY-SHA256 ;

: SET-BAD-SHA-SHAPE ( -- )
   SET-TEXT$ s" not-hex" SEED-VERIFY-SHA256 ;

: SET-BAD-SHA-VALUE ( -- )
   SET-TEXT$ s" e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" SEED-VERIFY-SHA256 ;

: SET-TEST-CODESIGN-ENSURE ( -- )
   SET-SCRIPT$ s" #!/bin/sh\nexit 0\n" WRITE-ALL
   SET-SCRIPT$ CHMOD-X
   SET-SCRIPT$ CODESIGN-VERIFY-RC 0 T<>
   SET-SCRIPT$ CODESIGN-ENSURE
   SET-SCRIPT$ CODESIGN-VERIFY ;

: SET-TEST-INSTALL-SMOKE ( -- )
   SET-SEED$ SET-COPY-BIN-HB
   SET-SEED$ SET-HEX SHA256-FILE-HEX 0 T=
   SET-SEED$ SET-HEX SEED-SHA256-HEX-U SEED-VERIFY-SHA256
   SET-SEED$ SET-HB$ SEED-INSTALL
   SET-HB$ FILE? TTRUE
   SET-HB$ SET-X? TTRUE
   SET-HB$ CODESIGN-VERIFY
   SET-HB$ SEED-SMOKE ;

: SET-TEST-BUILD-INVOKE-SAFE ( -- )
   s" /usr/bin/true" SEED-RUN-BUILD-FIXPOINT ;

: SET-FAIL-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   115 SB-APPEND-C
   34 SB-APPEND-C
   32 SB-APPEND-C
   s" /bin/cat" SB-APPEND
   34 SB-APPEND-C
   32 SB-APPEND-C
   s" SEED-RUN-BUILD-FIXPOINT" SB-APPEND
   10 SB-APPEND-C
   SB$ ;

: SET-FAIL-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" src/core/sha256.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/seed.f"  >LEN PROC-ARGV+
   SET-FAIL-SCRIPT$  >LEN PROC-ARGV+ ;

: SET-TEST-BUILD-FAIL-REPLAYS-ERR ( -- )
   SET-FAIL-SCRIPT$ SET-FAIL-SOURCE$ WRITE-ALL
   SET-FAIL-ARGV
   s" bin/hb" >LEN SET-OUT SET-CAP >LEN SET-ERR SET-CAP >LEN
   10000 >MS RUN-ARGV-CAPTURE
   {: outu erru rc :}
   rc RC>N 0 T<>
   SET-ERR erru LEN>N s" illegal option" CONTAINS? TTRUE
   SET-ERR erru LEN>N s" seed: build-fixpoint failed" CONTAINS? TTRUE ;

: SET-INSTALL-MISSING ( -- )
   SET-BAD$ SET-HB$ SEED-INSTALL ;

: SEED-TEST-MAIN ( -- )
   T-RESET
   SET-PREPARE
   SET-TEST-HEX-SHAPE
   SET-TEST-SHA
   [: SET-BAD-SHA-SHAPE ;] E-BUILD-SOURCE TTHROWSQ
   [: SET-BAD-SHA-VALUE ;] E-BUILD-SOURCE TTHROWSQ
   SET-TEST-CODESIGN-ENSURE
   SET-TEST-INSTALL-SMOKE
   SET-TEST-BUILD-INVOKE-SAFE
   SET-TEST-BUILD-FAIL-REPLAYS-ERR
   [: SET-INSTALL-MISSING ;] E-BUILD-PATH TTHROWSQ
   CLEANUP-RUN
   SET-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" seed-test: ok" type cr ;

SEED-TEST-MAIN
