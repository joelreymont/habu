\ warm-image-test.f - fixture for tools/warm-image-lib.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\ lib/source.f lib/codesign.f lib/memory.f tools/warm-image-lib.f tools/warm-image-test.f

65536 constant WIT-CAP
120000 constant WIT-TIMEOUT-MS

variable WIT-ROOT-U
variable WIT-WARM-U
variable WIT-SUP-U
variable WIT-GOOD-U
variable WIT-BAD-U
variable WIT-TRUST-U

create WIT-ROOT-BUF FS-PATH-CAP allot
create WIT-WARM-BUF FS-PATH-CAP allot
create WIT-SUP-BUF FS-PATH-CAP allot
create WIT-GOOD-BUF FS-PATH-CAP allot
create WIT-BAD-BUF FS-PATH-CAP allot
create WIT-TRUST-BUF FS-PATH-CAP allot
create WIT-OUT WIT-CAP allot
create WIT-ERR WIT-CAP allot
create WIT-RUN-OUT WIT-CAP allot
create WIT-RUN-ERR WIT-CAP allot

: WIT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: WIT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: WIT-ROOT ( -- ptr u8 n )
   WIT-ROOT-BUF WIT-ROOT-U @ ;

: WIT-WARM ( -- ptr u8 n )
   WIT-WARM-BUF WIT-WARM-U @ ;

: WIT-SUP ( -- ptr u8 n )
   WIT-SUP-BUF WIT-SUP-U @ ;

: WIT-GOOD ( -- ptr u8 n )
   WIT-GOOD-BUF WIT-GOOD-U @ ;

: WIT-BAD ( -- ptr u8 n )
   WIT-BAD-BUF WIT-BAD-U @ ;

: WIT-TRUST ( -- ptr u8 n )
   WIT-TRUST-BUF WIT-TRUST-U @ ;

: WIT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: WIT-SUP$ ( -- ptr u8 n )
   SB-RESET
   s" : WIT-INC ( i64 -- i64 ) 1 + ;" SB-APPEND
   STR-LF SB-APPEND-C
   s" : WIT-FOLD ( i64 -- i64 ) 7 + ;" SB-APPEND
   STR-LF SB-APPEND-C
   SB$ ;

: WIT-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" : WIT-RUN ( i64 -- i64 ) WIT-INC ;" SB-APPEND
   STR-LF SB-APPEND-C
   s" 41 WIT-RUN . CR" SB-APPEND
   STR-LF SB-APPEND-C
   s" 35 WIT-FOLD . CR" SB-APPEND
   STR-LF SB-APPEND-C
   SB$ ;

: WIT-BAD$ ( -- ptr u8 n )
   s" : WIT-BAD ( i64 -- i64 ) dup ;" ;

: WIT-42$ ( -- ptr u8 n )
   SB-RESET
   s" 42" SB-APPEND
   STR-LF SB-APPEND-C
   STR-LF SB-APPEND-C
   s" 42" SB-APPEND
   STR-LF SB-APPEND-C
   STR-LF SB-APPEND-C
   SB$ ;

: WIT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-warm-image-test" TMPDIR-MKDIR {: a:ptr u :}
   a u WIT-ROOT-BUF WIT-ROOT-U WIT-COPY!
   WIT-ROOT CLEANUP-TREE+
   WIT-ROOT s" warm-hb" WIT-WARM-BUF WIT-WARM-U WIT-PATH!
   WIT-ROOT s" support.f" WIT-SUP-BUF WIT-SUP-U WIT-PATH!
   WIT-ROOT s" good.f" WIT-GOOD-BUF WIT-GOOD-U WIT-PATH!
   WIT-ROOT s" bad.f" WIT-BAD-BUF WIT-BAD-U WIT-PATH!
   WIT-WARM s" .trust.f" WIT-TRUST-BUF WIT-TRUST-U WI-SUFFIX!
   WIT-SUP WIT-SUP$ WRITE-ALL
   WIT-GOOD WIT-GOOD$ WRITE-ALL
   WIT-BAD WIT-BAD$ WRITE-ALL ;

: WIT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: WIT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: WIT-ARGV-WARM ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" --load" WIT-ARG+
   s" lib/errors.f" WIT-ARG+
   s" lib/string.f" WIT-ARG+
   s" lib/memory.f" WIT-ARG+
   s" lib/fs.f" WIT-ARG+
   s" lib/fs-mutate.f" WIT-ARG+
   s" lib/process.f" WIT-ARG+
   s" lib/process-argv.f" WIT-ARG+
   s" lib/process-env.f" WIT-ARG+
   s" lib/source.f" WIT-ARG+
   s" lib/codesign.f" WIT-ARG+
   s" tools/warm-image-lib.f" WIT-ARG+
   s" tools/warm-image.f" WIT-ARG+
   s" --" WIT-ARG+
   WIT-WARM WIT-ARG+
   WIT-SUP WIT-ARG+ ;

: WIT-RUN-WARM-TOOL ( -- n n n )
   WIT-ARGV-WARM
   s" bin/hb" >LEN WIT-OUT WIT-CAP >LEN WIT-ERR WIT-CAP >LEN
   WIT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   WIT-CAPTURE>N ;

: WIT-RUN-WARM-GOOD ( -- n n n )
   PROC-ARGV-RESET
   s" --load" WIT-ARG+
   WIT-TRUST WIT-ARG+
   WIT-GOOD WIT-ARG+
   WIT-WARM >LEN WIT-RUN-OUT WIT-CAP >LEN WIT-RUN-ERR WIT-CAP >LEN
   WIT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   WIT-CAPTURE>N ;

: WIT-RUN-WARM-GOOD-NOTRUST ( -- n n n )
   PROC-ARGV-RESET
   s" --load" WIT-ARG+
   WIT-GOOD WIT-ARG+
   WIT-WARM >LEN WIT-RUN-OUT WIT-CAP >LEN WIT-RUN-ERR WIT-CAP >LEN
   WIT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   WIT-CAPTURE>N ;

: WIT-RUN-WARM-BAD ( -- n n n )
   PROC-ARGV-RESET
   s" --load" WIT-ARG+
   WIT-TRUST WIT-ARG+
   WIT-BAD WIT-ARG+
   WIT-WARM >LEN WIT-RUN-OUT WIT-CAP >LEN WIT-RUN-ERR WIT-CAP >LEN
   WIT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   WIT-CAPTURE>N ;

: WIT-TEST-BAKE ( -- )
   WIT-RUN-WARM-TOOL {: outu erru rc :}
   rc 0 <> if s" warm tool rc: " type rc . cr WIT-OUT outu type WIT-ERR erru type then
   rc 0 T=
   WIT-ERR erru WIT-EMPTY$ T$=
   WIT-OUT outu s" warm-image OK: " CONTAINS? TTRUE
   WIT-OUT outu s" warm-image trust: " CONTAINS? TTRUE
   WIT-WARM EXECUTABLE? TTRUE
   WIT-TRUST FILE? TTRUE
   WIT-TRUST WI-SRC-BUF WI-SRC-CAP READ-ALL {: u :}
   WI-SRC-BUF u s" WIT-INC" CONTAINS? TTRUE ;

: WIT-TEST-RUN ( -- )
   WIT-RUN-WARM-GOOD {: outu erru rc :}
   rc 0 <> if s" warm run rc: " type rc . cr WIT-RUN-OUT outu type WIT-RUN-ERR erru type then
   rc 0 T=
   WIT-RUN-ERR erru WIT-EMPTY$ T$=
   WIT-RUN-OUT outu WIT-42$ T$= ;

: WIT-TEST-NOTRUST-RUNS ( -- )
   WIT-RUN-WARM-GOOD-NOTRUST {: outu erru rc :}
   rc 0 T=
   WIT-RUN-ERR erru WIT-EMPTY$ T$=
   WIT-RUN-OUT outu WIT-42$ T$= ;

: WIT-TEST-CHECK-FAILS ( -- )
   WIT-RUN-WARM-BAD {: outu erru rc :}
   outu drop erru drop
   rc 0 T<> ;

: WIT-LIB-HAS? ( ptr u8 n -- bool )
   s" tools/warm-image-lib.f" WI-SRC-BUF WI-SRC-CAP READ-ALL {: u :}
   WI-SRC-BUF u 2swap CONTAINS? ;

: WIT-LIB-MUST-HAVE ( ptr u8 n -- )
   WIT-LIB-HAS? TTRUE ;

: WIT-LIB-MUST-NOT-HAVE ( ptr u8 n -- )
   WIT-LIB-HAS? TFALSE ;

: WIT-TEST-SNAPSHOT-TAIL ( -- )
   s" HIDE-DEFS-FROM" WIT-LIB-MUST-HAVE
   s" SNAP-OUT" WIT-LIB-MUST-HAVE
   s" src/habu/snap.f" WIT-LIB-MUST-HAVE
   s" src/arch/arm64/asm.f" WIT-LIB-MUST-NOT-HAVE
   s" src/arch/arm64/icode.f" WIT-LIB-MUST-NOT-HAVE
   s" src/core/roles.f" WIT-LIB-MUST-NOT-HAVE
   s" src/os/image-bytes.f" WIT-LIB-MUST-NOT-HAVE ;

: WIT-MAIN ( -- )
   T-RESET
   WIT-PREPARE
   WIT-TEST-SNAPSHOT-TAIL
   WIT-TEST-BAKE
   WIT-TEST-RUN
   WIT-TEST-NOTRUST-RUNS
   WIT-TEST-CHECK-FAILS
   CLEANUP-RUN
   WIT-ROOT EXISTS? TFALSE
   T-REPORT
   s" warm-image-test: ok" type cr ;

WIT-MAIN
