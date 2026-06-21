\ hb-build-test.f - checked fixture for tools/hb-build-lib.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/build.f tools/build-fixpoint.f tools/hb-build-lib.f tools/hb-build-test.f

65536 constant HBT-CAPTURE-CAP
120000 constant HBT-TIMEOUT-MS

variable HBT-ROOT-U
variable HBT-TMP-U
variable HBT-NEW-TMP-U
variable HBT-OK-SRC-U
variable HBT-OK-OUT-U
variable HBT-BAD-SRC-U
variable HBT-BAD-OUT-U

create HBT-ROOT-BUF FS-PATH-CAP allot
create HBT-TMP-BUF FS-PATH-CAP allot
create HBT-NEW-TMP-BUF FS-PATH-CAP allot
create HBT-OK-SRC-BUF FS-PATH-CAP allot
create HBT-OK-OUT-BUF FS-PATH-CAP allot
create HBT-BAD-SRC-BUF FS-PATH-CAP allot
create HBT-BAD-OUT-BUF FS-PATH-CAP allot
create HBT-OUT HBT-CAPTURE-CAP allot
create HBT-ERR HBT-CAPTURE-CAP allot
create HBT-RUN-OUT HBT-CAPTURE-CAP allot
create HBT-RUN-ERR HBT-CAPTURE-CAP allot

: HBT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: HBT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: HBT-ROOT ( -- ptr u8 n )
   HBT-ROOT-BUF HBT-ROOT-U @ ;

: HBT-TMP ( -- ptr u8 n )
   HBT-TMP-BUF HBT-TMP-U @ ;

: HBT-NEW-TMP ( -- ptr u8 n )
   HBT-NEW-TMP-BUF HBT-NEW-TMP-U @ ;

: HBT-OK-SRC ( -- ptr u8 n )
   HBT-OK-SRC-BUF HBT-OK-SRC-U @ ;

: HBT-OK-OUT ( -- ptr u8 n )
   HBT-OK-OUT-BUF HBT-OK-OUT-U @ ;

: HBT-BAD-SRC ( -- ptr u8 n )
   HBT-BAD-SRC-BUF HBT-BAD-SRC-U @ ;

: HBT-BAD-OUT ( -- ptr u8 n )
   HBT-BAD-OUT-BUF HBT-BAD-OUT-U @ ;

: HBT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: HBT-42$ ( -- ptr u8 n )
   SB-RESET
   s" 42" SB-APPEND
   HBB-LF SB-APPEND-C
   SB$ ;

: HBT-OK-SRC$ ( -- ptr u8 n )
   s" : MAIN ( -- ) 6 7 * . ;" ;

: HBT-BAD-SRC$ ( -- ptr u8 n )
   s" : MAIN ( -- ) here drop ;" ;

: HBT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-hb-build" TMPDIR-MKDIR {: a:ptr u :}
   a u HBT-ROOT-BUF HBT-ROOT-U HBT-COPY!
   HBT-ROOT CLEANUP-TREE+
   HBT-ROOT s" hbtmp" HBT-TMP-BUF HBT-TMP-U HBT-PATH!
   HBT-TMP MAKE-DIR
   HBT-ROOT s" hbtmp-new" HBT-NEW-TMP-BUF HBT-NEW-TMP-U HBT-PATH!
   HBT-ROOT s" ok.f" HBT-OK-SRC-BUF HBT-OK-SRC-U HBT-PATH!
   HBT-ROOT s" ok" HBT-OK-OUT-BUF HBT-OK-OUT-U HBT-PATH!
   HBT-ROOT s" bad.f" HBT-BAD-SRC-BUF HBT-BAD-SRC-U HBT-PATH!
   HBT-ROOT s" bad" HBT-BAD-OUT-BUF HBT-BAD-OUT-U HBT-PATH!
   HBT-OK-SRC HBT-OK-SRC$ WRITE-ALL
   HBT-BAD-SRC HBT-BAD-SRC$ WRITE-ALL ;

: HBT-ARGV-BASE-TMP ( ptr u8 n -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" 2swap PROC-ENV+
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
   s" tools/hb-build-lib.f" PROC-ARGV+
   s" tools/hb-build.f" PROC-ARGV+
   s" --" PROC-ARGV+ ;

: HBT-ARGV-BASE ( -- )
   HBT-TMP HBT-ARGV-BASE-TMP ;

: HBT-RUN-HB-BUILD ( -- n n n )
   s" bin/hb" HBT-OUT HBT-CAPTURE-CAP HBT-ERR HBT-CAPTURE-CAP
   HBT-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE ;

: HBT-ADD-OK ( -- )
   HBT-OK-SRC PROC-ARGV+
   s" -o" PROC-ARGV+
   HBT-OK-OUT PROC-ARGV+ ;

: HBT-ADD-BAD ( -- )
   s" --json-errors" PROC-ARGV+
   HBT-BAD-SRC PROC-ARGV+
   s" -o" PROC-ARGV+
   HBT-BAD-OUT PROC-ARGV+ ;

: HBT-BUILD-OK ( -- )
   HBT-ARGV-BASE
   HBT-ADD-OK
   HBT-RUN-HB-BUILD 0 T=
   {: outu erru :}
   HBT-ERR erru HBT-EMPTY$ T$=
   HBT-OUT outu s" hb-build OK: " CONTAINS? TTRUE
   HBT-OK-OUT FILE? TTRUE ;

: HBT-RUN-OK ( -- )
   HBT-OK-OUT HBT-RUN-OUT HBT-CAPTURE-CAP HBT-RUN-ERR HBT-CAPTURE-CAP
   HBT-TIMEOUT-MS RUN-CAPTURE 0 T=
   {: outu erru :}
   HBT-RUN-ERR erru HBT-EMPTY$ T$=
   HBT-RUN-OUT outu HBT-42$ T$= ;

: HBT-BUILD-MISSING-TMP ( -- )
   HBT-NEW-TMP EXISTS? TFALSE
   HBT-NEW-TMP HBT-ARGV-BASE-TMP
   HBT-ADD-OK
   HBT-RUN-HB-BUILD 0 T=
   {: outu erru :}
   HBT-ERR erru HBT-EMPTY$ T$=
   HBT-OUT outu s" hb-build OK: " CONTAINS? TTRUE
   HBT-NEW-TMP DIR? TTRUE
   HBT-OK-OUT FILE? TTRUE ;

: HBT-Q ( -- )
   HBB-DQ SB-APPEND-C ;

: HBT-QRAW ( ptr u8 n -- )
   HBT-Q
   SB-APPEND
   HBT-Q ;

: HBT-JSON-SCHEMA$ ( -- ptr u8 n )
   SB-RESET
   s" schema_version" HBT-QRAW
   s" :1" SB-APPEND
   SB$ ;

: HBT-JSON-KV$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: key:ptr keyu val:ptr valu :}
   SB-RESET
   key keyu HBT-QRAW
   s" :" SB-APPEND
   val valu HBT-QRAW
   SB$ ;

: HBT-JSON-CODE$ ( -- ptr u8 n )
   s" code" s" E-AOT-UNSUPPORTED" HBT-JSON-KV$ ;

: HBT-JSON-WORD$ ( -- ptr u8 n )
   s" word" s" MAIN" HBT-JSON-KV$ ;

: HBT-JSON-TOKEN$ ( -- ptr u8 n )
   s" token" s" here" HBT-JSON-KV$ ;

: HBT-REJECT-BAD ( -- )
   HBT-ARGV-BASE
   HBT-ADD-BAD
   HBT-RUN-HB-BUILD 0 T<>
   {: outu erru :}
   HBT-OUT outu HBT-EMPTY$ T$=
   HBT-ERR erru HBT-JSON-SCHEMA$ CONTAINS? TTRUE
   HBT-ERR erru HBT-JSON-CODE$ CONTAINS? TTRUE
   HBT-ERR erru HBT-JSON-WORD$ CONTAINS? TTRUE
   HBT-ERR erru HBT-JSON-TOKEN$ CONTAINS? TTRUE
   HBT-BAD-OUT EXISTS? TFALSE ;

: HBT-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-BUILD-OK
   HBT-RUN-OK
   HBT-BUILD-MISSING-TMP
   HBT-REJECT-BAD
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-test: ok" type cr ;

HBT-MAIN
