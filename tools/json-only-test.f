\ json-only-test.f - checked fixtures for tools/json-only.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/warm-run.f
\ tools/json-only-test.f

1024 constant JOT-BUF-CAP
10000 constant JOT-TIMEOUT-MS

variable JOT-ROOT-U
variable JOT-IN-U

create JOT-ROOT-BUF FS-PATH-CAP allot
create JOT-IN-BUF FS-PATH-CAP allot
create JOT-OUT JOT-BUF-CAP allot
create JOT-ERR JOT-BUF-CAP allot

: JOT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: JOT-ROOT ( -- ptr u8 n )
   JOT-ROOT-BUF JOT-ROOT-U @ ;

: JOT-IN ( -- ptr u8 n )
   JOT-IN-BUF JOT-IN-U @ ;

: JOT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-json-only" TMPDIR-MKDIR {: a:ptr u :}
   a u JOT-ROOT-BUF JOT-ROOT-U JOT-COPY!
   JOT-ROOT CLEANUP-DIR+
   JOT-ROOT s" stderr.in" JOT-IN-BUF JOIN-PATH JOT-IN-U !
   JOT-IN CLEANUP+ ;

: JOT-DQ ( -- )
   34 SB-APPEND-C ;

: JOT-LF ( -- )
   10 SB-APPEND-C ;

: JOT-OBJ-A ( -- )
   123 SB-APPEND-C
   JOT-DQ s" a" SB-APPEND JOT-DQ
   s" :1}" SB-APPEND ;

: JOT-OBJ-B ( -- )
   123 SB-APPEND-C
   JOT-DQ s" b" SB-APPEND JOT-DQ
   s" :2}" SB-APPEND ;

: JOT-MIXED-IN$ ( -- ptr u8 n )
   SB-RESET
   s" prose before json" SB-APPEND JOT-LF
   s"   " SB-APPEND JOT-OBJ-A s"   " SB-APPEND JOT-LF
   s" {bad" SB-APPEND JOT-LF
   s" [1]" SB-APPEND JOT-LF
   JOT-OBJ-B JOT-LF
   SB$ ;

: JOT-MIXED-OUT$ ( -- ptr u8 n )
   SB-RESET
   JOT-OBJ-A JOT-LF
   JOT-OBJ-B JOT-LF
   SB$ ;

: JOT-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" {bad" SB-APPEND JOT-LF
   SB$ ;

: JOT-ARRAY$ ( -- ptr u8 n )
   SB-RESET
   91 SB-APPEND-C
   JOT-OBJ-A
   93 SB-APPEND-C
   JOT-LF
   SB$ ;

: JOT-PROSE$ ( -- ptr u8 n )
   SB-RESET
   s" hello" SB-APPEND JOT-LF
   s" world" SB-APPEND JOT-LF
   SB$ ;

: JOT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: JOT-USAGE$ ( -- ptr u8 n )
   SB-RESET
   s" usage: tools/json-only.f stderr-file" SB-APPEND
   JOT-LF
   SB$ ;

: JOT-ARGV-LOAD ( -- )
   PROC-ARGV-RESET
   s" tools/json-only.f" WR-TOOLS-LOAD if exit then
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/json-only-core.f"  >LEN PROC-ARGV+
   s" tools/json-only.f"  >LEN PROC-ARGV+ ;

: JOT-CAPTURE>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: JOT-RUN ( ptr u8 n -- n n n n ) {: path:ptr pathu :}
   JOT-ARGV-LOAD
   WR-TOOLS? 0= if s" --"  >LEN PROC-ARGV+ then
   path pathu  >LEN PROC-ARGV+
   WR-TOOLS$  >LEN JOT-OUT JOT-BUF-CAP >LEN JOT-ERR JOT-BUF-CAP >LEN
   JOT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME JOT-CAPTURE>N ;

: JOT-RUN-NOARG ( -- n n n n )
   JOT-ARGV-LOAD
   WR-TOOLS$  >LEN JOT-OUT JOT-BUF-CAP >LEN JOT-ERR JOT-BUF-CAP >LEN
   JOT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME JOT-CAPTURE>N ;

: JOT-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: JOT-MIXED-CASE ( -- )
   JOT-IN JOT-MIXED-IN$ WRITE-ALL
   JOT-IN JOT-RUN 0 JOT-EXPECT-EXIT {: outu erru :}
   JOT-OUT outu JOT-MIXED-OUT$ T$=
   JOT-ERR erru JOT-EMPTY$ T$= ;

: JOT-BAD-CASE ( -- )
   JOT-IN JOT-BAD$ WRITE-ALL
   JOT-IN JOT-RUN 0 JOT-EXPECT-EXIT {: outu erru :}
   JOT-OUT outu JOT-EMPTY$ T$=
   JOT-ERR erru JOT-BAD$ T$= ;

: JOT-ARRAY-CASE ( -- )
   JOT-IN JOT-ARRAY$ WRITE-ALL
   JOT-IN JOT-RUN 0 JOT-EXPECT-EXIT {: outu erru :}
   JOT-OUT outu JOT-EMPTY$ T$=
   JOT-ERR erru JOT-ARRAY$ T$= ;

: JOT-PROSE-CASE ( -- )
   JOT-IN JOT-PROSE$ WRITE-ALL
   JOT-IN JOT-RUN 0 JOT-EXPECT-EXIT {: outu erru :}
   JOT-OUT outu JOT-EMPTY$ T$=
   JOT-ERR erru JOT-PROSE$ T$= ;

: JOT-ZERO-CASE ( -- )
   JOT-IN JOT-EMPTY$ WRITE-ALL
   JOT-IN JOT-RUN 0 JOT-EXPECT-EXIT {: outu erru :}
   JOT-OUT outu JOT-EMPTY$ T$=
   JOT-ERR erru JOT-EMPTY$ T$= ;

: JOT-NOARG ( -- )
   JOT-RUN-NOARG 64 JOT-EXPECT-EXIT {: outu erru :}
   JOT-OUT outu JOT-EMPTY$ T$=
   JOT-ERR erru JOT-USAGE$ T$= ;

: JOT-MAIN ( -- )
   T-RESET
   JOT-PREPARE
   JOT-MIXED-CASE
   JOT-BAD-CASE
   JOT-ARRAY-CASE
   JOT-PROSE-CASE
   JOT-ZERO-CASE
   JOT-NOARG
   CLEANUP-RUN
   JOT-ROOT EXISTS? TFALSE
   T-REPORT
   s" json-only-test: ok" type cr ;

JOT-MAIN
