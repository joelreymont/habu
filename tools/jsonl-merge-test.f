\ jsonl-merge-test.f - focused CLI coverage for JSONL merge.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f \
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/jsonl-merge-test.f

5000 constant JMT-TIMEOUT-MS
65536 constant JMT-CAP
10 constant JMT-LF

create JMT-ROOT FS-PATH-CAP allot
create JMT-IN1 FS-PATH-CAP allot
create JMT-IN2 FS-PATH-CAP allot
create JMT-BAD FS-PATH-CAP allot
create JMT-OUT-PATH FS-PATH-CAP allot
create JMT-BAD-OUT-PATH FS-PATH-CAP allot
create JMT-OUT JMT-CAP allot
create JMT-ERR JMT-CAP allot
create JMT-FILE JMT-CAP allot

variable JMT-ROOT-U
variable JMT-IN1-U
variable JMT-IN2-U
variable JMT-BAD-U
variable JMT-OUT-PATH-U
variable JMT-BAD-OUT-PATH-U
variable JMT-FILE-U

: JMT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: JMT-ROOT$ ( -- ptr u8 n )
   JMT-ROOT JMT-ROOT-U @ ;

: JMT-IN1$ ( -- ptr u8 n )
   JMT-IN1 JMT-IN1-U @ ;

: JMT-IN2$ ( -- ptr u8 n )
   JMT-IN2 JMT-IN2-U @ ;

: JMT-BAD$ ( -- ptr u8 n )
   JMT-BAD JMT-BAD-U @ ;

: JMT-OUT-PATH$ ( -- ptr u8 n )
   JMT-OUT-PATH JMT-OUT-PATH-U @ ;

: JMT-BAD-OUT-PATH$ ( -- ptr u8 n )
   JMT-BAD-OUT-PATH JMT-BAD-OUT-PATH-U @ ;

: JMT-DQ ( -- )
   34 SB-APPEND-C ;

: JMT-NL ( -- )
   JMT-LF SB-APPEND-C ;

: JMT-ROW-A ( -- )
   123 SB-APPEND-C JMT-DQ s" a" SB-APPEND JMT-DQ s" :1}" SB-APPEND ;

: JMT-ROW-B ( -- )
   123 SB-APPEND-C JMT-DQ s" b" SB-APPEND JMT-DQ s" :2}" SB-APPEND ;

: JMT-ROW-C ( -- )
   123 SB-APPEND-C JMT-DQ s" c" SB-APPEND JMT-DQ s" :3}" SB-APPEND ;

: JMT-IN1-TEXT$ ( -- ptr u8 n )
   SB-RESET
   JMT-ROW-A JMT-NL
   JMT-NL
   JMT-ROW-B
   SB$ ;

: JMT-IN2-TEXT$ ( -- ptr u8 n )
   SB-RESET
   JMT-ROW-C JMT-NL
   SB$ ;

: JMT-BAD-TEXT$ ( -- ptr u8 n )
   SB-RESET
   JMT-ROW-A JMT-NL
   s" bad" SB-APPEND JMT-NL
   SB$ ;

: JMT-WANT$ ( -- ptr u8 n )
   SB-RESET
   JMT-ROW-A JMT-NL
   JMT-ROW-B JMT-NL
   JMT-ROW-C JMT-NL
   SB$ ;

: JMT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-jsonl-merge" TMPDIR-MKDIR JMT-ROOT JMT-ROOT-U JMT-COPY!
   JMT-ROOT$ CLEANUP-TREE+
   JMT-ROOT$ s" in1.jsonl" JMT-IN1 JOIN-PATH JMT-IN1-U !
   JMT-ROOT$ s" in2.jsonl" JMT-IN2 JOIN-PATH JMT-IN2-U !
   JMT-ROOT$ s" bad.jsonl" JMT-BAD JOIN-PATH JMT-BAD-U !
   JMT-ROOT$ s" out.jsonl" JMT-OUT-PATH JOIN-PATH JMT-OUT-PATH-U !
   JMT-ROOT$ s" bad-out.jsonl" JMT-BAD-OUT-PATH JOIN-PATH JMT-BAD-OUT-PATH-U !
   JMT-IN1$ JMT-IN1-TEXT$ WRITE-ALL
   JMT-IN2$ JMT-IN2-TEXT$ WRITE-ALL
   JMT-BAD$ JMT-BAD-TEXT$ WRITE-ALL ;

: JMT-MERGE-LOADS ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/json-file.f"  >LEN PROC-ARGV+
   s" tools/jsonl-merge.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: JMT-RUN-GOOD ( -- n n n )
   PROC-ARGV-RESET
   JMT-MERGE-LOADS
   JMT-OUT-PATH$  >LEN PROC-ARGV+
   JMT-IN1$  >LEN PROC-ARGV+
   JMT-IN2$  >LEN PROC-ARGV+
   s" bin/hb" >LEN JMT-OUT JMT-CAP >LEN JMT-ERR JMT-CAP >LEN
   JMT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: JMT-RUN-BAD ( -- n n n )
   PROC-ARGV-RESET
   JMT-MERGE-LOADS
   JMT-BAD-OUT-PATH$  >LEN PROC-ARGV+
   JMT-BAD$  >LEN PROC-ARGV+
   s" bin/hb" >LEN JMT-OUT JMT-CAP >LEN JMT-ERR JMT-CAP >LEN
   JMT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: TEST-JMT-GOOD ( -- )
   JMT-RUN-GOOD {: outu erru rc :}
   rc 0 T=
   outu 0 T=
   erru 0 T=
   JMT-OUT-PATH$ JMT-FILE JMT-CAP READ-ALL JMT-FILE-U !
   JMT-FILE JMT-FILE-U @ JMT-WANT$ T$= ;

: TEST-JMT-BAD ( -- )
   JMT-RUN-BAD {: outu erru rc :}
   outu drop
   rc 0 T<>
   JMT-ERR erru s" jsonl-merge: invalid row" CONTAINS? TTRUE ;

: JSONL-MERGE-TEST ( -- )
   T-RESET
   JMT-PREPARE
   TEST-JMT-GOOD
   TEST-JMT-BAD
   CLEANUP-RUN
   JMT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" jsonl-merge-test: ok" type cr ;

JSONL-MERGE-TEST
