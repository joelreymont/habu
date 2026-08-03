\ gpt2-cli-device-test.f - end-to-end GPT-2 CLI generation proof.

require lib/test.f
require lib/memory.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require maki/infer/gpt2-cli.f
require maki/infer/gpt2-reference-data.f

package GPT2-CLI
private

4097 constant T-LONG-N
4096 constant T-CAP
240000 constant T-TIMEOUT-MS

create T-BAD FS-PATH-CAP allot
create T-MISS FS-PATH-CAP allot
create T-SRC FS-PATH-CAP allot
create T-DST FS-PATH-CAP allot
create T-LONG T-LONG-N allot
create T-OUT T-CAP allot
create T-ERR T-CAP allot

variable T-BAD-U
variable T-MISS-U

: T-BAD$ ( -- ptr u8 n )
   T-BAD T-BAD-U @ ;

: T-MISS$ ( -- ptr u8 n )
   T-MISS T-MISS-U @ ;

: T-COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr srcu:n dst:ptr lenp:ptr :}
   src dst srcu BYTE-COPY
   srcu lenp ! ;

: T-TEMP! ( ptr u8 n ptr u8 ptr n -- )
   {: prefix:ptr prefixu:n dst:ptr lenp:ptr :}
   prefix prefixu TMPDIR-MKDIR {: path:ptr pathu:n :}
   path pathu dst lenp T-COPY!
   dst pathu CLEANUP-TREE+ ;

: T-MERGES$ ( ptr u8 n ptr u8 -- ptr u8 n )
   {: root:ptr rootu:n dst:ptr :}
   root rootu GPT2PIN:MERGES-NAME$ dst JOIN-PATH {: pathu:n :}
   dst pathu ;

: T-COPY-MERGES ( ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n dst:ptr dstu:n :}
   root rootu T-SRC T-MERGES$
   dst dstu T-DST T-MERGES$
   COPY-FILE-STREAM ;

: T-PREPARE ( ptr u8 n -- ) {: root:ptr rootu:n :}
   CLEANUP-RESET
   s" gpt2-cli-bad" T-BAD T-BAD-U T-TEMP!
   s" gpt2-cli-missing" T-MISS T-MISS-U T-TEMP!
   root rootu T-BAD$ T-COPY-MERGES
   root rootu T-MISS$ T-COPY-MERGES
   T-BAD$ T-DST T-MERGES$ s" corrupt" WRITE-ALL
   T-LONG-N 0 ?do 120 T-LONG i + c! loop ;

: T-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: T-CAPTURE>N
   ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF
         PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0
      ENDOF
      err OF
         PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N erru LEN>N code RC>N
      ENDOF
   ;MATCH ;

: T-CLI ( ptr u8 n ptr u8 n -- n n n )
   {: root:ptr rootu:n prompt:ptr promptu:n :}
   PROC-ARGV-ENV-RESET
   s" --load" T-ARG+
   s" tools/gpt2.f" T-ARG+
   s" --" T-ARG+
   root rootu T-ARG+
   prompt promptu T-ARG+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN
   T-OUT T-CAP >LEN T-ERR T-CAP >LEN T-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE T-CAPTURE>N ;

: T-FAIL ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n prompt:ptr promptu:n want:ptr wantu:n :}
   root rootu prompt promptu T-CLI {: outu:n erru:n code:n :}
   code 0<> TTRUE
   outu 0 T=
   T-ERR erru want wantu CONTAINS? TTRUE ;

: T-FAILURES ( ptr u8 n -- ) {: root:ptr rootu:n :}
   s" digest mismatch writes no stdout" T-LABEL
   T-BAD$ s" Hello" s" -5664" T-FAIL
   s" empty prompt writes no stdout" T-LABEL
   root rootu s" " s" -5665" T-FAIL
   s" tokenizer overcapacity writes no stdout" T-LABEL
   root rootu T-LONG T-LONG-N s" -5324" T-FAIL
   s" model-open failure writes no stdout" T-LABEL
   T-MISS$ s" Hello" s" -2102" T-FAIL ;

using GPT2-REFERENCE

: T-IDS ( -- )
   STAGE-N @ CONT-N T=
   CONT-N 0 ?do
      i ID@ i REAL-ID T=
   loop
   CONT-N ID@ CANARY T= ;

: T-BYTES ( -- )
   OUT OUT-U @ REAL-BYTES$ T$= ;

: T-ENTRY ( ptr u8 n -- ) {: root:ptr rootu:n :}
   s" public CLI emits only the pinned continuation" T-LABEL
   root rootu s" Hello" T-CLI {: outu:n erru:n code:n :}
   erru 0<> if T-ERR erru type then
   code 0 T=
   erru 0 T=
   T-OUT outu REAL-BYTES$ T$= ;

;using

: T-CANARY ( -- )
   0 CONT-N ID!
   [: REQUIRE-STAGED ;] catch E-STATE T=
   CANARY CONT-N ID! ;

: T-SUCCESS ( ptr u8 n -- ) {: root:ptr rootu:n :}
   SAFET:LIVE-OWNERS {: owners:n :}
   SAFET-MAP:LIVE {: maps:n :}
   root rootu s" Hello" RUN-ACT
   s" production path stages 64 pinned continuation ids" T-LABEL
   T-IDS
   s" one post-close decode matches the pinned bytes" T-LABEL
   T-BYTES
   s" staging canary detects an overwrite" T-LABEL
   T-CANARY
   SAFET:LIVE-OWNERS owners T=
   SAFET-MAP:LIVE maps T= ;

: T-RUN ( -- )
   SCRIPT-ARGC 1 <> if E-USAGE throw then
   T-RESET
   0 SCRIPT-ARGV$ 2dup T-PREPARE
   2dup T-FAILURES
   2dup T-SUCCESS
   T-ENTRY
   CLEANUP-RUN
   T-REPORT ;

T-RUN

;package
