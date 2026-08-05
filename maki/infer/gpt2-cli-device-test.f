\ gpt2-cli-device-test.f - exact one-request GPT-2 CLI proof.

require lib/test.f
require lib/memory.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/checker-assert.f
require maki/infer/gpt2-cli.f
require maki/infer/gpt2-clean-test-lib.f
require maki/infer/gpt2-reference-data.f

package GPT2-CLI
private

4097 constant T-LONG-N
4096 constant T-CAP
240000 constant T-TIMEOUT-MS
721 constant E-T-MODEL
722 constant E-T-SESSION

create T-ROOT FS-PATH-CAP allot
create T-SRC FS-PATH-CAP allot
create T-DST FS-PATH-CAP allot
create T-LONG T-LONG-N allot
create T-OUT T-CAP allot
create T-ERR T-CAP allot

variable T-ROOT-U

: T-ROOT$ ( -- ptr u8 n )
   T-ROOT T-ROOT-U @ ;

: T-COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr srcu:n dst:ptr lenp:ptr :}
   src dst srcu BYTE-COPY
   srcu lenp ! ;

: T-TEMP! ( ptr u8 n ptr u8 ptr n -- )
   {: prefix:ptr prefixu:n dst:ptr lenp:ptr :}
   prefix prefixu TMPDIR-MKDIR {: path:ptr pathu:n :}
   path pathu dst lenp T-COPY!
   dst pathu CLEANUP-TREE+ ;

: T-COPY-ASSET ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n dst:ptr dstu:n name:ptr nameu:n :}
   root rootu name nameu T-SRC JOIN-PATH {: srcu:n :}
   dst dstu name nameu T-DST JOIN-PATH {: outu:n :}
   T-SRC srcu T-DST outu COPY-FILE-STREAM ;

: T-FILL ( ptr u8 n n -- )
   {: dst:ptr len:n byte:n :}
   len 0 ?do byte dst i + c! loop ;

: T-PREPARE ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   CLEANUP-RESET
   s" gpt2-cli-refusal" T-ROOT T-ROOT-U T-TEMP!
   root rootu T-ROOT$ GPT2PIN:CONFIG-NAME$ T-COPY-ASSET
   root rootu T-ROOT$ GPT2PIN:VOCAB-NAME$ T-COPY-ASSET
   root rootu T-ROOT$ GPT2PIN:MERGES-NAME$ T-COPY-ASSET
   T-ROOT$ GPT2PIN:MERGES-NAME$ T-DST JOIN-PATH
   T-DST swap s" corrupt" WRITE-ALL
   T-LONG T-LONG-N 120 T-FILL ;

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

: T-FAILURES ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   s" digest mismatch writes no stdout" T-LABEL
   T-ROOT$ s" Hello" s" -5664" T-FAIL
   s" empty prompt writes no stdout" T-LABEL
   root rootu s" " s" -5665" T-FAIL
   s" tokenizer overcapacity writes no stdout" T-LABEL
   root rootu T-LONG T-LONG-N s" -5324" T-FAIL
   root rootu T-ROOT$ GPT2PIN:MERGES-NAME$ T-COPY-ASSET
   T-ROOT$ GPT2PIN:CONFIG-NAME$ T-DST JOIN-PATH
   T-DST swap REMOVE-FILE
   s" model-open failure writes no stdout" T-LABEL
   T-ROOT$ s" Hello" s" -2102" T-FAIL ;

: T-ENTRY ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   s" direct CLI emits the exact 64-token continuation" T-LABEL
   root rootu s" Hello" T-CLI {: outu:n erru:n code:n :}
   erru 0<> if T-ERR erru type then
   code 0 T=
   erru 0 T=
   T-OUT outu GPT2-REFERENCE:REAL-BYTES$ T$= ;

using GPT2-CLEAN-TEST

: T-OWNERS ( -- GPU:session GPT2:model )
   GPU:OPEN
   MATCH result
      err OF throw ENDOF
      ok OF
         0 SCRIPT-ARGV$ FS-PATH:MAKE GPT2:OPEN
         MATCH result
            err OF SESSION-CLEAN throw ENDOF
            ok OF ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: T-CLOSE-CASE ( n n n -- )
   {: free:n sync:n want:n :}
   T-OWNERS
   free sync TRACK-FAULTS
   0 CLOSE-ALL {: code:n :}
   ASSERT-MODEL-SESSION
   code want T= ;

: T-PRIMARY ( -- )
   E-T-MODEL E-T-SESSION TRACK-FAULTS
   0 SCRIPT-ARGV$ s" " RUN-ACT drop ;

: T-BAD-OPEN ( -- )
   s" /tmp/habu-no-gpt2-cli-model" s" Hello" RUN-ACT drop ;

: T-CLEANUP-FAULTS ( -- )
   s" CLI cleanup preserves primary, model, then session failure order" T-LABEL
   [: T-PRIMARY ;] GPT2:E-PROMPT TTHROWSQ ASSERT-MODEL-SESSION
   E-T-MODEL E-T-SESSION E-T-MODEL T-CLOSE-CASE
   0 E-T-SESSION E-T-SESSION T-CLOSE-CASE
   s" CLI model-open refusal closes the real caller session" T-LABEL
   0 E-T-SESSION TRACK-FAULTS
   [: T-BAD-OPEN ;] E-FS-OPEN TTHROWSQ
   ASSERT-SESSION-ONLY ;

;using

: T-RUN ( -- )
   SCRIPT-ARGC 1 <> if E-USAGE throw then
   T-RESET
   s" GPT2-CLI:BL>N" XREF-FIND XREF-FOUND? TFALSE
   s" CLI-BL-PRIVATE ( CAD-NUM:byte-len -- n ) GPT2-CLI:BL>N"
   CHECK-QUIET-CANDIDATE! 1 T=
   0 SCRIPT-ARGV$ 2dup T-PREPARE
   2dup T-FAILURES
   T-ENTRY
   CLEANUP-RUN
   T-CLEANUP-FAULTS
   T-REPORT ;

T-RUN

;package
