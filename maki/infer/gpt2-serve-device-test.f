\ gpt2-serve-device-test.f - persistent real-device service proof.

require lib/test.f
require lib/memory.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require maki/infer/gpt2-serve.f
require maki/infer/gpt2-reference-data.f

package GPT2-SERVE
private

using GPT2-GEN

OUTPUT-CAP BL>N constant T-CAP
240000 constant T-TIMEOUT-MS

create T-TMP FS-PATH-CAP allot
create T-SRC FS-PATH-CAP allot
create T-ALIAS FS-PATH-CAP allot
create T-MOVED FS-PATH-CAP allot
create T-REQ BODY-CAP U32-N + allot
create T-BUF T-CAP allot

variable T-TMP-U
variable T-SRC-U
variable T-ALIAS-U
variable T-MOVED-U
variable T-IN-R
variable T-IN-W
variable T-OUT-R
variable T-OUT-W
variable T-ERR-R
variable T-ERR-W
variable T-PID

: T-TMP$ ( -- ptr u8 n )
   T-TMP T-TMP-U @ ;

: T-SRC$ ( -- ptr u8 n )
   T-SRC T-SRC-U @ ;

: T-ALIAS$ ( -- ptr u8 n )
   T-ALIAS T-ALIAS-U @ ;

: T-MOVED$ ( -- ptr u8 n )
   T-MOVED T-MOVED-U @ ;

: T-COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr srcu:n dst:ptr lenp:ptr :}
   src dst srcu BYTE-COPY
   srcu lenp ! ;

: T-ROOT! ( ptr u8 n -- ) {: root:ptr rootu:n :}
   rootu FS-PATH-CAP > if E-FS-PATH throw then
   rootu 0 > if root c@ $2F = else 0 0= 0= then if
      root rootu T-SRC T-SRC-U T-COPY!
   else
      s" PWD" GETENV root rootu T-SRC JOIN-PATH T-SRC-U !
   then ;

: T-ROOT-BOUND ( -- )
   s" absolute model root above FS-PATH-CAP is rejected before copy" T-LABEL
   [: T-REQ FS-PATH-CAP 1+ T-ROOT! ;] E-FS-PATH TTHROWSQ ;

: T-PATH! ( ptr u8 n ptr u8 ptr n -- )
   {: name:ptr nameu:n dst:ptr lenp:ptr :}
   T-TMP$ name nameu dst JOIN-PATH lenp ! ;

: T-PREPARE ( ptr u8 n -- )
   CLEANUP-RESET
   T-ROOT!
   s" gpt2-serve-device" TMPDIR-MKDIR T-TMP T-TMP-U T-COPY!
   T-TMP$ CLEANUP-TREE+
   s" model" T-ALIAS T-ALIAS-U T-PATH!
   s" retired" T-MOVED T-MOVED-U T-PATH!
   T-SRC$ T-ALIAS$ MAKE-SYMLINK ;

: T-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: T-PIPES ( -- )
   PIPE-PAIR T-IN-W ! T-IN-R !
   PIPE-PAIR T-OUT-W ! T-OUT-R !
   PIPE-PAIR T-ERR-W ! T-ERR-R !
   T-IN-R @ >FD FD-CLOEXEC!
   T-IN-W @ >FD FD-CLOEXEC!
   T-OUT-R @ >FD FD-CLOEXEC!
   T-OUT-W @ >FD FD-CLOEXEC!
   T-ERR-R @ >FD FD-CLOEXEC!
   T-ERR-W @ >FD FD-CLOEXEC! ;

: T-SPAWN ( ptr u8 n -- ) {: load:ptr loadu:n :}
   T-PIPES
   PROC-ARGV-ENV-RESET
   s" --load" T-ARG+
   load loadu T-ARG+
   s" --" T-ARG+
   T-ALIAS$ T-ARG+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN
      T-IN-R @ >FD T-OUT-W @ >FD T-ERR-W @ >FD
      PROC-SPAWN-ARGV-ENV-IO T-PID !
   T-IN-R @ close
   T-OUT-W @ close
   T-ERR-W @ close ;

: T-IO-OK ( result<n,n> -- )
   MATCH result
      ok OF 0 T= ENDOF
      err OF drop 1 0 T= ENDOF
   ;MATCH ;

: T-SEND ( n ptr u8 n -- ) {: max:n prompt:ptr promptu:n :}
   promptu 0 < promptu PROMPT-CAP BL>N > or if
      E-FS-CAPACITY throw
   then
   U32-N promptu + T-REQ U32!
   max T-REQ U32-N + U32!
   prompt T-REQ U32-N U32-N + + promptu BYTE-COPY
   T-IN-W @ >FD T-REQ U32-N U32-N + promptu + WRITE-EXACT T-IO-OK ;

: T-READY ( -- )
   T-OUT-R @ >FD T-TIMEOUT-MS >MS POLL-IN-OR-TIMEOUT drop ;

: T-READ-OK ( ptr u8 n -- ) {: want:ptr wantu:n :}
   wantu 0 < wantu T-CAP > or if E-FS-CAPACITY throw then
   T-READY
   T-OUT-R @ >FD HEAD U32-N READ-EXACT T-IO-OK
   HEAD U32@ {: bodyu:n :}
   bodyu wantu 1+ T=
   HEAD c@ bodyu $FF and T=
   HEAD 1 + c@ bodyu 8 rshift $FF and T=
   HEAD 2 + c@ bodyu 16 rshift $FF and T=
   HEAD 3 + c@ bodyu 24 rshift $FF and T=
   T-OUT-R @ >FD TAG-BUF TAG-N READ-EXACT T-IO-OK
   TAG-BUF c@ TAG-OK T=
   T-OUT-R @ >FD T-BUF wantu READ-EXACT T-IO-OK
   T-BUF wantu want wantu T$= ;

: T-BUFFER-BOUNDS ( -- )
   s" device request and response buffers reject oversized lengths" T-LABEL
   [: 1 T-BUF PROMPT-CAP BL>N 1+ T-SEND ;]
      E-FS-CAPACITY TTHROWSQ
   [: T-BUF T-CAP 1+ T-READ-OK ;] E-FS-CAPACITY TTHROWSQ ;

;using

: T-READ-REFUSAL ( n n -- ) {: lo:n hi:n :}
   T-READY
   T-OUT-R @ >FD T-BUF REFUSAL-N READ-EXACT T-IO-OK
   T-BUF c@ 9 T=
   T-BUF 1 + c@ 0 T=
   T-BUF 2 + c@ 0 T=
   T-BUF 3 + c@ 0 T=
   T-BUF 4 + c@ TAG-ERR T=
   T-BUF 5 + c@ lo T=
   T-BUF 6 + c@ hi T=
   7 begin dup REFUSAL-N < while
      T-BUF over + c@ $FF T=
      1+
   repeat drop ;

: T-WAIT ( -- )
   T-IN-W @ close
   T-PID @ >PID PROC-WAIT-RC
   MATCH result
      ok OF 0 T= ENDOF
      err OF drop 1 0 T= ENDOF
   ;MATCH
   T-OUT-R @ T-BUF 1 read 0 T=
   T-ERR-R @ T-BUF T-CAP read 0 T=
   T-OUT-R @ close
   T-ERR-R @ close ;

: T-WAIT-WRITE-FAIL ( -- )
   T-IN-W @ close
   T-PID @ >PID PROC-WAIT-RC
   MATCH result
      ok OF drop 1 0 T= ENDOF
      err OF 141 <> TTRUE ENDOF
   ;MATCH
   T-ERR-R @ T-BUF T-CAP read {: erru:n :}
   erru 0 > TTRUE
   T-BUF erru s" -2105" CONTAINS? TTRUE
   T-ERR-R @ close ;

: T-WAIT-GENERATE-FAIL ( -- )
   T-IN-W @ close
   T-PID @ >PID PROC-WAIT-RC
   MATCH result
      ok OF drop 1 0 T= ENDOF
      err OF 141 <> TTRUE ENDOF
   ;MATCH
   T-OUT-R @ T-BUF 1 read 0 T=
   T-ERR-R @ T-BUF T-CAP read {: erru:n :}
   erru 0 > TTRUE
   T-BUF erru s" -5324" CONTAINS? TTRUE
   T-OUT-R @ close
   T-ERR-R @ close ;

: T-SESSION ( -- )
   s" first request returns the exact 64-token continuation" T-LABEL
   64 s" Hello" T-SEND
   GPT2-REFERENCE:REAL-BYTES$ T-READ-OK
   T-ALIAS$ T-MOVED$ RENAME-FILE
   T-ALIAS$ SYMLINK? TFALSE
   T-MOVED$ SYMLINK? TTRUE
   s" max_tokens zero is refused and the session remains live" T-LABEL
   0 s" Hello" T-SEND
   $DD $E9 T-READ-REFUSAL
   s" empty prompt is refused and the session remains live" T-LABEL
   64 s" " T-SEND
   $DF $E9 T-READ-REFUSAL
   s" second request proves the opened model and BPE remain retained" T-LABEL
   64 s" Hello" T-SEND
   GPT2-REFERENCE:REAL-BYTES$ T-READ-OK
   T-WAIT ;

: T-WRITE-FAILURE ( -- )
   s" fresh production child converts closed stdout into E-FS-IO, not SIGPIPE" T-LABEL
   T-MOVED$ T-ALIAS$ RENAME-FILE
   s" tools/gpt2-serve.f" T-SPAWN
   T-OUT-R @ close
   0 s" Hello" T-SEND
   T-WAIT-WRITE-FAIL ;

: T-GENERATE-FAILURE ( -- )
   s" real GENERATE BPE refusal is tagged exactly and terminates" T-LABEL
   s" tools/gpt2-serve.f" T-SPAWN
   BODY-CAP U32-N - 0 ?do 0 T-BUF i + c! loop
   1 T-BUF BODY-CAP U32-N - T-SEND
   $34 $EB T-READ-REFUSAL
   T-WAIT-GENERATE-FAIL ;

: T-RUN ( -- )
   SCRIPT-ARGC 1 <> if E-USAGE throw then
   T-RESET
   T-ROOT-BOUND
   T-BUFFER-BOUNDS
   0 SCRIPT-ARGV$ T-PREPARE
   s" maki/infer/gpt2-serve-close-test.f" T-SPAWN
   T-SESSION
   T-WRITE-FAILURE
   T-GENERATE-FAILURE
   CLEANUP-RUN
   T-REPORT ;

T-RUN

;package
