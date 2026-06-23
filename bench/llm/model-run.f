\ model-run.f - checked native model invocation for LLM benchmark drivers.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f,
\ bench/llm/manifest.f, bench/llm/model.f, bench/llm/parse-resp-lib.f,
\ and bench/llm/codex-home.f.

262144 constant MRUN-OUT-NEED
65536 constant MRUN-ERR-NEED
4096 constant MRUN-TEXT-NEED
1024 constant MRUN-EXE-CAP
-3222 constant E-MRUN-TEMPLATE
-3223 constant E-MRUN-CAPACITY

variable MRUN-OUT-P
variable MRUN-OUT-CAP-U
variable MRUN-ERR-P
variable MRUN-ERR-CAP-U
variable MRUN-TEXT-P
variable MRUN-TEXT-CAP-U
create MRUN-EXE-BUF MRUN-EXE-CAP allot
create MRUN-FINAL-DIR-BUF FS-PATH-CAP allot
create MRUN-FINAL-PATH-BUF FS-PATH-CAP allot

variable MRUN-OUT-U
variable MRUN-ERR-U
variable MRUN-TEXT-U
variable MRUN-EXE-U
variable MRUN-FINAL-DIR-U
variable MRUN-FINAL-PATH-U
variable MRUN-RC
variable MRUN-TOKENS
variable MRUN-PROMPT-A
variable MRUN-PROMPT-U

TRUSTED: MRUN-OUT-BUF ( -- ptr u8 )
   MRUN-OUT-P @ ;

TRUSTED: MRUN-ERR-BUF ( -- ptr u8 )
   MRUN-ERR-P @ ;

TRUSTED: MRUN-TEXT-BUF ( -- ptr u8 )
   MRUN-TEXT-P @ ;

TRUSTED: MRUN-PROMPT$ ( -- ptr u8 n )
   MRUN-PROMPT-A @
   MRUN-PROMPT-U @ ;

: MRUN-OUT-CAP ( -- n )
   MRUN-OUT-CAP-U @ ;

: MRUN-ERR-CAP ( -- n )
   MRUN-ERR-CAP-U @ ;

: MRUN-TEXT-CAP ( -- n )
   MRUN-TEXT-CAP-U @ ;

: MRUN-OUT$ ( -- ptr u8 n )
   MRUN-OUT-BUF MRUN-OUT-U @ ;

: MRUN-ERR$ ( -- ptr u8 n )
   MRUN-ERR-BUF MRUN-ERR-U @ ;

: MRUN-TEXT$ ( -- ptr u8 n )
   MRUN-TEXT-BUF MRUN-TEXT-U @ ;

: MRUN-FINAL-PATH$ ( -- ptr u8 n )
   MRUN-FINAL-PATH-BUF MRUN-FINAL-PATH-U @ ;

: MRUN-MIN-ONE ( n -- n )
   dup 1 < if drop 1 then ;

: MRUN-STORE-OUT-SPAN ( ptr u8 n -- )
   MRUN-OUT-CAP-U ! MRUN-OUT-P ! ;

: MRUN-STORE-ERR-SPAN ( ptr u8 n -- )
   MRUN-ERR-CAP-U ! MRUN-ERR-P ! ;

: MRUN-STORE-TEXT-SPAN ( ptr u8 n -- )
   MRUN-TEXT-CAP-U ! MRUN-TEXT-P ! ;

: MRUN-ENSURE-OUT-CAP ( n -- ) {: need :}
   need MRUN-MIN-ONE MRUN-OUT-CAP <= if exit then
   need MRUN-MIN-ONE MEM-ALLOC-64K-SPAN MRUN-STORE-OUT-SPAN ;

: MRUN-ENSURE-ERR-CAP ( n -- ) {: need :}
   need MRUN-MIN-ONE MRUN-ERR-CAP <= if exit then
   need MRUN-MIN-ONE MEM-ALLOC-64K-SPAN MRUN-STORE-ERR-SPAN ;

: MRUN-ENSURE-TEXT-CAP ( n -- ) {: need :}
   need MRUN-MIN-ONE MRUN-TEXT-CAP <= if exit then
   need MRUN-MIN-ONE MEM-ALLOC-64K-SPAN MRUN-STORE-TEXT-SPAN ;

: MRUN-ENSURE-BUFFERS ( -- )
   MRUN-OUT-NEED MRUN-ENSURE-OUT-CAP
   MRUN-ERR-NEED MRUN-ENSURE-ERR-CAP
   MRUN-TEXT-NEED MRUN-ENSURE-TEXT-CAP ;

: MRUN-RESET ( -- )
   MRUN-ENSURE-BUFFERS
   0 MRUN-OUT-U !
   0 MRUN-ERR-U !
   0 MRUN-TEXT-U !
   0 MRUN-EXE-U !
   -1 MRUN-RC !
   0 MRUN-TOKENS ! ;

: MRUN-PROMPT! ( ptr u8 n -- ) {: a:ptr u :}
   a MRUN-PROMPT-A !
   u MRUN-PROMPT-U ! ;

: MRUN-COPY-TEXT ( ptr u8 n -- ) {: a:ptr u :}
   u MRUN-TEXT-CAP > if E-MRUN-CAPACITY throw then
   a MRUN-TEXT-BUF u BYTE-COPY
   u MRUN-TEXT-U ! ;

: MRUN-COPY-OUT ( ptr u8 n -- ) {: a:ptr u :}
   u MRUN-OUT-CAP > if E-MRUN-CAPACITY throw then
   a MRUN-OUT-BUF u BYTE-COPY
   u MRUN-OUT-U ! ;

: MRUN-COPY-ERR ( ptr u8 n -- ) {: a:ptr u :}
   u MRUN-ERR-CAP > if E-MRUN-CAPACITY throw then
   a MRUN-ERR-BUF u BYTE-COPY
   u MRUN-ERR-U ! ;

: MRUN-PARSE-ERR$ ( n -- ptr u8 n ) {: code :}
   code E-JSON-CAPACITY = if s" model response parse capacity" exit then
   s" model response parse failed" ;

: MRUN-CAPTURE-ERR$ ( n -- ptr u8 n ) {: code :}
   code E-PROC-TRUNCATED = if s" model output truncated" exit then
   s" model process capture failed" ;

: MRUN-PARSE-FAILED ( n -- ) {: code :}
   code MRUN-RC !
   code MRUN-PARSE-ERR$ MRUN-COPY-ERR ;

: MRUN-CAPTURE-LENS! ( -- )
   PROC-OUT-LEN @ LEN>N MRUN-OUT-U !
   PROC-ERR-LEN @ LEN>N MRUN-ERR-U ! ;

: MRUN-CAPTURE-FAILED ( n -- ) {: code :}
   MRUN-CAPTURE-LENS!
   code MRUN-RC !
   code MRUN-CAPTURE-ERR$ MRUN-COPY-ERR ;

: MRUN-PROC-ERROR? ( n -- bool ) {: code :}
   code E-PROC-FIRST <=
   code E-PROC-LAST >= and ;

: MRUN-RESOLVE ( -- ptr u8 n )
   MR-COMMAND$ >LEN MRUN-EXE-BUF RESOLVE-EXECUTABLE LEN>N MRUN-EXE-U !
   MRUN-EXE-BUF MRUN-EXE-U @ ;

: MRUN-COPY-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-MRUN-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: MRUN-FINAL-DIR$ ( -- ptr u8 n )
   MRUN-FINAL-DIR-BUF MRUN-FINAL-DIR-U @ ;

: MRUN-FINAL-PREPARE ( -- )
   s" habu-codex-final" TMPDIR-MKDIR
   MRUN-FINAL-DIR-BUF MRUN-FINAL-DIR-U MRUN-COPY-PATH!
   MRUN-FINAL-DIR$ CLEANUP-TREE+
   MRUN-FINAL-DIR$ s" last-message.txt" MRUN-FINAL-PATH-BUF JOIN-PATH
   MRUN-FINAL-PATH-U ! ;

: MRUN-EMPTY$ ( -- ptr u8 n )
   s" " drop 0 ;

: MRUN-ARGS-PROMPT ( ptr u8 n -- )
    >LEN PROC-ARGV+ ;

: MRUN-ARGS-CLAUDE ( ptr u8 n -- ) {: prompt:ptr promptu :}
   s" -p"  >LEN PROC-ARGV+
   prompt promptu  >LEN PROC-ARGV+
   s" --output-format"  >LEN PROC-ARGV+
   s" json"  >LEN PROC-ARGV+ ;

: MRUN-ARGS-CODEX-DISABLE ( ptr u8 n -- )
   s" --disable"  >LEN PROC-ARGV+
    >LEN PROC-ARGV+ ;

: MRUN-ARGS-CODEX ( ptr u8 n -- ) {: prompt:ptr promptu :}
   s" exec"  >LEN PROC-ARGV+
   s" plugins" MRUN-ARGS-CODEX-DISABLE
   s" apps" MRUN-ARGS-CODEX-DISABLE
   s" multi_agent" MRUN-ARGS-CODEX-DISABLE
   s" tool_suggest" MRUN-ARGS-CODEX-DISABLE
   s" workspace_dependencies" MRUN-ARGS-CODEX-DISABLE
   s" --skip-git-repo-check"  >LEN PROC-ARGV+
   s" --ignore-rules"  >LEN PROC-ARGV+
   s" --ignore-user-config"  >LEN PROC-ARGV+
   s" --sandbox"  >LEN PROC-ARGV+
   s" read-only"  >LEN PROC-ARGV+
   s" --cd"  >LEN PROC-ARGV+
   MRUN-FINAL-DIR$  >LEN PROC-ARGV+
   s" --json"  >LEN PROC-ARGV+
   s" --output-last-message"  >LEN PROC-ARGV+
   MRUN-FINAL-PATH$  >LEN PROC-ARGV+
   prompt promptu  >LEN PROC-ARGV+ ;

: MRUN-CODEX-ARGS? ( -- bool )
   MR-ARGS$ s" codex-exec {prompt}" STR= ;

: MRUN-BUILD-ARGS ( ptr u8 n -- ) {: prompt:ptr promptu :}
   MR-ARGS$ s" -p {prompt} --output-format json" STR= if
      prompt promptu MRUN-ARGS-CLAUDE exit
   then
   MR-ARGS$ s" codex-exec {prompt}" STR= if
      prompt promptu MRUN-ARGS-CODEX exit
   then
   MR-ARGS$ s" {prompt}" STR= if
      prompt promptu MRUN-ARGS-PROMPT exit
   then
   MR-ARGS$ nip 0= if
      prompt promptu MRUN-ARGS-PROMPT exit
   then
   E-MRUN-TEMPLATE throw ;

: MRUN-CODEX-CLEAN? ( -- bool )
   MRUN-CODEX-ARGS?
   MR-PARSER$ s" codex-jsonl" STR= and ;

: MRUN-CAPTURE ( ptr u8 n -- ) {: prompt:ptr promptu :}
   MRUN-RESET
   PROC-ARGV-ENV-RESET
   MRUN-CODEX-ARGS? if MRUN-FINAL-PREPARE then
   prompt promptu MRUN-BUILD-ARGS
   MRUN-CODEX-CLEAN? if CODEX-HOME-PREPARE-ENV then
   PROC-ENV-INHERIT-MISSING
   MRUN-RESOLVE >LEN MRUN-EMPTY$ >LEN
   MRUN-OUT-BUF MRUN-OUT-CAP >LEN MRUN-ERR-BUF MRUN-ERR-CAP >LEN
   MR-TIMEOUT 1000 * >MS RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME {: outu erru kind code :}
   kind code PROC-OUTCOME>RC RC>N MRUN-RC !
   erru LEN>N MRUN-ERR-U !
   outu LEN>N MRUN-OUT-U ! ;

: MRUN-PARSE ( -- )
   MRUN-CODEX-CLEAN? if
      MRUN-OUT$ MR-PARSER$ MR-TOKEN-FIELDS$ PR-PARSE-BUFFER
      PR-TOKEN-COUNT MRUN-TOKENS !
      MRUN-FINAL-PATH$ MRUN-TEXT-BUF MRUN-TEXT-CAP READ-ALL MRUN-TEXT-U !
      exit
   then
   MRUN-OUT$ MR-PARSER$ MR-TOKEN-FIELDS$ PR-PARSE-BUFFER
   PR-OUT$ MRUN-COPY-TEXT
   PR-TOKEN-COUNT MRUN-TOKENS ! ;

: MRUN-CAPTURE-SAVED ( -- )
   MRUN-PROMPT$ MRUN-CAPTURE ;

: MRUN-RUN ( ptr u8 n -- ) {: prompt:ptr promptu :}
   prompt promptu MRUN-PROMPT!
   [: MRUN-CAPTURE-SAVED ;] catch dup 0= if
      drop
   else
      dup MRUN-PROC-ERROR? if MRUN-CAPTURE-FAILED exit then
      throw
   then
   MRUN-RC @ 0= if
      [: MRUN-PARSE ;] catch dup 0= if drop exit then
      MRUN-PARSE-FAILED
   then ;
