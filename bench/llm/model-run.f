\ model-run.f - checked native model invocation for LLM benchmark drivers.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f,
\ bench/llm/manifest.f, bench/llm/model.f, bench/llm/parse-resp-lib.f,
\ and bench/llm/codex-home.f.

262144 constant MRUN-OUT-CAP
65536 constant MRUN-ERR-CAP
4096 constant MRUN-TEXT-CAP
1024 constant MRUN-EXE-CAP
-3222 constant E-MRUN-TEMPLATE
-3223 constant E-MRUN-CAPACITY

create MRUN-OUT-BUF MRUN-OUT-CAP allot
create MRUN-ERR-BUF MRUN-ERR-CAP allot
create MRUN-TEXT-BUF MRUN-TEXT-CAP allot
create MRUN-EXE-BUF MRUN-EXE-CAP allot

variable MRUN-OUT-U
variable MRUN-ERR-U
variable MRUN-TEXT-U
variable MRUN-EXE-U
variable MRUN-RC
variable MRUN-TOKENS

: MRUN-OUT$ ( -- ptr u8 n )
   MRUN-OUT-BUF MRUN-OUT-U @ ;

: MRUN-ERR$ ( -- ptr u8 n )
   MRUN-ERR-BUF MRUN-ERR-U @ ;

: MRUN-TEXT$ ( -- ptr u8 n )
   MRUN-TEXT-BUF MRUN-TEXT-U @ ;

: MRUN-RESET ( -- )
   0 MRUN-OUT-U !
   0 MRUN-ERR-U !
   0 MRUN-TEXT-U !
   0 MRUN-EXE-U !
   -1 MRUN-RC !
   0 MRUN-TOKENS ! ;

: MRUN-COPY-TEXT ( ptr u8 n -- ) {: a:ptr u :}
   u MRUN-TEXT-CAP > if E-MRUN-CAPACITY throw then
   a MRUN-TEXT-BUF u BYTE-COPY
   u MRUN-TEXT-U ! ;

: MRUN-RESOLVE ( -- ptr u8 n )
   MR-COMMAND$ MRUN-EXE-BUF RESOLVE-EXECUTABLE MRUN-EXE-U !
   MRUN-EXE-BUF MRUN-EXE-U @ ;

: MRUN-EMPTY$ ( -- ptr u8 n )
   s" " drop 0 ;

: MRUN-ARGS-PROMPT ( ptr u8 n -- )
   PROC-ARGV+ ;

: MRUN-ARGS-CLAUDE ( ptr u8 n -- ) {: prompt:ptr promptu :}
   s" -p" PROC-ARGV+
   prompt promptu PROC-ARGV+
   s" --output-format" PROC-ARGV+
   s" json" PROC-ARGV+ ;

: MRUN-ARGS-CODEX-DISABLE ( ptr u8 n -- )
   s" --disable" PROC-ARGV+
   PROC-ARGV+ ;

: MRUN-ARGS-CODEX ( ptr u8 n -- ) {: prompt:ptr promptu :}
   s" exec" PROC-ARGV+
   s" plugins" MRUN-ARGS-CODEX-DISABLE
   s" apps" MRUN-ARGS-CODEX-DISABLE
   s" multi_agent" MRUN-ARGS-CODEX-DISABLE
   s" tool_suggest" MRUN-ARGS-CODEX-DISABLE
   s" workspace_dependencies" MRUN-ARGS-CODEX-DISABLE
   s" --skip-git-repo-check" PROC-ARGV+
   s" --ignore-rules" PROC-ARGV+
   s" --ignore-user-config" PROC-ARGV+
   s" --sandbox" PROC-ARGV+
   s" read-only" PROC-ARGV+
   s" --json" PROC-ARGV+
   prompt promptu PROC-ARGV+ ;

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
   MR-ARGS$ s" codex-exec {prompt}" STR=
   MR-PARSER$ s" codex-jsonl" STR= and ;

: MRUN-CAPTURE ( ptr u8 n -- ) {: prompt:ptr promptu :}
   MRUN-RESET
   PROC-ARGV-ENV-RESET
   prompt promptu MRUN-BUILD-ARGS
   MRUN-CODEX-CLEAN? if CODEX-HOME-PREPARE-ENV then
   PROC-ENV-INHERIT-MISSING
   MRUN-RESOLVE MRUN-EMPTY$ MRUN-OUT-BUF MRUN-OUT-CAP MRUN-ERR-BUF MRUN-ERR-CAP MR-TIMEOUT 1000 * RUN-ARGV-ENV-STDIN-CAPTURE
   MRUN-RC !
   MRUN-ERR-U !
   MRUN-OUT-U ! ;

: MRUN-PARSE ( -- )
   MRUN-OUT$ MR-PARSER$ MR-TOKEN-FIELDS$ PR-PARSE-BUFFER
   PR-OUT$ MRUN-COPY-TEXT
   PR-TOKEN-COUNT MRUN-TOKENS ! ;

: MRUN-RUN ( ptr u8 n -- )
   MRUN-CAPTURE
   MRUN-RC @ 0= if MRUN-PARSE then ;
