\ process-command-test.f - focused tests for lib/process-command.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/process-command.f lib/process-command-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-command.f
require lib/test/outcome.f

create PCMDT-ENV-OUT 97 c, 108 c, 112 c, 104 c, 97 c, 10 c, 10 c, 10 c,
create PCMDT-ENTRY-OUT 101 c, 110 c, 116 c, 114 c, 121 c, 10 c, 10 c, 10 c,

5000 constant PCMDT-HB-TIMEOUT-MS
1000 constant PCMDT-CMD-TIMEOUT-MS
50 constant PCMDT-SHORT-TIMEOUT-MS

: PCMDT-PROC-RUN-RC ( ptr u8 n n -- n ) {: path:ptr pathu timeout :}
   path pathu >LEN timeout >MS PROC-CMD-RUN-RC RC>N ;

: PCMDT-RUN-OUTCOME ( ptr u8 n n -- outcome ) {: path:ptr pathu:n timeout:n :}
   path pathu >LEN timeout >MS PROC-CMD-RUN-OUTCOME ;

: PCMDT-ENV+ ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu val:ptr valu :}
   name nameu >LEN val valu >LEN PROC-CMD-ENV+ ;

: PCMDT-ENV-ENTRY+ ( ptr u8 n -- ) {: a:ptr u :}
   a u >LEN PROC-CMD-ENV-ENTRY+ ;

: PCMDT-IN! ( ptr u8 n -- ) {: a:ptr u :}
   a u >LEN PROC-CMD-IN! ;

: PCMDT-OUT-LEN ( -- n )
   PROC-CMD-OUT$ {: a:ptr u :} u ;

: PCMDT-ERR-LEN ( -- n )
   PROC-CMD-ERR$ {: a:ptr u :} u ;

: PCMDT-INHERIT-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" alpha" SB-APPEND
   10 SB-APPEND-C
   s" HOME" GETENV SB-APPEND
   10 SB-APPEND-C
   s" PATH" GETENV SB-APPEND
   10 SB-APPEND-C
   SB$ ;

: PCMDT-RUN-PRINTF ( -- )
   PROC-CMD-RESET
   s" %s:%s" >LEN PROC-CMD-ARG+
   s" left" >LEN PROC-CMD-ARG+
   s" right" >LEN PROC-CMD-ARG+
   s" /usr/bin/printf" PCMDT-CMD-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD-OUT$ s" left:right" T$=
   PCMDT-ERR-LEN 0 T=
   PROC-ARGV-N @ COUNT>N 0 T=
   PROC-ENV-N @ COUNT>N 0 T= ;

: PCMDT-RUN-STDIN ( -- )
   PROC-CMD-RESET
   s" cmd-stdin" PCMDT-IN!
   s" /bin/cat" PCMDT-CMD-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD-OUT$ s" cmd-stdin" T$=
   PCMDT-ERR-LEN 0 T= ;

: PCMDT-RUN-HERMETIC-ENV ( -- )
   PROC-CMD-RESET
   PROC-CMD-ENV-HERMETIC
   s" test/process-env-child.f" >LEN PROC-CMD-ARG+
   s" HABU_PROC_ENV_TEST" s" alpha" PCMDT-ENV+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD-OUT$ PCMDT-ENV-OUT 8 T$=
   PCMDT-ERR-LEN 0 T= ;

: PCMDT-RUN-ENTRY-ENV ( -- )
   PROC-CMD-RESET
   PROC-CMD-ENV-HERMETIC
   s" test/process-env-child.f" >LEN PROC-CMD-ARG+
   s" HABU_PROC_ENV_TEST=entry" PCMDT-ENV-ENTRY+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD-OUT$ PCMDT-ENTRY-OUT 8 T$=
   PCMDT-ERR-LEN 0 T= ;

: PCMDT-RUN-INHERITED-ENV ( -- )
   PROC-CMD-RESET
   s" test/process-env-child.f" >LEN PROC-CMD-ARG+
   s" HABU_PROC_ENV_TEST" s" alpha" PCMDT-ENV+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD-OUT$ PCMDT-INHERIT-EXPECTED$ T$= ;

: PCMDT-RUN-TIMEOUT-OUTCOME ( -- )
   PROC-CMD-RESET
   s" 5" >LEN PROC-CMD-ARG+
   s" /bin/sleep" PCMDT-SHORT-TIMEOUT-MS PCMDT-RUN-OUTCOME
   T-OUTCOME-TIMEOUT
   PROC-CMD-RC@ RC>N 137 T=
   PCMDT-OUT-LEN 0 T=
   PCMDT-ERR-LEN 0 T= ;

: PCMDT-RUN-YES-TRUNCATED ( -- )
   PROC-CMD-RESET
   s" /usr/bin/yes" PCMDT-CMD-TIMEOUT-MS PCMDT-PROC-RUN-RC drop ;

: PCMDT-TOO-MANY-ARGS ( -- )
   PROC-CMD-RESET
   0 begin dup PROC-CMD-ARG-MAX < while
      s" x" >LEN PROC-CMD-ARG+
      1+
   repeat drop
   s" overflow" >LEN PROC-CMD-ARG+ ;

: PCMDT-BAD-ENV-NAME ( -- )
   PROC-CMD-RESET
   s" BAD=NAME" s" x" PCMDT-ENV+ ;

: PCMDT-BAD-ENV-ENTRY ( -- )
   PROC-CMD-RESET
   s" MISSING_EQUALS" PCMDT-ENV-ENTRY+ ;

: PROCESS-COMMAND-TEST-MAIN ( -- )
   T-RESET
   PCMDT-RUN-PRINTF
   PCMDT-RUN-STDIN
   PCMDT-RUN-HERMETIC-ENV
   PCMDT-RUN-ENTRY-ENV
   PCMDT-RUN-INHERITED-ENV
   PCMDT-RUN-TIMEOUT-OUTCOME
   [: PCMDT-RUN-YES-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   [: PCMDT-TOO-MANY-ARGS ;] E-PROC-OUTPUT TTHROWSQ
   [: PCMDT-BAD-ENV-NAME ;] E-PROC-ENV TTHROWSQ
   [: PCMDT-BAD-ENV-ENTRY ;] E-PROC-ENV TTHROWSQ
   T-REPORT
   s" process-command-test: ok" type cr ;

PROCESS-COMMAND-TEST-MAIN
