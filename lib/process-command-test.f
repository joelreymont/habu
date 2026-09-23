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
require lib/fs-mutate.f
require lib/process-command.f
require lib/test/outcome.f

create PCMDT-ENV-OUT 97 c, 108 c, 112 c, 104 c, 97 c, 10 c, 10 c, 10 c,
create PCMDT-ENTRY-OUT 101 c, 110 c, 116 c, 114 c, 121 c, 10 c, 10 c, 10 c,

5000 constant PCMDT-HB-TIMEOUT-MS
1000 constant PCMDT-CMD-TIMEOUT-MS
50 constant PCMDT-SHORT-TIMEOUT-MS

: PCMDT-RC>N ( result<n,n> -- n )   \ 0 on clean exit, else the completion code
   MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;

: PCMDT-PROC-RUN-RC ( ptr u8 n n -- n ) {: path:ptr pathu timeout :}
   path pathu >LEN timeout >MS PROC-CMD:RUN-RC PCMDT-RC>N ;

: PCMDT-RUN-OUTCOME ( ptr u8 n n -- outcome ) {: path:ptr pathu:n timeout:n :}
   path pathu >LEN timeout >MS PROC-CMD:RUN-OUTCOME ;

: PCMDT-ENV+ ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu val:ptr valu :}
   name nameu >LEN val valu >LEN PROC-CMD:ENV+ ;

: PCMDT-ENV-ENTRY+ ( ptr u8 n -- ) {: a:ptr u :}
   a u >LEN PROC-CMD:ENV-ENTRY+ ;

: PCMDT-IN! ( ptr u8 n -- ) {: a:ptr u :}
   a u >LEN PROC-CMD:IN! ;

: PCMDT-OUT-LEN ( -- n )
   PROC-CMD:OUT$ {: a:ptr u:n :} u ;

: PCMDT-ERR-LEN ( -- n )
   PROC-CMD:ERR$ {: a:ptr u:n :} u ;

: PCMDT-CHECK-LINE ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: out:ptr outu:n expected:ptr expectedu:n :}
   outu expectedu 1+ < if
      outu expectedu 1+ T=
      out outu exit
   then
   out expectedu expected expectedu T$=
   out expectedu + c@ 10 T=
   out expectedu 1+ + outu expectedu 1+ - ;

: PCMDT-CHECK-INHERITED-ENV ( -- )
   PROC-CMD:OUT$
   s" alpha" PCMDT-CHECK-LINE
   s" HOME" GETENV PCMDT-CHECK-LINE
   s" PATH" GETENV PCMDT-CHECK-LINE
   nip 0 T= ;

: PCMDT-RUN-PRINTF ( -- )
   PROC-CMD:RESET
   s" %s:%s" >LEN PROC-CMD:ARG+
   s" left" >LEN PROC-CMD:ARG+
   s" right" >LEN PROC-CMD:ARG+
   s" /usr/bin/printf" PCMDT-CMD-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD:OUT$ s" left:right" T$=
   PCMDT-ERR-LEN 0 T=
   PROC-ARGV-N @ COUNT>N 0 T=
   PROC-ENV-N @ COUNT>N 0 T= ;

: PCMDT-RUN-STDIN ( -- )
   PROC-CMD:RESET
   s" cmd-stdin" PCMDT-IN!
   s" /bin/cat" PCMDT-CMD-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD:OUT$ s" cmd-stdin" T$=
   PCMDT-ERR-LEN 0 T= ;

: PCMDT-RUN-HERMETIC-ENV ( -- )
   PROC-CMD:RESET
   PROC-CMD:ENV-HERMETIC
   s" test/process-env-child.f" >LEN PROC-CMD:ARG+
   s" HABU_PROC_ENV_TEST" s" alpha" PCMDT-ENV+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD:OUT$ PCMDT-ENV-OUT 8 T$=
   PCMDT-ERR-LEN 0 T= ;

: PCMDT-RUN-ENTRY-ENV ( -- )
   PROC-CMD:RESET
   PROC-CMD:ENV-HERMETIC
   s" test/process-env-child.f" >LEN PROC-CMD:ARG+
   s" HABU_PROC_ENV_TEST=entry" PCMDT-ENV-ENTRY+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PROC-CMD:OUT$ PCMDT-ENTRY-OUT 8 T$=
   PCMDT-ERR-LEN 0 T= ;

: PCMDT-RUN-INHERITED-ENV ( -- )
   PROC-CMD:RESET
   s" test/process-env-child.f" >LEN PROC-CMD:ARG+
   s" HABU_PROC_ENV_TEST" s" alpha" PCMDT-ENV+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-PROC-RUN-RC 0 T=
   PCMDT-CHECK-INHERITED-ENV ;

: PCMDT-RUN-TIMEOUT-OUTCOME ( -- )
   PROC-CMD:RESET
   s" 5" >LEN PROC-CMD:ARG+
   s" /bin/sleep" PCMDT-SHORT-TIMEOUT-MS PCMDT-RUN-OUTCOME
   T-OUTCOME-TIMEOUT
   PROC-CMD:RC@ MATCH result ok OF drop 1 0 T= ENDOF err OF 137 T= ENDOF ;MATCH   \ timeout reaped as SIGKILL -> err(137)

   PCMDT-OUT-LEN 0 T=
   PCMDT-ERR-LEN 0 T= ;

: PCMDT-RUN-YES-TRUNCATED ( -- )
   PROC-CMD:RESET
   s" /usr/bin/yes" PCMDT-CMD-TIMEOUT-MS PCMDT-PROC-RUN-RC drop ;

: PCMDT-TOO-MANY-ARGS ( -- )
   PROC-CMD:RESET
   0 begin dup PROC-CMD:ARG-MAX < while
      s" x" >LEN PROC-CMD:ARG+
      1+
   repeat drop
   s" overflow" >LEN PROC-CMD:ARG+ ;

: PCMDT-BAD-ENV-NAME ( -- )
   PROC-CMD:RESET
   s" BAD=NAME" s" x" PCMDT-ENV+ ;

: PCMDT-BAD-ENV-ENTRY ( -- )
   PROC-CMD:RESET
   s" MISSING_EQUALS" PCMDT-ENV-ENTRY+ ;

\ Direct both-arm coverage for the migrated result<n,n>: a clean exit MATCHes the
\ ok arm carrying 0; a nonzero exit MATCHes the err arm carrying the code.
: PCMDT-RUN-RC-OK ( -- )
   PROC-CMD:RESET
   s" /usr/bin/true" >LEN PCMDT-CMD-TIMEOUT-MS >MS PROC-CMD:RUN-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH ;

: PCMDT-RUN-RC-ERR ( -- )
   PROC-CMD:RESET
   s" /usr/bin/false" >LEN PCMDT-CMD-TIMEOUT-MS >MS PROC-CMD:RUN-RC
   MATCH result ok OF drop 1 0 T= ENDOF err OF 1 T= ENDOF ;MATCH ;

\ CWD!: the child runs from the given directory, proved by a relative-path
\ effect (cat of a file that exists only there), so a symlinked or slash-ended
\ temp base cannot fake a mismatch; the directory is registered for cleanup
\ before the run so a throw does not leak it. A missing directory is refused
\ before any spawn.
create PCMDT-DIR 256 allot   variable PCMDT-DIR-U
create PCMDT-REL 512 allot   variable PCMDT-REL-U
: PCMDT-CWD-BODY$ ( -- ptr u8 n )  s" from the private directory" ;

: PCMDT-RUN-CWD ( -- )
   CLEANUP-RESET
   s" hb-proc-cmd-cwd" HB-TMP-MKDIR {: d:ptr du :}
   d du CLEANUP-TREE+
   du 256 <= TTRUE
   du 256 > if CLEANUP-RUN exit then
   d PCMDT-DIR du BYTE-COPY  du PCMDT-DIR-U !
   PCMDT-DIR PCMDT-DIR-U @ s" here.txt" PCMDT-REL JOIN-PATH PCMDT-REL-U !
   PCMDT-REL PCMDT-REL-U @ PCMDT-CWD-BODY$ WRITE-ALL
   PROC-CMD:RESET
   PCMDT-DIR PCMDT-DIR-U @ >LEN PROC-CMD:CWD!
   s" here.txt" >LEN PROC-CMD:ARG+
   s" /bin/cat" >LEN PCMDT-CMD-TIMEOUT-MS >MS PROC-CMD:RUN-RC PCMDT-RC>N 0 T=
   PROC-CMD:OUT$ PCMDT-CWD-BODY$ T$=
   s" here.txt" FILE? TFALSE
   CLEANUP-RUN ;

: PCMDT-CWD-MISSING ( -- )
   PROC-CMD:RESET
   s" /nonexistent-hb-proc-cmd-dir" >LEN PROC-CMD:CWD! ;

\ Inspect full capacities: empty OUT$/ERR$ alone cannot prove erasure.
package PROC-CMD

: WIPE-ZERO? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 u 0 ?do a i + c@ or loop 0= ;

: WIPE-CHECK ( -- )
   PROC-CMD-IN PROC-CMD-IN-CAP WIPE-ZERO? TTRUE
   PROC-CMD-OUT PROC-CMD-OUT-CAP WIPE-ZERO? TTRUE
   PROC-CMD-ERR PROC-CMD-ERR-CAP WIPE-ZERO? TTRUE
   PROC-CMD-IN-LEN @ LEN>N 0 T=
   OUT$ nip 0 T= ERR$ nip 0 T= ;

: WIPE-TAINT ( -- )
   165 PROC-CMD-IN PROC-CMD-IN-CAP SPAN:MAKE SPAN:FILL
   165 PROC-CMD-OUT PROC-CMD-OUT-CAP SPAN:MAKE SPAN:FILL
   165 PROC-CMD-ERR PROC-CMD-ERR-CAP SPAN:MAKE SPAN:FILL ;

: WIPE-REFUSED-RUN ( -- )
   s" " >LEN PCMDT-CMD-TIMEOUT-MS >MS RUN-RC drop ;

public

: TEST-WIPE ( -- )
   WIPE WIPE-CHECK                  \ safe before the first run
   WIPE-TAINT RESET                 \ RESET still only forgets live lengths
   PROC-CMD-IN c@ 165 T=
   PROC-CMD-OUT c@ 165 T=
   PROC-CMD-ERR c@ 165 T=
   s" staged input" >LEN IN!
   s" /bin/cat" >LEN PCMDT-CMD-TIMEOUT-MS >MS RUN-RC PCMDT-RC>N 0 T=
   OUT$ s" staged input" T$=       \ RUN has not wiped its capture
   s" /usr/bin/false" >LEN PCMDT-CMD-TIMEOUT-MS >MS RUN-RC PCMDT-RC>N 1 T=
   s" kept" >LEN ARG+
   s" WIPE_TEST" >LEN s" kept" >LEN ENV+
   s" /tmp" >LEN CWD! ENV-HERMETIC
   WIPE WIPE-CHECK                  \ stale tails and unused stderr are erased too
   PROC-CMD-ARG-N @ COUNT>N 1 T=
   PROC-CMD-ENV-N @ COUNT>N 1 T=
   PROC-CMD-CWD PROC-CMD-CWD-LEN @ LEN>N s" /tmp" T$=
   PROC-CMD-INHERIT @ 0 T=
   RC@ PCMDT-RC>N 1 T=             \ wiping does not reset the outcome
   RESET
   s" later input" >LEN IN!
   s" /bin/cat" >LEN PCMDT-CMD-TIMEOUT-MS >MS RUN-RC PCMDT-RC>N 0 T=
   OUT$ s" later input" T$=
   [: WIPE-REFUSED-RUN ;] E-PROC-OUTPUT TTHROWSQ
   WIPE WIPE-CHECK
   WIPE WIPE-CHECK ;

;package

: PROCESS-COMMAND-TEST-MAIN ( -- )
   T-RESET
   PROC-CMD:TEST-WIPE
   PCMDT-RUN-PRINTF
   PCMDT-RUN-STDIN
   PCMDT-RUN-HERMETIC-ENV
   PCMDT-RUN-ENTRY-ENV
   PCMDT-RUN-INHERITED-ENV
   PCMDT-RUN-RC-OK
   PCMDT-RUN-RC-ERR
   PCMDT-RUN-TIMEOUT-OUTCOME
   PCMDT-RUN-CWD
   [: PCMDT-CWD-MISSING ;] E-PROC-PATH TTHROWSQ
   [: PCMDT-RUN-YES-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   [: PCMDT-TOO-MANY-ARGS ;] E-PROC-OUTPUT TTHROWSQ
   [: PCMDT-BAD-ENV-NAME ;] E-PROC-ENV TTHROWSQ
   [: PCMDT-BAD-ENV-ENTRY ;] E-PROC-ENV TTHROWSQ
   T-REPORT
   s" process-command-test: ok" type cr ;

PROCESS-COMMAND-TEST-MAIN
