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

\ Inspect full capacities: empty OUT$/ERR$ alone cannot prove erasure. The
\ storage is the context's, so the whole wipe row is stated once, on a handle,
\ and run for the shared command below and for a declared one.
package CMD

: WIPE-ZERO? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 u 0 ?do a i + c@ or loop 0= ;

: WIPE-TAINT ( ptr ptr u8 -- ) {: h:ptr :}
   165 h IN-BUF IN-CAP SPAN:MAKE SPAN:FILL
   165 h OUT-BUF OUT-CAP SPAN:MAKE SPAN:FILL
   165 h ERR-BUF ERR-CAP SPAN:MAKE SPAN:FILL ;

public

: WIPE-CHECK ( ptr ptr u8 -- ) {: h:ptr :}
   h IN-BUF IN-CAP WIPE-ZERO? TTRUE
   h OUT-BUF OUT-CAP WIPE-ZERO? TTRUE
   h ERR-BUF ERR-CAP WIPE-ZERO? TTRUE
   h IN-LEN-CELL @ 0 T=
   h OUT$ nip 0 T= h ERR$ nip 0 T= ;

: TEST-WIPE ( ptr ptr u8 -- ) {: h:ptr :}
   h WIPE h WIPE-CHECK              \ safe before the first run
   h WIPE-TAINT h RESET             \ RESET still only forgets live lengths
   h IN-BUF c@ 165 T=
   h OUT-BUF c@ 165 T=
   h ERR-BUF c@ 165 T=
   h s" staged input" >LEN IN!
   h s" /bin/cat" >LEN PCMDT-CMD-TIMEOUT-MS >MS RUN-RC PCMDT-RC>N 0 T=
   h OUT$ s" staged input" T$=      \ RUN has not wiped its capture
   h s" /usr/bin/false" >LEN PCMDT-CMD-TIMEOUT-MS >MS RUN-RC PCMDT-RC>N 1 T=
   h s" kept" >LEN ARG+
   h s" WIPE_TEST" >LEN s" kept" >LEN ENV+
   h s" /usr" >LEN CWD! h ENV-HERMETIC
   h WIPE h WIPE-CHECK              \ stale tails and unused stderr are erased too
   h ARG-N-CELL @ 1 T=
   h ENV-N-CELL @ 1 T=
   h CWDZ h CWD-LEN-CELL @ s" /usr" T$=
   h INHERIT-CELL @ 0 T=
   h RC@ PCMDT-RC>N 1 T=            \ wiping does not reset the outcome
   h RESET
   h s" later input" >LEN IN!
   h s" /bin/cat" >LEN PCMDT-CMD-TIMEOUT-MS >MS RUN-RC PCMDT-RC>N 0 T=
   h OUT$ s" later input" T$=
   h WIPE h WIPE-CHECK
   h WIPE h WIPE-CHECK ;

;package

package PROC-CMD
public

: TEST-WIPE ( -- )
   CTX0 CMD:TEST-WIPE ;

: TEST-REFUSED-RUN ( -- )
   CTX0 s" " >LEN PCMDT-CMD-TIMEOUT-MS >MS CMD:RUN-RC drop ;

: TEST-WIPE-AFTER-REFUSAL ( -- )
   [: TEST-REFUSED-RUN ;] E-PROC-OUTPUT TTHROWSQ
   CTX0 CMD:WIPE CTX0 CMD:WIPE-CHECK ;

;package

\ ---- the same surface on a caller's own command ------------------------------
\ One declaration, one handle, no PROC-CMD word in sight: this is what a task
\ that must not share the command uses.
CMD:COMMAND PCMDT-CMD

: PCMDT-H-RUN-RC ( ptr ptr u8 ptr u8 n n -- n ) {: h:ptr path:ptr pathu timeout :}
   h path pathu >LEN timeout >MS CMD:RUN-RC PCMDT-RC>N ;

: PCMDT-H-PRINTF ( ptr ptr u8 -- ) {: h:ptr :}
   h CMD:RESET
   h s" %s:%s" >LEN CMD:ARG+
   h s" left" >LEN CMD:ARG+
   h s" right" >LEN CMD:ARG+
   h s" /usr/bin/printf" PCMDT-CMD-TIMEOUT-MS PCMDT-H-RUN-RC 0 T=
   h CMD:OUT$ s" left:right" T$=
   h CMD:ERR$ nip 0 T=
   PROC-ARGV-N @ COUNT>N 0 T=            \ the owned path writes no process-wide staging
   PROC-ENV-N @ COUNT>N 0 T= ;

: PCMDT-H-STDIN ( ptr ptr u8 -- ) {: h:ptr :}
   h CMD:RESET
   h s" cmd-stdin" >LEN CMD:IN!
   h s" /bin/cat" PCMDT-CMD-TIMEOUT-MS PCMDT-H-RUN-RC 0 T=
   h CMD:OUT$ s" cmd-stdin" T$= ;

: PCMDT-H-HERMETIC-ENV ( ptr ptr u8 -- ) {: h:ptr :}
   h CMD:RESET
   h CMD:ENV-HERMETIC
   h s" test/process-env-child.f" >LEN CMD:ARG+
   h s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN CMD:ENV+
   h s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-H-RUN-RC 0 T=
   h CMD:OUT$ PCMDT-ENV-OUT 8 T$=
   h CMD:ERR$ nip 0 T= ;

: PCMDT-H-ENTRY-ENV ( ptr ptr u8 -- ) {: h:ptr :}
   h CMD:RESET
   h CMD:ENV-HERMETIC
   h s" test/process-env-child.f" >LEN CMD:ARG+
   h s" HABU_PROC_ENV_TEST=entry" >LEN CMD:ENV-ENTRY+
   h s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-H-RUN-RC 0 T=
   h CMD:OUT$ PCMDT-ENTRY-OUT 8 T$= ;

: PCMDT-H-INHERITED-ENV ( ptr ptr u8 -- ) {: h:ptr :}
   h CMD:RESET
   h s" test/process-env-child.f" >LEN CMD:ARG+
   h s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN CMD:ENV+
   h s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-H-RUN-RC 0 T=
   h CMD:OUT$
   s" alpha" PCMDT-CHECK-LINE
   s" HOME" GETENV PCMDT-CHECK-LINE
   s" PATH" GETENV PCMDT-CHECK-LINE
   nip 0 T= ;

: PCMDT-H-RC-ARMS ( ptr ptr u8 -- ) {: h:ptr :}
   h CMD:RESET
   h s" /usr/bin/true" >LEN PCMDT-CMD-TIMEOUT-MS >MS CMD:RUN-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   h CMD:RESET
   h s" /usr/bin/false" >LEN PCMDT-CMD-TIMEOUT-MS >MS CMD:RUN-RC
   MATCH result ok OF drop 1 0 T= ENDOF err OF 1 T= ENDOF ;MATCH
   h CMD:RC@ MATCH result ok OF drop 1 0 T= ENDOF err OF 1 T= ENDOF ;MATCH ;

: PCMDT-H-TIMEOUT ( ptr ptr u8 -- ) {: h:ptr :}
   h CMD:RESET
   h s" 5" >LEN CMD:ARG+
   h s" /bin/sleep" >LEN PCMDT-SHORT-TIMEOUT-MS >MS CMD:RUN-OUTCOME
   T-OUTCOME-TIMEOUT
   h CMD:OUTCOME@ T-OUTCOME-TIMEOUT
   h CMD:OUT$ nip 0 T=
   h CMD:ERR$ nip 0 T= ;

: PCMDT-H-CWD ( ptr ptr u8 -- ) {: h:ptr :}
   CLEANUP-RESET
   s" hb-cmd-cwd" HB-TMP-MKDIR {: d:ptr du :}
   d du CLEANUP-TREE+
   du 256 <= TTRUE
   du 256 > if CLEANUP-RUN exit then
   d PCMDT-DIR du BYTE-COPY  du PCMDT-DIR-U !
   PCMDT-DIR PCMDT-DIR-U @ s" here.txt" PCMDT-REL JOIN-PATH PCMDT-REL-U !
   PCMDT-REL PCMDT-REL-U @ PCMDT-CWD-BODY$ WRITE-ALL
   h CMD:RESET
   h PCMDT-DIR PCMDT-DIR-U @ >LEN CMD:CWD!
   h s" here.txt" >LEN CMD:ARG+
   h s" /bin/cat" PCMDT-CMD-TIMEOUT-MS PCMDT-H-RUN-RC 0 T=
   h CMD:OUT$ PCMDT-CWD-BODY$ T$=
   s" here.txt" FILE? TFALSE
   CLEANUP-RUN ;

: PCMDT-H-CWD-MISSING ( -- )
   PCMDT-CMD CMD:RESET
   PCMDT-CMD s" /nonexistent-hb-cmd-dir" >LEN CMD:CWD! ;

: PCMDT-H-TOO-MANY-ARGS ( -- )
   PCMDT-CMD CMD:RESET
   0 begin dup CMD:ARG-MAX < while
      PCMDT-CMD s" x" >LEN CMD:ARG+
      1+
   repeat drop
   PCMDT-CMD s" overflow" >LEN CMD:ARG+ ;

\ The envp vector's last cell belongs to the NULL, so the ceiling is ENV-ROWS-1
\ caller rows. The refusal writes its line to stderr the way lib/process-env.f's
\ ceilings do - visible in this file's output, and the point of the row.
: PCMDT-H-TOO-MANY-ENVS ( -- )
   PCMDT-CMD CMD:RESET
   PCMDT-CMD CMD:ENV-HERMETIC
   0 begin dup CMD:ENV-ROWS 1 - < while
      PCMDT-CMD s" R" >LEN s" v" >LEN CMD:ENV+
      1+
   repeat drop
   PCMDT-CMD s" R" >LEN s" v" >LEN CMD:ENV+ ;

\ A command bound to storage the caller allocated itself: same surface, no
\ declaration. Both mappings go back before the row returns.
: PCMDT-BOUND-COMMAND ( -- )
   CMD:BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: b:ptr :}
   CMD:VEC-CELLS >COUNT MEM-ALLOC-CELLS {: h:ptr :}
   b h CMD:BIND
   h s" bound:%s" >LEN CMD:ARG+
   h s" storage" >LEN CMD:ARG+
   h s" /usr/bin/printf" PCMDT-CMD-TIMEOUT-MS PCMDT-H-RUN-RC 0 T=
   h CMD:OUT$ s" bound:storage" T$=
   b CMD:BYTES MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   h BYTE-VIEW CMD:VEC-CELLS cells MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

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
   PROC-CMD:TEST-WIPE-AFTER-REFUSAL
   PCMDT-CMD CMD:TEST-WIPE
   PCMDT-CMD PCMDT-H-PRINTF
   PCMDT-CMD PCMDT-H-STDIN
   PCMDT-CMD PCMDT-H-HERMETIC-ENV
   PCMDT-CMD PCMDT-H-ENTRY-ENV
   PCMDT-CMD PCMDT-H-INHERITED-ENV
   PCMDT-CMD PCMDT-H-RC-ARMS
   PCMDT-CMD PCMDT-H-TIMEOUT
   PCMDT-CMD PCMDT-H-CWD
   [: PCMDT-H-CWD-MISSING ;] E-PROC-PATH TTHROWSQ
   [: PCMDT-H-TOO-MANY-ARGS ;] E-PROC-OUTPUT TTHROWSQ
   [: PCMDT-H-TOO-MANY-ENVS ;] E-PROC-ENV TTHROWSQ
   PCMDT-BOUND-COMMAND
   T-REPORT
   s" process-command-test: ok" type cr ;

PROCESS-COMMAND-TEST-MAIN
