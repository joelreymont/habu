\ process-env-test.f - focused tests for lib/process-env.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/process-env-test.f

4096 constant PET-CAP

create PET-OUT PET-CAP allot
create PET-ERR PET-CAP allot
create PET-PATH FS-PATH-CAP allot
create PET-ENV-OUT 97 c, 108 c, 112 c, 104 c, 97 c, 10 c, 10 c, 10 c,
create PET-EMPTY-OUT 10 c, 10 c, 10 c,

: PET-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: PET-RUN-ENV-CHILD ( -- )
   PET-RESET
   s" test/process-env-child.f" PROC-ARGV+
   s" HABU_PROC_ENV_TEST" s" alpha" PROC-ENV+
   s" bin/hb" PET-OUT PET-CAP PET-ERR PET-CAP 1000 RUN-ARGV-ENV-CAPTURE
   0 T= 0 T= 8 T=
   PET-OUT 8 PET-ENV-OUT 8 T$=
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T= ;

: PET-RUN-EMPTY-ENV-CHILD ( -- )
   PET-RESET
   s" test/process-env-child.f" PROC-ARGV+
   s" bin/hb" PET-OUT PET-CAP PET-ERR PET-CAP 1000 RUN-ARGV-ENV-CAPTURE
   0 T= 0 T= 3 T=
   PET-OUT 3 PET-EMPTY-OUT 3 T$= ;

: PET-INHERIT-EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" alpha" SB-APPEND
   10 SB-APPEND-C
   s" HOME" GETENV SB-APPEND
   10 SB-APPEND-C
   s" PATH" GETENV SB-APPEND
   10 SB-APPEND-C
   SB$ ;

: PET-RUN-INHERIT-ENV-CHILD ( -- )
   PET-RESET
   s" test/process-env-child.f" PROC-ARGV+
   s" HABU_PROC_ENV_TEST" s" alpha" PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" PET-OUT PET-CAP PET-ERR PET-CAP 1000 RUN-ARGV-ENV-CAPTURE
   0 T= 0 T= {: outu :}
   PET-OUT outu PET-INHERIT-EXPECTED$ T$= ;

: PET-RUN-ENV-STDIN-OUTCOME ( -- )
   PET-RESET
   s" /bin/cat" s" env-stdin" PET-OUT PET-CAP PET-ERR PET-CAP 1000 RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   0 T= PROC-OUTCOME-EXIT T= 0 T= 9 T=
   PET-OUT 9 s" env-stdin" T$= ;

: PET-BAD-ENV-NAME ( -- )
   PET-RESET
   s" BAD=NAME" s" x" PROC-ENV+ ;

: PET-BAD-ENV-ENTRY ( -- )
   PET-RESET
   s" MISSING_EQUALS" PROC-ENV-ENTRY+ ;

: PET-BAD-ENV-EMPTY ( -- )
   PET-RESET
   s" " s" x" PROC-ENV+ ;

: PET-PATH-FIND-HB ( -- )
   s" hb" s" bin" PET-PATH FIND-EXECUTABLE-IN-PATH TTRUE
   PET-PATH swap s" bin/hb" T$= ;

: PET-PATH-DIRECT-HB ( -- )
   s" bin/hb" s" nowhere" PET-PATH FIND-EXECUTABLE-IN-PATH TTRUE
   PET-PATH swap s" bin/hb" T$= ;

: PET-PATH-MISSING ( -- )
   s" no-habu-process-env-test" s" bin" PET-PATH FIND-EXECUTABLE-IN-PATH TFALSE
   drop ;

: PET-RESOLVE-MISSING ( -- )
   s" no-habu-process-env-test" PET-PATH RESOLVE-EXECUTABLE drop ;

: PROCESS-ENV-TEST-MAIN ( -- )
   T-RESET
   PET-RUN-ENV-CHILD
   PET-RUN-EMPTY-ENV-CHILD
   PET-RUN-INHERIT-ENV-CHILD
   PET-RUN-ENV-STDIN-OUTCOME
   ['] PET-BAD-ENV-NAME E-PROC-ENV TTHROWS
   ['] PET-BAD-ENV-ENTRY E-PROC-ENV TTHROWS
   ['] PET-BAD-ENV-EMPTY E-PROC-ENV TTHROWS
   PET-PATH-FIND-HB
   PET-PATH-DIRECT-HB
   PET-PATH-MISSING
   ['] PET-RESOLVE-MISSING E-PROC-PATH TTHROWS
   T-REPORT
   s" process-env-test: ok" type cr ;

PROCESS-ENV-TEST-MAIN
