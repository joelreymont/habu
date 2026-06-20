\ process-env-test.f - focused tests for lib/process-env.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/process-env-test.f

create PET-OUT 128 allot
create PET-ERR 128 allot
create PET-PATH FS-PATH-CAP allot
create PET-ENV-OUT 97 c, 108 c, 112 c, 104 c, 97 c, 10 c, 10 c,
create PET-EMPTY-OUT 10 c, 10 c,

: PET-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: PET-RUN-ENV-CHILD ( -- )
   PET-RESET
   s" test/process-env-child.f" PROC-ARGV+
   s" HABU_PROC_ENV_TEST" s" alpha" PROC-ENV+
   s" bin/hb" PET-OUT 128 PET-ERR 128 1000 RUN-ARGV-ENV-CAPTURE
   0 T= 0 T= 7 T=
   PET-OUT 7 PET-ENV-OUT 7 T$=
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T= ;

: PET-RUN-EMPTY-ENV-CHILD ( -- )
   PET-RESET
   s" test/process-env-child.f" PROC-ARGV+
   s" bin/hb" PET-OUT 128 PET-ERR 128 1000 RUN-ARGV-ENV-CAPTURE
   0 T= 0 T= 2 T=
   PET-OUT 2 PET-EMPTY-OUT 2 T$= ;

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
