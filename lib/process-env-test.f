\ process-env-test.f - focused tests for lib/process-env.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/process-env-test.f

4096 constant PET-CAP
131072 constant PET-EARLY-IN-CAP

create PET-OUT PET-CAP allot
create PET-ERR PET-CAP allot
create PET-PATH FS-PATH-CAP allot
create PET-EARLY-IN PET-EARLY-IN-CAP allot
create PET-ENV-OUT 97 c, 108 c, 112 c, 104 c, 97 c, 10 c, 10 c, 10 c,
create PET-EMPTY-OUT 10 c, 10 c, 10 c,
variable PET-I

: PET-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: PET-EARLY-IN! ( -- )
   0 PET-I !
   begin PET-I @ PET-EARLY-IN-CAP < while
      97 PET-EARLY-IN PET-I @ + c!
      PET-I @ 1+ PET-I !
   repeat ;

: PET-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: PET-OUTCOME>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: PET-FIND>N ( len bool -- n bool ) {: gotu found :}
   gotu LEN>N found ;

: PET-ENV+ ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu val:ptr valu :}
   name nameu >LEN val valu >LEN PROC-ENV+ ;

: PET-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-ENV-CAPTURE
   PET-CAPTURE>N ;

: PET-STDIN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS
   RUN-ARGV-ENV-STDIN-CAPTURE PET-CAPTURE>N ;

: PET-STDIN-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   PET-OUTCOME>N ;

: PET-FIND-IN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n bool )
   {: cmd:ptr cmdu path:ptr pathu dst:ptr :}
   cmd cmdu >LEN path pathu >LEN dst FIND-EXECUTABLE-IN-PATH
   PET-FIND>N ;

: PET-RESOLVE ( ptr u8 n ptr u8 -- n )
   {: cmd:ptr cmdu dst:ptr :}
   cmd cmdu >LEN dst RESOLVE-EXECUTABLE LEN>N ;

: PET-RUN-ENV-CHILD ( -- )
   PET-RESET
   s" test/process-env-child.f"  >LEN PROC-ARGV+
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   s" bin/hb" PET-OUT PET-CAP PET-ERR PET-CAP 1000 PET-CAPTURE
   0 T= 0 T= 8 T=
   PET-OUT 8 PET-ENV-OUT 8 T$=
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T= ;

: PET-RUN-EMPTY-ENV-CHILD ( -- )
   PET-RESET
   s" test/process-env-child.f"  >LEN PROC-ARGV+
   s" bin/hb" PET-OUT PET-CAP PET-ERR PET-CAP 1000 PET-CAPTURE
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
   s" test/process-env-child.f"  >LEN PROC-ARGV+
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" PET-OUT PET-CAP PET-ERR PET-CAP 1000 PET-CAPTURE
   0 T= 0 T= {: outu :}
   PET-OUT outu PET-INHERIT-EXPECTED$ T$= ;

: PET-RUN-ENV-STDIN-OUTCOME ( -- )
   PET-RESET
   s" /bin/cat" s" env-stdin" PET-OUT PET-CAP PET-ERR PET-CAP 1000 PET-STDIN-OUTCOME
   0 T= PROC-OUTCOME-EXIT T= 0 T= 9 T=
   PET-OUT 9 s" env-stdin" T$= ;

: PET-RUN-ENV-STDIN-FALSE-LARGE ( -- )
   PET-RESET
   PET-EARLY-IN!
   s" /usr/bin/false" PET-EARLY-IN PET-EARLY-IN-CAP
   PET-OUT PET-CAP PET-ERR PET-CAP 1000 PET-STDIN-CAPTURE
   1 T= 0 T= 0 T= ;

: PET-RUN-ENV-STDIN-OUTCOME-FALSE-LARGE ( -- )
   PET-RESET
   PET-EARLY-IN!
   s" /usr/bin/false" PET-EARLY-IN PET-EARLY-IN-CAP
   PET-OUT PET-CAP PET-ERR PET-CAP 1000 PET-STDIN-OUTCOME
   1 T= PROC-OUTCOME-EXIT T= 0 T= 0 T= ;

: PET-BAD-ENV-NAME ( -- )
   PET-RESET
   s" BAD=NAME" s" x" PET-ENV+ ;

: PET-BAD-ENV-ENTRY ( -- )
   PET-RESET
   s" MISSING_EQUALS"  >LEN PROC-ENV-ENTRY+ ;

: PET-BAD-ENV-EMPTY ( -- )
   PET-RESET
   s" " s" x" PET-ENV+ ;

: PET-PATH-FIND-HB ( -- )
   s" hb" s" bin" PET-PATH PET-FIND-IN-PATH TTRUE
   PET-PATH swap s" bin/hb" T$= ;

: PET-PATH-DIRECT-HB ( -- )
   s" bin/hb" s" nowhere" PET-PATH PET-FIND-IN-PATH TTRUE
   PET-PATH swap s" bin/hb" T$= ;

: PET-PATH-MISSING ( -- )
   s" no-habu-process-env-test" s" bin" PET-PATH PET-FIND-IN-PATH TFALSE
   drop ;

: PET-RESOLVE-MISSING ( -- )
   s" no-habu-process-env-test" PET-PATH PET-RESOLVE drop ;

: PROCESS-ENV-TEST-MAIN ( -- )
   T-RESET
   PET-RUN-ENV-CHILD
   PET-RUN-EMPTY-ENV-CHILD
   PET-RUN-INHERIT-ENV-CHILD
   PET-RUN-ENV-STDIN-OUTCOME
   PET-RUN-ENV-STDIN-FALSE-LARGE
   PET-RUN-ENV-STDIN-OUTCOME-FALSE-LARGE
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
