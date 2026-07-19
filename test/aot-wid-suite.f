\ aot-wid-suite.f - protected-WID boot-integration regression (TFAM 2b-v(f)).
\
\ What this locks: the protected-WID registry that an engine bakes into its
\ ahead-of-time (AOT) section must be restored at engine STARTUP, before any
\ batch program (piped stdin or --load file) runs. Batch input is the primary
\ path LLM-generated Forth takes. If the restore ran too late, a batch program
\ could publish a definition into a sealed constructor word-list and the guard
\ would never fire.
\
\ The restore currently runs inside EM-STARTUP-RUNTIME-STATE (src/habu/habu2.f),
\ via COLD-RESET -> the RESTORE-HOOK installed by EM-AOT-RESTORE-HOOK-INIT
\ (habu2.f:3628). That single hook line is load-bearing: neutralizing it moves
\ the restore back past the batch interpret loop and turns every probe below red
\ (registry empty, WIDN not advanced, forge into a baked wid exits 0 not 84).
\
\ How it is proven: test/aot-wid-build.f is spawned in a child process with a
\ private HB_TMP; it builds a throwaway `hb-pwid` engine whose AOT registry holds
\ two baked word-list ids (300 and 70000). This suite then probes hb-pwid on the
\ real batch paths:
\   - the two baked ids read back from PROT-WID-OFF slots 0 and 1 (they are
\     restored FIRST, ahead of the engine's own boot-time constructor families),
\   - WIDN has advanced past the largest baked id (70000),
\   - publishing into either baked id exits 84 with the protected-publish
\     diagnostic, on BOTH stdin and --load,
\   - an ordinary user word-list define still exits 0,
\   - and the shipped engine (control) protects neither id, so the exit-84
\     behaviour comes from the baked registry, not the engine baseline.
\
\ Note on counts: an earlier revision of this test asserted the registry count
\ was exactly 2 with a plain-engine baseline of 0. The engine now registers
\ boot-time protected word-lists for its own constructor families (a plain
\ bin/hb shows six), so the exact-count proxy is stale. Reading the two baked
\ ids back from slots 0 and 1 is the direct, stable proof that the restore ran
\ and ran before those boot-time families were added.
\
\ Heavy child build (~12 s): registered as `TEST:SUITE aot-wid-restore` in
\ test/gate-stdlib-cases.f and listed in SC-MANUAL-TABLE (suite-coverage-lint),
\ so it runs in the standalone stdlib gate (a required master gate), like the
\ sibling heavy-build suite test/owner-wid-snapshot.f - not the fast tail-process
\ fork tier, whose perf ratchet the build cost would exceed. Run standalone:
\ bin/hb --load test/aot-wid-suite.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package AOT-WID-SUITE

\ Test vectors - mirror the ids baked by test/aot-wid-build.f.
300   constant WID-A                 \ baked protected word-list id > 255
70000 constant WID-B                 \ baked protected word-list id > 65535
ENGINE-ERROR:SEAL-PACKAGE constant FORGE-RC   \ publish-into-protected exit code (84)

$8000 constant CAP                   \ build + probe stdout/stderr capture
2048  constant FORGE-CAP             \ stdin-piped forge source
240000 constant BUILD-TIMEOUT-MS
30000  constant PROBE-TIMEOUT-MS

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create FIN FORGE-CAP allot   variable FIN-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC
variable EXITED

create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U
create HBPWID-BUF FS-PATH-CAP allot   variable HBPWID-U
create FORGE-BUF FS-PATH-CAP allot    variable FORGE-U

: ROOT$ ( -- ptr u8 n )   ROOT-BUF ROOT-U @ ;
: HBPWID$ ( -- ptr u8 n ) HBPWID-BUF HBPWID-U @ ;
: FORGE$ ( -- ptr u8 n )  FORGE-BUF FORGE-U @ ;
: PLAIN$ ( -- ptr u8 n )  s" bin/hb" ;      \ the shipped engine = the engine under test
: ERR$ ( -- ptr u8 n )    ERR ERR-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-aot-wid" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-pwid" HBPWID-BUF JOIN-PATH HBPWID-U !
   ROOT$ s" forge.f" FORGE-BUF JOIN-PATH FORGE-U ! ;

\ --- spawn the variant builder as a child with a private HB_TMP ---
: BUILD-VARIANT ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-wid-build.f" >LEN PROC-ARGV+
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

\ --- forge child spawn + outcome capture (parameterised by engine) ---
: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: FORGE-LOAD ( ptr u8 n ptr u8 n -- ) {: e:ptr eu:n s:ptr su:n :}   \ e=engine s=source, run as --load
   FORGE$ s su WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   FORGE$ >LEN PROC-ARGV+
   e eu >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  STORE! ;

: FORGE-STDIN ( ptr u8 n ptr u8 n -- ) {: e:ptr eu:n s:ptr su:n :}   \ same source piped on stdin
   su FORGE-CAP > if E-FS-CAPACITY throw then
   s FIN su BYTE-COPY  su FIN-U !
   PROC-ARGV-RESET
   e eu >LEN  FIN FIN-U @ >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  STORE! ;

: PARSE-OUT ( -- n )                 \ child stdout -> number, fail the test if not numeric
   OUT OUT-U @ TRIM STR>NUMBER? MATCH option
     some OF ENDOF
     none OF T-FAIL 0 ENDOF
   ;MATCH ;

: READ-N ( ptr u8 n ptr u8 n -- n )  \ e=engine s=probe-source, run --load, require clean exit, parse stdout
   FORGE-LOAD
   EXITED @ TTRUE  RC @ 0 T=
   PARSE-OUT ;

: ASSERT-REJECT ( -- )               \ child exited 84 naming the protected-publish guard
   EXITED @ TTRUE
   RC @ FORGE-RC T=
   ERR$ s" hb: cannot publish into protected word" CONTAINS? TTRUE ;

: ASSERT-OK ( -- )                   \ child exited cleanly
   EXITED @ TTRUE
   RC @ 0 T= ;

\ --- forge / probe sources (interpreted by the child engine) ---
: FORGE-300$ ( -- ptr u8 n )   s" 300 set-current : FOO ( -- n ) 1 ;" ;
: FORGE-70000$ ( -- ptr u8 n ) s" 70000 set-current : FOO ( -- n ) 1 ;" ;
: FORGE-USER$ ( -- ptr u8 n )  s" wordlist set-current : FOO ( -- n ) 1 ;" ;
: PROBE-E0$ ( -- ptr u8 n )    s" data-base PROT-WID-OFF + @ $FFFFFFFF and . " ;
: PROBE-E1$ ( -- ptr u8 n )    s" data-base PROT-WID-OFF + 4 + @ $FFFFFFFF and . " ;
: PROBE-WORDLIST$ ( -- ptr u8 n )  s" wordlist . " ;

: PROBE-VARIANT ( -- )
   s" baked wid 300 restored at PROT-WID-OFF slot 0 before batch input" T-LABEL
   HBPWID$ PROBE-E0$ READ-N  WID-A T=
   s" baked wid 70000 (> 65535) restored at PROT-WID-OFF slot 1" T-LABEL
   HBPWID$ PROBE-E1$ READ-N  WID-B T=
   s" WIDN advanced past the largest baked wid before batch input" T-LABEL
   HBPWID$ PROBE-WORDLIST$ READ-N  WID-B >  TTRUE
   s" publish into baked wid 300 exits 84 (--load)" T-LABEL
   HBPWID$ FORGE-300$ FORGE-LOAD  ASSERT-REJECT
   s" publish into baked wid 300 exits 84 (stdin)" T-LABEL
   HBPWID$ FORGE-300$ FORGE-STDIN  ASSERT-REJECT
   s" publish into baked wid 70000 exits 84 (--load)" T-LABEL
   HBPWID$ FORGE-70000$ FORGE-LOAD  ASSERT-REJECT
   s" publish into baked wid 70000 exits 84 (stdin)" T-LABEL
   HBPWID$ FORGE-70000$ FORGE-STDIN  ASSERT-REJECT
   s" a user word-list define into an unprotected wid exits 0 (--load)" T-LABEL
   HBPWID$ FORGE-USER$ FORGE-LOAD  ASSERT-OK ;

: PROBE-CONTROL ( -- )
   s" shipped engine does not protect wid 300 (control, --load)" T-LABEL
   PLAIN$ FORGE-300$ FORGE-LOAD  ASSERT-OK
   s" shipped engine does not protect wid 70000 (control, --load)" T-LABEL
   PLAIN$ FORGE-70000$ FORGE-LOAD  ASSERT-OK
   s" shipped engine registry does not begin with the baked ids (control)" T-LABEL
   PLAIN$ PROBE-E0$ READ-N  WID-A <>  TTRUE ;

: BODY ( -- )
   SETUP
   BUILD-VARIANT
   s" aot-wid variant engine builds cleanly" T-LABEL
   RC @ 0 T=
   RC @ 0 <> if s" aot-wid-suite: builder stderr:" type cr  ERR$ type cr  RC @ throw then
   s" hb-pwid variant exists after build" T-LABEL
   HBPWID$ EXISTS? TTRUE
   PROBE-VARIANT
   PROBE-CONTROL ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-wid-suite: ok" type cr ;

;package

AOT-WID-SUITE:RUN
