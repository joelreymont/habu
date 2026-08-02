\ aot-wid-suite.f - protected-WID boot-integration regression (TFAM 2b-v(f)).
\
\ What this locks: the protected-WID registry that an engine bakes into its
\ ahead-of-time (AOT) section must be restored at engine STARTUP, before any
\ batch program (piped stdin or --load file) runs. Batch input is the primary
\ path LLM-generated Forth takes. If the restore ran too late, a batch program
\ could publish a definition into a sealed constructor word-list and the guard
\ would never fire.
\
\ Cold startup restores the baked entries immediately after clearing the live
\ registry, before the cold prefix registers its constructor families and before
\ any user source runs. Warm snapshot startup skips both the clear and the baked
\ replay, preserving the registry restored from the snapshot DATA image.
\
\ How it is proven: test/aot-wid-build.f is spawned in a child process with a
\ private HB_TMP; it builds a throwaway `hb-pwid` engine whose AOT registry holds
\ two baked word-list ids (300 and 70000). This suite then probes hb-pwid on the
\ real batch paths:
\   - the two baked ids read back from PROT-WID-OFF slots 0 and 1,
\   - the final count is greater than two and slot 2 rejects publication on both
\     batch paths, proving cold-prefix registrations append after the baked ids,
\   - WIDN has advanced past the largest baked id (70000),
\   - publishing into either baked id exits 84 with the protected-publish
\     diagnostic, on BOTH stdin and --load,
\   - an ordinary user word-list define still exits 0,
\   - and the shipped engine (control) protects neither id, so the exit-84
\     behaviour comes from the baked registry, not the engine baseline.
\
\ The suite also builds a corrupt-count variant and proves validation fails
\ before publication with the complete newline-terminated boot diagnostic.
\
\ Heavy child build (~12 s): registered as `TEST:SUITE aot-wid-restore` in
\ test/gate-stdlib-cases.f, so it runs in the standalone stdlib gate (a required
\ master gate), like the
\ heavy-build suite, not the fast tail-process
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
create BAD-ROOT-BUF FS-PATH-CAP allot   variable BAD-ROOT-U
create BAD-HB-BUF FS-PATH-CAP allot     variable BAD-HB-U
create FORGE-BUF FS-PATH-CAP allot    variable FORGE-U

: ROOT$ ( -- ptr u8 n )   ROOT-BUF ROOT-U @ ;
: HBPWID$ ( -- ptr u8 n ) HBPWID-BUF HBPWID-U @ ;
: BAD-ROOT$ ( -- ptr u8 n ) BAD-ROOT-BUF BAD-ROOT-U @ ;
: BAD-HB$ ( -- ptr u8 n )   BAD-HB-BUF BAD-HB-U @ ;
: FORGE$ ( -- ptr u8 n )  FORGE-BUF FORGE-U @ ;
: PLAIN$ ( -- ptr u8 n )  s" bin/hb" ;      \ the shipped engine = the engine under test
: ERR$ ( -- ptr u8 n )    ERR ERR-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-aot-wid" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-pwid" HBPWID-BUF JOIN-PATH HBPWID-U !
   s" habu-aot-wid-corrupt" TMPDIR-MKDIR {: ba:ptr bu:n :}
   ba BAD-ROOT-BUF bu BYTE-COPY  bu BAD-ROOT-U !
   BAD-ROOT$ CLEANUP-TREE+
   BAD-ROOT$ s" hb-pwid" BAD-HB-BUF JOIN-PATH BAD-HB-U !
   ROOT$ s" forge.f" FORGE-BUF JOIN-PATH FORGE-U ! ;

\ --- spawn the variant builder as a child with a private HB_TMP ---
: BUILD-IN ( ptr u8 n bool -- ) {: root:ptr rootu:n corrupt:bool :}
   PROC-ENV-RESET
   s" HB_TMP" >LEN root rootu >LEN PROC-ENV+
   corrupt if s" HABU_AOT_PWID_CORRUPT" >LEN s" 1" >LEN PROC-ENV+ then
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

: BUILD-VARIANT ( -- )
   ROOT$ 0 0= 0= BUILD-IN ;

: BUILD-CORRUPT ( -- )
   BAD-ROOT$ 0 0= BUILD-IN ;

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
: FORGE-E2$ ( -- ptr u8 n )    s" data-base PROT-WID-OFF + 8 + @ $FFFFFFFF and set-current : FOO ( -- n ) 1 ;" ;
: PROBE-E0$ ( -- ptr u8 n )    s" data-base PROT-WID-OFF + @ $FFFFFFFF and . " ;
: PROBE-E1$ ( -- ptr u8 n )    s" data-base PROT-WID-OFF + 4 + @ $FFFFFFFF and . " ;
: PROBE-N$ ( -- ptr u8 n )     s" data-base PROT-WID-N-CELL + @ . " ;
: PROBE-WORDLIST$ ( -- ptr u8 n )  s" wordlist . " ;

: PROBE-VARIANT ( -- )
   s" baked wid 300 restored at PROT-WID-OFF slot 0 before batch input" T-LABEL
   HBPWID$ PROBE-E0$ READ-N  WID-A T=
   s" baked wid 70000 (> 65535) restored at PROT-WID-OFF slot 1" T-LABEL
   HBPWID$ PROBE-E1$ READ-N  WID-B T=
   s" cold-prefix protected WIDs append after the two baked entries" T-LABEL
   HBPWID$ PROBE-N$ READ-N  2 >  TTRUE
   s" publish into protected slot 2 exits 84 (--load)" T-LABEL
   HBPWID$ FORGE-E2$ FORGE-LOAD  ASSERT-REJECT
   s" publish into protected slot 2 exits 84 (stdin)" T-LABEL
   HBPWID$ FORGE-E2$ FORGE-STDIN  ASSERT-REJECT
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

: PROBE-CORRUPT ( -- )
   BUILD-CORRUPT
   s" corrupt protected-WID variant builds" T-LABEL
   RC @ 0 T=
   RC @ 0 <> if s" aot-wid-suite: corrupt builder stderr:" type cr  ERR$ type cr  RC @ throw then
   s" corrupt protected-WID variant exists" T-LABEL
   BAD-HB$ EXISTS? TTRUE
   BAD-HB$ s" " FORGE-LOAD
   s" corrupt protected-WID count exits ENGINE-ERROR:AOT-SEED" T-LABEL
   EXITED @ TTRUE  RC @ ENGINE-ERROR:AOT-SEED T=
   s" corrupt protected-WID count prints the named diagnostic" T-LABEL
   ERR$ s" hb: AOT protected-WID corrupt" CONTAINS? TTRUE
   s" corrupt protected-WID diagnostic ends in newline" T-LABEL
   ERR-U @ 0 > TTRUE
   ERR-U @ 0 > if ERR ERR-U @ 1- + c@ 10 T= then ;

\ AOT DATA-reserve span guard (dot habu-guard-aot-data-49de2ee6): the sibling
\ seed-pass forge test/aot-data-span-forge.f builds an oversized-span variant and
\ PTY-boots it (the reserve+guard only run on interactive REPL entry), proving a
\ forged span dies named/exit-82 while the legal span still boots. It is a
\ spawn-only helper; run it as a child here and gate on its exit code.
: PROBE-DATA-SPAN ( -- )
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-data-span-forge.f" >LEN PROC-ARGV+
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   s" AOT data-span guard: forged span dies named, legal span boots" T-LABEL
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 0= TTRUE ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !
            s" aot-wid-suite: data-span child failed rc " type  c RC>N .  cr
            s" aot-wid-suite: data-span child stdout:" type cr  OUT OUT-U @ type cr
            s" aot-wid-suite: data-span child stderr:" type cr  ERR$ type cr
            0 0= 0= TTRUE ENDOF
   ;MATCH ;

: BODY ( -- )
   SETUP
   BUILD-VARIANT
   s" aot-wid variant engine builds cleanly" T-LABEL
   RC @ 0 T=
   RC @ 0 <> if s" aot-wid-suite: builder stderr:" type cr  ERR$ type cr  RC @ throw then
   s" hb-pwid variant exists after build" T-LABEL
   HBPWID$ EXISTS? TTRUE
   PROBE-VARIANT
   PROBE-CONTROL
   PROBE-CORRUPT
   PROBE-DATA-SPAN ;

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
