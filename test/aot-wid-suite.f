\ aot-wid-suite.f - protected-WID boot-integration regression (TFAM 2b-v(f)).
\
\ What this locks: the protected-WID bitmap that an engine bakes into its
\ ahead-of-time (AOT) section must be restored at engine STARTUP, before any
\ batch program (piped stdin or --load file) runs. Batch input is the primary
\ path LLM-generated Forth takes. If the restore ran too late, a batch program
\ could publish a definition into a sealed constructor word-list and the guard
\ would never fire.
\
\ The restore currently runs inside EM-STARTUP-RUNTIME-STATE (src/habu/habu2.f),
\ via COLD-RESET -> the RESTORE-HOOK installed by EM-AOT-RESTORE-HOOK-INIT.
\ That single hook line is load-bearing: neutralizing it moves the restore back
\ past the batch interpret loop and turns every probe below red (no shape tag, no
\ bits, WIDN not advanced, forge into a baked wid exits 0 not 84).
\
\ The registry is a WID-INDEXED BITMAP (dot habu-replace-the-protected-ca920a8f),
\ so what gets baked is a fixed-width image of a SET, behind a shape tag. The
\ three parts of that format are what the probes below read:
\
\   the tag        the restore release-publishes PROT-REG-TAG into the tag cell
\                  LAST, so a half-copied band is never observable as a bitmap;
\                  a restored engine whose tag cell does not carry it has not
\                  completed a restore.
\   the band       membership is one bit per wordlist id, read through the shared
\                  tools/prot-wid-probe.f - the same bits the engine's PROT-WID?
\                  routine tests. The probes assert WHICH ids came back, not how
\                  many, and assert that the neighbour of the highest baked id did
\                  NOT, which a smeared or mis-shifted restore would set.
\   the legacy leg the capture converts a table-era host's rows into bits when the
\                  tag cell holds a row count instead. Its guards are probed by
\                  the two refusal builds below. The row->bit conversion itself
\                  needs a table-era host, which no longer exists; that gap is
\                  recorded in test/aot-wid-build.f and owned by dot
\                  habu-retire-the-legacy-31ad57bc, which deletes the leg.
\
\ How it is proven: test/aot-wid-build.f is spawned in a child process with a
\ private HB_TMP; it builds a throwaway `hb-pwid` engine whose AOT band has two
\ extra word-list ids set (300 and 8000) on top of the ones the metabuild host
\ protects for itself. This suite then probes hb-pwid on the real batch paths, and
\ spawns the same builder twice more in its refusal modes.
\
\ Note on the negative leg: an earlier revision proved "not protected" by having
\ the same forge exit 0 against an unprotected id. Since the absent-package-context
\ reject landed, `set-current` into any word-list leaves no authenticated package,
\ so that program is refused whatever the id - with a different code (70) and a
\ different diagnostic. The discrimination is intact and is what ASSERT-NOT-PROTECTED
\ asserts; a separate case proves the child engine still defines normally, so the
\ exit-84 results cannot be read as "this engine refuses everything".
\
\ Note on counts: an earlier revision asserted the registry count was exactly 2
\ with a plain-engine baseline of 0. The engine registers boot-time protected
\ word-lists for its own constructor families, so an exact-count proxy is stale
\ the moment a family is added. Reading the two baked ids back BY ID is the direct,
\ stable proof, and it is what a bitmap makes cheap.
\
\ Cost: three child engine builds (~12 s each). It is registered as
\ `TEST:SUITE aot-wid-restore` in test/gate-stdlib-cases.f, so it runs in the
\ standalone stdlib gate
\ (a required master gate), like the sibling heavy-build suite
\ test/owner-wid-snapshot.f - not the fast tail-process fork tier, whose perf
\ ratchet the build cost would exceed. Run standalone:
\ bin/hb --load test/aot-wid-suite.f

require lib/errors.f
require lib/string.f
require lib/fmt.f
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
8000  constant WID-B                 \ baked protected word-list id high in the band
8001  constant WID-NEIGHBOUR         \ one id past WID-B: must come back UNprotected
70000 constant WID-OOR               \ far above the bound: the capture must refuse it
PROT-WID-LEGACY-MAX 1+ constant LEGACY-N-BAD   \ not a legacy row count at all
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
create REFUSE-BUF FS-PATH-CAP allot   variable REFUSE-U
create REFUSE-HB-BUF FS-PATH-CAP allot  variable REFUSE-HB-U

: ROOT$ ( -- ptr u8 n )   ROOT-BUF ROOT-U @ ;
: HBPWID$ ( -- ptr u8 n ) HBPWID-BUF HBPWID-U @ ;
: FORGE$ ( -- ptr u8 n )  FORGE-BUF FORGE-U @ ;
: REFUSE-ROOT$ ( -- ptr u8 n ) REFUSE-BUF REFUSE-U @ ;
: REFUSE-HB$ ( -- ptr u8 n )   REFUSE-HB-BUF REFUSE-HB-U @ ;
: PLAIN$ ( -- ptr u8 n )  s" bin/hb" ;      \ the shipped engine = the engine under test
: ERR$ ( -- ptr u8 n )    ERR ERR-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-aot-wid" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-pwid" HBPWID-BUF JOIN-PATH HBPWID-U !
   ROOT$ s" forge.f" FORGE-BUF JOIN-PATH FORGE-U ! ;

\ Each refusal build gets its own tree, so "no engine appeared" is a statement
\ about that build and not about a leftover from the good one.
: REFUSE-SETUP ( -- )
   s" habu-aot-wid-refuse" TMPDIR-MKDIR {: a:ptr u:n :}
   a REFUSE-BUF u BYTE-COPY  u REFUSE-U !
   REFUSE-ROOT$ CLEANUP-TREE+
   REFUSE-ROOT$ s" hb-pwid" REFUSE-HB-BUF JOIN-PATH REFUSE-HB-U ! ;

\ --- decimal text for the builder's environment knobs ---
create NUM-BUF 32 allot   variable NUM-U
: NUM$! ( n -- ) {: v:n :}
   SB-RESET  v FMT:SB-U  SB$ {: a:ptr u:n :}
   a NUM-BUF u BYTE-COPY  u NUM-U ! ;
: NUM$ ( -- ptr u8 n ) NUM-BUF NUM-U @ ;

\ --- spawn the variant builder as a child with a private HB_TMP ---
: BUILDER-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-wid-build.f" >LEN PROC-ARGV+ ;

: RUN-BUILDER ( -- )                 \ env already staged; captures rc/out/err
   PROC-ENV-INHERIT-MISSING
   BUILDER-ARGV
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: BUILD-VARIANT ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   RUN-BUILDER ;

\ A refusal build: the named knob carries a value the capture must reject.
: BUILD-REFUSED ( ptr u8 n n -- ) {: k:ptr ku:n v:n :}
   REFUSE-SETUP
   v NUM$!
   PROC-ENV-RESET
   s" HB_TMP" >LEN REFUSE-ROOT$ >LEN PROC-ENV+
   k ku >LEN  NUM$ >LEN PROC-ENV+
   RUN-BUILDER ;

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

\ A define into an UNPROTECTED word-list is refused too, but for an unrelated
\ reason: since the absent-package-context reject landed, redirecting publication
\ with `set-current` leaves no authenticated package, and the engine says so and
\ exits 70 (CHECKER-PKG-CONTEXT-REJECT). That refusal is what the shipped engine
\ gives for these ids, and it is exactly what makes the exit-84 cases above
\ evidence: the two outcomes carry different codes AND different diagnostics, so
\ an 84 can only have come from the protected-WID bitmap.
70 constant CTX-RC                   \ src/core/checker.f PKGCTX-REJECT-RC (private there)
: ASSERT-NOT-PROTECTED ( -- )
   EXITED @ TTRUE
   RC @ CTX-RC T=
   ERR$ s" hb: cannot publish into protected word" CONTAINS? 0= TTRUE ;

\ --- forge / probe sources (interpreted by the child engine) ---
\ Every probe prints one number so the parent can read it back; the membership
\ probes go through the shared read-only view of the band rather than reading raw
\ offsets, so they exercise the same bit arithmetic the engine does.
2048 constant PRB-CAP
create PRB PRB-CAP allot   variable PRB-U
: PRB-RESET ( -- ) 0 PRB-U ! ;
: PRB+ ( ptr u8 n -- ) {: a:ptr u:n :}
   PRB-U @ u + PRB-CAP > if E-FS-CAPACITY throw then
   a PRB PRB-U @ + u BYTE-COPY
   PRB-U @ u + PRB-U ! ;
: PRB-NL ( -- ) 10 PRB PRB-U @ + c!  PRB-U @ 1+ PRB-U ! ;
: PRB-N ( n -- ) NUM$! NUM$ PRB+ ;
: PRB$ ( -- ptr u8 n ) PRB PRB-U @ ;

\ `if` is compile-only, so each probe defines one word and calls it - which is also
\ what puts the probe body through the checker in the child engine.
: MEMBER-PROBE$ ( n -- ptr u8 n ) {: wid:n :}
   PRB-RESET
   s" require tools/prot-wid-probe.f" PRB+ PRB-NL
   s" : PRB-MEMBER ( -- ) " PRB+  wid PRB-N
   s"  PROT-WID-PROBE:MEMBER? if 1 else 0 then . ;" PRB+ PRB-NL
   s" PRB-MEMBER" PRB+
   PRB$ ;

: FORGE-WID$ ( n -- ptr u8 n ) {: wid:n :}
   PRB-RESET
   wid PRB-N
   s"  set-current : FOO ( -- n ) 1 ;" PRB+
   PRB$ ;

\ The child engine can still define at all: a plain packaged definition, no
\ redirection. Without this the exit-84 cases could be read as "this engine
\ refuses every definition".
: DEFINE-OK$ ( -- ptr u8 n )
   S\" package PRBOK\npublic\n: FOO ( -- n ) 1 ;\n;package" ;
: PROBE-TAG$ ( -- ptr u8 n )
   S\" : PRB-TAG ( -- ) data-base PROT-REG-TAG-CELL + @ PROT-REG-TAG = if 1 else 0 then . ;\nPRB-TAG" ;
: PROBE-WORDLIST$ ( -- ptr u8 n )  s" wordlist . " ;

: PROBE-VARIANT ( -- )
   s" restored band carries the bitmap shape tag before batch input" T-LABEL
   HBPWID$ PROBE-TAG$ READ-N  1 T=
   s" baked wid 300 (> 255) is protected before batch input" T-LABEL
   HBPWID$ WID-A MEMBER-PROBE$ READ-N  1 T=
   s" baked wid 8000 is protected before batch input" T-LABEL
   HBPWID$ WID-B MEMBER-PROBE$ READ-N  1 T=
   s" wid 8001, one bit past the highest baked id, is not protected" T-LABEL
   HBPWID$ WID-NEIGHBOUR MEMBER-PROBE$ READ-N  0 T=
   s" WIDN advanced past the largest baked wid before batch input" T-LABEL
   HBPWID$ PROBE-WORDLIST$ READ-N  WID-B >  TTRUE
   s" publish into baked wid 300 exits 84 (--load)" T-LABEL
   HBPWID$ WID-A FORGE-WID$ FORGE-LOAD  ASSERT-REJECT
   s" publish into baked wid 300 exits 84 (stdin)" T-LABEL
   HBPWID$ WID-A FORGE-WID$ FORGE-STDIN  ASSERT-REJECT
   s" publish into baked wid 8000 exits 84 (--load)" T-LABEL
   HBPWID$ WID-B FORGE-WID$ FORGE-LOAD  ASSERT-REJECT
   s" publish into baked wid 8000 exits 84 (stdin)" T-LABEL
   HBPWID$ WID-B FORGE-WID$ FORGE-STDIN  ASSERT-REJECT
   s" an ordinary packaged define still exits 0 on the variant (--load)" T-LABEL
   HBPWID$ DEFINE-OK$ FORGE-LOAD  ASSERT-OK ;

: PROBE-CONTROL ( -- )
   s" shipped engine does not protect wid 300 (control, --load)" T-LABEL
   PLAIN$ WID-A FORGE-WID$ FORGE-LOAD  ASSERT-NOT-PROTECTED
   s" shipped engine does not protect wid 8000 (control, --load)" T-LABEL
   PLAIN$ WID-B FORGE-WID$ FORGE-LOAD  ASSERT-NOT-PROTECTED
   s" shipped engine's band holds neither baked id (control)" T-LABEL
   PLAIN$ WID-A MEMBER-PROBE$ READ-N  0 T= ;

\ --- the capture's two refusals ------------------------------------------------
\ Both are memory-safety guards on a caller-supplied index, so both are proved on
\ the real build path: the builder dies named, and no engine is produced. The
\ runtime twin of the first - `prot-wid-add` refusing an id at the bound, exit 84 -
\ lives in test/seal.f beside the other seal forges; this is the AOT capture's own
\ guard, which no other test reaches.
: ASSERT-BUILD-REFUSED ( ptr u8 n -- ) {: m:ptr mu:n :}
   RC @ 0 <> TTRUE
   ERR$ m mu CONTAINS? TTRUE
   REFUSE-HB$ EXISTS? 0= TTRUE ;

: PROBE-REFUSALS ( -- )
   s" capture refuses a protected wid above the bitmap bound" T-LABEL
   s" HABU_PWID_OOR" WID-OOR BUILD-REFUSED
   s" aot-capture: protected WID above the bitmap bound" ASSERT-BUILD-REFUSED
   s" capture refuses a legacy row count that is not a registry shape" T-LABEL
   s" HABU_PWID_LEGACY_N" LEGACY-N-BAD BUILD-REFUSED
   s" aot-capture: unrecognised protected-WID registry shape" ASSERT-BUILD-REFUSED ;

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
   PROBE-REFUSALS
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
