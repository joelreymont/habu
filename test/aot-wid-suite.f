\ aot-wid-suite.f - protected-WID boot-integration regression (TFAM 2b-v(f)).
\
\ What this locks: the protected-WID bitmap that an engine bakes into its
\ ahead-of-time (AOT) section must be restored at engine STARTUP, before any
\ batch program (piped stdin or --load file) runs. Batch input is the primary
\ path LLM-generated Forth takes. If the restore ran too late, a batch program
\ could publish a definition into a sealed constructor word-list and the guard
\ would never fire.
\
\ Cold startup restores the baked band immediately after clearing the live one,
\ before the cold prefix registers its constructor families and before any user
\ source runs: EM-STARTUP-RUNTIME-STATE (src/habu/habu2.f) clears the band,
\ publishes the shape tag and then calls the LAOTPROT routine
\ (EMIT-AOT-PROT-RESTORE). Warm snapshot startup skips both the clear and the
\ baked replay, keeping the band the snapshot DATA image carried. That call is
\ load-bearing: removing it turns every probe below red (no baked bits, WIDN not
\ advanced, forge into a baked wid exits 0 not 84).
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
\ extra word-list ids set (8000 and a low one derived from the shipped engine's
\ own live band, handed over as HABU_PWID_A) on top of the ones the metabuild host
\ protects for itself. This suite then probes hb-pwid on the real batch paths, and
\ spawns the same builder in its three refusal modes and its two boot-gate
\ modes.
\
\ The registry has two ends and this suite now holds both. A program the engine
\ READS must not publish into a protected word-list (the forge cases). A name the
\ engine BAKED must not resolve into one either: the AOT seed rewrites a call, an
\ xt or a branch for every stored name, and LAOTWIDGATE stands between the lookup
\ and all three. PROBE-BOOT-GATE is that end (dot
\ habu-return-the-record-9c9b1731); before it, nothing in the tree executed the
\ routine.
\
\ Note on the negative leg: an earlier revision proved "not protected" by having
\ the same forge exit 0 against an unprotected id. Since the absent-package-context
\ reject landed, `set-current` into any word-list leaves no authenticated package,
\ so that program is refused whatever the id - with a different code (70) and a
\ different diagnostic. The discrimination is intact and is what ASSERT-NOT-PROTECTED
\ asserts; a separate case proves the child engine still defines normally, so the
\ exit-84 results cannot be read as "this engine refuses everything".
\
\ NOT covered here, and recorded rather than faked: a baked band that is invalid
\ on its face. EMIT-AOT-PROT-RESTORE rejects two shapes at cold startup - a frame
\ whose tag is not PROT-REG-TAG, and a band with bit 0 set (WID 0 is not a
\ wordlist) - and exits ENGINE-ERROR:AOT-SEED with a named newline-terminated
\ diagnostic. This builder cannot stage either one: it injects after CAPTURE-REPL,
\ and the maker hb-pwid-mk is itself emitted from that same injected source, so a
\ corrupt band kills the maker run before hb-pwid ever exists. A forge that
\ corrupts only the FINAL image is what the probe needs - dot
\ habu-forge-a-corrupt-844064a9.
\
\ Note on counts: an earlier revision asserted the registry count was exactly 2
\ with a plain-engine baseline of 0. The engine registers boot-time protected
\ word-lists for its own constructor families, so an exact-count proxy is stale
\ the moment a family is added. Reading the two baked ids back BY ID is the direct,
\ stable proof, and it is what a bitmap makes cheap.
\
\ Cost: six child engine builds (~12 s each; two of them are the boot-gate
\ modes). It is registered as
\ `TEST:SUITE aot-wid-restore` in test/gate-stdlib-cases.f, so it runs in the
\ standalone stdlib gate (a required master gate) - not the fast tail-process
\ fork tier, whose perf ratchet the build cost would exceed. Run standalone:
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
variable WID-A-V                     \ baked protected id > 255, DERIVED (see DERIVE-WID-A)
: WID-A ( -- n ) WID-A-V @ ;
8000  constant WID-B                 \ baked protected word-list id high in the band
8001  constant WID-NEIGHBOUR         \ one id past WID-B: must come back UNprotected
70000 constant WID-OOR               \ far above the bound: the capture must refuse it
PROT-WID-LEGACY-MAX 1+ constant LEGACY-N-BAD   \ not a legacy row count at all
\ Raise the capture window's DATA span start by more than the whole span, so the
\ span holds nothing and EVERY address chain the band recorded in the blob falls
\ outside both spans. A small skew would depend on where the first data word
\ happened to land; this size cannot: the REPL's own `here` growth over one
\ compile is far below 16 MiB, so d0 + this is past d1 on any tree.
$1000000 constant D0-SKEW-PAST-SPAN
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
create GATE-BUF FS-PATH-CAP allot     variable GATE-U
create GATE-HB-BUF FS-PATH-CAP allot  variable GATE-HB-U

: ROOT$ ( -- ptr u8 n )   ROOT-BUF ROOT-U @ ;
: HBPWID$ ( -- ptr u8 n ) HBPWID-BUF HBPWID-U @ ;
: FORGE$ ( -- ptr u8 n )  FORGE-BUF FORGE-U @ ;
: REFUSE-ROOT$ ( -- ptr u8 n ) REFUSE-BUF REFUSE-U @ ;
: REFUSE-HB$ ( -- ptr u8 n )   REFUSE-HB-BUF REFUSE-HB-U @ ;
: GATE-ROOT$ ( -- ptr u8 n )   GATE-BUF GATE-U @ ;
: GATE-HB$ ( -- ptr u8 n )     GATE-HB-BUF GATE-HB-U @ ;
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

\ Same rule for each boot-gate mode: its own tree, so the engine a probe boots
\ can only be the one that mode's build wrote.
: GATE-SETUP ( -- )
   s" habu-aot-wid-gate" TMPDIR-MKDIR {: a:ptr u:n :}
   a GATE-BUF u BYTE-COPY  u GATE-U !
   GATE-ROOT$ CLEANUP-TREE+
   GATE-ROOT$ s" hb-pwid" GATE-HB-BUF JOIN-PATH GATE-HB-U ! ;

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
   WID-A NUM$!
   s" HABU_PWID_A" >LEN NUM$ >LEN PROC-ENV+
   RUN-BUILDER ;

\ A refusal build: the named knob carries a value the capture must reject.
: BUILD-REFUSED ( ptr u8 n n -- ) {: k:ptr ku:n v:n :}
   REFUSE-SETUP
   v NUM$!
   PROC-ENV-RESET
   s" HB_TMP" >LEN REFUSE-ROOT$ >LEN PROC-ENV+
   k ku >LEN  NUM$ >LEN PROC-ENV+
   RUN-BUILDER ;

\ A boot-gate build: the mode number picks the fixture the builder injects.
: BUILD-GATE ( n -- ) {: mode:n :}
   GATE-SETUP
   mode NUM$!
   PROC-ENV-RESET
   s" HB_TMP" >LEN GATE-ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_GATE" >LEN  NUM$ >LEN PROC-ENV+
   RUN-BUILDER ;

\ The same build, with the fixture's package landed on an exact wordlist id in
\ the METABUILD HOST. The id is one the shipped engine already uses, so what the
\ capture carries is an alias of a live target wordlist.
create GWID-BUF 32 allot   variable GWID-U
: GWID$! ( n -- ) {: v:n :}
   v NUM$!  NUM$ {: a:ptr u:n :}
   a GWID-BUF u BYTE-COPY  u GWID-U ! ;
: GWID$ ( -- ptr u8 n ) GWID-BUF GWID-U @ ;

: BUILD-GATE-AT ( n n -- ) {: mode:n wid:n :}
   GATE-SETUP
   wid GWID$!
   mode NUM$!
   PROC-ENV-RESET
   s" HB_TMP" >LEN GATE-ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_GATE" >LEN  NUM$ >LEN PROC-ENV+
   s" HABU_AOT_GATE_WID" >LEN  GWID$ >LEN PROC-ENV+
   RUN-BUILDER ;

\ The gate fixture built with the baked wid window moved AFTER the capture, so
\ the records name wordlists it does not contain. `k` is the knob that moves it.
: BUILD-GATE-FORGED ( ptr u8 n n -- ) {: k:ptr ku:n v:n :}
   GATE-SETUP
   v GWID$!
   2 NUM$!
   PROC-ENV-RESET
   s" HB_TMP" >LEN GATE-ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_GATE" >LEN  NUM$ >LEN PROC-ENV+
   k ku >LEN  GWID$ >LEN PROC-ENV+
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

\ Boot the engine with no program at all: the AOT boot-run list is what runs, and
\ it runs before any input is read.
: BOOT-EMPTY ( ptr u8 n -- ) {: e:ptr eu:n :}
   PROC-ARGV-RESET
   e eu >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  STORE! ;

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;

\ The same reading, from a TAGGED line: an engine that carries a boot-run entry
\ prints before any probe of it does, so its whole stdout is not a number.
: LINE-AT ( n -- ptr u8 n ) {: at:n :}
   OUT at +  OUT-U @ at -  {: a:ptr u:n :}
   a u 10 INDEX-OF MATCH option
     none OF a u ENDOF
     some OF IDX>N a swap ENDOF
   ;MATCH ;

: TAG-AT ( ptr u8 n -- n ) {: t:ptr tu:n :}
   OUT$ t tu FIND-SUB MATCH option
     none OF T-FAIL 0 ENDOF
     some OF IDX>N tu + ENDOF
   ;MATCH ;

: TAGGED-N ( ptr u8 n -- n )
   TAG-AT LINE-AT TRIM STR>NUMBER? MATCH option
     some OF ENDOF
     none OF T-FAIL 0 ENDOF
   ;MATCH ;

: ASSERT-GATE-RAN ( -- )             \ the same fixture, unprotected: it boots and the entry runs
   EXITED @ TTRUE
   RC @ 0 T=
   OUT$ s" awb-gate=open" CONTAINS? TTRUE ;

\ The seed wired the call site up and the engine reached its entry word. The
\ stderr half is what separates "the gate admitted it" from "the gate was never
\ asked": a reject writes that line before exit 84, so its absence with rc 0 is
\ the admit, not a fixture that quietly stopped compiling the call.
: ASSERT-GATE-ADMITTED ( -- )
   EXITED @ TTRUE
   RC @ 0 T=
   ERR$ s" hb: AOT protected-WID gate reject" CONTAINS? 0= TTRUE
   OUT$ s" awb-gate=open" CONTAINS? TTRUE ;

\ A define into an UNPROTECTED word-list is refused too, but for an unrelated
\ reason: since the absent-package-context reject landed, redirecting publication
\ with `set-current` leaves no authenticated package, and the engine says so and
\ exits 70 (CHECKER-PKG-CONTEXT-REJECT). That refusal is what the shipped engine
\ gives for these ids, and it is exactly what makes the exit-84 cases above
\ evidence: the two outcomes carry different codes AND different diagnostics, so
\ an 84 can only have come from the protected-WID bitmap.
82 constant SEED-RC                  \ src/core/engine-error.f ENGINE-ERROR:AOT-SEED
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

\ The AWBGATE package's public wordlist id in a built engine, and how many
\ package records claim it. One is what a dictionary says about an id it handed
\ out; two is two packages sharing a wordlist. Both are printed on TAGGED lines,
\ because these engines carry a boot-run entry that prints first.
: ALIAS-PROBE$ ( -- ptr u8 n )
   PRB-RESET
   s" require tools/pkg-wid-probe.f" PRB+ PRB-NL
   s" require tools/prot-wid-probe.f" PRB+ PRB-NL
   S\" : PRB-ALIAS ( -- ) s\" AWBGATE\" PKG-WID-PROBE:WID-OF {: w:n :}" PRB+ PRB-NL
   S\"    s\" awb-wid=\" type w FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-owners=\" type w PKG-WID-PROBE:OWNERS FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-high=\" type PKG-WID-PROBE:HIGH FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-widn=\" type PROT-WID-PROBE:WIDS FMT:.U cr ;" PRB+ PRB-NL
   s" PRB-ALIAS" PRB+
   PRB$ ;

\ The low baked id is read off the SHIPPED engine rather than pinned: its only
\ stated property is that the shipped engine does not protect it, so the shipped
\ engine is the only thing that can say which ids qualify. Above the live
\ wordlist count by a margin, and above 255 (the u8 ceiling the old row table
\ existed to clear) - then checked against the live band, so the derivation
\ proves its own premise instead of assuming it.
: DERIVE-WID-A ( -- )
   PLAIN$ PROBE-WORDLIST$ READ-N {: live:n :}
   live 64 + 256 max WID-A-V !
   WID-A 255 > TTRUE
   WID-A WID-B < TTRUE
   PLAIN$ WID-A MEMBER-PROBE$ READ-N 0 T= ;

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

\ --- the capture's three refusals -----------------------------------------------
\ All three are proved on the real build path: the builder dies named, and no
\ engine is produced. The first two are memory-safety guards on a caller-supplied
\ index; the runtime twin of the first - `prot-wid-add` refusing an id at the
\ bound, exit 84 - lives in test/seal.f beside the other seal forges.
\ The third is a different kind of guard and the reason it needs a build to reach
\ it. Since the capture stopped recognising an address chain by the value it
\ carries and started reading the address-literal band, a recorded site is known
\ to hold a real address and the only question left is WHICH span it belongs to.
\ A site in neither span is an address the window does not carry, so there is
\ nothing correct to bake: rebasing it by this window's delta would be wrong and
\ skipping it would leave the building host's address in the seeded engine, which
\ is exactly the silence the old value scan produced. The refusal is what turns
\ that into a stop, and this case is the only thing that executes it.
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
   s" aot-capture: unrecognised protected-WID registry shape" ASSERT-BUILD-REFUSED
   s" capture refuses a recorded chain its window cannot place" T-LABEL
   s" HABU_AOT_D0_SKEW" D0-SKEW-PAST-SPAN BUILD-REFUSED
   s" aot-capture: recorded address site outside both window spans" ASSERT-BUILD-REFUSED ;

\ --- the AOT boot gate (dot habu-return-the-record-9c9b1731) --------------------
\ The other end of the protected-WID registry. Everything above proves that a
\ program the engine READS cannot publish into a sealed word-list. These three
\ prove the same about what the engine BAKED: the AOT seed resolves every stored
\ name and then rewrites a call, writes an xt, or branches to the word, and
\ LAOTWIDGATE is what stands between the lookup and all three. Until this landed,
\ nothing in the tree executed that routine at all - it could be deleted whole and
\ every suite stayed green.
\
\ BOTH BOOT, AND THAT IS THE POINT. The window compiles a call to a QUALIFIED
\ prefix word, so the seed resolves it through that package's public slot and the
\ gate's last layer decides: a callee in a SEALED wordlist the engine already had
\ is admitted when the wid is the package's PUBLIC one. Calling a public word of a
\ sealed package is what checked source does every day - EMIT-STORE-DEF-NAME
\ refuses DEFINING into a sealed wid and C-PACKAGE-SEAL-GUARD refuses OPENING one,
\ and neither forbids the call - so a gate that rejected here would refuse
\ legitimate work, and did: the compiler chain's own CODE-RECLAIM:WATCH sites and
\ its A64RAV:DKEEP-HOOK-DEFAULT boot-run entry all died at this routine before the
\ layers landed.
\
\ WHAT SEPARATES THE TWO IS THE MUTATION THEY SURVIVE. Delete the public-slot
\ admit and mode 1 dies 84 naming WATCHERS while mode 2, whose callee's package
\ seals nothing, still boots. That contrast is what attributes mode 1's verdict to
\ the bitmap rather than to a name that merely failed to resolve, and the builder
\ asserts both packages' seal status on the live host before it builds either, so
\ a tree that sealed CHECKER-TAPE or unsealed CODE-RECLAIM stops the build by name
\ instead of quietly testing one thing twice.
\
\ NO CASE HERE CAN MAKE THE GATE REFUSE, and that is a property of the format
\ rather than a gap in the fixture: a call site's scope is a window coordinate the
\ seed rebases into the window, and LFIND's qualifier path reads only a package's
\ public slot, so nothing an artifact carries reaches the refuse leg.
\ EM-AOTWIDGATE's own note carries that proof and the reason the leg is kept.
: PROBE-BOOT-GATE ( -- )
   s" a call site into an UNSEALED prefix package boots and runs (control)" T-LABEL
   2 BUILD-GATE
   RC @ 0 T=
   GATE-HB$ EXISTS? TTRUE
   GATE-HB$ BOOT-EMPTY
   ASSERT-GATE-ADMITTED
   s" a call site into a SEALED prefix package's PUBLIC word-list is admitted" T-LABEL
   1 BUILD-GATE
   RC @ 0 T=
   GATE-HB$ EXISTS? TTRUE
   GATE-HB$ BOOT-EMPTY
   ASSERT-GATE-ADMITTED ;

\ --- the wid rebase (dot habu-rebase-captured-wids-54dec421) --------------------
\ A captured record travels with the wordlist id it had in the METABUILD HOST,
\ whose wid space is not the target's: the host compiles the EMITTER while the
\ target's prefix compiles the checker and the stdlib, so the two number their
\ wordlists independently. Registering such an id verbatim puts the captured word
\ into whichever wordlist the target keeps at that number - a sealed one refuses
\ the boot (exit 84 with nothing in the fixture asking for protection), an
\ ordinary one takes the word in silence and two packages own one wordlist.
\
\ THE TWO IDS ARE THE ENGINE'S, NOT THIS FILE'S. The shipped engine is asked for
\ its own WIDN and then for the seal state of the ids below it, so the sealed
\ case and the ordinary case each name a wordlist that engine really has. A
\ number written down here would stop being an alias the first time the prefix
\ moved.
variable ALIAS-SEALED   variable ALIAS-OPEN   variable ALIAS-W

: PICK-ALIAS-WIDS ( -- )
   0 ALIAS-SEALED !  0 ALIAS-OPEN !
   PLAIN$ PROBE-WORDLIST$ READ-N 1 - ALIAS-W !
   begin
      ALIAS-W @ 1 >
      ALIAS-SEALED @ 0=  ALIAS-OPEN @ 0=  or
      and
   while
      PLAIN$ ALIAS-W @ MEMBER-PROBE$ READ-N 1 = if
         ALIAS-SEALED @ 0= if ALIAS-W @ ALIAS-SEALED ! then
      else
         ALIAS-OPEN @ 0= if ALIAS-W @ ALIAS-OPEN ! then
      then
      ALIAS-W @ 1 - ALIAS-W !
   repeat ;

\ One alias case: build the control fixture (mode 2, nothing protected) on the
\ given id, and require that the engine boots, that its entry word runs, and that
\ the id the seed actually gave the captured package is owned by that package
\ alone.
: ALIAS-CASE ( n -- ) {: wid:n :}
   2 wid BUILD-GATE-AT
   RC @ 0 <> if s" aot-wid-suite: builder stderr:" type cr ERR$ type cr then
   RC @ 0 T=
   GATE-HB$ EXISTS? TTRUE
   GATE-HB$ BOOT-EMPTY  ASSERT-GATE-RAN
   GATE-HB$ ALIAS-PROBE$ FORGE-LOAD
   EXITED @ TTRUE  RC @ 0 T=
   s" ... and the captured package owns its wordlist alone" T-LABEL
   s" awb-owners=" TAGGED-N  1 T=
   s" ... at an id the target's own prefix never handed out" T-LABEL
   s" awb-wid=" TAGGED-N  wid >  TTRUE
   s" ... and the engine's next id is past every id its records claim" T-LABEL
   s" awb-widn=" TAGGED-N  s" awb-high=" TAGGED-N  >  TTRUE ;

\ The capture's own refusal, reached by telling it the window made fewer
\ wordlists than it did. It names the RECORD, which is what the boot's refusal
\ cannot do, and it stops the build before an engine exists.
: BUILD-GATE-NARROW ( n -- ) {: v:n :}
   GATE-SETUP
   v GWID$!
   2 NUM$!
   PROC-ENV-RESET
   s" HB_TMP" >LEN GATE-ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_GATE" >LEN  NUM$ >LEN PROC-ENV+
   s" HABU_AOT_WID_NARROW" >LEN  GWID$ >LEN PROC-ENV+
   RUN-BUILDER ;

: PROBE-WID-CAPTURE-REFUSAL ( -- )
   s" a captured wid its declared window does not contain stops the build" T-LABEL
   2 BUILD-GATE-NARROW
   RC @ 0 <> TTRUE
   ERR$ s" aot-capture: captured wid outside the window" CONTAINS? TTRUE
   s" ... and the refusal names the record that carries it" T-LABEL
   OUT$ s" window record AWBGATE names wordlist" CONTAINS?
   ERR$ s" window record AWBGATE names wordlist" CONTAINS? or TTRUE
   s" ... and no engine is written" T-LABEL
   GATE-HB$ EXISTS? 0= TTRUE ;

\ The seed's own refusal, which the capture-side audit makes unreachable from a
\ real capture: only a baked window that disagrees with the baked records can
\ reach it, and that is what the two forges bake.
: FORGED-CASE ( ptr u8 n n -- ) {: k:ptr ku:n v:n :}
   k ku v BUILD-GATE-FORGED
   RC @ 0 T=
   GATE-HB$ EXISTS? TTRUE
   GATE-HB$ BOOT-EMPTY
   EXITED @ TTRUE
   RC @ SEED-RC T=
   ERR$ s" hb: AOT wid outside the capture window" CONTAINS? TTRUE
   OUT$ s" awb-gate=" CONTAINS? 0= TTRUE ;

: PROBE-WID-FORGED ( -- )
   s" a baked wid below the baked window is refused at the seed" T-LABEL
   s" HABU_AOT_WID_SKEW" 5 FORGED-CASE
   s" a baked wid past the baked window's end is refused at the seed" T-LABEL
   s" HABU_AOT_WID_SPAN" 1 FORGED-CASE ;

: PROBE-WID-REBASE ( -- )
   PICK-ALIAS-WIDS
   s" the shipped engine offers a sealed and an unsealed id below its WIDN" T-LABEL
   ALIAS-SEALED @ 0 >  ALIAS-OPEN @ 0 >  and TTRUE
   s" a captured package on a SEALED target wordlist boots (was exit 84)" T-LABEL
   ALIAS-SEALED @ ALIAS-CASE
   s" a captured package on an ORDINARY target wordlist boots" T-LABEL
   ALIAS-OPEN @ ALIAS-CASE ;

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
   s" derived low baked id is outside the shipped engine's band" T-LABEL
   DERIVE-WID-A
   BUILD-VARIANT
   s" aot-wid variant engine builds cleanly" T-LABEL
   RC @ 0 T=
   RC @ 0 <> if s" aot-wid-suite: builder stderr:" type cr  ERR$ type cr  RC @ throw then
   s" hb-pwid variant exists after build" T-LABEL
   HBPWID$ EXISTS? TTRUE
   PROBE-VARIANT
   PROBE-CONTROL
   PROBE-REFUSALS
   PROBE-BOOT-GATE
   PROBE-WID-REBASE
   PROBE-WID-CAPTURE-REFUSAL
   PROBE-WID-FORGED
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
