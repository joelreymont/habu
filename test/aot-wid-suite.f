\ aot-wid-suite.f - protected-WID boot-integration regression (TFAM 2b-v(f)).
\
\ What this locks: the protected-WID bitmap that an engine bakes into its
\ ahead-of-time (AOT) section must be restored at engine STARTUP, before any
\ batch program (piped stdin or --load file) runs. Batch input is the primary
\ path LLM-generated Forth takes. If the restore ran too late, a batch program
\ could publish a definition into a sealed constructor word-list and the guard
\ would never fire.
\
\ Cold startup re-seals the word-lists the capture window sealed immediately
\ after clearing the live band, before the cold prefix registers its constructor
\ families and before any user source runs: EM-STARTUP-RUNTIME-STATE
\ (src/habu/habu2.f) clears the band, publishes the shape tag and then calls the
\ LAOTPROT routine (EMIT-AOT-PROT-RESTORE). Warm snapshot startup skips both the
\ clear and the replay, keeping the band the snapshot DATA image carried. That
\ call is load-bearing: removing it turns every probe below red (nothing sealed,
\ WIDN not advanced, a forge into the sealed wid exits 0 not 84).
\
\ The registry is a WID-INDEXED BITMAP (dot habu-replace-the-protected-ca920a8f)
\ and what an engine BAKES is the set of word-lists its own capture window sealed,
\ stored window-relative. The three parts of that are what the probes below read:
\
\   the tag        the restore release-publishes PROT-REG-TAG into the tag cell
\                  LAST, so a half-copied band is never observable as a bitmap;
\                  a restored engine whose tag cell does not carry it has not
\                  completed a restore.
\   the band       membership is one bit per wordlist id, read through the shared
\                  tools/prot-wid-probe.f - the same bits the engine's PROT-WID?
\                  routine tests. The probes assert WHICH ids came back, and that
\                  BOTH NEIGHBOURS of the restored id did not, which a smeared or
\                  mis-shifted restore would set.
\   the rows       a captured protected WID is an offset from the window's first
\                  wordlist id, and the seed rebases it onto the WIDN of the
\                  engine it boots (habu2.f AOT-WINDOW:SEAL-WIDS,). No build-host id
\                  survives the cut, so this suite cannot write down the id it
\                  probes: it asks the built engine which id the fixture's package
\                  got (tools/pkg-wid-probe.f) and probes that one.
\
\ How it is proven: test/aot-wid-build.f is spawned in a child process with a
\ private HB_TMP; it builds a throwaway `hb-pwid` engine whose capture window
\ holds two packages of the fixture's own - one sealed with `prot-wid-add`, one
\ left open - and checks the capture's contract against the live band while it is
\ still there to check. This suite then probes hb-pwid on the real batch paths,
\ and spawns the same builder in its three refusal modes, its two boot-gate modes
\ and the wid rebase and forge modes further down.
\
\ WHAT THE RETIREMENT CHANGED, and what it cost (commit 3e29a730b0d4, closing dot
\ habu-retire-the-legacy-31ad57bc). The capture used to copy the host's whole live
\ band into the artifact, with a second leg that converted a table-era host's u32
\ rows into bits when the tag cell held a row count instead of PROT-REG-TAG. A
\ fixture could therefore poke any absolute id into the captured buffer
\ (ACAP-PWID-SET) and probe that exact id in the built engine. Those words are
\ gone: membership is derived live, one WID at a time (ACAP-LIVE-PWID?), and only
\ the window's own seals travel. Three things went with them, and none of them was
\ weakened into something cheaper that would still pass:
\
\   the table-era leg's two cases - an empty registry accepted, and a row count
\     that is no registry at all refused (HABU_PWID_LEGACY_N) - existed only for
\     the changeover in which a table-era host built a bitmap-era engine. There is
\     no table-era host and no capture word that reads a row count. The empty case
\     keeps its intent live: the build takes a capture of the window BEFORE the
\     fixture seals anything and requires it to record no row at all.
\   the absolute baked ids (300 and 8000, plus 8001 as the neighbour of the
\     highest). A fixture can no longer choose the id it bakes, because the rows
\     are rebased onto the target's WIDN, and the ids the seed hands out sit just
\     above it. So the band's HIGH END - a bit near 8000, a thousand bytes into
\     the band - is no longer reachable through any build this fixture can make. The
\     restored id and both its neighbours are probed instead. That is a real
\     reduction in coverage and it is recorded here rather than papered over:
\     nothing in the tree now sets a bit in the band's upper half through a build.
\   the HABU_PWID_OOR refusal, whose guard was ACAP-PWID-SET's check on a
\     caller-supplied index. No capture word takes a WID from a caller any more.
\     The live owner of that bound is `prot-wid-add`, which refuses an id at or
\     above it by name, and the build mode that replaced OOR (HABU_PWID_BAD)
\     drives that word - so the refusal is still proved on the real build path,
\     with the in-process twin in test/seal.f.
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
\ NOT covered here, and recorded rather than faked: the restore's own refusal. The
\ cold restore no longer reads a baked band, so the two shapes it used to reject
\ (a frame whose tag is not PROT-REG-TAG, a band with bit 0 set) no longer exist.
\ What it can refuse now is one baked ROW whose rebased id leaves the window or
\ passes PROT-WID-MAX - "hb: AOT wid outside the capture window",
\ ENGINE-ERROR:AOT-SEED. PROBE-WID-FORGED below reaches that message through a
\ forged RECORD wid; no mode forges a protected-WID ROW, which would need a poke
\ into the capture buffer after the capture and before the emit. A forge that
\ corrupts only the FINAL image is still what the direct probe needs - dot
\ habu-forge-a-corrupt-844064a9.
\
\ Note on counts: an earlier revision asserted the registry count was exactly 2
\ with a plain-engine baseline of 0. The engine registers boot-time protected
\ word-lists for its own constructor families, so an exact-count proxy is stale
\ the moment a family is added. Reading the two baked ids back BY ID is the direct,
\ stable proof, and it is what a bitmap makes cheap - now read back at the id the
\ engine says it gave the fixture's package, since the id is the seed's to choose.
\
\ Cost: eleven child engine builds (~12 s each), plus the data-span child's own.
\ It is registered directly in test/gate-stdlib-cases.f. Run standalone:
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
require lib/process-cwd.f

package AOT-WID-SUITE

\ Test vectors - the two ids the BUILT ENGINE gave the fixture's packages, read
\ back from it by READ-FIXTURE-WIDS. Neither can be written down here: a captured
\ protected WID is window-relative and the seed rebases it onto that engine's WIDN.
variable PROT-WID-V                  \ AWBPROT's public wordlist id, the one sealed
variable OPEN-WID-V                  \ AWBOPEN's, which the fixture left open
: PROT-WID ( -- n ) PROT-WID-V @ ;
: OPEN-WID ( -- n ) OPEN-WID-V @ ;
PROT-WID-MAX constant WID-AT-BOUND   \ no bit exists for it: prot-wid-add must refuse
0 constant WID-NOT-A-WORDLIST        \ bit 0 set is a band no capture may accept
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
create COLD-ROOT-BUF FS-PATH-CAP allot variable COLD-ROOT-U
create COLD-SRC FS-PATH-CAP allot
create COLD-DST FS-PATH-CAP allot

: ROOT$ ( -- ptr u8 n )   ROOT-BUF ROOT-U @ ;
: HBPWID$ ( -- ptr u8 n ) HBPWID-BUF HBPWID-U @ ;
: FORGE$ ( -- ptr u8 n )  FORGE-BUF FORGE-U @ ;
: REFUSE-ROOT$ ( -- ptr u8 n ) REFUSE-BUF REFUSE-U @ ;
: REFUSE-HB$ ( -- ptr u8 n )   REFUSE-HB-BUF REFUSE-HB-U @ ;
: GATE-ROOT$ ( -- ptr u8 n )   GATE-BUF GATE-U @ ;
: GATE-HB$ ( -- ptr u8 n )     GATE-HB-BUF GATE-HB-U @ ;
: COLD-ROOT$ ( -- ptr u8 n ) COLD-ROOT-BUF COLD-ROOT-U @ ;
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

create GWID-BUF 32 allot   variable GWID-U
: GWID$! ( n -- ) {: v:n :}
   v NUM$!  NUM$ {: a:ptr u:n :}
   a GWID-BUF u BYTE-COPY  u GWID-U ! ;
: GWID$ ( -- ptr u8 n ) GWID-BUF GWID-U @ ;

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
   s" require lib/fmt.f" PRB+ PRB-NL
   s" require tools/pkg-wid-probe.f" PRB+ PRB-NL
   s" require tools/prot-wid-probe.f" PRB+ PRB-NL
   S\" : PRB-ALIAS ( -- ) wordlist {: next:n :} s\" AWBGATE\" PKG-WID-PROBE:WID-OF {: w:n :}" PRB+ PRB-NL
   S\"    s\" AWBTARGET\" PKG-WID-PROBE:WID-OF {: target:n :}" PRB+ PRB-NL
   S\"    s\" awb-target-wid=\" type target FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-target-owners=\" type target PKG-WID-PROBE:OWNERS FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-target-sealed=\" type target PROT-WID-PROBE:MEMBER? if 1 else 0 then FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-target-value=\" type AWBTARGET:VALUE FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-wid=\" type w FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-owners=\" type w PKG-WID-PROBE:OWNERS FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-high=\" type PKG-WID-PROBE:HIGH FMT:.U cr" PRB+ PRB-NL
   S\"    s\" awb-next=\" type next FMT:.U cr ;" PRB+ PRB-NL
   s" PRB-ALIAS" PRB+
   PRB$ ;

\ Which wordlist id the built engine gave each fixture package. The seed chooses
\ it - the capture carries an offset, not an id - so the engine is asked, through
\ the same package records tools/pkg-wid-probe.f reads for the alias cases below.
: WID-PROBE-HEAD ( -- )
   PRB-RESET
   s" require tools/pkg-wid-probe.f" PRB+ PRB-NL ;

: PROT-PKG-WID$ ( -- ptr u8 n )
   WID-PROBE-HEAD
   S\" : PRB-WID ( -- ) s\" AWBPROT\" PKG-WID-PROBE:WID-OF . ;" PRB+ PRB-NL
   s" PRB-WID" PRB+
   PRB$ ;

: OPEN-PKG-WID$ ( -- ptr u8 n )
   WID-PROBE-HEAD
   S\" : PRB-WID ( -- ) s\" AWBOPEN\" PKG-WID-PROBE:WID-OF . ;" PRB+ PRB-NL
   s" PRB-WID" PRB+
   PRB$ ;

: READ-FIXTURE-WIDS ( -- )
   HBPWID$ PROT-PKG-WID$ READ-N PROT-WID-V !
   HBPWID$ OPEN-PKG-WID$ READ-N OPEN-WID-V ! ;

: PROBE-VARIANT ( -- )
   s" restored band carries the bitmap shape tag before batch input" T-LABEL
   HBPWID$ PROBE-TAG$ READ-N  1 T=
   s" the engine gave the fixture's two packages two different word-lists" T-LABEL
   READ-FIXTURE-WIDS
   PROT-WID 0 >  OPEN-WID 0 >  and  PROT-WID OPEN-WID <>  and TTRUE
   s" the word-list the window sealed is protected before batch input" T-LABEL
   HBPWID$ PROT-WID MEMBER-PROBE$ READ-N  1 T=
   s" the id one below it is not protected" T-LABEL
   HBPWID$ PROT-WID 1 - MEMBER-PROBE$ READ-N  0 T=
   s" the id one above it is not protected" T-LABEL
   HBPWID$ PROT-WID 1 + MEMBER-PROBE$ READ-N  0 T=
   s" a window word-list the fixture left open is not protected" T-LABEL
   HBPWID$ OPEN-WID MEMBER-PROBE$ READ-N  0 T=
   s" WIDN advanced past the baked word-list ids before batch input" T-LABEL
   HBPWID$ PROBE-WORDLIST$ READ-N  PROT-WID >  TTRUE
   s" publish into the sealed baked wid exits 84 (--load)" T-LABEL
   HBPWID$ PROT-WID FORGE-WID$ FORGE-LOAD  ASSERT-REJECT
   s" publish into the sealed baked wid exits 84 (stdin)" T-LABEL
   HBPWID$ PROT-WID FORGE-WID$ FORGE-STDIN  ASSERT-REJECT
   s" publish into the open baked wid is refused for the other reason" T-LABEL
   HBPWID$ OPEN-WID FORGE-WID$ FORGE-LOAD  ASSERT-NOT-PROTECTED
   s" an ordinary packaged define still exits 0 on the variant (--load)" T-LABEL
   HBPWID$ DEFINE-OK$ FORGE-LOAD  ASSERT-OK ;

\ At the same ordinal, the baseline has no protection bit and refuses a publish
\ for the unrelated package-context reason. This does not assert that the
\ baseline has never allocated the ordinal; the cases below prove collisions.
: PROBE-CONTROL ( -- )
   s" shipped engine's band does not hold the variant's sealed id (control)" T-LABEL
   PLAIN$ PROT-WID MEMBER-PROBE$ READ-N  0 T=
   s" shipped engine refuses a publish into it for the other reason (control)" T-LABEL
   PLAIN$ PROT-WID FORGE-WID$ FORGE-LOAD  ASSERT-NOT-PROTECTED ;

\ --- the three refusals on the build path ---------------------------------------
\ All three are proved on the real build path: the builder dies named, and no
\ engine is produced. The first refuses before the capture is asked anything -
\ `prot-wid-add` owns the bitmap bound, and an id at or above it has no bit, so
\ protecting it is impossible rather than approximate; its in-process twin lives
\ in test/seal.f beside the other seal forges. The second is the capture's own:
\ WID 0 is not a wordlist, so a band whose bit 0 is set is not a registry at all,
\ and the re-capture the mode runs is what asks.
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
   s" the engine refuses to protect a wid at the bitmap bound" T-LABEL
   s" HABU_PWID_BAD" WID-AT-BOUND BUILD-REFUSED
   s" hb: protected-WID id above the bound" ASSERT-BUILD-REFUSED
   s" capture refuses a band that marks WID 0" T-LABEL
   s" HABU_PWID_BAD" WID-NOT-A-WORDLIST BUILD-REFUSED
   s" aot-capture: protected-WID registry marks WID 0" ASSERT-BUILD-REFUSED
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
\ legitimate work: the compiler chain's A64RAV:DKEEP-HOOK-DEFAULT boot-run entry
\ died at this routine before the layers landed.
\
\ WHAT SEPARATES THE TWO IS THE MUTATION THEY SURVIVE. Delete the public-slot
\ admit and mode 1 dies 84 naming FLOOR-FROM while mode 2, whose callee's package
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
\ The maker has already allocated past the cold target's prefix WIDs. Read its
\ actual captured ordinal, then let the target allocate an owner there BEFORE
\ the seed. Only top-row.f, the last cold-prefix file, gains fixture source;
\ every other file is linked unchanged into this private invocation directory.
: COLD-PATH ( ptr u8 n -- ptr u8 n )
   COLD-ROOT$ 2swap COLD-DST JOIN-PATH COLD-DST swap ;

: COLD-LINK ( ptr u8 n -- ) {: a:ptr u:n :}
   SOURCE-ROOT:CWD$ a u COLD-SRC JOIN-PATH {: srcu:n :}
   COLD-SRC srcu a u COLD-PATH MAKE-SYMLINK ;

: COLD-CORE-LINK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" src/core/top-row.f" STR= if exit then
   a u COLD-PATH 2dup SOURCE-ROOT:DIRNAME MAKE-DIRS 2drop
   a u COLD-LINK ;

: COLD-FIXTURE$ ( n bool -- ptr u8 n ) {: wid:n sealed:bool :}
   PRB-RESET
   s" package AWB-COLLISION-SETUP" PRB+ PRB-NL
   s" : TARGET-WID ( -- n ) " PRB+ wid PRB-N s"  ;" PRB+ PRB-NL
   s" : BURN ( -- ) data-base WIDN-CELL + @ TARGET-WID > if" PRB+ PRB-NL
   S\"    s\" aot-wid: target has passed the captured ordinal\" 74 die then" PRB+ PRB-NL
   s"    begin data-base WIDN-CELL + @ TARGET-WID < while wordlist drop repeat ;" PRB+ PRB-NL
   s" public : CHECK ( n -- ) dup TARGET-WID <> if" PRB+ PRB-NL
   S\"    s\" aot-wid: target owner missed the captured ordinal\" 74 die then" PRB+ PRB-NL
   sealed if s"    dup prot-wid-add" PRB+ PRB-NL then
   S\"    s\" awb-target-before=\" type . cr ;" PRB+ PRB-NL
   s" ' BURN ;package execute" PRB+ PRB-NL
   s" package AWBTARGET public : VALUE ( -- n ) 41 ;" PRB+ PRB-NL
   s" get-current ;package AWB-COLLISION-SETUP:CHECK" PRB+ PRB-NL
   PRB$ ;

: COLD-SETUP ( n bool -- ) {: wid:n sealed:bool :}
   GATE-ROOT$ s" cold" COLD-ROOT-BUF JOIN-PATH COLD-ROOT-U !
   COLD-ROOT$ MAKE-DIR
   s" lib" COLD-LINK s" tools" COLD-LINK
   s" src/core" COLD-PATH MAKE-DIRS
   s" src/habu" COLD-LINK s" src/os" COLD-LINK
   s" src/arch" COLD-LINK s" src/compiler" COLD-LINK
   s" src/core" [: COLD-CORE-LINK ;] WALK-FILES
   s" src/core/top-row.f" s" src/core/top-row.f" COLD-PATH COPY-FILE-STREAM
   s" src/core/top-row.f" COLD-PATH wid sealed COLD-FIXTURE$ APPEND-FILE ;

: COLD-RUN ( ptr u8 n -- )
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   >LEN COLD-ROOT$ >LEN EMPTY 0 >LEN
   OUT CAP >LEN ERR CAP >LEN PROBE-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME STORE! ;

: COLD-PROBE ( -- )
   FORGE$ ALIAS-PROBE$ WRITE-ALL
   PROC-ARGV-RESET s" --load" >LEN PROC-ARGV+ FORGE$ >LEN PROC-ARGV+
   GATE-HB$ COLD-RUN ;

\ Both engines must execute their boot entry, keep the target owner's value,
\ and give the restored package its own newly allocated wordlist.
: ALIAS-CASE ( bool -- ) {: sealed:bool :}
   2 BUILD-GATE
   RC @ 0 <> if s" aot-wid-suite: builder stderr:" type cr ERR$ type cr then
   RC @ 0 T=
   GATE-HB$ EXISTS? TTRUE
   s" awb-source-wid=" TAGGED-N {: wid:n :}
   wid sealed COLD-SETUP
   PROC-ARGV-RESET GATE-HB$ COLD-RUN ASSERT-GATE-RAN
   s" awb-target-before=" TAGGED-N wid T=
   COLD-PROBE
   EXITED @ TTRUE  RC @ 0 T=
   s" awb-target-before=" TAGGED-N wid T=
   s" awb-target-wid=" TAGGED-N wid T=
   s" awb-target-owners=" TAGGED-N 1 T=
   s" awb-target-sealed=" TAGGED-N sealed if 1 else 0 then T=
   s" awb-target-value=" TAGGED-N 41 T=
   s" ... and the captured package owns its wordlist alone" T-LABEL
   s" awb-owners=" TAGGED-N  1 T=
   s" ... at an id the target's own prefix never handed out" T-LABEL
   s" awb-wid=" TAGGED-N  wid >  TTRUE
   s" ... and the engine's next id is past every id its records claim" T-LABEL
   s" awb-next=" TAGGED-N  s" awb-high=" TAGGED-N  >  TTRUE ;

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
   s" a captured package on a SEALED target wordlist boots (was exit 84)" T-LABEL
   true ALIAS-CASE
   s" a captured package on an ORDINARY target wordlist boots" T-LABEL
   false ALIAS-CASE ;

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
