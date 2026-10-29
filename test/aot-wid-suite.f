\ aot-wid-suite.f - protected-WID boot integration. The bitmap an engine bakes
\ (the wordlists its capture window sealed, stored window-relative) is restored
\ at cold startup before any batch program runs, or a batch program publishes
\ into a sealed wordlist unguarded. test/aot-wid-build.f builds a throwaway
\ `hb-pwid` in a child with a private TMPDIR; this suite probes it on the real
\ batch paths and runs the builder's refusal, rebase and forge modes.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/test/outcome.f
require test/pcap-result.f

package AOT-WID-SUITE

\ The ids the built engine gave the fixture's packages, read back by
\ READ-FIXTURE-WIDS: a captured WID is window-relative and the seed rebases it
\ onto that engine's WIDN. Probes read ids, never a count: the engine seals
\ wordlists for its own constructor families.
variable PROT-WID-V                  \ AWBPROT's public wordlist id, the one sealed
variable OPEN-WID-V                  \ AWBOPEN's, which the fixture left open
: PROT-WID ( -- n ) PROT-WID-V @ ;
: OPEN-WID ( -- n ) OPEN-WID-V @ ;
\ Past the whole span, so every address chain the band recorded falls outside
\ both spans on any tree: one compile's `here` growth is far below 16 MiB.
$1000000 constant D0-SKEW-PAST-SPAN
ENGINE-ERROR:SEAL-PACKAGE constant FORGE-RC   \ publish-into-protected exit code (84)

$8000 constant CAP
2048  constant FORGE-CAP
240000 constant BUILD-TIMEOUT-MS
30000  constant PROBE-TIMEOUT-MS
$40    constant ARG-RC

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create FIN FORGE-CAP allot   variable FIN-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC
variable EXITED

create ROOT-BUF PATH-CAP allot    variable ROOT-U
create HBPWID-BUF PATH-CAP allot   variable HBPWID-U
create FORGE-BUF PATH-CAP allot    variable FORGE-U
create REFUSE-BUF PATH-CAP allot   variable REFUSE-U
create REFUSE-HB-BUF PATH-CAP allot  variable REFUSE-HB-U
create GATE-BUF PATH-CAP allot     variable GATE-U
create GATE-HB-BUF PATH-CAP allot  variable GATE-HB-U
create COLD-ROOT-BUF PATH-CAP allot variable COLD-ROOT-U
create COLD-SRC PATH-CAP allot
create COLD-DST PATH-CAP allot

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
   FS:CLEANUP-RESET
   s" habu-aot-wid" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" hb-pwid" HBPWID-BUF FS:JOIN-PATH HBPWID-U !
   ROOT$ s" forge.f" FORGE-BUF FS:JOIN-PATH FORGE-U ! ;

: REQUIRE-PROBE ( -- )
   T-FAILURES 0 <> if
      s" aot-wid-suite: stopping after failed probe" type cr
      T-EX-FAIL throw
   then ;

\ Each refusal build gets its own tree, so "no engine appeared" is a statement
\ about that build and not about a leftover from the good one.
: REFUSE-SETUP ( -- )
   s" habu-aot-wid-refuse" FS:MKTEMP-DIR {: a:ptr u:n :}
   a REFUSE-BUF u BYTE-COPY  u REFUSE-U !
   REFUSE-ROOT$ FS:CLEANUP-TREE+
   REFUSE-ROOT$ s" hb-pwid" REFUSE-HB-BUF FS:JOIN-PATH REFUSE-HB-U ! ;

\ The boot-gate build gets its own tree too, so a probe boots only the engine it
\ wrote.
: GATE-SETUP ( -- )
   s" habu-aot-wid-gate" FS:MKTEMP-DIR {: a:ptr u:n :}
   a GATE-BUF u BYTE-COPY  u GATE-U !
   GATE-ROOT$ FS:CLEANUP-TREE+
   GATE-ROOT$ s" hb-pwid" GATE-HB-BUF FS:JOIN-PATH GATE-HB-U ! ;

create NUM-BUF 32 allot   variable NUM-U
: NUM$! ( n -- ) {: v:n :}
   SB-RESET  v FMT:SB-U  SB$ {: a:ptr u:n :}
   a NUM-BUF u BYTE-COPY  u NUM-U ! ;
: NUM$ ( -- ptr u8 n ) NUM-BUF NUM-U @ ;

\ --- spawn the variant builder as a child with a private TMPDIR ---
: BUILDER-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-wid-build.f" >LEN PROC-ARGV+ ;

: RUN-BUILDER ( -- )                 \ env already staged; captures rc/out/err
   PROC-ENV-INHERIT-MISSING
   BUILDER-ARGV
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U !
   \ The builder exits PROC-TIMEOUT-RC when a deadline expired in it: replay its
   \ stderr, which names the step, and throw E-PROC-TIMEOUT again.
   RC @ PROC-TIMEOUT-RC = if ERR$ type cr E-PROC-TIMEOUT throw then ;

: BUILD-VARIANT ( -- )
   PROC-ENV-RESET
   s" TMPDIR" >LEN ROOT$ >LEN PROC-ENV+
   RUN-BUILDER ;

\ A refusal build: the named knob carries a value the capture must reject.
: BUILD-REFUSED ( ptr u8 n n -- ) {: k:ptr ku:n v:n :}
   REFUSE-SETUP
   v NUM$!
   PROC-ENV-RESET
   s" TMPDIR" >LEN REFUSE-ROOT$ >LEN PROC-ENV+
   k ku >LEN  NUM$ >LEN PROC-ENV+
   RUN-BUILDER ;

\ A boot-gate build: the builder injects the gate fixture.
: BUILD-GATE ( -- )
   GATE-SETUP
   PROC-ENV-RESET
   s" TMPDIR" >LEN GATE-ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_GATE" >LEN  s" 1" >LEN PROC-ENV+
   RUN-BUILDER ;

\ The gate engine written again from the capture the last BUILD-GATE left in its
\ tree, with the baked wid window moved AFTER the capture, so the records name
\ wordlists it does not contain. `k` is the knob that moves it. The forge is a
\ patch the writer applies, so the window it forges is the one the unforged
\ cases booted.
: REWRITE-GATE-FORGED ( ptr u8 n n -- ) {: k:ptr ku:n v:n :}
   v NUM$!
   PROC-ENV-RESET
   s" TMPDIR" >LEN GATE-ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_REWRITE" >LEN  s" 1" >LEN PROC-ENV+
   k ku >LEN  NUM$ >LEN PROC-ENV+
   RUN-BUILDER ;

\ --- forge child spawn + outcome capture (parameterised by engine) ---
: STORE! ( len len outcome ptr u8 n -- )
   {: outu:len erru:len oc src:ptr srcu:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   oc MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF src srcu OUT OUT-U @ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: FORGE-LOAD ( ptr u8 n ptr u8 n -- ) {: e:ptr eu:n s:ptr su:n :}
   FORGE$ s su FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   FORGE$ >LEN PROC-ARGV+
   e eu >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  s su STORE! ;

: FORGE-STDIN ( ptr u8 n ptr u8 n -- ) {: e:ptr eu:n s:ptr su:n :}
   su FORGE-CAP > if E-FS-CAPACITY throw then
   s FIN su BYTE-COPY  su FIN-U !
   PROC-ARGV-RESET
   e eu >LEN  FIN FIN-U @ >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  s su STORE! ;

: PARSE-OUT ( -- n )
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

: ASSERT-OK ( -- )
   EXITED @ TTRUE
   RC @ 0 T= ;

\ Boot the engine with no program at all: the AOT boot-run list is what runs, and
\ it runs before any input is read.
: BOOT-EMPTY ( ptr u8 n -- ) {: e:ptr eu:n :}
   PROC-ARGV-RESET
   e eu >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  e eu STORE! ;

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

\ The stderr half separates admitted from never asked: a reject writes its line
\ before exit 84, so its absence with rc 0 is the admit.
: ASSERT-GATE-ADMITTED ( -- )
   EXITED @ TTRUE
   RC @ 0 T=
   ERR$ s" hb: AOT protected-WID gate reject" CONTAINS? 0= TTRUE
   OUT$ s" awb-gate=open" CONTAINS? TTRUE ;

\ A define into an unprotected wordlist is refused for another reason:
\ `set-current` leaves no authenticated package, exit 70
\ (CHECKER-PKG-CONTEXT-REJECT). The code and the diagnostic differ, so an 84 can
\ only come from the protected-WID bitmap.
82 constant SEED-RC                  \ src/core/engine-error.f ENGINE-ERROR:AOT-SEED
: ASSERT-NOT-PROTECTED ( -- )
   EXITED @ TTRUE
   RC @ CHECKER-REJECT-RC T=
   ERR$ s" hb: cannot publish into protected word" CONTAINS? 0= TTRUE ;

\ --- forge / probe sources (interpreted by the child engine) ---
\ Every probe prints one number for the parent. Membership is one bit per
\ wordlist id, read through tools/prot-wid-probe.f, the bits the engine's
\ LPROTWIDQ routine tests: the probes assert which ids came back and that both
\ neighbours of the restored id did not, which a smeared or mis-shifted restore
\ would set. No build sets a bit in the band's upper half: the seed hands out
\ ids just above the target's WIDN.
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

\ The child engine still defines: without this the exit-84 cases could mean it
\ refuses every definition.
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
   s" require src/lib/fmt.f" PRB+ PRB-NL
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
   HBPWID$ PROT-PKG-WID$ READ-N PROT-WID-V !  REQUIRE-PROBE
   HBPWID$ OPEN-PKG-WID$ READ-N OPEN-WID-V !  REQUIRE-PROBE ;

: PROBE-VARIANT ( -- )
   s" restored band carries the bitmap shape tag before batch input" T-LABEL
   HBPWID$ PROBE-TAG$ READ-N  1 T=  REQUIRE-PROBE
   s" the engine gave the fixture's two packages two different word-lists" T-LABEL
   READ-FIXTURE-WIDS
   PROT-WID 0 >  OPEN-WID 0 >  and  PROT-WID OPEN-WID <>  and TTRUE  REQUIRE-PROBE
   s" the word-list the window sealed is protected before batch input" T-LABEL
   HBPWID$ PROT-WID MEMBER-PROBE$ READ-N  1 T=  REQUIRE-PROBE
   s" the id one below it is not protected" T-LABEL
   HBPWID$ PROT-WID 1 - MEMBER-PROBE$ READ-N  0 T=  REQUIRE-PROBE
   s" the id one above it is not protected" T-LABEL
   HBPWID$ PROT-WID 1 + MEMBER-PROBE$ READ-N  0 T=  REQUIRE-PROBE
   s" a window word-list the fixture left open is not protected" T-LABEL
   HBPWID$ OPEN-WID MEMBER-PROBE$ READ-N  0 T=  REQUIRE-PROBE
   s" WIDN advanced past the baked word-list ids before batch input" T-LABEL
   HBPWID$ PROBE-WORDLIST$ READ-N  PROT-WID >  TTRUE  REQUIRE-PROBE
   s" publish into the sealed baked wid exits 84 (--load)" T-LABEL
   HBPWID$ PROT-WID FORGE-WID$ FORGE-LOAD  ASSERT-REJECT  REQUIRE-PROBE
   s" publish into the sealed baked wid exits 84 (stdin)" T-LABEL
   HBPWID$ PROT-WID FORGE-WID$ FORGE-STDIN  ASSERT-REJECT  REQUIRE-PROBE
   s" publish into the open baked wid is refused for the other reason" T-LABEL
   HBPWID$ OPEN-WID FORGE-WID$ FORGE-LOAD  ASSERT-NOT-PROTECTED  REQUIRE-PROBE
   s" an ordinary packaged define still exits 0 on the variant (--load)" T-LABEL
   HBPWID$ DEFINE-OK$ FORGE-LOAD  ASSERT-OK ;

\ --- the refusal on the build path ---------------------------------------------
\ The builder dies named and no engine is produced. The bitmap's upper bound is
\ test/seal.f's: `prot-wid-add` refuses an id at or above it before any capture.
\ The capture reads the address-literal band, so a recorded site holds a real
\ address; one in neither span is an address the window does not carry, and
\ rebasing or skipping it would bake a wrong or build-host address. This case is
\ the only one that executes the refusal.
: ASSERT-BUILD-REFUSED ( ptr u8 n -- ) {: m:ptr mu:n :}
   RC @ 0 <> TTRUE
   ERR$ m mu CONTAINS? TTRUE
   REFUSE-HB$ FS:EXISTS? 0= TTRUE ;

: PROBE-REFUSE-ADDRESS-SPAN ( -- )
   s" capture refuses a recorded chain its window cannot place" T-LABEL
   s" HABU_AOT_D0_SKEW" D0-SKEW-PAST-SPAN BUILD-REFUSED
   s" aot-capture: recorded address site outside both window spans" ASSERT-BUILD-REFUSED ;

\ --- the AOT boot gate ---------------------------------------------------------
\ The registry's other end: a name the engine baked must not resolve into a
\ protected wordlist. The AOT seed rewrites a call, writes an xt or branches for
\ each stored name, and LAOTWIDGATE stands between the lookup and all three; the
\ admit PROBE-WID-REBASE asserts on both target boots is the only execution of
\ it.
\
\ The window calls a qualified prefix word, so the seed resolves it through that
\ package's public slot and the gate judges the wid the lookup used. The
\ callee's package seals nothing; the builder asserts that before it builds, so
\ a tree that sealed CHECKER-TAPE stops the build by name.
\
\ No case can make the gate refuse: a call site's scope is a window coordinate
\ the seed rebases into the window, and LFIND's qualifier path reads only a
\ public slot. EM-AOTWIDGATE's note carries the proof and why the leg is kept.
: REQUIRE-GATE-BUILD ( -- )
   RC @ 0 <> if
      s" aot-wid-suite: builder stderr:" type cr ERR$ type cr
      RC @ throw
   then
   GATE-HB$ FS:EXISTS? 0= if
      s" aot-wid-suite: builder wrote no engine" type cr
      E-FS-OPEN throw
   then ;

: BUILT-GATE ( -- )
   BUILD-GATE
   RC @ 0 T=
   GATE-HB$ FS:EXISTS? TTRUE
   REQUIRE-GATE-BUILD ;

\ --- the wid rebase ------------------------------------------------------------
\ A captured record carries the wid it had in the metabuild host, which numbers
\ its wordlists independently of the target. Registered verbatim, the word lands
\ in whatever wordlist the target keeps at that number: a sealed one refuses the
\ boot (exit 84), an ordinary one takes the word silently and two packages own
\ one wordlist.
\
\ The maker has allocated past the cold target's prefix WIDs: read its captured
\ ordinal and let the target allocate an owner there before the seed. Only
\ top-row.f, the last cold-prefix file, gains fixture source; every other file
\ is linked unchanged.
: COLD-PATH ( ptr u8 n -- ptr u8 n )
   COLD-ROOT$ 2swap COLD-DST FS:JOIN-PATH COLD-DST swap ;

: COLD-LINK ( ptr u8 n -- ) {: a:ptr u:n :}
   SOURCE-ROOT:CWD$ a u COLD-SRC FS:JOIN-PATH {: srcu:n :}
   COLD-SRC srcu a u COLD-PATH FS:MAKE-SYMLINK ;

: COLD-CORE-LINK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" src/core/top-row.f" STR= if exit then
   a u COLD-PATH 2dup SOURCE-ROOT:DIRNAME FS:MAKE-DIRS 2drop
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

\ Each target gets its own directory, so the sealed and the ordinary target
\ boot the one GATE=2 engine against two prefixes.
: COLD-SETUP ( ptr u8 n n bool -- ) {: dir:ptr diru:n wid:n sealed:bool :}
   GATE-ROOT$ dir diru COLD-ROOT-BUF FS:JOIN-PATH COLD-ROOT-U !
   COLD-ROOT$ FS:MAKE-DIR
   s" tools" COLD-LINK
   s" src/core" COLD-PATH FS:MAKE-DIRS
   s" src/lib" COLD-LINK
   s" src/habu" COLD-LINK s" src/os" COLD-LINK
   s" src/arch" COLD-LINK s" src/compiler" COLD-LINK
   s" src/core" [: COLD-CORE-LINK ;] FS:WALK-FILES
   s" src/core/top-row.f" s" src/core/top-row.f" COLD-PATH FS:COPY-FILE-STREAM
   s" src/core/top-row.f" COLD-PATH wid sealed COLD-FIXTURE$ FS:APPEND-FILE ;

: COLD-RUN ( ptr u8 n -- )
   {: e:ptr eu:n :}
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   e eu >LEN COLD-ROOT$ >LEN EMPTY 0 >LEN
   OUT CAP >LEN ERR CAP >LEN PROBE-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME e eu STORE! ;

: COLD-PROBE ( -- )
   FORGE$ ALIAS-PROBE$ FS:WRITE-ALL
   PROC-ARGV-RESET s" --load" >LEN PROC-ARGV+ FORGE$ >LEN PROC-ARGV+
   GATE-HB$ COLD-RUN ;

variable SOURCE-WID                  \ the ordinal the capture gave AWBGATE, as its build printed it

: TARGET-LABEL ( bool ptr u8 n ptr u8 n -- ) {: sealed:bool s:ptr su:n o:ptr ou:n :}
   sealed if s su else o ou then T-LABEL ;

\ Both targets must execute the engine's boot entry, keep the target owner's
\ value, and give the restored package its own newly allocated wordlist.
: ALIAS-CASE ( ptr u8 n bool -- ) {: dir:ptr diru:n sealed:bool :}
   SOURCE-WID @ {: wid:n :}
   dir diru wid sealed COLD-SETUP
   sealed s" SEALED target: the call into an UNSEALED prefix package is admitted at the seed"
   s" ORDINARY target: the call into an UNSEALED prefix package is admitted at the seed" TARGET-LABEL
   PROC-ARGV-RESET GATE-HB$ COLD-RUN ASSERT-GATE-ADMITTED  REQUIRE-PROBE
   sealed s" SEALED target: the target allocated its owner at the captured ordinal"
   s" ORDINARY target: the target allocated its owner at the captured ordinal" TARGET-LABEL
   s" awb-target-before=" TAGGED-N wid T=
   COLD-PROBE
   EXITED @ TTRUE  RC @ 0 T=  REQUIRE-PROBE
   s" awb-target-before=" TAGGED-N wid T=
   s" awb-target-wid=" TAGGED-N wid T=
   s" awb-target-owners=" TAGGED-N 1 T=
   s" awb-target-sealed=" TAGGED-N sealed if 1 else 0 then T=
   s" awb-target-value=" TAGGED-N 41 T=
   sealed s" SEALED target: the captured package owns its wordlist alone"
   s" ORDINARY target: the captured package owns its wordlist alone" TARGET-LABEL
   s" awb-owners=" TAGGED-N  1 T=
   sealed s" SEALED target: ... at an id the target's own prefix never handed out"
   s" ORDINARY target: ... at an id the target's own prefix never handed out" TARGET-LABEL
   s" awb-wid=" TAGGED-N  wid >  TTRUE
   sealed s" SEALED target: the engine's next id is past every id its records claim"
   s" ORDINARY target: the engine's next id is past every id its records claim" TARGET-LABEL
   s" awb-next=" TAGGED-N  s" awb-high=" TAGGED-N  >  TTRUE ;

\ The capture's own refusal, reached by telling it the window made fewer
\ wordlists than it did. It names the RECORD, which is what the boot's refusal
\ cannot do, and it stops the build before an engine exists.
: BUILD-GATE-NARROW ( n -- ) {: v:n :}
   GATE-SETUP
   v NUM$!
   PROC-ENV-RESET
   s" TMPDIR" >LEN GATE-ROOT$ >LEN PROC-ENV+
   s" HABU_AOT_GATE" >LEN  s" 1" >LEN PROC-ENV+
   s" HABU_AOT_WID_NARROW" >LEN  NUM$ >LEN PROC-ENV+
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
   GATE-HB$ FS:EXISTS? 0= TTRUE ;

\ The seed's own refusal, which the capture-side audit makes unreachable from a
\ real capture: only a baked window that disagrees with the baked records can
\ reach it, and that is what the two forges bake. The rewrite removes the image
\ it replaces, so the engine booted here is this forge's and never the last one.
: FORGED-CASE ( ptr u8 n n -- ) {: k:ptr ku:n v:n :}
   k ku v REWRITE-GATE-FORGED
   RC @ 0 T=
   REQUIRE-GATE-BUILD
   GATE-HB$ BOOT-EMPTY
   EXITED @ TTRUE
   RC @ SEED-RC T=
   ERR$ s" hb: AOT wid outside the capture window" CONTAINS? TTRUE
   OUT$ s" awb-gate=" CONTAINS? 0= TTRUE ;

\ Not covered: no mode forges a protected-WID row, which needs a poke into the
\ capture buffer between capture and emit. This reaches the same refusal, "hb:
\ AOT wid outside the capture window" (ENGINE-ERROR:AOT-SEED), through a forged
\ record wid, over the capture PROBE-WID-REBASE left in the gate tree.
: PROBE-WID-FORGED ( -- )
   s" a baked wid below the baked window is refused at the seed" T-LABEL
   s" HABU_AOT_WID_SKEW" 5 FORGED-CASE  REQUIRE-PROBE
   s" a baked wid past the baked window's end is refused at the seed" T-LABEL
   s" HABU_AOT_WID_SPAN" 1 FORGED-CASE ;

\ One capture serves every case below: the GATE=2 build boots a sealed and an
\ ordinary target, and the writer writes it twice more forged.
: PROBE-WID-REBASE ( -- )
   s" the GATE=2 engine builds and reports the ordinal its capture used" T-LABEL
   BUILT-GATE
   s" awb-source-wid=" TAGGED-N SOURCE-WID !  REQUIRE-PROBE
   s" cold-sealed" true ALIAS-CASE  REQUIRE-PROBE
   s" cold-open" false ALIAS-CASE  REQUIRE-PROBE
   PROBE-WID-FORGED ;

: REQUIRE-VARIANT-BUILD ( -- )
   RC @ 0 <> if
      s" aot-wid-suite: builder stderr:" type cr ERR$ type cr
      RC @ throw
   then
   HBPWID$ FS:EXISTS? 0= if
      s" aot-wid-suite: builder wrote no engine" type cr
      E-FS-OPEN throw
   then ;

\ EM-STARTUP-RUNTIME-STATE (src/habu/habu2.f) clears the live band, publishes
\ the shape tag and calls the LAOTPROT routine (EMIT-AOT-PROT-RESTORE) before
\ the cold prefix registers its constructor families; warm snapshot startup
\ skips both and keeps the snapshot's band. The restore release-publishes
\ PROT-REG-TAG into the tag cell last, so a half-copied band is never
\ observable. Without the call every probe goes red.
: PROBE-RESTORE ( -- )
   BUILD-VARIANT
   s" aot-wid variant engine builds cleanly" T-LABEL
   RC @ 0 T=
   s" hb-pwid variant exists after build" T-LABEL
   HBPWID$ FS:EXISTS? TTRUE
   REQUIRE-VARIANT-BUILD
   PROBE-VARIANT ;

: FULL ( -- )
   PROBE-RESTORE  REQUIRE-PROBE
   PROBE-REFUSE-ADDRESS-SPAN  REQUIRE-PROBE
   PROBE-WID-REBASE  REQUIRE-PROBE
   PROBE-WID-CAPTURE-REFUSAL ;

: ARG= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ 2swap STR= ;

: ARG-ERROR ( -- )
   s" aot-wid-suite: expected exactly one known case" type cr
   ARG-RC throw ;

: SELECTED ( -- )
   SETUP
   s" restore" ARG= if PROBE-RESTORE exit then
   s" refuse-address-span" ARG= if PROBE-REFUSE-ADDRESS-SPAN exit then
   s" rebase" ARG= if PROBE-WID-REBASE exit then
   s" capture-refusal" ARG= if PROBE-WID-CAPTURE-REFUSAL exit then
   ARG-ERROR ;

\ Cost: four independent captures, one of which the rebase case writes three
\ ways. The gate registers one case for each capture but restore, which
\ test/package-seal.f covers on the shipped engine; a standalone run with no
\ case keeps the complete focused sequence.
\ Run standalone: bin/hb --load test/aot-wid-suite.f [-- CASE]
: BODY ( -- )
   SCRIPT-ARGC 0= if SETUP FULL exit then
   SCRIPT-ARGC 1 <> if ARG-ERROR then
   SELECTED ;

public

: RUN ( -- )
   T-RESET
   FS:CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   FS:CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-wid-suite: ok" type cr ;

;package

AOT-WID-SUITE:RUN
