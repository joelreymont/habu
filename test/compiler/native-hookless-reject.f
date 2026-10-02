\ native-hookless-reject.f - with the check hook cell empty, a definition the
\ checker leaves without an effect is refused with the checker's reason.
\
\ Nothing certifies while the cell is empty: a `0 set-check` session, and a
\ window's core prefix, which tools/native-build-core.f LOGICAL-RESET compiles
\ from src/core/util.f up to src/core/check-hook.f with the cell cleared. The
\ native compiler compiles against the effect the checker holds for the name,
\ and the checker records none for a body it rejects or cannot check, so the
\ compiler refuses one (src/compiler/native/compiler.f KEEP-ARITY) with the check
\ hook's reject status, after printing the diagnostic of the owner's quiet scan
\ (CHECK-HOOKLESS). A rejected body the checker still records is compiled.
\
\ Tier-neutral by design: each session subject sets the tier it compiles at, and
\ the window is built by the engine under test. The window cases build a private
\ copy of src/, lib/ and tools/ whose src/core/util.f ends in one probe
\ definition; the printed directory keeps the copy and the last build's
\ temporary directory.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f
require src/habu/verify-source.f

package HOOKLESS-REJECT-TEST
private

$4000 constant CAP
20000 constant SUBJECT-TIMEOUT-MS
600000 constant BUILD-TIMEOUT-MS
70 constant RC-REJECT                \ the check hook's reject status (src/core/check-hook.f CHECK-RC)

create ROOT FS-PATH-CAP allot          variable ROOT-U
create TREE FS-PATH-CAP allot          variable TREE-U
create TMP FS-PATH-CAP allot           variable TMP-U
create IMAGE FS-PATH-CAP allot         variable IMAGE-U
create UTIL FS-PATH-CAP allot          variable UTIL-U
create DEST FS-PATH-CAP allot          variable DEST-U
create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: UTIL$ ( -- ptr u8 n ) UTIL UTIL-U @ ;
: DEST$ ( -- ptr u8 n ) DEST DEST-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ Both streams are shown when the expected text is missing.
: SHOW ( bool -- bool )
   dup 0= if OUT$ type ERR$ type then ;

: SAYS ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR$ a u CONTAINS? SHOW TTRUE ;

: PRINTS ( ptr u8 n -- ) {: a:ptr u:n :}
   OUT$ a u CONTAINS? SHOW TTRUE ;

\ ---- sessions: `0 set-check` at tier 1 ---------------------------------------
: SUBJECT-STORE! ( len len outcome ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N ERR-U !  outu LEN>N OUT-U !
   oc MATCH outcome
      timeout OF src u OUT$ ERR$ T-TIMED-OUT ENDOF
      exited OF RC ! ENDOF
      signaled OF drop -1 RC ! ENDOF
   ;MATCH ;

: SESSION ( ptr u8 n -- ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN SUBJECT-TIMEOUT-MS >MS SUBJECT:RUN
   src u SUBJECT-STORE! ;

\ Both arms of the `if` are well typed and leave different depths, so the body
\ has no effect to record and the checker rejects it at `then`.
: UNEVEN$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-UNEVEN ( bool -- n ) if 1 2 else 3 then ;" ;

: SESSION-UNEVEN ( -- )
   s" uneven branches are refused with the checker's reason" T-LABEL
   UNEVEN$ SESSION
   RC @ RC-REJECT T=
   s" habu: in hr-uneven: at 'then'" SAYS
   s" ncomp: cannot compile HR-UNEVEN" SAYS ;

\ An undefined callee leaves the body uncheckable, a verdict whose text the scan
\ itself never prints (src/core/check-hook.f REPORT-UNCHECKABLE does).
: UNDEFINED$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-UNDEFINED ( n -- bool ) HR-NO-SUCH-WORD ;" ;

: SESSION-UNDEFINED ( -- )
   s" an undefined callee is refused naming the callee" T-LABEL
   UNDEFINED$ SESSION
   RC @ RC-REJECT T=
   s" E-UNDEFINED habu: in hr-undefined: undefined word 'HR-NO-SUCH-WORD'" SAYS ;

\ A definer's own body is scanned before its does> clause, which the checker
\ scans next, so the reason has to be printed before that second scan.
: DEFINER$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-DEFINER ( n -- ) create , HR-NO-SUCH-WORD does> ( -- n ) @ ;" ;

: SESSION-DEFINER ( -- )
   s" a does> definer's own body is refused with its reason" T-LABEL
   DEFINER$ SESSION
   RC @ RC-REJECT T=
   s" E-UNDEFINED habu: in hr-definer: undefined word 'HR-NO-SUCH-WORD'" SAYS ;

\ The source pre-pass marks the wordlist a rendering statement runs in, and a
\ word it never saw defined there is left to the run (checker verdict 2) rather
\ than refused. An unsigned body naming one has no effect either.
: RENDERING$ ( -- ptr u8 n )
   S\" : HR-MAKE ( -- ) s\" : HR-SEVEN ( -- n ) 7 ;\" INCLUDE-EVALUATE ; HR-MAKE" ;

: MARK-RENDERED ( -- )
   RENDERING$ VERIFY:SOURCE-BUF-IN-SCOPE ;

: UNSEEN$ ( -- ptr u8 n )
   s" MARK-RENDERED 1 set-tier 0 set-check : HR-UNSEEN HR-SEVEN ;" ;

: SESSION-UNSEEN ( -- )
   s" a name left to the run is refused naming it" T-LABEL
   UNSEEN$ SESSION
   RC @ RC-REJECT T=
   s" E-UNDEFINED habu: in hr-unseen: undefined word 'HR-SEVEN'" SAYS ;

\ Multi-error mode records a rejected body's declaration (src/core/checker.f
\ CHECK), so the compiler has an effect and compiles it; the reject it counts
\ is still printed.
: RECORDED$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check MULTI-ERR-BEGIN : HR-KEPT ( n -- bool ) 1 + ; 5 HR-KEPT . MULTI-ERR-END . cr" ;

: SESSION-RECORDED ( -- )
   s" a rejected body the checker records still compiles and runs" T-LABEL
   RECORDED$ SESSION
   RC @ 0 T=
   S\" 6\n1\n" PRINTS
   s" habu: in hr-kept: at '+'" SAYS ;

\ ---- the window: a probe at the end of src/core/util.f ----------------------
: ROOT-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   ROOT$ a u dst JOIN-PATH up ! ;

: SETUP ( -- )
   s" native-hookless-reject" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" tree" TREE TREE-U ROOT-PATH!
   s" tmp" TMP TMP-U ROOT-PATH! TMP$ MAKE-DIRS
   s" hb" IMAGE IMAGE-U ROOT-PATH!
   TREE$ s" src/core/util.f" UTIL JOIN-PATH UTIL-U ! ;

: PARENT-U ( ptr u8 n -- n ) {: a:ptr u:n :}
   u begin dup 0 > while
      1 -
      a over + c@ 47 = if exit then
   repeat ;

: COPY-MEMBER ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE? 0= if exit then
   a u SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE {: rel:ptr relu:n :}
   TREE$ rel relu DEST JOIN-PATH DEST-U !
   DEST$ {: dest:ptr destu:n :}
   dest destu PARENT-U {: parentu:n :}
   dest parentu MAKE-DIRS
   a u DEST$ COPY-FILE-STREAM ;

: COPY-TREE ( -- )
   s" src" [: COPY-MEMBER ;] WALK-FILES
   s" lib" [: COPY-MEMBER ;] WALK-FILES
   s" tools" [: COPY-MEMBER ;] WALK-FILES ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc RC>N RC ! ENDOF
   ;MATCH ;

: ARGV+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ENV! ( -- )
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_WHITEBOX_IMAGE" >LEN NULL$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ The copy's util.f is the checkout's with one definition appended. A window
\ build exits 74 for any failure (tools/native-build-core.f BUILD-RC), so the
\ reason and the refusal's status are read from what the build printed.
: BUILD-PROBE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" src/core/util.f" UTIL$ COPY-FILE-STREAM
   UTIL$ a u APPEND-FILE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARGV+
   s" tools/native-build.f" ARGV+
   s" --" ARGV+
   IMAGE$ ARGV+
   ENV!
   ENGINE-CANDIDATE:PATH$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: WINDOW-UNDEFINED ( -- )
   s" the window's prefix names a callee it does not define" T-LABEL
   S\" : PROBE-NZ ( n -- bool ) NO-SUCH-PROBE-WORD ;\n" BUILD-PROBE
   RC @ 0 T<>
   s" E-UNDEFINED habu: in probe-nz: undefined word 'NO-SUCH-PROBE-WORD'" SAYS
   s" native-build: uncaught throw code 70" PRINTS ;

\ `0<>` is lib/prelude.f's, which the window has not loaded at util.f.
: WINDOW-PRELUDE ( -- )
   s" the window's prefix names a prelude word it has not loaded" T-LABEL
   S\" : PROBE-NZ ( n -- bool ) 0<> ;\n" BUILD-PROBE
   RC @ 0 T<>
   s" E-UNDEFINED habu: in probe-nz: undefined word '0<>'" SAYS
   s" native-build: uncaught throw code 70" PRINTS ;

: RUN ( -- )
   T-RESET
   SESSION-UNEVEN
   SESSION-UNDEFINED
   SESSION-DEFINER
   SESSION-UNSEEN
   SESSION-RECORDED
   SETUP
   s" native-hookless-reject artifacts: " type ROOT$ type cr
   COPY-TREE
   WINDOW-UNDEFINED
   WINDOW-PRELUDE
   T-REPORT ;

RUN
;package
