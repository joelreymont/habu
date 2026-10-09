\ native-hookless-reject.f - ordinary hookless compilation keeps the declared
\ row; a native build requires a certified scan of its pre-hook prefix.
\
\ In an ordinary `0 set-check` session, the owner's quiet scan prints its
\ reason and enforces nothing (CHECK-HOOKLESS); the declaration becomes the
\ definition's row, without authority (DECLARE-HOOKLESS), and the native
\ compiler compiles against it. A body it cannot compile against that row -
\ branches that leave different depths, a callee that is not defined - dies
\ there as an uncaught throw, naming the token where it can. A body with no
\ declaration, or one the checker cannot record, has no row, and KEEP-ARITY
\ refuses it with the check hook's reject status.
\
\ A hook that certifies a definition through the checker and then answers 0
\ drops it, and the rows it recorded go with it: at tier 0 the load goes on
\ without it, at tier 1 the compiler refuses it (E-NCOMP-VERDICT). Either way
\ a later word is checked against the word the engine binds.
\
\ The native build requires the same scan's verdict for its hookless prefix.
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
require lib/tree-copy.f

package HOOKLESS-REJECT-TEST
private

$4000 constant CAP
20000 constant SUBJECT-TIMEOUT-MS
600000 constant BUILD-TIMEOUT-MS
70 constant RC-REJECT                \ the check hook's reject status (src/core/check-hook.f CHECK-RC)
67 constant RC-THROW                 \ a load's uncaught throw
74 constant RC-BUILD                 \ a window build's failure status (tools/native-build-args.f BUILD-RC)

create ROOT FS-PATH-CAP allot          variable ROOT-U
create TREE FS-PATH-CAP allot          variable TREE-U
create TMP FS-PATH-CAP allot           variable TMP-U
create IMAGE FS-PATH-CAP allot         variable IMAGE-U
create UTIL FS-PATH-CAP allot          variable UTIL-U
create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: UTIL$ ( -- ptr u8 n ) UTIL UTIL-U @ ;
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

\ `0=` leaves a bool where the declaration says n: the scan refuses the body,
\ and the compiled word runs as declared.
: DECLARED$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-DECLARED ( n -- n ) 0= ; 5 HR-DECLARED ." ;

: SESSION-DECLARED ( -- )
   s" a refused body compiles against its declaration after its reason" T-LABEL
   DECLARED$ SESSION
   RC @ 0 T=
   S\" 0\n" PRINTS
   s" habu: in hr-declared: at '0='" SAYS ;

\ Both arms of the `if` are well typed and leave different depths: the checker
\ rejects the body at `then`, and the elaborator cannot join the arms either
\ (E-NELAB-JOIN).
: UNEVEN$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-UNEVEN ( bool -- n ) if 1 2 else 3 then ;" ;

: SESSION-UNEVEN ( -- )
   s" uneven branches are refused by the compiler after the checker's reason" T-LABEL
   UNEVEN$ SESSION
   RC @ RC-THROW T=
   s" habu: in hr-uneven: at 'then'" SAYS
   s" ncomp: cannot compile HR-UNEVEN" SAYS
   s" hb: uncaught throw code -8503" SAYS ;

\ An undefined callee leaves the body uncheckable, a verdict whose text the scan
\ itself never prints (src/core/check-hook.f REPORT-UNCHECKABLE does); the
\ compiler then cannot lower the call (E-HIR-UNMODELED).
: UNDEFINED$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-UNDEFINED ( n -- bool ) HR-NO-SUCH-WORD ;" ;

: SESSION-UNDEFINED ( -- )
   s" an undefined callee is refused naming the callee" T-LABEL
   UNDEFINED$ SESSION
   RC @ RC-THROW T=
   s" E-UNDEFINED habu: in hr-undefined: undefined word 'HR-NO-SUCH-WORD'" SAYS
   s" ncomp: cannot compile HR-UNDEFINED at HR-NO-SUCH-WORD" SAYS ;

\ A definer's own body is scanned before its does> clause, which the checker
\ scans next, so the reason has to be printed before that second scan.
: DEFINER$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-DEFINER ( n -- ) create , HR-NO-SUCH-WORD does> ( -- n ) @ ;" ;

: SESSION-DEFINER ( -- )
   s" a does> definer's own body is refused with its reason" T-LABEL
   DEFINER$ SESSION
   RC @ RC-THROW T=
   s" E-UNDEFINED habu: in hr-definer: undefined word 'HR-NO-SUCH-WORD'" SAYS
   s" ncomp: cannot compile HR-DEFINER at HR-NO-SUCH-WORD" SAYS ;

\ The source pre-pass marks the wordlist a rendering statement runs in, and a
\ word it never saw defined there is left to the run (checker verdict 2) rather
\ than refused. An unsigned body naming one has no effect, and no declaration
\ to compile against.
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

\ A declaration the checker cannot record leaves no row either: the refusal is
\ the reject status, and the definition retracts nothing. The checker interns
\ the name before it refuses the declaration, and at top level the bare name
\ binds that rowless global, so the subject closes this package first.
: UNRECORDED$ ( -- ptr u8 n )
   s" ;package 1 set-tier 0 set-check : HR-UNRECORDED ( n -- no-such-type ) ;" ;

: SESSION-UNRECORDED ( -- )
   s" an unrecordable declaration is refused with the reject status" T-LABEL
   UNRECORDED$ SESSION
   RC @ RC-REJECT T=
   s" habu: in hr-unrecorded: unknown type 'no-such-type' in signature" SAYS ;

\ Multi-error mode records a rejected body's declaration as a recovery fact
\ (src/core/checker.f CHECK), which the declared row leaves in place; the reject
\ it counts is still printed.
: RECORDED$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check MULTI-ERR-BEGIN : HR-KEPT ( n -- bool ) 1 + ; 5 HR-KEPT . MULTI-ERR-END . cr" ;

: SESSION-RECORDED ( -- )
   s" a rejected body the checker records still compiles and runs" T-LABEL
   RECORDED$ SESSION
   RC @ 0 T=
   S\" 6\n1\n" PRINTS
   s" habu: in hr-kept: at '+'" SAYS ;

\ ---- a hook's zero verdict: the dropped definition keeps no row -------------
\ LIAR-FORGET certifies every definition through the engine's hook, which
\ records its row and opens its pending window, then answers 0 for a name
\ that starts with W. The texts outrun a source line, so each is assembled in
\ TEXT; each closes this package first, so its words are globals as in a file.
create TEXT CAP allot                  variable TEXT-U

: TEXT$ ( -- ptr u8 n ) TEXT TEXT-U BUF-LEN@ ;
: TEXT+ ( ptr u8 n -- ) TEXT CAP TEXT-U BUF-APPEND ;

: TIER-TEXT ( n -- ) {: tier:n :}
   TEXT-U BUF-RESET
   tier 0= if s" ;package 0 set-tier " else s" ;package 1 set-tier " then TEXT+ ;

: GLOBAL-W+ ( -- ) s" : W-FORGET ( -- n ) 1 ; " TEXT+ ;
: P-W+ ( -- ) s" package P-FORGET public : W-FORGET ( -- n ) 2 ; ;package " TEXT+ ;
: R-W+ ( -- ) s" package R-FORGET public : W-FORGET ( -- n ) 5 ; ;package " TEXT+ ;
: Q-USING-P+ ( -- ) s" package Q-FORGET public using P-FORGET " TEXT+ ;

: LIAR+ ( -- )
   s" : LIAR-FORGET ( ptr u8 n -- n ) over c@ [char] W = >r " TEXT+
   s" LOWER-CERT-HOOK:HOOK r> if drop 0 then ; ' LIAR-FORGET set-check " TEXT+ ;

: SAYS-NOT ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR$ a u CONTAINS? 0= SHOW TTRUE ;

\ The tier-1 refusal of the dropped definition, uncaught: the load ends there.
: REFUSED-W ( -- )
   RC @ RC-THROW T=
   s" ncomp: cannot compile W-FORGET" SAYS
   s" hb: uncaught throw code -8572" SAYS ;

\ With a global W-FORGET and a used P-FORGET:W-FORGET, a bare W-FORGET is
\ ambiguous to the engine: tier 0 refuses T-FORGET, which named it. A row of
\ the dropped Q-FORGET:W-FORGET would bind T-FORGET's call instead. These run
\ at tier 0 alone: tier 1 refuses the definition of W-FORGET itself for that
\ ambiguity before its hook runs.
: SHADOWED ( -- )
   RC @ RC-REJECT T=
   s" E-USING-SHADOW-GLOBAL" SAYS
   OUT-U @ 0 T= ;

: ZERO-CALL ( -- )
   s" a dropped definition leaves a call to the engine's shadowed word" T-LABEL
   0 TIER-TEXT GLOBAL-W+ P-W+ LIAR+ Q-USING-P+
   s" : W-FORGET 3 ; : T-FORGET ( -- n ) W-FORGET ; ;using ;package Q-FORGET:T-FORGET . cr" TEXT+
   TEXT$ SESSION  SHADOWED ;

: ZERO-TICK ( -- )
   s" a dropped definition leaves a tick to the engine's shadowed word" T-LABEL
   0 TIER-TEXT GLOBAL-W+ P-W+ LIAR+ Q-USING-P+
   s" : W-FORGET 3 ; : T-FORGET ( -- n ) ['] W-FORGET drop 4 ; ;using ;package " TEXT+
   s" Q-FORGET:T-FORGET . cr" TEXT+
   TEXT$ SESSION  SHADOWED ;

\ Two used packages and a global: the engine's bare W-FORGET is ambiguous.
: AMBIGUOUS-TEXT ( n -- ) {: tier:n :}
   tier TIER-TEXT GLOBAL-W+ P-W+ R-W+ LIAR+ Q-USING-P+
   s" using R-FORGET : W-FORGET 3 ; " TEXT+ ;

: AMBIGUOUS ( n -- ) {: tier:n :}
   RC @ RC-THROW T=
   s" E-USING-AMBIGUOUS" SAYS
   OUT-U @ 0 T=
   tier 0= if s" hb: uncaught throw code 7144" SAYS else REFUSED-W then ;

: ZERO-AMBIGUOUS-CALL ( n -- ) {: tier:n :}
   s" a dropped definition leaves a call to the engine's ambiguous word" T-LABEL
   tier AMBIGUOUS-TEXT
   s" : T-FORGET ( -- n ) W-FORGET ; ;using ;using ;package Q-FORGET:T-FORGET . cr" TEXT+
   TEXT$ SESSION  tier AMBIGUOUS ;

: ZERO-AMBIGUOUS-TICK ( n -- ) {: tier:n :}
   s" a dropped definition leaves a tick to the engine's ambiguous word" T-LABEL
   tier AMBIGUOUS-TEXT
   s" : T-FORGET ( -- n ) ['] W-FORGET drop 4 ; ;using ;using ;package " TEXT+
   s" Q-FORGET:T-FORGET . cr" TEXT+
   TEXT$ SESSION  tier AMBIGUOUS ;

\ The dropped W-FORGET left two cells; the engine's left one. Typed against the
\ dropped row, T-FORGET ( -- n n ) certified and ran one cell short.
: ZERO-SHAPE ( -- )
   s" a dropped definition's effect does not type a later caller" T-LABEL
   0 TIER-TEXT GLOBAL-W+ P-W+ LIAR+ Q-USING-P+
   s" : W-FORGET 3 4 ; : T-FORGET ( -- n n ) W-FORGET ; ;using ;package " TEXT+
   s" Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT$ SESSION  SHADOWED
   s" interpret stack underdepth" SAYS-NOT ;

\ With no global, T-FORGET's W-FORGET is the used P-FORGET:W-FORGET ( -- n ):
\ tier 0 refuses T-FORGET's ( -- n n ); tier 1 refuses the dropped definition.
: ZERO-USED ( n -- ) {: tier:n :}
   s" a dropped definition leaves a call to the used package's word" T-LABEL
   tier TIER-TEXT P-W+ LIAR+ Q-USING-P+
   s" : W-FORGET 3 4 ; : T-FORGET ( -- n n ) W-FORGET ; ;using ;package " TEXT+
   s" Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT$ SESSION
   tier 0= if
      RC @ RC-REJECT T=
      s" habu: in t-forget: at 'W-FORGET' expected: n n actual: n" SAYS
      s" interpret stack underdepth" SAYS-NOT
   else REFUSED-W then ;

\ The same refusal caught: the load goes on, and T-FORGET is still typed
\ against P-FORGET:W-FORGET. Tier 0 drops the definition without a throw.
: ZERO-USED-CAUGHT ( n -- ) {: tier:n :}
   s" a caught drop leaves a call to the used package's word" T-LABEL
   tier TIER-TEXT P-W+ LIAR+ Q-USING-P+
   S\" : TRY-FORGET ( -- n ) [: s\" : W-FORGET 3 4 ;\" evaluate-closed ;] catch ; " TEXT+
   s" TRY-FORGET . cr " TEXT+
   s" : T-FORGET ( -- n n ) W-FORGET ; ;using ;package Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT$ SESSION
   RC @ RC-REJECT T=
   tier 0= if S\" 0\n" else S\" -8572\n" then PRINTS
   s" habu: in t-forget: at 'W-FORGET' expected: n n actual: n" SAYS ;

\ A later definition of the dropped name in the same package is no duplicate.
: ZERO-REDEFINE ( n -- ) {: tier:n :}
   s" a dropped definition's name is free for the next one" T-LABEL
   tier TIER-TEXT P-W+ LIAR+
   s" package Q-FORGET public : W-FORGET 3 ; ;package LOWER-CERT-HOOK:INSTALL " TEXT+
   s" package Q-FORGET public : W-FORGET ( -- n ) 7 ; ;package Q-FORGET:W-FORGET . cr" TEXT+
   TEXT$ SESSION
   tier 0= if
      RC @ 0 T=
      S\" 7\n" PRINTS
      s" duplicate definition" SAYS-NOT
   else REFUSED-W then ;

: ZERO-VERDICT ( n -- ) {: tier:n :}
   tier ZERO-AMBIGUOUS-CALL
   tier ZERO-AMBIGUOUS-TICK
   tier ZERO-USED
   tier ZERO-USED-CAUGHT
   tier ZERO-REDEFINE ;

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

\ The copy's util.f is the checkout's with one definition appended. The entry
\ selects either the production source builder or the child that also observes
\ dispatch restoration after a caught refusal.
: BUILD-PROBE ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n entry:ptr entryu:n :}
   s" src/core/util.f" UTIL$ COPY-FILE-STREAM
   UTIL$ a u APPEND-FILE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARGV+
   entry entryu ARGV+
   s" --" ARGV+
   IMAGE$ ARGV+
   ENV!
   ENGINE-CANDIDATE:PATH$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: WINDOW-DECLARED ( -- )
   s" the build refuses a mismatched pre-hook definition before publication" T-LABEL
   S\" : PROBE-NZ ( n -- n ) 0= ;\n"
      s" tools/native-build.f" BUILD-PROBE
   RC @ RC-BUILD T=
   s" habu: in probe-nz: at '0='" SAYS
   s" native-build: uncaught throw code 70" PRINTS
   IMAGE$ FILE? 0= TTRUE ;

\ `0<>` is lib/prelude.f's, which the window has not loaded at util.f.
: WINDOW-PRELUDE ( -- )
   s" a caught pre-hook refusal restores the prior compiler dispatch" T-LABEL
   S\" : PROBE-NZ ( n -- bool ) 0<> ;\n"
      s" test/compiler/native-build-dispatch-child.f" BUILD-PROBE
   RC @ 0 T=
   s" E-UNDEFINED habu: in probe-nz: undefined word '0<>'" SAYS
   s" native-build: uncaught throw code 70" PRINTS
   s" native-build-dispatch: ok" PRINTS
   IMAGE$ FILE? 0= TTRUE ;

: WINDOW-TRUSTED ( -- )
   s" an explicit trusted pre-hook definition remains buildable" T-LABEL
   S\" TRUSTED: PROBE-NZ ( n -- n ) 0= ;\n"
      s" tools/native-build.f" BUILD-PROBE
   RC @ 0 T=
   s" native-build OK" PRINTS
   IMAGE$ FILE? TTRUE ;

: RUN ( -- )
   T-RESET
   SESSION-DECLARED
   SESSION-UNEVEN
   SESSION-UNDEFINED
   SESSION-DEFINER
   SESSION-UNSEEN
   SESSION-UNRECORDED
   SESSION-RECORDED
   ZERO-CALL ZERO-TICK ZERO-SHAPE
   2 0 do i ZERO-VERDICT loop
   SETUP
   s" native-hookless-reject artifacts: " type ROOT$ type cr
   TREE$ TREE-COPY:BUILD-SOURCES
   s" test/compiler/native-build-dispatch-child.f" TREE$ TREE-COPY:FILE
   WINDOW-DECLARED
   WINDOW-PRELUDE
   WINDOW-TRUSTED
   T-REPORT ;

RUN
;package
