\ native-hookless-reject.f - ordinary hookless compilation keeps the declared
\ row; a native build requires a certified scan of its pre-hook prefix. In a
\ `0 set-check` session the quiet scan prints its reason and enforces nothing
\ (CHECK-HOOKLESS); the declaration becomes the row without authority
\ (DECLARE-HOOKLESS), and a body without a recordable declaration has no row:
\ KEEP-ARITY refuses it with the reject status. A subject that needs tier 1
\ sets it; the rest run at the default tier.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require src/habu/verify-source.f
require src/lib/tree-copy.f

package HOOKLESS-REJECT-TEST
private

$4000 constant CAP
20000 constant SUBJECT-TIMEOUT-MS
600000 constant BUILD-TIMEOUT-MS
70 constant RC-REJECT                \ the check hook's reject status (src/core/check-hook.f CHECK-RC)
67 constant RC-THROW                 \ a load's uncaught throw
74 constant RC-BUILD                 \ a window build's failure status (tools/native-build-args.f BUILD-RC)

create ROOT PATH-CAP allot          variable ROOT-U
create TREE PATH-CAP allot          variable TREE-U
create TMP PATH-CAP allot           variable TMP-U
create IMAGE PATH-CAP allot         variable IMAGE-U
create UTIL PATH-CAP allot          variable UTIL-U
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

: SHOW ( bool -- bool )
   dup 0= if OUT$ type ERR$ type then ;

: SAYS ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR$ a u CONTAINS? SHOW TTRUE ;

: PRINTS ( ptr u8 n -- ) {: a:ptr u:n :}
   OUT$ a u CONTAINS? SHOW TTRUE ;

\ ---- sessions: `0 set-check` at a native tier --------------------------------
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

\ An undefined callee: the scan prints why and enforces nothing, and either
\ native compiler refuses the call as undefined with the reject status, as the
\ JIT's lookup does (src/compiler/native/definition.f SETTLE-UNIT). Nothing
\ after the definition runs. The first session compiles at the host's default
\ tier, the second at tier 1, which refuses at its own binding step.
: UNDEFINED-REFUSED ( -- )
   RC @ RC-REJECT T=
   OUT-U @ 0 T=
   s" E-UNDEFINED habu: in hr-undefined: undefined word 'HR-NO-SUCH-WORD'" SAYS
   s" ncomp: cannot compile HR-UNDEFINED at HR-NO-SUCH-WORD" SAYS
   s" E-UNDEFINED: HR-NO-SUCH-WORD" SAYS ;

: SESSION-UNDEFINED ( -- )
   s" an undefined callee is refused naming the callee" T-LABEL
   s" 0 set-check : HR-UNDEFINED ( n -- bool ) HR-NO-SUCH-WORD ; 5 ." SESSION
   UNDEFINED-REFUSED
   s" a tier-1 undefined callee is refused naming the callee" T-LABEL
   s" 1 set-tier 0 set-check : HR-UNDEFINED ( n -- bool ) HR-NO-SUCH-WORD ; 5 ." SESSION
   UNDEFINED-REFUSED ;

\ A bare callee two used packages (UA, UC) export: the checker renders the
\ ambiguity as its refusal, so either native compiler's refusal of the
\ definition, uncaught, exits with the reject status under the definition's
\ name, as an undefined callee does.
: AMBIGUOUS-REFUSED ( -- )
   RC @ RC-REJECT T=
   s" E-USING-AMBIGUOUS habu: bare 'AW' is ambiguous under using" SAYS
   s" ncomp: cannot compile UT-AMB" SAYS ;

: SESSION-AMBIGUOUS ( -- )
   s" an ambiguous bare callee is refused naming the callee" T-LABEL
   s" ;package 0 set-check package UA public : AW ( -- n ) 11 ; ;package package UC public : AW ( -- n ) 33 ; ;package using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using" SESSION
   AMBIGUOUS-REFUSED
   s" a tier-1 ambiguous bare callee is refused naming the definition" T-LABEL
   s" ;package 1 set-tier 0 set-check package UA public : AW ( -- n ) 11 ; ;package package UC public : AW ( -- n ) 33 ; ;package using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using" SESSION
   AMBIGUOUS-REFUSED ;

\ A definer's own body is scanned before its does> clause, which the checker
\ scans next, so the reason has to be printed before that second scan.
: DEFINER$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-DEFINER ( n -- ) create , HR-NO-SUCH-WORD does> ( -- n ) @ ;" ;

: SESSION-DEFINER ( -- )
   s" a does> definer's own body is refused with its reason" T-LABEL
   DEFINER$ SESSION
   RC @ RC-REJECT T=
   s" E-UNDEFINED habu: in hr-definer: undefined word 'HR-NO-SUCH-WORD'" SAYS
   s" E-UNDEFINED: HR-NO-SUCH-WORD" SAYS ;

\ A clause is checked in its definer's regime: a hookless definer's body may
\ call a word defined with checking off, so its clause may too, as at tier 0.
: CLAUSE$ ( -- ptr u8 n )
   s" 1 set-tier 0 set-check : HR-CELL ( -- n ) 5 ; : HR-MAKE ( -- ) create does> ( -- n ) drop HR-CELL ; HR-MAKE HR-X HR-X . cr" ;

: SESSION-CLAUSE ( -- )
   s" a hookless definer's clause calls what its body may" T-LABEL
   CLAUSE$ SESSION
   RC @ 0 T=
   S\" 5\n" PRINTS ;

\ The regime is the definer's own, not the last one seen: after a hookless
\ definer's clause calling the unchecked word has compiled, a checked definer's
\ clause calling it is still refused (E-CAP-TRUSTED) and cannot compile.
: CHECKED-CLAUSE$ ( -- ptr u8 n )
   s" 1 set-tier check@ 0 set-check : HR-CELL ( -- n ) 5 ; : HR-MAKE ( -- ) create does> ( -- n ) drop HR-CELL ; HR-MAKE HR-X set-check : HR-MAKE2 ( -- ) create does> ( -- n ) drop HR-CELL ; HR-MAKE2 HR-Y HR-Y . cr" ;

: SESSION-CHECKED-CLAUSE ( -- )
   s" a checked definer's clause is refused what a hookless one may call" T-LABEL
   CHECKED-CLAUSE$ SESSION
   RC @ RC-REJECT T=
   s" ncomp: cannot compile HR-MAKE2" SAYS ;

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

\ ---- a hook's zero verdict refuses the definition ----------------------------
\ A hook that certifies a definition and answers 0 refuses it: the native
\ compiler throws E-NCOMP-VERDICT at the definition, so uncaught the load ends
\ there, and caught the definition keeps no row and a later word is checked
\ against the word the engine binds. LIAR-FORGET certifies through the engine's
\ hook, then answers 0 for a name that starts with W. The texts outrun a source
\ line, so each is assembled in TEXT; each closes this package first, so its
\ words are globals as in a file.
create TEXT CAP allot                  variable TEXT-U

: TEXT$ ( -- ptr u8 n ) TEXT TEXT-U BUF-LEN@ ;
: TEXT+ ( ptr u8 n -- ) TEXT CAP TEXT-U BUF-APPEND ;

: ZERO-TEXT ( -- )
   TEXT-U BUF-RESET  s" ;package " TEXT+ ;

: P-W+ ( -- ) s" package P-FORGET public : W-FORGET ( -- n ) 2 ; ;package " TEXT+ ;
: Q-USING-P+ ( -- ) s" package Q-FORGET public using P-FORGET " TEXT+ ;

: LIAR+ ( -- )
   s" : LIAR-FORGET ( ptr u8 n -- n ) over c@ [char] W = >r " TEXT+
   s" LOWER-CERT-HOOK:HOOK r> if drop 0 then ; ' LIAR-FORGET set-check " TEXT+ ;

\ Uncaught, the refusal ends the load at W-FORGET.
: ZERO-UNCAUGHT ( -- )
   s" a definition its hook answers 0 for is refused" T-LABEL
   ZERO-TEXT LIAR+  s" : W-FORGET 3 ; " TEXT+
   TEXT$ SESSION
   RC @ RC-THROW T=
   s" ncomp: cannot compile W-FORGET" SAYS
   s" hb: uncaught throw code -8572" SAYS ;

\ The refusal caught: the load goes on, the refused W-FORGET keeps no row,
\ and T-FORGET's ( -- n n ) is refused against P-FORGET:W-FORGET's ( -- n ).
: ZERO-CAUGHT ( -- )
   s" a caught refusal leaves a call to the used package's word" T-LABEL
   ZERO-TEXT P-W+ LIAR+ Q-USING-P+
   S\" : TRY-FORGET ( -- n ) [: s\" : W-FORGET 3 4 ;\" evaluate-closed ;] catch ; " TEXT+
   s" TRY-FORGET . cr " TEXT+
   s" : T-FORGET ( -- n n ) W-FORGET ; ;using ;package Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT$ SESSION
   RC @ RC-REJECT T=
   S\" -8572\n" PRINTS
   s" habu: in t-forget: at 'W-FORGET' expected: n n actual: n" SAYS ;

\ ---- the window: a probe at the end of src/core/util.f ----------------------
\ The engine under test builds a private copy of src/, src/lib/ and tools/; the
\ printed directory keeps the copy and the last build's temporary directory.
: ROOT-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   ROOT$ a u dst FS:JOIN-PATH up ! ;

: SETUP ( -- )
   s" native-hookless-reject" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" tree" TREE TREE-U ROOT-PATH!
   s" tmp" TMP TMP-U ROOT-PATH! TMP$ FS:MAKE-DIRS
   s" hb" IMAGE IMAGE-U ROOT-PATH!
   TREE$ s" src/core/util.f" UTIL FS:JOIN-PATH UTIL-U ! ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc RC>N RC ! ENDOF
   ;MATCH ;

: ARGV+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ENV! ( -- )
   s" TMPDIR" >LEN TMP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ The copy's util.f is the checkout's with one definition appended. The entry
\ selects either the production source builder or the child that also observes
\ dispatch restoration after a caught refusal.
\ The ordinary CLI forwards a failed maker's output to stderr; the preloaded
\ dispatch child invokes the source driver directly, whose status is on stdout.
: BUILD-PROBE ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n entry:ptr entryu:n :}
   s" src/core/util.f" UTIL$ FS:COPY-FILE-STREAM
   UTIL$ a u FS:APPEND-FILE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARGV+
   entry entryu ARGV+
   s" --" ARGV+
   IMAGE$ ARGV+
   ENV!
   ENGINE-CANDIDATE:PATH$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

\ `0<>` is src/lib/prelude.f's, which the window has not loaded at util.f.
: WINDOW-PRELUDE ( -- )
   s" a caught pre-hook refusal restores the prior compiler dispatch" T-LABEL
   S\" : PROBE-NZ ( n -- bool ) 0<> ;\n"
      s" test/compiler/native-build-dispatch-child.f" BUILD-PROBE
   RC @ 0 T=
   s" E-UNDEFINED habu: in probe-nz: undefined word '0<>'" SAYS
   s" native-build: uncaught throw code 70" PRINTS
   s" native-build-dispatch: ok" PRINTS
   IMAGE$ FS:FILE? 0= TTRUE ;

: WINDOW-TRUSTED ( -- )
   s" a trusted pre-hook definition compiles and supplies a caller's row" T-LABEL
   S\" TRUSTED: PROBE-NZ ( n -- n ) 0= ;\n: PROBE-USE ( n -- n ) PROBE-NZ ;\n: PROBE-STOP ( n -- n ) 0= ;\n"
      s" tools/native-build.f" BUILD-PROBE
   RC @ RC-BUILD T=
   s" habu: in probe-stop: at '0='" SAYS
   s" native-build: uncaught throw code 70" SAYS
   IMAGE$ FS:FILE? 0= TTRUE ;

: RUN ( -- )
   T-RESET
   SESSION-DECLARED
   SESSION-UNEVEN
   SESSION-UNDEFINED
   SESSION-AMBIGUOUS
   SESSION-DEFINER
   SESSION-CLAUSE
   SESSION-CHECKED-CLAUSE
   SESSION-UNSEEN
   SESSION-UNRECORDED
   SESSION-RECORDED
   ZERO-UNCAUGHT
   ZERO-CAUGHT
   SETUP
   s" native-hookless-reject artifacts: " type ROOT$ type cr
   TREE$ TREE-COPY:BUILD-SOURCES
   s" test/compiler/native-build-dispatch-child.f" TREE$ TREE-COPY:FILE
   WINDOW-PRELUDE
   WINDOW-TRUSTED
   T-REPORT ;

RUN
;package
