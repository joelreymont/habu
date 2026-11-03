\ native-hookless-reject.f - the native compilers refuse an undefined or
\ ambiguous callee and stray control flow, tier 1 unjoinable arms; a refused
\ definition keeps no row; a native build requires a certified scan of its
\ pre-hook prefix.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require src/lib/tree-copy.f
require src/habu/layout.f

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

\ This file: SUBJECT:RUN evaluates a session's text, which is not this file's bytes,
\ while the file loads, so a refusal in it names the file alone
\ (SRCLOC:FILE-BUFFER?).
SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL LOADING

\ Text a, then `<this file>` and a newline.
: HERE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SB-APPEND
   LOADING @ data-base SRCLOC:PATHLEN-CELL + @ SB-APPEND
   S\" \n" SB-APPEND ;

\ ---- the native compilers refuse a body at its token -------------------------
\ Unclosed control flow and a loop word outside a loop (src/compiler/native/elaborate.f
\ DO-ACTIVE-NTH) end a tier-1 TRUSTED: body with the reject status (definition.f MALFORMED?).

\ T0 refuses a TRUSTED: body's undefined callee with the reject status and names
\ it, as tier 1 does (test/tier.f TEST-REFUSAL-TOKEN).
: TRUSTED-UNDEFINED ( -- )
   s" a TRUSTED: body's undefined callee is refused naming the callee" T-LABEL
   s" TRUSTED: HR-UNDEFINED ( n -- bool ) HR-NO-SUCH-WORD ; 5 ." SESSION
   RC @ RC-REJECT T=
   OUT-U @ 0 T=
   SB-RESET  s" ncomp: cannot compile HR-UNDEFINED at HR-NO-SUCH-WORD in " HERE+  SB$ SAYS
   SB-RESET  s" E-UNDEFINED: HR-NO-SUCH-WORD at " HERE+  SB$ SAYS ;

\ The checker's ambiguity refusal makes either native compiler exit with the
\ reject status under the definition's name.
: AMBIGUOUS-REFUSED ( -- )
   RC @ RC-REJECT T=
   s" E-USING-AMBIGUOUS habu: bare 'AW' is ambiguous under using" SAYS
   s" ncomp: cannot compile UT-AMB" SAYS ;

: AMBIGUOUS-CALLEE ( -- )
   s" an ambiguous bare callee is refused naming the callee" T-LABEL
   s" ;package package UA public : AW ( -- n ) 11 ; ;package package UC public : AW ( -- n ) 33 ; ;package using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using" SESSION
   AMBIGUOUS-REFUSED
   s" a tier-1 ambiguous bare callee is refused naming the definition" T-LABEL
   s" ;package 1 set-tier package UA public : AW ( -- n ) 11 ; ;package package UC public : AW ( -- n ) 33 ; ;package using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using" SESSION
   AMBIGUOUS-REFUSED ;

\ Arms leaving different depths: the checker refuses a checked body at
\ `then`, and tier 1 cannot join a TRUSTED: body's arms (E-NELAB-JOIN).
: TRUSTED-UNEVEN ( -- )
   s" a tier-1 TRUSTED: body's uneven branches are refused by the compiler" T-LABEL
   s" 1 set-tier TRUSTED: HR-UNEVEN ( bool -- n ) if 1 2 else 3 then ;" SESSION
   RC @ RC-THROW T=
   s" ncomp: cannot compile HR-UNEVEN" SAYS
   s" hb: uncaught throw code -8503" SAYS ;

: STRAY-LOOP$ ( -- ptr u8 n )
   S\" 1 set-tier : HR-TRY-J ( -- n ) [: s\" TRUSTED: HR-XJ ( -- ) 1 0 do j drop loop ;\" evaluate-closed ;] catch ; : HR-TRY-U ( -- n ) [: s\" TRUSTED: HR-XU ( -- ) unloop ;\" evaluate-closed ;] catch ; : HR-TRY-I ( -- n ) [: s\" TRUSTED: HR-XI ( -- ) 1 i drop ;\" evaluate-closed ;] catch ; HR-TRY-J . HR-TRY-U . HR-TRY-I . cr" ;

: TRUSTED-STRAY-LOOP ( -- )
   s" tier-1 TRUSTED: bodies are refused at a loop word outside a loop" T-LABEL
   STRAY-LOOP$ SESSION
   SB-RESET
   s" ncomp: cannot compile HR-XJ at j in " HERE+
   s" ncomp: cannot compile HR-XU at unloop in " HERE+
   s" ncomp: cannot compile HR-XI at i in " HERE+
   ERR$ SB$ T$=
   OUT$ S\" 70\n70\n70\n\n" T$= ;

: TRUSTED-ORPHAN-THEN ( -- )
   s" a tier-1 TRUSTED: body is refused at a `then` with no `if`" T-LABEL
   s" 1 set-tier TRUSTED: HR-ORPHAN ( n -- n ) [: 7 ;] then ;" SESSION
   SB-RESET
   s" ncomp: cannot compile HR-ORPHAN at then in " HERE+
   ERR$ SB$ T$=
   RC @ RC-REJECT T= ;

\ ---- a refused definition keeps no row ---------------------------------------
\ The texts outrun a source line, so TEXT holds them; `;package` first makes
\ them global.
create TEXT CAP allot                  variable TEXT-U

: TEXT$ ( -- ptr u8 n ) TEXT TEXT-U BUF-LEN@ ;
: TEXT+ ( ptr u8 n -- ) TEXT CAP TEXT-U BUF-APPEND ;

: REFUSED-TEXT ( -- )
   TEXT-U BUF-RESET  s" ;package " TEXT+ ;

: P-W+ ( -- ) s" package P-FORGET public : W-FORGET ( -- n ) 2 ; ;package " TEXT+ ;
: Q-USING-P+ ( -- ) s" package Q-FORGET public using P-FORGET " TEXT+ ;

\ TRY-FORGET prints the refusal's code, 70 (RC-REJECT). A row kept from the
\ refused ( -- n n ) would certify a T-FORGET ( -- n n ) over W-FORGET.
: REFUSED+ ( -- )
   S\" : TRY-FORGET ( -- n ) [: s\" : W-FORGET ( -- n n ) 3 ;\" evaluate-closed ;] catch ; " TEXT+
   s" TRY-FORGET . cr " TEXT+ ;

\ The refused W-FORGET keeps no row, so T-FORGET binds P-FORGET:W-FORGET.
: REFUSED-CAUGHT ( -- )
   s" a caught refusal leaves a call to the used package's word" T-LABEL
   REFUSED-TEXT P-W+ Q-USING-P+ REFUSED+
   s" : T-FORGET ( -- n n ) W-FORGET ; ;using ;package Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT$ SESSION
   RC @ RC-REJECT T=
   S\" 70\n" PRINTS
   s" habu: in t-forget: at 'W-FORGET' expected: n n actual: n" SAYS ;

\ ---- the JIT keeps no row of a refused definition ----------------------------
\ These rows select the JIT. The JIT is arm64's, so they run only there.

: JIT-TEXT ( -- )
   REFUSED-TEXT  s" 0 data-base NCOMP-DISPATCH:COMP-CELL + ! " TEXT+ ;

: GLOBAL-W+ ( -- ) s" : W-FORGET ( -- n ) 1 ; " TEXT+ ;
: R-W+ ( -- ) s" package R-FORGET public : W-FORGET ( -- n ) 5 ; ;package " TEXT+ ;

: SAYS-NOT ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR$ a u CONTAINS? 0= SHOW TTRUE ;

\ The global and the used P-FORGET both export W-FORGET, so T-FORGET's
\ reference is refused (E-USING-SHADOW-GLOBAL); a row of the refused
\ Q-FORGET:W-FORGET would bind it.
: SHADOWED ( -- )
   RC @ RC-REJECT T=
   s" E-USING-SHADOW-GLOBAL" SAYS
   OUT$ S\" 70\n\n" T$= ;

: REFUSED-CALL ( -- )
   s" a refused definition leaves a call to the engine's shadowed word" T-LABEL
   JIT-TEXT GLOBAL-W+ P-W+ Q-USING-P+ REFUSED+
   s" : T-FORGET ( -- n ) W-FORGET ; ;using ;package Q-FORGET:T-FORGET . cr" TEXT+
   TEXT$ SESSION  SHADOWED ;

: REFUSED-TICK ( -- )
   s" a refused definition leaves a tick to the engine's shadowed word" T-LABEL
   JIT-TEXT GLOBAL-W+ P-W+ Q-USING-P+ REFUSED+
   s" : T-FORGET ( -- n ) ['] W-FORGET drop 4 ; ;using ;package " TEXT+
   s" Q-FORGET:T-FORGET . cr" TEXT+
   TEXT$ SESSION  SHADOWED ;

\ Two used packages and a global: the engine's bare W-FORGET is ambiguous.
: AMBIGUOUS-TEXT ( -- )
   JIT-TEXT GLOBAL-W+ P-W+ R-W+ Q-USING-P+
   s" using R-FORGET " TEXT+ REFUSED+ ;

: AMBIGUOUS ( -- )
   OUT$ S\" 70\n\n" T$=
   RC @ RC-REJECT T=
   s" E-USING-AMBIGUOUS" SAYS
   s" hb: uncaught throw code 7144" SAYS ;

: REFUSED-AMBIGUOUS-CALL ( -- )
   s" a refused definition leaves a call to the engine's ambiguous word" T-LABEL
   AMBIGUOUS-TEXT
   s" : T-FORGET ( -- n ) W-FORGET ; ;using ;using ;package Q-FORGET:T-FORGET . cr" TEXT+
   TEXT$ SESSION  AMBIGUOUS ;

: REFUSED-AMBIGUOUS-TICK ( -- )
   s" a refused definition leaves a tick to the engine's ambiguous word" T-LABEL
   AMBIGUOUS-TEXT
   s" : T-FORGET ( -- n ) ['] W-FORGET drop 4 ; ;using ;using ;package " TEXT+
   s" Q-FORGET:T-FORGET . cr" TEXT+
   TEXT$ SESSION  AMBIGUOUS ;

\ With no global, T-FORGET's W-FORGET is the used P-FORGET:W-FORGET ( -- n ),
\ so T-FORGET's ( -- n n ) is refused.
: REFUSED-USED-CAUGHT ( -- )
   s" a refused definition leaves a call to the used package's word" T-LABEL
   JIT-TEXT P-W+ Q-USING-P+ REFUSED+
   s" : T-FORGET ( -- n n ) W-FORGET ; ;using ;package Q-FORGET:T-FORGET . . cr" TEXT+
   TEXT$ SESSION
   RC @ RC-REJECT T=
   S\" 70\n" PRINTS
   s" habu: in t-forget: at 'W-FORGET' expected: n n actual: n" SAYS
   s" interpret stack underdepth" SAYS-NOT ;

\ A later definition of the refused name in the same package is no duplicate.
: REFUSED-REDEFINE ( -- )
   s" a refused definition's name is free for the next one" T-LABEL
   JIT-TEXT P-W+
   s" package Q-FORGET public " TEXT+ REFUSED+ s" ;package " TEXT+
   s" package Q-FORGET public : W-FORGET ( -- n ) 7 ; ;package Q-FORGET:W-FORGET . cr" TEXT+
   TEXT$ SESSION
   RC @ 0 T=
   S\" 70\n\n7\n" PRINTS
   s" duplicate definition" SAYS-NOT ;

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
   TRUSTED-UNDEFINED
   AMBIGUOUS-CALLEE
   TRUSTED-UNEVEN
   TRUSTED-STRAY-LOOP
   TRUSTED-ORPHAN-THEN
   REFUSED-CAUGHT
   HB-TARGET-LINUX-X86-64? 0= if
      REFUSED-CALL
      REFUSED-TICK
      REFUSED-AMBIGUOUS-CALL
      REFUSED-AMBIGUOUS-TICK
      REFUSED-USED-CAUGHT
      REFUSED-REDEFINE
   then
   SETUP
   s" native-hookless-reject artifacts: " type ROOT$ type cr
   TREE$ TREE-COPY:BUILD-SOURCES
   s" test/compiler/native-build-dispatch-child.f" TREE$ TREE-COPY:FILE
   WINDOW-PRELUDE
   WINDOW-TRUSTED
   T-REPORT ;

RUN
;package
