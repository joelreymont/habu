\ aot-capture-bound.f - the capture refusal names the record that crossed the
\ bound (dot habu-name-the-record-eac72c28).
\
\ WHAT IT PINS. A window holding more dictionary records than AOT-REC-MAX
\ (src/habu/aot-decl.f) used to die `aot-capture: too many records` and nothing
\ else, so finding the definition that crossed it meant bisecting a tree.
\ ACAP-REC-REFUSE (src/habu/aot-capture.f) prints the captured count, the bound
\ and the name of the record being added first, and this fixture reads those
\ three lines off a real refusal. They are three lines and not one because the
\ engine's `.` ends its line and the capture layer has no mid-line formatter.
\
\ HOW THE BOUND IS LOWERED. A `require` path resolves against the --load entry's
\ directory before the working directory (docs/forth-card.md 7), so a directory
\ holding only src/habu/aot-decl.f and the entry file is an overlay: the child
\ takes THAT declaration and every other require falls through to this tree.
\ Neither aot-decl.f nor aot-capture.f is baked into bin/hb - AOT-BUF:AOT-REC-MAX
\ is E-UNDEFINED in a bare engine - so the lowered bound costs no engine build.
\ The copy is this tree's own file with one line rewritten; the fixture dies by
\ name when that line is gone, so the overlay cannot drift from the source.
\
\ WHY THE WINDOW IS PACKAGES. AOT-SIG-MAX is AOT-REC-MAX (aot-decl.f), so a
\ lowered bound lowers the signature buffer with it and a window of checked words
\ is refused there first (`aot-capture: the window has more checked words than
\ the signature buffer holds`, measured). A package record is exempt from the
\ signature walk (ACAP-?SIG skips DICT-WL:NAMESPACE) and still costs a record, so
\ the window is two packages of one word each: four records against a bound of
\ three, two signatures against the same three.
\
\ THE TWO NUMBERS AGREE BY CONSTRUCTION. ACAP-ADD-REC refuses at N = MAX, so the
\ captured count a crossing prints is always the bound; the assertion pins the
\ text of both lines, not a difference between them.
\
\ Cost: one child engine run, no builds. Registered as `SUITE aot-capture-bound`
\ in test/gate-stdlib-cases.f.
\ Standalone: bin/hb --load test/aot-capture-bound.f

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f

package AOT-CAPTURE-BOUND

private

3 constant OVERLAY-BOUND               \ AOT-REC-MAX in the private declaration
74 constant REFUSE-RC                  \ the capture's own die code
76 constant DRIFT-RC                   \ this fixture cannot build its overlay
30000 constant TIMEOUT-MS
$4000 constant CAP                     \ child stdout/stderr capture
$10000 constant DECL-CAP               \ src/habu/aot-decl.f is 35 KB today
$1000 constant TEXT-CAP                \ one generated text at a time

CAP BUFFER: OUT                 variable OUT-U
CAP BUFFER: ERR                 variable ERR-U
1 BUFFER: EMPTY                                  \ zero-length stdin
DECL-CAP BUFFER: DECL           variable DECL-U
TEXT-CAP BUFFER: TEXT           variable TEXT-U
FS-PATH-CAP BUFFER: ROOT-BUF    variable ROOT-U
FS-PATH-CAP BUFFER: SRCDIR-BUF  variable SRCDIR-U
FS-PATH-CAP BUFFER: DECLDST-BUF variable DECLDST-U
FS-PATH-CAP BUFFER: ENTRY-BUF   variable ENTRY-U
variable RC
variable EXITED

: ROOT$ ( -- ptr u8 n )     ROOT-BUF ROOT-U @ ;
: SRCDIR$ ( -- ptr u8 n )   SRCDIR-BUF SRCDIR-U @ ;
: DECLDST$ ( -- ptr u8 n )  DECLDST-BUF DECLDST-U @ ;
: ENTRY$ ( -- ptr u8 n )    ENTRY-BUF ENTRY-U @ ;
: OUT$ ( -- ptr u8 n )      OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )      ERR ERR-U @ ;
: TEXT$ ( -- ptr u8 n )     TEXT TEXT-U @ ;
: DECL$ ( -- ptr u8 n )     DECL DECL-U @ ;

\ The engine under test: the gate's freshly built candidate when it exported
\ one, else the running engine itself (lib/engine-candidate.f).
: HB$ ( -- ptr u8 n )       ENGINE-CANDIDATE:PATH$ ;

: DECL-SRC$ ( -- ptr u8 n ) s" src/habu/aot-decl.f" ;
: NEEDLE$ ( -- ptr u8 n )   s" constant AOT-REC-MAX" ;
: DIE-MSG$ ( -- ptr u8 n )  s" aot-capture: too many records" ;
: REFUSED$ ( -- ptr u8 n )  s" RB-TWO" ;      \ the window's fourth record

\ --- one text at a time: each build below is consumed before the next reset ---
: TEXT-RESET ( -- )          TEXT-U BUF-RESET ;
: TEXT+ ( ptr u8 n -- )      TEXT TEXT-CAP TEXT-U BUF-APPEND ;
: NL+ ( -- )                 10 TEXT TEXT-CAP TEXT-U BUF-APPEND-C ;
: LINE+ ( ptr u8 n -- )      TEXT+ NL+ ;
: DEC+ ( n -- )              SB-RESET FMT:SB-U SB$ TEXT+ ;

\ --- the private tree --------------------------------------------------------
: SETUP ( -- )
   s" habu-aot-capture-bound" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" src/habu" SRCDIR-BUF JOIN-PATH SRCDIR-U !
   SRCDIR$ MAKE-DIRS
   SRCDIR$ s" aot-decl.f" DECLDST-BUF JOIN-PATH DECLDST-U !
   ROOT$ s" entry.f" ENTRY-BUF JOIN-PATH ENTRY-U ! ;

: DRIFT ( -- )
   s" aot-capture-bound: no `" type NEEDLE$ type s" ` in " type DECL-SRC$ type cr
   s" aot-capture-bound: the overlay cannot lower a bound it cannot find"
   DRIFT-RC die ;

: BOUND-AT ( -- n )                          \ offset of the bound's name, -1 absent
   DECL$ NEEDLE$ FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: LINE-START ( n -- n )                      \ first byte of the line holding it
   begin dup 0 > while
      DECL over 1 - + c@ 10 = if exit then
      1 -
   repeat ;

: LOWERED$ ( -- ptr u8 n )                   \ `3 constant AOT-REC-MAX`
   TEXT-RESET  OVERLAY-BOUND DEC+  s"  " TEXT+  NEEDLE$ TEXT+  TEXT$ ;

\ This tree's own declaration file with one line rewritten: the bytes above that
\ line, the lowered bound, then the bytes from the end of the matched name on.
: OVERLAY-DECL ( -- )
   DECL-SRC$ DECL DECL-CAP READ-ALL DECL-U !
   BOUND-AT {: at:n :}
   at 0 < if DRIFT then
   at NEEDLE$ nip + {: to:n :}
   DECLDST$ DECL at LINE-START WRITE-ALL
   DECLDST$ LOWERED$ APPEND-FILE
   DECLDST$ DECL to + DECL-U @ to - APPEND-FILE ;

\ --- the child program, in test/dynamic-buffer-capture.f's shape -------------
: ENTRY-PRELUDE+ ( -- )
   s" \ generated by test/aot-capture-bound.f" LINE+
   s" package RB-ENTRY" LINE+
   s" public" LINE+
   s" ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !" LINE+
   s" ;package" LINE+
   s" require src/arch/arm64/asm.f" LINE+
   s" require src/arch/arm64/icode.f" LINE+
   s" require src/habu/layout.f" LINE+
   s" require src/habu/aot-decl.f" LINE+
   s" require src/habu/aot-arm.f" LINE+
   s" require src/habu/aot-capture.f" LINE+
   s" require src/compiler/native/string.f" LINE+ ;

: ENTRY-WINDOW+ ( -- )
   s" AOT-ARM:WINDOW-OPEN" LINE+
   s" NSTR:WINDOW-OPEN" LINE+
   s" package RB-ALPHA" LINE+
   s" public" LINE+
   s" : RB-ONE ( -- n ) 1 ;" LINE+
   s" ;package" LINE+
   s" package RB-BRAVO" LINE+
   s" public" LINE+
   s" : RB-TWO ( -- n ) 2 ;" LINE+
   s" ;package" LINE+
   s" AOT-ARM:WINDOW-CLOSE" LINE+ ;

: ENTRY-CAPTURE+ ( -- )
   s" package RB-ENTRY" LINE+
   s" : CAPTURE ( -- )" LINE+
   s"    PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK" LINE+
   s"    AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE ;" LINE+
   s" CAPTURE" LINE+
   s" ;package" LINE+ ;

: ENTRY-WRITE ( -- )
   TEXT-RESET  ENTRY-PRELUDE+ ENTRY-WINDOW+ ENTRY-CAPTURE+
   ENTRY$ TEXT$ WRITE-ALL ;

\ --- the child run -----------------------------------------------------------
: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: RUN-ENTRY ( -- )                           \ hb --load <overlay>/entry.f
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   ENTRY$ >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  STORE! ;

: SHOW ( -- )
   s" aot-capture-bound: child stdout:" type cr OUT$ type
   s" aot-capture-bound: child stderr:" type cr ERR$ type ;

: EXPECT$ ( -- ptr u8 n )                    \ the three lines the refusal prints
   TEXT-RESET
   s" aot-capture: records captured " TEXT+ OVERLAY-BOUND DEC+ NL+
   s" aot-capture: record bound " TEXT+ OVERLAY-BOUND DEC+ NL+
   s" aot-capture: adding " TEXT+ REFUSED$ TEXT+ NL+
   TEXT$ ;

: DETAIL? ( -- bool )   OUT$ EXPECT$ CONTAINS? ;
: DIED? ( -- bool )     ERR$ DIE-MSG$ CONTAINS? ;

: BODY ( -- )
   SETUP
   OVERLAY-DECL
   ENTRY-WRITE
   RUN-ENTRY
   s" the capture ends the child rather than baking a window over the bound"
   T-LABEL
   EXITED @ TTRUE
   RC @ REFUSE-RC = 0= if SHOW then
   RC @ REFUSE-RC T=
   s" the refusal names the count, the bound and the record being added" T-LABEL
   DETAIL? 0= if SHOW then
   DETAIL? TTRUE
   s" the die message is unchanged" T-LABEL
   DIED? TTRUE ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-capture-bound: ok" type cr ;

;package

AOT-CAPTURE-BOUND:RUN
