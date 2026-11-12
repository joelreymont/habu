\ linear-suite.f - `LINEAR:` mints and erases a DEFLINEAR token in its owner alone.
\ In its owner's private section `LINEAR: NAME ( payload -- PKG:tok )` mints and
\ `( PKG:tok -- payload )` erases once CHECKER-LINEAR certifies the row; nothing
\ mints a top-level DEFLINEAR. Fronts: the engine keyword, the source pre-pass,
\ tools/check.f fixtures in fork children, an hb built with --repl, and a
\ program tools/hb-build.f builds from files that name a linear type.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/test/subject.f
require src/habu/verify-source.f
require src/lib/engine-candidate.f
require test/pcap-result.f
require tools/check-core.f

\ Expected refusals are asserted, not printed.
package LIN-DIAG
create BUF 65536 allot
BUF 65536 DIAG-BUFFER!
;package

package LIN-OWN
public
DEFLINEAR LIN-OWN:tok
DEFLINEAR LIN-OWN:aux
;package
package LIN-OTHER
public
DEFLINEAR LIN-OTHER:tok
NEWTYPE ofam
;package
DEFLINEAR lin-free

\ The engine reads the text as it reads a file, so a `linear:` row meets the
\ live keyword and checker. DECL answers the code a refusal threw, or 0.
package LIN-RUN
public
TYPED-VARIABLE SRC-A ptr u8
variable SRC-U
: EVAL ( -- ) SRC-A @ SRC-U @ INCLUDE-EVALUATE ;
: DECL ( ptr u8 n -- n )
   SRC-U !  SRC-A !
   [: EVAL ;] catch ;
;package

T-RESET

\ ---- the owner's private section mints and erases ------------------------------
package LIN-OWN
variable WID
get-current WID !
: ABSENT? ( ptr u8 n -- bool ) WID @ search-wl 0= ;
s" the owner's private section declares a mint and an erase" T-LABEL
s" LINEAR: MINT ( ptr n -- LIN-OWN:tok )"  LIN-RUN:DECL 0 T=
s" LINEAR: ERASE ( LIN-OWN:tok -- ptr n )" LIN-RUN:DECL 0 T=
s" LINEAR: MINT-ROW ( R n -- R LIN-OWN:tok )" LIN-RUN:DECL 0 T=
s" LINEAR: ERASE-ROW ( R LIN-OWN:tok -- R n )" LIN-RUN:DECL 0 T=
s" : ROW-ROUND ( n n -- n n ) MINT-ROW ERASE-ROW ;" LIN-RUN:DECL 0 T=
variable ROW-LOW  variable ROW-HIGH
s" 7 42 ROW-ROUND ROW-HIGH ! ROW-LOW !" LIN-RUN:DECL 0 T=
ROW-LOW @ 7 T=  ROW-HIGH @ 42 T=
s" LINEAR: Q-MINT ( R n -- S LIN-OWN:tok )" LIN-RUN:DECL E-CAST-ARITY T=
s" LINEAR: LIN-OWN:PUBLIC-MINT ( n -- LIN-OWN:tok )" LIN-RUN:DECL E-LINEAR-SCOPE T=
s" LINEAR: LIN-NO-PACKAGE:PUBLIC-MINT ( n -- LIN-OWN:tok )" LIN-RUN:DECL E-LINEAR-SCOPE T=
s" LINEAR: LIN-OWN:BAD-PAYLOAD ( n -- n )" LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LIN-OWN:BAD-OWNER ( n -- LIN-OTHER:tok )" LIN-RUN:DECL E-LINEAR-OWNER T=
s" MINT" ABSENT? TFALSE
s" ERASE" ABSENT? TFALSE
s" Q-MINT" ABSENT? TTRUE
s" checked words in the owner mint, erase and conserve the token" T-LABEL
s" : PEEK ( LIN-OWN:tok -- LIN-OWN:tok n ) ERASE dup @ swap MINT swap ;" LIN-RUN:DECL 0 T=
s" LV1 ( LIN-OWN:tok -- ) ERASE drop"                CHECK-QUIET-CANDIDATE! -1 T=
s" LV2 ( LIN-OWN:tok -- ) dup ERASE drop ERASE drop" CHECK-QUIET-CANDIDATE! 0 T=
s" LV3 ( LIN-OWN:tok -- ) ERASE drop ERASE drop"     CHECK-QUIET-CANDIDATE! 0 T=
s" LV4 ( ptr n -- ) MINT drop"                       CHECK-QUIET-CANDIDATE! 0 T=
s" the declarer inside a checked body is unsafe" T-LABEL
s" LV5 ( LIN-OWN:tok -- ) linear:"                   CHECK-QUIET-CANDIDATE! 0 T=
s" exactly one side is linear and the other a payload" T-LABEL
s" LINEAR: LP1 ( LIN-OWN:tok -- LIN-OWN:tok )"     LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LP2 ( LIN-OWN:tok -- LIN-OWN:aux )"     LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LP3 ( n -- n )"                         LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LP4 ( n -- idx )"                       LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LP5 ( n -- ptr LIN-OWN:tok )"           LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" a payload is a non-linear constructor or a pointer chain ending at one" T-LABEL
s" LINEAR: LP6 ( ptr LIN-OWN:aux -- LIN-OWN:tok )" LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LP7 ( ptr a -- LIN-OWN:tok )"           LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LP8 ( LIN-OWN:tok -- a )"               LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LP9 ( LIN-OTHER:ofam -- LIN-OWN:tok )"  LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" LINEAR: LP10 ( [ -- ] -- LIN-OWN:tok )"         LIN-RUN:DECL E-LINEAR-PAYLOAD T=
s" the shapes cast: refuses" T-LABEL
s" LINEAR: LA1 ( n n -- LIN-OWN:tok )"             LIN-RUN:DECL E-CAST-ARITY T=
s" LINEAR: LA2 ( -- LIN-OWN:tok )"                 LIN-RUN:DECL E-CAST-ARITY T=
s" LINEAR: LA3 ( n -- neverdecl )"                 LIN-RUN:DECL E-CAST-FAM T=
\ An output scope variable no input supplies is a bad signature here: only a
\ cast: view pack leaves that fault to its scope rule.
s" LINEAR: LA4 ( n -- read-view<p,q,u8> )"         LIN-RUN:DECL E-BAD-STORED-SIGNATURE T=
s" cast: carries no linear type, in the owner too" T-LABEL
s" cast: LC1 ( n -- LIN-OWN:tok )"                 LIN-RUN:DECL E-CAST-LINEAR T=
s" cast: LC2 ( LIN-OWN:tok -- n )"                 LIN-RUN:DECL E-CAST-LINEAR T=
s" a refused row publishes no name" T-LABEL
s" LP1" ABSENT? TTRUE
s" LP9" ABSENT? TTRUE
s" LA1" ABSENT? TTRUE
s" LC1" ABSENT? TTRUE
s" the owner's public section refuses a row" T-LABEL
public
s" LINEAR: LS1 ( n -- LIN-OWN:tok )"               LIN-RUN:DECL E-LINEAR-SCOPE T=
s" LINEAR: LS2 ( LIN-OWN:tok -- n )"               LIN-RUN:DECL E-LINEAR-SCOPE T=
s" : GIVE ( ptr n -- LIN-OWN:tok ) MINT ;"         LIN-RUN:DECL 0 T=
s" : LOOK ( LIN-OWN:tok -- LIN-OWN:tok n ) PEEK ;" LIN-RUN:DECL 0 T=
s" : TAKE ( LIN-OWN:tok -- ptr n ) ERASE ;"        LIN-RUN:DECL 0 T=
;package
s" LIN-OWN:LS1" XREF-FIND XREF-FOUND? TFALSE
s" LIN-OWN:PUBLIC-MINT" XREF-FIND XREF-FOUND? TFALSE
s" LIN-NO-PACKAGE:PUBLIC-MINT" XREF-FIND XREF-FOUND? TFALSE
s" LIN-OWN:BAD-PAYLOAD" XREF-FIND XREF-FOUND? TFALSE
s" LIN-OWN:BAD-OWNER" XREF-FIND XREF-FOUND? TFALSE
s" cast: BAD-TAIL ( R n -- S n )" LIN-RUN:DECL E-CAST-ARITY T=

\ ---- no other scope mints or erases --------------------------------------------
s" another package neither mints nor erases the owner's type" T-LABEL
package LIN-OTHER
s" LINEAR: LO1 ( n -- LIN-OWN:tok )"     LIN-RUN:DECL E-LINEAR-OWNER T=
s" LINEAR: LO2 ( LIN-OWN:tok -- n )"     LIN-RUN:DECL E-LINEAR-OWNER T=
s" LINEAR: LO3 ( n -- lin-free )"        LIN-RUN:DECL E-LINEAR-OWNER T=
;package
s" nor does the top level, for an owned type or an unowned one" T-LABEL
s" LINEAR: LT1 ( n -- LIN-OWN:tok )"     LIN-RUN:DECL E-LINEAR-OWNER T=
s" LINEAR: LT2 ( n -- lin-free )"        LIN-RUN:DECL E-LINEAR-OWNER T=
s" the mint stays private while the owner's public words resolve" T-LABEL
s" LIN-OWN:MINT" XREF-FIND XREF-FOUND? TFALSE
s" LIN-OWN:GIVE" XREF-FIND XREF-FOUND? TTRUE

\ ---- a caller outside holds the token through the public words -----------------
TYPED-VARIABLE LIN-BOX n
variable LIN-OUT
s" a token round-trips its payload outside the owner" T-LABEL
s" : LIN-RT ( -- n ) 42 LIN-BOX ! LIN-BOX LIN-OWN:GIVE LIN-OWN:LOOK swap LIN-OWN:TAKE drop ;"
   LIN-RUN:DECL 0 T=
s" LIN-RT LIN-OUT !" LIN-RUN:DECL 0 T=
LIN-OUT @ 42 T=
s" and may not copy or drop it" T-LABEL
s" LIN-DUP ( ptr n -- ) LIN-OWN:GIVE dup LIN-OWN:TAKE drop LIN-OWN:TAKE drop"
   CHECK-QUIET-CANDIDATE! 0 T=
s" LIN-LEAK ( ptr n -- ) LIN-OWN:GIVE drop" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- the source pre-pass applies the same rules ---------------------------------
package LIN-VRF
public
: OWNED ( -- )
   s" package LV-A public DEFLINEAR LV-A:tok ;package package LV-A LINEAR: VM ( n -- LV-A:tok ) LINEAR: VE ( LV-A:tok -- n ) : VR ( n -- n ) VM VE ; ;package"
   VERIFY:SOURCE-BUF ;
: IN-PUBLIC ( -- )
   s" package LV-B public DEFLINEAR LV-B:tok LINEAR: WM ( n -- LV-B:tok ) ;package"
   VERIFY:SOURCE-BUF ;
: FOREIGN ( -- )
   s" package LV-C public DEFLINEAR LV-C:tok ;package package LV-D LINEAR: YM ( n -- LV-C:tok ) ;package"
   VERIFY:SOURCE-BUF ;
: NO-TOKEN ( -- )
   s" package LV-E public DEFLINEAR LV-E:tok ;package package LV-E LINEAR: ZM ( n -- n ) ;package"
   VERIFY:SOURCE-BUF ;
: QUALIFIED ( -- )
   s" package LV-F public DEFLINEAR LV-F:tok ;package package LV-F LINEAR: LV-F:QM ( n -- LV-F:tok ) ;package"
   VERIFY:SOURCE-BUF ;
: DISTINCT-TAILS ( -- )
   s" package LV-G public DEFLINEAR LV-G:tok ;package package LV-G LINEAR: TM ( R n -- S LV-G:tok ) ;package"
   VERIFY:SOURCE-BUF ;
: SAME-TAIL ( -- )
   s" package LV-H public DEFLINEAR LV-H:tok ;package package LV-H LINEAR: TM ( R n -- R LV-H:tok ) ;package"
   VERIFY:SOURCE-BUF ;
: CAST-TAIL ( -- )
   s" cast: LV-BAD-TAIL ( R n -- S n )" VERIFY:SOURCE-BUF ;
;package
s" the pre-pass certifies a row in the owner and its checked caller" T-LABEL
' LIN-VRF:OWNED catch 0 T=
s" and refuses a public row, a foreign owner and a row with no token" T-LABEL
' LIN-VRF:IN-PUBLIC catch E-LINEAR-SCOPE T=
' LIN-VRF:FOREIGN catch E-LINEAR-OWNER T=
' LIN-VRF:NO-TOKEN catch E-LINEAR-PAYLOAD T=
' LIN-VRF:QUALIFIED catch E-LINEAR-SCOPE T=
' LIN-VRF:DISTINCT-TAILS catch E-CAST-ARITY T=
' LIN-VRF:SAME-TAIL catch 0 T=
' LIN-VRF:CAST-TAIL catch E-CAST-ARITY T=

\ ---- what the engine itself refuses at top level --------------------------------
\ The reader exits after these diagnostics, so each runs in a child.
package LIN-CHILD
public
$400 constant CAP
10000 constant CHILD-MS
create OUT CAP allot
create ERR CAP allot
variable ERR-U
: RUN ( ptr u8 n -- n ) {: src:ptr u:n :}   \ source -> child exit status (-1 = signal)
   src u OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
   erru LEN>N ERR-U !
   oc MATCH PROCESS:ending
     exited OF ENDOF
     signaled OF drop -1 ENDOF
     timeout OF src u OUT outu LEN>N ERR erru LEN>N SUBJECT:TIMED-OUT ENDOF
   ;MATCH ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
;package
s" the keyword with nothing after it names itself" T-LABEL
s" linear:" LIN-CHILD:RUN $4A T=
LIN-CHILD:ERR$ s" reader keyword needs a name: linear:" CONTAINS? TTRUE
s" a row's bare call at an empty stack is refused by name" T-LABEL
s" package LCH public DEFLINEAR LCH:tok ;package package LCH LINEAR: LCM ( n -- LCH:tok ) LCM"
   LIN-CHILD:RUN 70 T=
LIN-CHILD:ERR$ s" interpret stack underdepth: LCM" CONTAINS? TTRUE

\ ---- tools/check.f, an hb built with --repl and a built program -----------------
package LIN-FILES
4096 constant CAP
$2000 constant PROG-CAP
60000 constant CHECK-MS
600000 constant IMAGE-MS
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
create ROOT-BUF PATH-CAP allot  variable ROOT-U
create PATH-BUF PATH-CAP allot  variable PATH-U
create IMAGE-BUF PATH-CAP allot  variable IMAGE-U
create OBJ-BUF PATH-CAP allot  variable OBJ-U
PROG-CAP BUFFER: PROG
variable PROG-U

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: PATH$ ( -- ptr u8 n ) PATH-BUF PATH-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: OBJ$ ( -- ptr u8 n ) OBJ-BUF OBJ-U @ ;
: PROG$ ( -- ptr u8 n ) PROG PROG-U @ ;

: PROG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a PROG PROG-U @ + u BYTE-COPY
   PROG-U @ u + PROG-U ! ;
: PROG-C+ ( n -- )
   PROG PROG-U @ + c!
   PROG-U @ 1 + PROG-U ! ;

\ The caller stages the argv.
: RUN ( ptr u8 n n -- ) {: path:ptr pathu:n ms:n :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN  PROG$ >LEN  OUT CAP >LEN  ERR CAP >LEN  ms >MS
   RUN-ARGV-ENV-STDIN-CAPTURE PCAP-RESULT:>N
   RC ! ERR-U ! OUT-U ! ;

: ROOT ( -- )
   s" linear-suite" FS:MKTEMP-DIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT-BUF ROOT-U @ FS:CLEANUP-TREE+ ;

\ Ends as tools/check.f does: a refusal's status is the child's exit code.
: CHECK-CHILD ( -- )
   LSP-CHECK:RESET
   PATH$ LSP-CHECK:FILE
   LSP-CHECK:RUN dup 0 <> if throw then drop ;

: CHECK ( ptr u8 n -- ) {: name:ptr nameu:n :}
   ROOT-BUF ROOT-U @ name nameu PATH-BUF FS:JOIN-PATH PATH-U !
   PATH$ PROG$ FS:WRITE-ALL
   0 PROG-U !
   s" CHECK-CHILD" OUT CAP >LEN ERR CAP >LEN CHECK-MS >MS SUBJECT:RUN
   PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   rc RC !  erru LEN>N ERR-U !  outu LEN>N OUT-U ! ;

: OK-FIXTURE ( -- )
   0 PROG-U !
   S\" package LIN-FX\npublic\nDEFLINEAR LIN-FX:tok\n;package\npackage LIN-FX\n" PROG+
   S\" LINEAR: MINT ( ptr n -- LIN-FX:tok )\nLINEAR: ERASE ( LIN-FX:tok -- ptr n )\n" PROG+
   S\" public\n: GIVE ( ptr n -- LIN-FX:tok ) MINT ;\n" PROG+
   S\" : LOOK ( LIN-FX:tok -- LIN-FX:tok n ) ERASE dup @ swap MINT swap ;\n" PROG+
   S\" : TAKE ( LIN-FX:tok -- ptr n ) ERASE ;\n;package\n" PROG+ ;

: BAD-FIXTURE ( -- )
   0 PROG-U !
   S\" package LIN-FB\npublic\nDEFLINEAR LIN-FB:tok\nLINEAR: MINT ( n -- LIN-FB:tok )\n;package\n" PROG+ ;

: QUAL-FIXTURE ( -- )
   0 PROG-U !
   S\" package LIN-FQ\npublic\nDEFLINEAR LIN-FQ:tok\n;package\npackage LIN-FQ\nLINEAR: LIN-FQ:MINT ( n -- LIN-FQ:tok )\n;package\n" PROG+ ;

: TAIL-FIXTURE ( -- )
   0 PROG-U !
   S\" package LIN-FT\npublic\nDEFLINEAR LIN-FT:tok\n;package\npackage LIN-FT\nLINEAR: MINT ( R n -- S LIN-FT:tok )\n;package\n" PROG+ ;

\ Push the type table past its 256-row boot store (src/core/checker.f
\ CT-CAP-INIT) before the owner declares, so the owner's row lives only in the
\ grown store that the built hb must carry.
: FILLER ( -- )
   256 0 ?do
      s" DEFLINEAR lf" PROG+
      i 16 / [char] a + PROG-C+
      i 16 mod [char] a + PROG-C+
      10 PROG-C+
   loop ;

: SAVE ( -- )
   ROOT-BUF ROOT-U @ s" lim.f" PATH-BUF FS:JOIN-PATH PATH-U !
   ROOT-BUF ROOT-U @ s" app" IMAGE-BUF FS:JOIN-PATH IMAGE-U !
   0 PROG-U !
   FILLER
   S\" package LIM\npublic\nDEFLINEAR LIM:tok\n;package\n: MAIN ( -- ) ;\n" PROG+
   PATH$ PROG$ FS:WRITE-ALL
   0 PROG-U !
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+  s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+  s" --repl" >LEN PROC-ARGV+
   PATH$ >LEN PROC-ARGV+  s" -o" >LEN PROC-ARGV+  IMAGE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ IMAGE-MS RUN ;

: REOPEN ( -- )
   0 PROG-U !
   s" require " PROG+  PATH$ PROG+  10 PROG-C+
   S\" package LIM\nLINEAR: MINT ( n -- LIM:tok )\nLINEAR: ERASE ( LIM:tok -- n )\n" PROG+
   S\" : RT ( n -- n ) MINT ERASE ;\n42 RT .\n;package\n" PROG+
   S\" package LIM-X\nLINEAR: XM ( n -- LIM:tok )\n;package\n" PROG+
   PROC-ARGV-ENV-RESET
   IMAGE$ IMAGE-MS RUN ;

: AT-ROOT ( ptr u8 n -- ) {: name:ptr nameu:n :}
   ROOT-BUF ROOT-U @ name nameu PATH-BUF FS:JOIN-PATH PATH-U ! ;

: WRITE-PROG ( ptr u8 n -- )
   AT-ROOT  PATH$ PROG$ FS:WRITE-ALL  0 PROG-U ! ;

: LOAD-LINE+ ( ptr u8 n ptr u8 n -- ) {: word:ptr wordu:n name:ptr nameu:n :}
   name nameu AT-ROOT
   word wordu PROG+  s"  " PROG+  PATH$ PROG+  10 PROG-C+ ;

\ lb-main.f; with `more` it gains a word naming `tok`, so a build after the edit links
\ lb-own.f and lb-use.f and compiles a file whose type comes from an object.
: MAIN-SOURCE ( bool -- ) {: more:bool :}
   0 PROG-U !
   s" require" s" lb-use.f" LOAD-LINE+
   more if S\" : MORE ( LB:tok -- LB:tok ) LB-USE:USE ;\n" PROG+ then
   S\" : MAIN ( -- ) 42 LB:RT . ;\n" PROG+
   s" lb-main.f" WRITE-PROG ;

\ The owner's file names its type in a later `LINEAR:` row; another file names
\ `aux` in a SUMTYPE payload and `tok` in an effect, the reverse of their order.
: LINK-SOURCES ( -- )
   0 PROG-U !
   S\" package LB\npublic\nDEFLINEAR LB:tok\nDEFLINEAR LB:aux\n: PASS ( LB:tok -- LB:tok ) ;\n" PROG+
   S\" private\nLINEAR: MINT ( n -- LB:tok )\nLINEAR: ERASE ( LB:tok -- n )\n" PROG+
   S\" public\n: RT ( n -- n ) MINT ERASE ;\n;package\n" PROG+
   s" lb-own.f" WRITE-PROG
   s" require" s" lb-own.f" LOAD-LINE+
   S\" package LB-USE\npublic\nSUMTYPE box\n   VARIANT hold LB:aux ;VARIANT\n;SUMTYPE\n" PROG+
   S\" : USE ( LB:tok -- LB:tok ) LB:PASS ;\n;package\n" PROG+
   s" lb-use.f" WRITE-PROG
   false MAIN-SOURCE ;

: TWICE-SOURCE ( -- )
   0 PROG-U !
   s" include" s" lb-own.f" LOAD-LINE+
   s" include" s" lb-own.f" LOAD-LINE+
   S\" : MAIN ( -- ) ;\n" PROG+
   s" lb-twice.f" WRITE-PROG ;

\ Builds a source under ROOT to ROOT/lb over the objects directory ROOT/<objs>. Each build is a
\ fresh hb because the build of a program is what is under test.
: LINK-BUILD ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n objs:ptr objsu:n :}
   ROOT-BUF ROOT-U @ objs objsu OBJ-BUF FS:JOIN-PATH OBJ-U !
   ROOT-BUF ROOT-U @ s" lb" IMAGE-BUF FS:JOIN-PATH IMAGE-U !
   src srcu AT-ROOT
   0 PROG-U !
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+  s" tools/hb-build.f" >LEN PROC-ARGV+  s" --" >LEN PROC-ARGV+
   PATH$ >LEN PROC-ARGV+  s" -o" >LEN PROC-ARGV+  IMAGE$ >LEN PROC-ARGV+
   s" HABU_OUT" >LEN OBJ$ >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ IMAGE-MS RUN ;

: LINK-RUN ( -- )
   0 PROG-U !
   PROC-ARGV-ENV-RESET
   IMAGE$ CHECK-MS RUN ;

\ The tail a walk of OBJ$ looks for, the last path ending with it and how many paths do.
create TAIL-BUF PATH-CAP allot  variable TAIL-U
create FOUND-BUF PATH-CAP allot  variable FOUND-U  variable FOUND-N
\ Slot i holds an object's path, 0 long if none, its size and its change time.
2 constant SLOTS
create KEPT-BUF SLOTS PATH-CAP * allot
SLOTS TYPED-BUFFER KEPT-U n
SLOTS TYPED-BUFFER KEPT-SIZE n
SLOTS TYPED-BUFFER KEPT-S n
SLOTS TYPED-BUFFER KEPT-NS n

: TAIL$ ( -- ptr u8 n ) TAIL-BUF TAIL-U @ ;
: KEPT-PATH ( n -- ptr u8 ) PATH-CAP * KEPT-BUF swap + ;
: KEPT$ ( n -- ptr u8 n ) {: i:n :} i KEPT-PATH i KEPT-U @ ;

: TAIL-SEEN ( ptr u8 n -- ) {: p:ptr pu:n :}
   p pu TAIL$ ENDS-WITH? 0= if exit then
   1 FOUND-N +!  p FOUND-BUF pu BYTE-COPY  pu FOUND-U ! ;

\ Holds in slot i the size and change time of the one object under OBJ$ whose path ends with
\ the tail; the slot stays empty when none or several do.
: KEEP ( ptr u8 n n -- ) {: t:ptr tu:n i:n :}
   0 i KEPT-U !
   t TAIL-BUF tu BYTE-COPY  tu TAIL-U !
   0 FOUND-N !  0 FOUND-U !
   OBJ$ FS:DIR? if OBJ$ [: TAIL-SEEN ;] FS:WALK-FILES then
   FOUND-N @ 1 <> if exit then
   FOUND-BUF i KEPT-PATH FOUND-U @ BYTE-COPY  FOUND-U @ i KEPT-U !
   i KEPT$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   size i KEPT-SIZE !  cs i KEPT-S !  cns i KEPT-NS ! ;

\ The label holds when the object in slot i keeps the size and change time KEEP read: the
\ build linked it, since a compile replaces the object.
: LINKED ( ptr u8 n n -- ) {: a:ptr u:n i:n :}
   a u T-LABEL
   i KEPT-U @ 0= if false TTRUE exit then
   i KEPT$ FS:FILE? 0= if false TTRUE exit then
   i KEPT$ FS:FILE-META {: size:n ms:n mns:n cs:n cns:n :}
   size i KEPT-SIZE @ =  cs i KEPT-S @ =  and  cns i KEPT-NS @ =  and TTRUE ;

: MAIN ( -- )
   FS:CLEANUP-RESET
   ROOT
   s" tools/check.f certifies a module that mints in its owner" T-LABEL
   OK-FIXTURE s" lin-ok.f" CHECK
   RC @ 0 T=
   s" and refuses a row in a public section by its code" T-LABEL
   BAD-FIXTURE s" lin-bad.f" CHECK
   RC @ 70 T=
   ERR$ S\" \"code\":\"E-LINEAR-SCOPE\",\"repair_class\":\"fix_cast_declaration\"" CONTAINS? TTRUE
   ERR$ S\" \"token\":\"MINT\",\"reason\":\"the row is outside its owner's private section\"" CONTAINS? TTRUE
   ERR$ S\" lin-bad.f\",\"line\":4," CONTAINS? TTRUE
   s" and refuses a qualified public destination" T-LABEL
   QUAL-FIXTURE s" lin-qual.f" CHECK
   RC @ 70 T=
   ERR$ S\" \"code\":\"E-LINEAR-SCOPE\",\"repair_class\":\"fix_cast_declaration\"" CONTAINS? TTRUE
   ERR$ S\" \"token\":\"LIN-FQ:MINT\",\"reason\":\"a qualified name publishes past the private section\"" CONTAINS? TTRUE
   ERR$ S\" lin-qual.f\",\"line\":6," CONTAINS? TTRUE
   s" and refuses an identity whose stack tails differ" T-LABEL
   TAIL-FIXTURE s" lin-tail.f" CHECK
   RC @ 70 T=
   ERR$ S\" \"code\":\"E-CAST-ARITY\",\"repair_class\":\"fix_cast_declaration\"" CONTAINS? TTRUE
   ERR$ S\" \"token\":\"MINT\",\"reason\":\"the input and output rows end in different tails\"" CONTAINS? TTRUE
   ERR$ S\" lin-tail.f\",\"line\":6," CONTAINS? TTRUE
   s" an hb built with --repl keeps the owner of a type in its grown table" T-LABEL
   SAVE
   RC @ 0 T=
   IMAGE$ FS:EXECUTABLE? TTRUE
   s" so it knows its source, its reopened owner mints and erases, and another package may not" T-LABEL
   REOPEN
   OUT$ s\" 42\n" CONTAINS? TTRUE
   ERR$ S\" E-LINEAR-OWNER habu: linear: 'XM': the linear type belongs to package 'LIM'\nhb: uncaught throw code 7196\n" CONTAINS? TTRUE
   RC @ 70 T=
   s" a build compiles a linear type its own file and another file name" T-LABEL
   LINK-SOURCES
   s" lb-main.f" s" objs" LINK-BUILD
   RC @ 0 T=
   IMAGE$ FS:EXECUTABLE? TTRUE
   LINK-RUN
   OUT$ s" 42" CONTAINS? TTRUE
   RC @ 0 T=
   s" /lb-own.f.hfo" 0 KEEP
   s" /lb-use.f.hfo" 1 KEEP
   s" and a build over the same objects after the main file gains a use of the type" T-LABEL
   true MAIN-SOURCE
   s" lb-main.f" s" objs" LINK-BUILD
   RC @ 0 T=
   s" and links the file that owns the type" 0 LINKED
   s" and links the file that uses it" 1 LINKED
   LINK-RUN
   OUT$ s" 42" CONTAINS? TTRUE
   RC @ 0 T=
   s" and a type declared twice is refused the same by both" T-LABEL
   TWICE-SOURCE
   s" lb-twice.f" s" twice-objs" LINK-BUILD
   RC @  dup 0 T<>
   ERR$ s" checker: bad or duplicate signature type" CONTAINS? TTRUE
   s" /lb-own.f.hfo" 0 KEEP
   s" lb-twice.f" s" twice-objs" LINK-BUILD
   RC @ T=
   ERR$ s" checker: bad or duplicate signature type" CONTAINS? TTRUE
   s" and the second build links the file's first load" 0 LINKED
   FS:CLEANUP-RUN ;

MAIN
;package

T-REPORT
