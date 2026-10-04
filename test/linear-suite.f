\ linear-suite.f - `LINEAR:` mints and erases a DEFLINEAR token in its owner alone.
\ Run: bin/hb --load test/linear-suite.f
\
\ A DEFLINEAR type records the package that declared it. In that package's
\ private section `LINEAR: NAME ( payload -- PKG:tok )` mints a token and
\ `LINEAR: NAME ( PKG:tok -- payload )` erases one: the engine's `linear:`
\ reader keyword publishes a body-free identity word, as `cast:` does, once the
\ checker (CHECKER-LINEAR) has certified the row. A refusal throws its named
\ code and publishes nothing:
\   - E-LINEAR-PAYLOAD: the row is not one linear type and one payload, a
\                       non-linear constructor or a pointer chain ending at one
\   - E-LINEAR-OWNER  : the current package did not declare the linear type; a
\                       top-level DEFLINEAR has no owner, so nothing mints it
\   - E-LINEAR-SCOPE  : the row is outside its owner's private section or its
\                       qualified name would publish into a public wordlist
\   - E-CAST-ARITY, E-CAST-FAM: the shapes `cast:` refuses, including different
\                       row tails beneath the operand of an identity
\ `cast:` still carries no linear type (E-CAST-LINEAR), in the owner too. Every
\ front end runs: the engine's keyword evaluated as source, the source pre-pass,
\ tools/check.f on written fixtures, and an application image saved after its
\ type table grew past the boot store, then reopened to mint for the owner it
\ restored.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/fs-mutate.f
require lib/engine-candidate.f
require lib/test/subject.f
require src/habu/verify-source.f
require test/app-image-engine.f

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
NEWTYPE ofam 0
;package
DEFLINEAR lin-free

\ One source text through the production path: the engine reads it as it reads
\ a file, so a `linear:` row meets the live keyword and checker. The answer is
\ the code a refusal threw, or 0.
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
\ VERIFY:SOURCE-BUF replays package, public and private, records each
\ DEFLINEAR's package and certifies each row through the engine's own checker.
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
   oc MATCH outcome
     exited OF ENDOF
     signaled OF drop -1 ENDOF
     timeout OF src u OUT outu LEN>N ERR erru LEN>N SUBJECT:TIMED-OUT ENDOF
   ;MATCH ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
;package
s" the keyword with nothing after it names itself" T-LABEL
s" linear:" LIN-CHILD:RUN $4A T=
LIN-CHILD:ERR$ s" hb: reader keyword needs a name: linear:" CONTAINS? TTRUE
s" a row's bare call at an empty stack is refused by name" T-LABEL
s" package LCH public DEFLINEAR LCH:tok ;package package LCH LINEAR: LCM ( n -- LCH:tok ) LCM"
   LIN-CHILD:RUN 70 T=
LIN-CHILD:ERR$ s" hb: interpret stack underdepth: LCM" CONTAINS? TTRUE

\ ---- tools/check.f and a saved application image --------------------------------
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
create ROOT-BUF FS-PATH-CAP allot  variable ROOT-U
create PATH-BUF FS-PATH-CAP allot  variable PATH-U
create IMAGE-BUF FS-PATH-CAP allot  variable IMAGE-U
PROG-CAP BUFFER: PROG
variable PROG-U

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: PATH$ ( -- ptr u8 n ) PATH-BUF PATH-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: PROG$ ( -- ptr u8 n ) PROG PROG-U @ ;

: PROG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a PROG PROG-U @ + u BYTE-COPY
   PROG-U @ u + PROG-U ! ;
: PROG-C+ ( n -- )
   PROG PROG-U @ + c!
   PROG-U @ 1 + PROG-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U !  erru LEN>N ERR-U !  0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N OUT-U !  erru LEN>N ERR-U !  rc RC>N RC ! ENDOF
   ;MATCH ;

\ The argv is staged; run PATH with PROG on stdin.
: RUN ( ptr u8 n n -- ) {: path:ptr pathu:n ms:n :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN  PROG$ >LEN  OUT CAP >LEN  ERR CAP >LEN  ms >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT ;

: ROOT ( -- )
   s" linear-suite" HB-TMP-MKDIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT-BUF ROOT-U @ CLEANUP-TREE+ ;

\ Write PROG to the fixture NAME under the root and run tools/check.f on it.
: CHECK ( ptr u8 n -- ) {: name:ptr nameu:n :}
   ENGINE-CANDIDATE:PATH$ {: hb:ptr hbu:n :}
   ROOT-BUF ROOT-U @ name nameu PATH-BUF JOIN-PATH PATH-U !
   PATH$ PROG$ WRITE-ALL
   0 PROG-U !
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   PATH$ >LEN PROC-ARGV+
   hb hbu CHECK-MS RUN ;

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
\ grown store that an image save must persist.
: FILLER ( -- )
   256 0 ?do
      s" DEFLINEAR lf" PROG+
      i 16 / [char] a + PROG-C+
      i 16 mod [char] a + PROG-C+
      10 PROG-C+
   loop ;

: SAVE ( -- )
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   ROOT-BUF ROOT-U @ s" app" IMAGE-BUF JOIN-PATH IMAGE-U !
   0 PROG-U !
   S\" 1 set-tier\n" PROG+
   FILLER
   S\" package LIM\npublic\nDEFLINEAR LIM:tok\n;package\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" PROG+
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   host hostu IMAGE-MS RUN ;

\ The restored owner mints and erases; another package still may not.
: REOPEN ( -- )
   0 PROG-U !
   S\" package LIM\nLINEAR: MINT ( n -- LIM:tok )\nLINEAR: ERASE ( LIM:tok -- n )\n" PROG+
   S\" : RT ( n -- n ) MINT ERASE ;\n42 RT .\n;package\n" PROG+
   S\" package LIM-X\nLINEAR: XM ( n -- LIM:tok )\n;package\n" PROG+
   PROC-ARGV-ENV-RESET
   IMAGE$ IMAGE-MS RUN ;

: MAIN ( -- )
   CLEANUP-RESET
   ROOT
   s" tools/check.f certifies a module that mints in its owner" T-LABEL
   OK-FIXTURE s" lin-ok.f" CHECK
   RC @ 0 T=
   s" and refuses a row in a public section by its code" T-LABEL
   BAD-FIXTURE s" lin-bad.f" CHECK
   RC @ 70 T=
   ERR$ s" throw 7197 at 'MINT'" CONTAINS? TTRUE
   s" and refuses a qualified public destination" T-LABEL
   QUAL-FIXTURE s" lin-qual.f" CHECK
   RC @ 70 T=
   ERR$ s" throw 7197 at 'LIN-FQ:MINT'" CONTAINS? TTRUE
   s" and refuses an identity whose stack tails differ" T-LABEL
   TAIL-FIXTURE s" lin-tail.f" CHECK
   RC @ 70 T=
   ERR$ s" throw 7129 at 'MINT'" CONTAINS? TTRUE
   s" an application image keeps the owner of a type in its grown table" T-LABEL
   SAVE
   RC @ 0 T=
   IMAGE$ EXECUTABLE? TTRUE
   s" so the reopened owner mints and erases, and another package may not" T-LABEL
   REOPEN
   OUT$ s\" 42\n" CONTAINS? TTRUE
   ERR$ s" hb: uncaught throw code 7196" CONTAINS? TTRUE
   RC @ 67 T=
   CLEANUP-RUN ;

MAIN
;package

T-REPORT
