\ checker-surface.f - the checker symbols a product engine ships. A capture
\ keeps a checker symbol only when a record the image ships with its name
\ resolves to it (src/core/checker-surface.f, src/core/checker.f CHECKER-SWEEP):
\ stripped helper names stop resolving; everything a source can name keeps its
\ effect.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/fs-mutate.f
require src/lib/test/subject.f
require test/app-image-engine.f
require test/pcap-result.f

\ ---- a program's own definitions, compiled after boot --------------------------
defer CS-HOOK ( n -- n )
: CS-HOOK-INSTALL ( -- ) [: 2 * ;] is CS-HOOK ;
CS-HOOK-INSTALL

: CS-DEF ( n -- ) create , does> ( -- ptr n ) ;
7 CS-DEF CS-SEVEN

TYPED-VARIABLE CS-CELL n

STRUCTURE cspair
   FIELD left n
   FIELD right n
;STRUCTURE

package CS-OWN
public
: CS-INC ( n -- n ) 1 + ;
;package

package CS-ALIAS
public
EXPORT CS-OWN:CS-INC
;package

package CHECKER-SURFACE-TEST

-1 constant ACCEPTED
0 constant REFUSED

4096 constant CAP
30000 constant TIMEOUT-MS
70 constant REJECT-RC

600000 constant IMAGE-TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
create ROOT-BUF PATH-CAP allot  variable ROOT-U
create IMAGE-BUF PATH-CAP allot  variable IMAGE-U

: VERDICT ( ptr u8 n -- n )
   CHECK-QUIET-CANDIDATE! ;

: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;

\ One program, evaluated by a disposable fork of this engine.
: LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   {: outu:len erru:len outcome:outcome :}
   outu LEN>N OUT-U !
   erru LEN>N ERR-U !
   outcome PROC-OUTCOME>RC RC ! ;

: KNOWN? ( ptr u8 n -- bool ) EFFECT-QUERY ;

\ The image class is asserted first: a whitebox engine leaves these names open.
: SECTION-CLASS ( -- )
   s" the engine under test is the sealed product" T-LABEL
   ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-SEALED T= ;

: SECTION-INTERNAL ( -- )
   s" a named internal checker helper keeps its effect" T-LABEL
   s" HIDX-BUILD" KNOWN? TTRUE
   s" a named internal global of another file keeps its effect" T-LABEL
   s" CORE-FOLD-C" KNOWN? TTRUE
   s" checked definitions cannot call either internal record" T-LABEL
   s" CS-U1 ( -- ) HIDX-BUILD" VERDICT REFUSED T=
   s" CS-U2 ( n -- n ) CORE-FOLD-C" VERDICT REFUSED T=
   s" a compiled call is refused at load: only a TRUSTED: body may make it" T-LABEL
   s\" : CS-U3 ( -- ) HIDX-BUILD ;\n" LOAD
   RC @ REJECT-RC T=
   ERR$ s" E-CAP-TRUSTED" CONTAINS? TTRUE
   ERR$ s" HIDX-BUILD" CONTAINS? TTRUE
   s" an interpreted call is refused as an internal engine word" T-LABEL
   s\" CORE-FOLD-C\n" LOAD
   RC @ REJECT-RC T=
   ERR$ s" hb: internal engine word: CORE-FOLD-C" CONTAINS? TTRUE ;

: PRIVATE-KNOWN? ( ptr u8 n ptr u8 n -- bool ) {: pa:ptr pu:n na:ptr nu:n :}
   pa pu false na nu CHECKER-ASIG-KNOWN? ;

\ A private symbol stays only while a shipped record resolves to it and its
\ package can be reopened; the build seals every package it bakes
\ (src/core/internal-mark.f SEAL-PACKAGES). FRAME's package seals itself,
\ DECIDE's name is stripped, CHECKED-ROW's is kept: all three are gone.
: SECTION-PRIVATE ( -- )
   s" a private of a package that seals itself keeps no symbol" T-LABEL
   s" CHECKER-DECL-FRAME" s" FRAME" PRIVATE-KNOWN? TFALSE
   s" nor a private whose name the image strips" T-LABEL
   s" CHECKER-SWEEP" s" DECIDE" PRIVATE-KNOWN? TFALSE
   s" a private of every baked package keeps no symbol, named or not" T-LABEL
   s" CHECKER-REG" s" CHECKED-ROW" PRIVATE-KNOWN? TFALSE ;

\ TFL-CVAR?, a DNAME-INT word habu2.f C-FIND-GLOBAL reads for `construct`, keeps
\ its effect so a TRUSTED: body at tier 1 still compiles a call to it: the
\ native compiler asks the checker for the callee's arity.
: SECTION-RETAINED ( -- )
   s" an internal word the image keeps by name keeps its effect" T-LABEL
   s" TFL-CVAR?" KNOWN? TTRUE
   s" and a tier-1 TRUSTED: body compiles a call to it" T-LABEL
   s\" 1 set-tier\nTRUSTED: CS-T1 ( ptr u8 n n -- n n bool ) TFL-CVAR? ;\n" LOAD
   RC @ 0 T= ;

: SECTION-PUBLIC ( -- )
   s" a public word certifies" T-LABEL
   s" CS-P1 ( ptr u8 n -- bool ) CHECKER-RESOLVES?" VERDICT ACCEPTED T=
   s" a package public certifies" T-LABEL
   s" CS-P2 ( -- n ) ENGINE-INTERNAL:IMAGE-CLASS" VERDICT ACCEPTED T=
   s" a call short of its inputs is refused" T-LABEL
   s" CS-P3 ( ptr u8 -- bool ) CHECKER-RESOLVES?" VERDICT REFUSED T=
   s" a primitive axiom certifies" T-LABEL
   s" CS-P4 ( n -- n n ) dup" VERDICT ACCEPTED T=
   s" the return-stack words the control prefix names certify" T-LABEL
   s" CS-P5 ( n -- n ) >r r>" VERDICT ACCEPTED T=
   s" CS-P6 ( n n -- n n ) 2>r 2r>" VERDICT ACCEPTED T=
   s" and a return-stack imbalance is still refused" T-LABEL
   s" CS-P7 ( n -- ) >r" VERDICT REFUSED T= ;

: SECTION-AFTER-BOOT ( -- )
   s" a defer runs what `is` installed, and certifies" T-LABEL
   3 CS-HOOK 6 T=
   s" CS-A1 ( n -- n ) CS-HOOK" VERDICT ACCEPTED T=
   s" a does> definer's word holds and certifies its clause effect" T-LABEL
   CS-SEVEN @ 7 T=
   s" CS-A2 ( -- ptr n ) CS-SEVEN" VERDICT ACCEPTED T=
   s" CS-A3 ( -- n ) CS-SEVEN" VERDICT REFUSED T=
   s" a TYPED-VARIABLE stores and certifies" T-LABEL
   11 CS-CELL !  CS-CELL @ 11 T=
   s" CS-A4 ( -- ptr n ) CS-CELL" VERDICT ACCEPTED T=
   s" a STRUCTURE makes and unmakes" T-LABEL
   4 5 CSPAIR:MAKE CSPAIR:UNMAKE 5 T= 4 T=
   s" CS-A5 ( n n -- n n ) CSPAIR:MAKE CSPAIR:UNMAKE" VERDICT ACCEPTED T=
   s" an EXPORT alias runs and certifies" T-LABEL
   8 CS-ALIAS:CS-INC 9 T=
   s" CS-A6 ( n -- n ) CS-ALIAS:CS-INC" VERDICT ACCEPTED T= ;

\ ---- an application image, saved and reopened ----------------------------------
: RESULT ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

: SHOW ( -- )
   RC @ 0<> if s" checker-surface child rc " type RC @ . OUT$ type ERR$ type cr then ;

: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: SAVE-APP ( -- )
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   s" checker-surface" FS:MKTEMP-DIR {: path:ptr size:n :}
   path ROOT-BUF size BYTE-COPY size ROOT-U !
   ROOT-BUF ROOT-U @ FS:CLEANUP-TREE+
   ROOT-BUF ROOT-U @ s" app" IMAGE-BUF FS:JOIN-PATH IMAGE-U !
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   host hostu >LEN
   S\" 1 set-tier\npackage CSAPP\n: PRIV ( n -- n ) 3 + ;\npublic\n: PUB ( n -- n ) PRIV 2 * ;\n;package\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN IMAGE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT SHOW ;

: REOPEN-APP ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN
   S\" : CS-SAY ( ptr u8 n bool -- ) {: a:ptr u:n f:bool :} a u type f if s\"  known\" else s\"  unknown\" then type cr ;\ns\" pub\" s\" CSAPP:PUB\" EFFECT-QUERY CS-SAY\npackage CSAPP\ns\" priv\" s\" PRIV\" EFFECT-QUERY CS-SAY\n: Z ( n -- n ) PRIV ;\n4 Z .\n;package\n5 CSAPP:PUB .\n" >LEN
   OUT CAP >LEN ERR CAP >LEN IMAGE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT SHOW ;

: REOPEN-ENGINE ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN
   S\" package TOP-ROW ;package\n" >LEN
   OUT CAP >LEN ERR CAP >LEN IMAGE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT ;

\ An application image keeps its own package's private and public symbols: a
\ snapshot ships every record with its name and an application package is not
\ sealed. An engine package stays sealed in it.
: SECTION-APP-IMAGE ( -- )
   FS:CLEANUP-RESET
   s" an application image saves its own package" T-LABEL
   SAVE-APP
   RC @ 0 T=
   IMAGE$ FS:EXECUTABLE? TTRUE
   s" the image knows its package's public word" T-LABEL
   REOPEN-APP
   RC @ 0 T=
   OUT$ s" pub known" CONTAINS? TTRUE
   s" and, reopened, its private word, which certifies and runs" T-LABEL
   OUT$ s" priv known" CONTAINS? TTRUE
   OUT$ s\" 7\n" CONTAINS? TTRUE
   OUT$ s\" 16\n" CONTAINS? TTRUE
   s" while an engine package stays sealed in it" T-LABEL
   REOPEN-ENGINE
   RC @ ENGINE-ERROR:SEAL-PACKAGE T=
   FS:CLEANUP-RUN ;

: MAIN ( -- )
   T-RESET
   SECTION-CLASS
   SECTION-INTERNAL
   SECTION-PRIVATE
   SECTION-RETAINED
   SECTION-PUBLIC
   SECTION-AFTER-BOOT
   SECTION-APP-IMAGE
   T-REPORT ;

MAIN

;package
