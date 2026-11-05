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
require test/app-image-engine.f
require test/pcap-result.f

package CHECKER-SURFACE-TEST

4096 constant CAP

600000 constant IMAGE-TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
create ROOT-BUF PATH-CAP allot  variable ROOT-U
create IMAGE-BUF PATH-CAP allot  variable IMAGE-U

: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;

\ The image class is asserted first: a whitebox engine leaves these names open.
: SECTION-CLASS ( -- )
   s" the engine under test is the sealed product" T-LABEL
   ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-SEALED T= ;

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
   SECTION-PRIVATE
   SECTION-APP-IMAGE
   T-REPORT ;

MAIN

;package
