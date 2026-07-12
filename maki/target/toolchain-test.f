\ maki/target/toolchain-test.f - toolchain identity owner tests.
\
\ Reopens package TOOLCHAIN so the private validated refinements (ROW>ID / ID>ROW,
\ CC-CK, DRV-CK, PATH>CC, NAME>DRV, HEX>, FIND-DIG, DIG-HIT, INTERN) each get a
\ focused test, not just coverage through the public surface.
\
\ Three of these are regressions for identity defects the owner shipped with, and each
\ is a case where the owner silently named the *wrong toolchain* rather than throwing:
\ a non-injective canonical form (delimiter injection), an id that outlived the RESET
\ that retired it, and an ADOPT that reused facts a previous ADOPT had already
\ consumed. Each has a reproducing pair below.

require lib/test.f
require test/checker-assert.f
require maki/target/toolchain.f

package TOOLCHAIN

\ CHECK verdicts: -1 certified, 0 rejected, 1 uncheckable (the name does not
\ resolve). A private word has no qualified path, so a candidate that reaches for
\ one is undefined rather than ill-typed - and stays fatal on the load path
\ (E-UNDEFINED, rc 70), never certified.
: YES   ( ptr u8 n -- )  CHECK-QUIET-CANDIDATE! -1 T= ;
: NO    ( ptr u8 n -- )  CHECK-QUIET-CANDIDATE!  0 T= ;
: UNDEF ( ptr u8 n -- )  CHECK-QUIET-CANDIDATE!  1 T= ;

\ ---- fixtures -----------------------------------------------------------------
: PTXAS-PATH$ ( -- ptr u8 n )  s" /usr/local/cuda-12.6/bin/ptxas" ;

: DEF-A ( -- CAD-KIND:toolchain-id )    \ the reference toolchain
   PTXAS s" 12.6.85" CUDA s" 580.65.06" s" -arch=sm_87" DEFINE ;

: DEF-CC-VER ( -- CAD-KIND:toolchain-id )    \ same but a newer assembler
   PTXAS s" 12.7.0" CUDA s" 580.65.06" s" -arch=sm_87" DEFINE ;

: DEF-DRV-VER ( -- CAD-KIND:toolchain-id )   \ same but a newer driver
   PTXAS s" 12.6.85" CUDA s" 585.10.01" s" -arch=sm_87" DEFINE ;

: DEF-CFG ( -- CAD-KIND:toolchain-id )       \ same but a different config
   PTXAS s" 12.6.85" CUDA s" 580.65.06" s" -arch=sm_90" DEFINE ;

: DEF-Z ( -- CAD-KIND:toolchain-id )         \ an unrelated toolchain, to take a reused row
   PTXAS s" 99.9" CUDA s" 1.0" s" -arch=sm_90" DEFINE ;

\ The proven delimiter-injection pair. Under an unframed `k=v;k=v` rendering both of
\ these render exactly "...;drvver=2;cfg=x;cfg=y", so they digest alike and the second
\ collapses onto the first - an identity reporting a driver version and a config that
\ it was never defined with. Framed by length, the drvver field is 0007 in one and
\ 0001 in the other, so the forms cannot coincide.
: DEF-INJ-A ( -- CAD-KIND:toolchain-id )     \ the driver version carries the delimiter
   PTXAS s" 1" CUDA s" 2;cfg=x" s" y" DEFINE ;

: DEF-INJ-B ( -- CAD-KIND:toolchain-id )     \ ...and here the config carries the tail
   PTXAS s" 1" CUDA s" 2" s" x;cfg=y" DEFINE ;

\ a descriptor with every variable field at its cap: the largest form the owner must
\ still render through SB and still fit in the arena
create MAXBUF FACT-CAP allot
: MAX-FILL ( -- )  FACT-CAP 0 ?do  $78 MAXBUF i + c!  loop ;

: DEF-MAX ( -- CAD-KIND:toolchain-id )
   MAX-FILL
   PTXAS MAXBUF FACT-CAP CUDA MAXBUF FACT-CAP MAXBUF FACT-CAP DEFINE ;

: MAX-ROW ( n -- ) {: ix:n :}                \ a maximal descriptor, distinct per row
   MAX-FILL
   ix $30 + MAXBUF c!
   PTXAS MAXBUF FACT-CAP CUDA MAXBUF FACT-CAP MAXBUF FACT-CAP DEFINE drop ;

: FILL-MAX-TABLE ( -- )                      \ TC-CAP maximal rows: the arena's worst case
   RESET
   TC-CAP 0 ?do  i MAX-ROW  loop ;

: ADOPT-FACTS ( -- )                         \ a complete audited discovery
   FACTS-RESET
   PTXAS-PATH$    FACT-COMPILER-PATH!
   s" 12.6.85"    FACT-COMPILER-VERSION!
   s" cuda"       FACT-DRIVER-NAME!
   s" 580.65.06"  FACT-DRIVER-VERSION!
   s" -arch=sm_87" FACT-CONFIG! ;

T-RESET

\ ---- identity is a function of the facts ---------------------------------------
RESET
DEF-A drop
IDS 1 T=
DEF-A DEF-A ID= TTRUE            \ equal facts collapse to one identity
IDS 1 T=

DEF-A DEF-CC-VER   ID= TFALSE    \ compiler version change -> distinct identity
DEF-A DEF-DRV-VER  ID= TFALSE    \ driver version change   -> distinct identity
DEF-A DEF-CFG      ID= TFALSE    \ config change           -> distinct identity
DEF-CC-VER DEF-CFG ID= TFALSE
IDS 4 T=

\ ---- typed projections ----------------------------------------------------------
DEF-A VERSION$        s" 12.6.85"    T$=
DEF-A DRIVER-VERSION$ s" 580.65.06"  T$=
DEF-A CONFIG$         s" -arch=sm_87" T$=
DEF-A COMPILER@ PTXAS COMPILER= TTRUE
DEF-A DRIVER@   CUDA  DRIVER=   TTRUE
DEF-CFG CONFIG$ s" -arch=sm_90" T$=

PTXAS COMPILER-NAME$ s" ptxas" T$=
CUDA  DRIVER-NAME$   s" cuda"  T$=

\ ---- canonical form + digest -----------------------------------------------------
\ every field framed by its exact byte length, so no field's content can be read as
\ another field's structure
DEF-A CANONICAL$
   s" cc=0005:ptxas;ver=0007:12.6.85;drv=0004:cuda;drvver=0009:580.65.06;cfg=000B:-arch=sm_87" T$=
DEF-A DIGEST$ nip DIGEST-LEN T=
DEF-A DIGEST$ DEF-CFG DIGEST$ STR= TFALSE    \ distinct facts -> distinct digests

\ ---- regression: the canonical form is injective -----------------------------------
\ Distinct facts, one rendering, one identity - the defect this pair exists to pin.
RESET
DEF-INJ-A DEF-INJ-B ID= TFALSE                          \ distinct identities...
DEF-INJ-A CANONICAL$ DEF-INJ-B CANONICAL$ STR= TFALSE   \ ...distinct canonical forms...
DEF-INJ-A DIGEST$    DEF-INJ-B DIGEST$    STR= TFALSE   \ ...distinct digests
IDS 2 T=                                                \ ...and two rows, not a collapse

\ each identity reports the facts it was actually defined with
DEF-INJ-A DRIVER-VERSION$ s" 2;cfg=x" T$=
DEF-INJ-A CONFIG$         s" y"       T$=
DEF-INJ-B DRIVER-VERSION$ s" 2"       T$=
DEF-INJ-B CONFIG$         s" x;cfg=y" T$=

\ the delimiter lands inside a framed field, so it cannot forge a field boundary
DEF-INJ-A CANONICAL$
   s" cc=0005:ptxas;ver=0001:1;drv=0004:cuda;drvver=0007:2;cfg=x;cfg=0001:y" T$=
DEF-INJ-B CANONICAL$
   s" cc=0005:ptxas;ver=0001:1;drv=0004:cuda;drvver=0001:2;cfg=0007:x;cfg=y" T$=

\ ---- canonical round-trip preserves the identity ---------------------------------
RESET
DEF-A drop
DEF-CFG drop
DEF-A DIGEST$ LOOKUP DEF-A ID= TTRUE
DEF-CFG DIGEST$ LOOKUP DEF-CFG ID= TTRUE
DEF-A DIGEST$ KNOWN? TTRUE
s" 0000000000000000" KNOWN? TFALSE           \ well-formed, and not interned

\ ---- the audited PTXTC discovery adapter -----------------------------------------
\ ADOPT must land on exactly the identity the typed constructor would have built.
ADOPT-FACTS
ADOPT DEF-A ID= TTRUE
IDS 2 T=

\ ---- regression: a discovery is adopted exactly once -------------------------------
\ ADOPT used to leave its facts staged, so restaging one fact and adopting again built
\ an identity from that fact plus four left over from the discovery already consumed.
: TN-ADOPT-TWICE ( -- )                      \ the facts were consumed by the first adopt
   ADOPT-FACTS  ADOPT drop
   ADOPT drop ;
: TN-ADOPT-RESTAGE ( -- )                    \ one fresh fact is not a discovery
   ADOPT-FACTS  ADOPT drop
   s" -arch=sm_90" FACT-CONFIG!
   ADOPT drop ;
: TN-RESET-CLEARS-FACTS ( -- )               \ RESET clears the staged facts too
   ADOPT-FACTS  RESET  ADOPT drop ;

' TN-ADOPT-TWICE        E-TC-FACT TTHROWS
' TN-ADOPT-RESTAGE      E-TC-FACT TTHROWS
' TN-RESET-CLEARS-FACTS E-TC-FACT TTHROWS

\ ---- private refinements ----------------------------------------------------------
RESET
DEF-A drop
PTXAS CC-CK  0 T=                       \ CC>RAW / RAW>CC round-trip
CUDA  DRV-CK 0 T=                       \ DRV>RAW / RAW>DRV round-trip
DEF-A ID>ROW 0 T=                       \ TC>RAW: the first identity is row 0
0 ROW>ID DEF-A ID= TTRUE                \ RAW>TC: validated allocation
PTXAS-PATH$ PATH>CC PTXAS COMPILER= TTRUE
s" cuda" NAME>DRV CUDA DRIVER= TTRUE
\ a digest hit is verified against the canonical form it claims to name, so a
\ collision could never quietly hand back the wrong toolchain
0 DEF-A CANONICAL$ HIT-AGREES? TTRUE
0 s" cc=0005:ptxas;ver=0003:9.9;drv=0004:cuda;drvver=0001:1;cfg=0001:x" HIT-AGREES? TFALSE
s" 00000000000000ff" HEX> $FF T=
s" abcdef0123456789" HEX> $ABCDEF0123456789 T=
s" ABCDEF0123456789" HEX> $ABCDEF0123456789 T=
$3039 FIND-DIG -1 T=                    \ an uninterned digest is a miss, not a row

\ ---- regression: RESET retires every id it issued -----------------------------------
\ An id used to be a bare row index, so after a RESET the next descriptor to take that
\ row answered to it - the old id silently reported the new toolchain's facts. An id
\ now carries the generation it was issued under, and RESET advances it.
variable STALE-ID
: TN-STALE-SETUP ( -- )
   RESET  DEF-A TC>RAW STALE-ID !        \ an id issued in this generation
   RESET  DEF-Z drop ;                   \ ...and a different toolchain now holds its row
: TN-STALE-USE ( -- )  STALE-ID @ RAW>TC VERSION$ 2drop ;
: TN-STALE-ROW ( -- )  STALE-ID @ RAW>TC ID>ROW drop ;

TN-STALE-SETUP
' TN-STALE-USE E-TC-ID TTHROWS          \ before: reported DEF-Z's version, silently
' TN-STALE-ROW E-TC-ID TTHROWS
IDS 1 T=                                \ the new generation has exactly one identity...
DEF-Z VERSION$ s" 99.9" T$=             \ ...and row 0 is DEF-Z, not the retired DEF-A
DEF-Z ID>ROW 0 T=                       \ a live id still names its row

\ the row is reused, the id is not: same facts, same row, a new generation
variable EPOCH1-ID
RESET
DEF-A TC>RAW EPOCH1-ID !
RESET
DEF-A TC>RAW EPOCH1-ID @ T<>            \ a different id cell...
DEF-A ID>ROW 0 T=                       \ ...naming the same reused row

\ a forged id is rejected before its row is indexed
: TN-FORGED-GEN ( -- )                  \ an id from a generation that is not live
   TC-GEN @ 1+ TC-IX-BITS lshift RAW>TC ID>ROW drop ;
: TN-FORGED-ROW ( -- )                  \ the live generation, but a row past the table
   TC-GEN @ TC-IX-BITS lshift TC-N @ or RAW>TC ID>ROW drop ;
: TN-ROW-NEG ( -- )   -1 ROW>ID drop ;
: TN-ROW-HIGH ( -- )  TC-N @ ROW>ID drop ;

' TN-FORGED-GEN E-TC-ID TTHROWS
' TN-FORGED-ROW E-TC-ID TTHROWS
' TN-ROW-NEG    E-TC-ID TTHROWS
' TN-ROW-HIGH   E-TC-ID TTHROWS

\ the generation does not wrap: it is exhausted, not reissued under live ids
: TN-EPOCH-END ( -- )  TC-GEN-MAX TC-GEN !  RESET ;
' TN-EPOCH-END E-TC-EPOCH TTHROWS
1 TC-GEN !                              \ TN-EPOCH-END pinned the generation at its last
RESET

\ ---- fail closed: incomplete discovery facts ---------------------------------------
: TN-NO-FACTS ( -- )     FACTS-RESET ADOPT drop ;
: TN-PARTIAL ( -- )                      \ path only: version/driver/config missing
   FACTS-RESET  PTXAS-PATH$ FACT-COMPILER-PATH!  ADOPT drop ;
: TN-NO-CONFIG ( -- )                    \ everything but the config
   FACTS-RESET
   PTXAS-PATH$   FACT-COMPILER-PATH!
   s" 12.6.85"   FACT-COMPILER-VERSION!
   s" cuda"      FACT-DRIVER-NAME!
   s" 580.65.06" FACT-DRIVER-VERSION!
   ADOPT drop ;
: TN-EMPTY-FACT ( -- )   s" " FACT-CONFIG! ;
: TN-EMPTY-VER ( -- )    PTXAS s" " CUDA s" 580.65.06" s" -arch=sm_87" DEFINE drop ;

' TN-NO-FACTS   E-TC-FACT TTHROWS
' TN-PARTIAL    E-TC-FACT TTHROWS
' TN-NO-CONFIG  E-TC-FACT TTHROWS
' TN-EMPTY-FACT E-TC-FACT TTHROWS
' TN-EMPTY-VER  E-TC-FACT TTHROWS

\ ---- fail closed: an unaudited compiler or driver is not a toolchain ----------------
: TN-BAD-CC ( -- )    s" /usr/local/cuda-12.6/bin/nvcc" PATH>CC drop ;
: TN-BAD-DRV ( -- )   s" rocm" NAME>DRV drop ;
: TN-ADOPT-BAD-CC ( -- )
   ADOPT-FACTS  s" /usr/bin/clang" FACT-COMPILER-PATH!  ADOPT drop ;
: TN-ADOPT-BAD-DRV ( -- )
   ADOPT-FACTS  s" rocm" FACT-DRIVER-NAME!  ADOPT drop ;

' TN-BAD-CC        E-TC-KIND TTHROWS
' TN-BAD-DRV       E-TC-KIND TTHROWS
' TN-ADOPT-BAD-CC  E-TC-KIND TTHROWS
' TN-ADOPT-BAD-DRV E-TC-KIND TTHROWS

\ ---- fail closed: malformed / unknown digests ----------------------------------------
\ KNOWN? answers about a *digest*. A string that is not one is not an unknown toolchain,
\ so it throws rather than reporting a legitimate miss.
: TN-DIG-SHORT ( -- )  s" ff" LOOKUP drop ;
: TN-DIG-LONG ( -- )   s" 00000000000000000" LOOKUP drop ;
: TN-DIG-CHAR ( -- )   s" 00000000000000zz" LOOKUP drop ;
: TN-DIG-MISS ( -- )   s" 0000000000000000" LOOKUP drop ;
: TN-KNOWN-SHORT ( -- ) s" ff" KNOWN? drop ;
: TN-KNOWN-LONG ( -- )  s" 00000000000000000" KNOWN? drop ;
: TN-KNOWN-CHAR ( -- )  s" 00000000000000zz" KNOWN? drop ;
: TN-KNOWN-EMPTY ( -- ) s" " KNOWN? drop ;

' TN-DIG-SHORT   E-TC-DIGEST TTHROWS
' TN-DIG-LONG    E-TC-DIGEST TTHROWS
' TN-DIG-CHAR    E-TC-DIGEST TTHROWS
' TN-DIG-MISS    E-TC-MISS   TTHROWS
' TN-KNOWN-SHORT E-TC-DIGEST TTHROWS
' TN-KNOWN-LONG  E-TC-DIGEST TTHROWS
' TN-KNOWN-CHAR  E-TC-DIGEST TTHROWS
' TN-KNOWN-EMPTY E-TC-DIGEST TTHROWS

\ ---- fail closed: a digest hit that does not agree with the form it names -------------
: TN-COLLIDE ( -- )
   RESET  DEF-A drop
   SB-RESET s" cc=0005:ptxas;ver=0003:9.9;drv=0004:cuda;drvver=0001:1;cfg=0001:x" SB-APPEND
   0 DIG-HIT drop ;

' TN-COLLIDE E-TC-COLLIDE TTHROWS

\ ---- the derived layout: a maximal descriptor is a valid one ---------------------------
\ Every variable field at FACT-CAP must still render through SB and still intern, so a
\ valid bounded fact can never leak a foreign E-STR-CAPACITY instead of a toolchain error.
RESET
DEF-MAX VERSION$   nip FACT-CAP T=
DEF-MAX CONFIG$    nip FACT-CAP T=
DEF-MAX CANONICAL$ nip CANON-CAP <= TTRUE     \ the derived cap really does bound the form
IDS 1 T=

\ and the arena holds TC-CAP of them, which is what makes exhaustion unreachable from
\ DEFINE / ADOPT
FILL-MAX-TABLE
IDS TC-CAP T=

\ ---- fail closed: capacity ----------------------------------------------------------------
create CAPBUF $4 allot
: CAP-VER$ ( n -- ptr u8 n ) {: ix:n :}     \ "vNN": a distinct version per row
   $76 CAPBUF c!
   ix $A /   $30 + CAPBUF 1+ c!
   ix $A mod $30 + CAPBUF 2 + c!
   CAPBUF 3 ;

: TN-TAB-FULL ( -- )
   RESET
   TC-CAP 1+ 0 ?do
      PTXAS i CAP-VER$ CUDA s" 1" s" -arch=sm_87" DEFINE drop
   loop ;

create BIGBUF $200 allot
: BIG-FILL ( -- )  $200 0 ?do  $78 BIGBUF i + c!  loop ;
: TN-FACT-BIG ( -- )  BIG-FILL  BIGBUF FACT-CAP 1+  FACT-CONFIG! ;

\ the interning guard itself: unreachable through DEFINE / ADOPT (the arena is derived to
\ hold TC-CAP maximal rows), so it is driven directly through the private seam
: TN-ARENA-FULL ( -- )
   RESET
   TC-ARENA-CAP TC-ARENA-U !
   PTXAS-PATH$ INTERN 2drop ;

' TN-TAB-FULL   E-TC-CAP TTHROWS
' TN-FACT-BIG   E-TC-CAP TTHROWS
' TN-ARENA-FULL E-TC-CAP TTHROWS

RESET                                       \ TN-ARENA-FULL pinned the arena at its cap

\ ---- the checked boundary: identities do not swap and raw cells do not convert ----------
s" TC-POS-VER ( CAD-KIND:toolchain-id -- ptr u8 n ) TOOLCHAIN:VERSION$" YES
s" TC-POS-CFG ( CAD-KIND:toolchain-id -- ptr u8 n ) TOOLCHAIN:CONFIG$" YES
s" TC-POS-KINDS ( -- TOOLCHAIN:compiler TOOLCHAIN:driver ) TOOLCHAIN:PTXAS TOOLCHAIN:CUDA" YES
s" TC-POS-ADOPT ( -- CAD-KIND:toolchain-id ) TOOLCHAIN:ADOPT" YES
s" TC-POS-DEFINE ( TOOLCHAIN:compiler ptr u8 n TOOLCHAIN:driver ptr u8 n ptr u8 n -- CAD-KIND:toolchain-id ) TOOLCHAIN:DEFINE" YES

\ canonical round-trip preserves the family, and the checker proves it
s" TC-POS-RT ( CAD-KIND:toolchain-id -- CAD-KIND:toolchain-id ) TOOLCHAIN:DIGEST$ TOOLCHAIN:LOOKUP" YES

\ a target is not a toolchain, in either direction
s" TC-NEG-TARGET-IN ( CAD-KIND:target-id -- ptr u8 n ) TOOLCHAIN:VERSION$" NO
s" TC-NEG-TARGET-OUT ( CAD-KIND:toolchain-id -- CAD-KIND:target-id )" NO
s" TC-NEG-TARGET-DEF ( TOOLCHAIN:compiler ptr u8 n TOOLCHAIN:driver ptr u8 n ptr u8 n -- CAD-KIND:target-id ) TOOLCHAIN:DEFINE" NO
s" TC-NEG-TARGET-ADOPT ( -- CAD-KIND:target-id ) TOOLCHAIN:ADOPT" NO

\ a compiler is not a driver
s" TC-NEG-CC-AS-DRV ( TOOLCHAIN:driver -- ptr u8 n ) TOOLCHAIN:COMPILER-NAME$" NO
s" TC-NEG-DRV-AS-CC ( TOOLCHAIN:compiler -- ptr u8 n ) TOOLCHAIN:DRIVER-NAME$" NO
s" TC-NEG-DEFINE-SWAP ( TOOLCHAIN:driver ptr u8 n TOOLCHAIN:compiler ptr u8 n ptr u8 n -- CAD-KIND:toolchain-id ) TOOLCHAIN:DEFINE" NO

\ no public raw conversions: an `n` is not an identity and an identity is not an `n`
s" TC-NEG-RAW-IN ( n -- ptr u8 n ) TOOLCHAIN:VERSION$" NO
s" TC-NEG-RAW-OUT ( CAD-KIND:toolchain-id -- n )" NO
s" TC-NEG-RAW-CC ( n -- ptr u8 n ) TOOLCHAIN:COMPILER-NAME$" NO
s" TC-NEG-RAW-DRV ( n -- ptr u8 n ) TOOLCHAIN:DRIVER-NAME$" NO
s" TC-NEG-IDS ( -- CAD-KIND:toolchain-id ) TOOLCHAIN:IDS" NO

\ the refinements are private: no qualified path reaches them, so a forge is an
\ undefined word rather than a certified conversion
s" TC-NEG-PRIV-IN ( n -- CAD-KIND:toolchain-id ) TOOLCHAIN:RAW>TC" UNDEF
s" TC-NEG-PRIV-OUT ( CAD-KIND:toolchain-id -- n ) TOOLCHAIN:TC>RAW" UNDEF
s" TC-NEG-PRIV-CC ( n -- TOOLCHAIN:compiler ) TOOLCHAIN:RAW>CC" UNDEF
s" TC-NEG-PRIV-DRV ( n -- TOOLCHAIN:driver ) TOOLCHAIN:RAW>DRV" UNDEF
s" TC-NEG-PRIV-COMMIT ( n -- CAD-KIND:toolchain-id ) TOOLCHAIN:COMMIT" UNDEF
s" TC-NEG-PRIV-ROW ( n -- CAD-KIND:toolchain-id ) TOOLCHAIN:ROW>ID" UNDEF
s" TC-NEG-PRIV-ID-ROW ( CAD-KIND:toolchain-id -- n ) TOOLCHAIN:ID>ROW" UNDEF

T-REPORT

;package
