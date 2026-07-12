\ maki/target/toolchain-test.f - toolchain identity owner tests.
\
\ Reopens package TOOLCHAIN so the private validated refinements (RAW-ID / ID-CK,
\ CC-CK, DRV-CK, PATH>CC, NAME>DRV, HEX>, FIND-DIG) each get a focused test, not
\ just coverage through the public surface.

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
DEF-A CANONICAL$
   s" cc=ptxas;ver=12.6.85;drv=cuda;drvver=580.65.06;cfg=-arch=sm_87" T$=
DEF-A DIGEST$ nip $10 T=
DEF-A DIGEST$ DEF-CFG DIGEST$ STR= TFALSE    \ distinct facts -> distinct digests

\ ---- canonical round-trip preserves the identity ---------------------------------
DEF-A DIGEST$ LOOKUP DEF-A ID= TTRUE
DEF-CFG DIGEST$ LOOKUP DEF-CFG ID= TTRUE
DEF-A DIGEST$ KNOWN? TTRUE
s" 0000000000000000" KNOWN? TFALSE

\ ---- the audited PTXTC discovery adapter -----------------------------------------
\ ADOPT must land on exactly the identity the typed constructor would have built.
ADOPT-FACTS
ADOPT DEF-A ID= TTRUE
IDS 4 T=

\ ---- private refinements ----------------------------------------------------------
PTXAS CC-CK  0 T=                       \ CC>RAW / RAW>CC round-trip
CUDA  DRV-CK 0 T=                       \ DRV>RAW / RAW>DRV round-trip
DEF-A ID-CK  0 T=                       \ TC>RAW: the first identity is row 0
0 RAW-ID DEF-A ID= TTRUE                \ RAW>TC: validated allocation
PTXAS-PATH$ PATH>CC PTXAS COMPILER= TTRUE
s" cuda" NAME>DRV CUDA DRIVER= TTRUE
\ a digest hit is verified against the canonical form it claims to name, so a
\ collision could never quietly hand back the wrong toolchain
0 DEF-A CANONICAL$ HIT-AGREES? TTRUE
0 s" cc=ptxas;ver=9.9.9;drv=cuda;drvver=1;cfg=-arch=sm_87" HIT-AGREES? TFALSE
s" 00000000000000ff" HEX> $FF T=
s" abcdef0123456789" HEX> $ABCDEF0123456789 T=
s" ABCDEF0123456789" HEX> $ABCDEF0123456789 T=
$3039 FIND-DIG -1 T=                    \ an uninterned digest is a miss, not a row

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
: TN-DIG-SHORT ( -- )  s" ff" LOOKUP drop ;
: TN-DIG-LONG ( -- )   s" 00000000000000000" LOOKUP drop ;
: TN-DIG-CHAR ( -- )   s" 00000000000000zz" LOOKUP drop ;
: TN-DIG-MISS ( -- )   s" 0000000000000000" LOOKUP drop ;

' TN-DIG-SHORT E-TC-DIGEST TTHROWS
' TN-DIG-LONG  E-TC-DIGEST TTHROWS
' TN-DIG-CHAR  E-TC-DIGEST TTHROWS
' TN-DIG-MISS  E-TC-MISS   TTHROWS

\ ---- fail closed: a stale id does not survive a RESET -----------------------------------
variable STALE
: TN-STALE-SETUP ( -- )  RESET  DEF-A ID-CK STALE !  RESET ;
: TN-STALE-USE ( -- )    STALE @ RAW-ID drop ;

TN-STALE-SETUP
' TN-STALE-USE E-TC-ID TTHROWS

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

' TN-TAB-FULL E-TC-CAP TTHROWS
' TN-FACT-BIG E-TC-CAP TTHROWS

RESET

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

T-REPORT

;package
