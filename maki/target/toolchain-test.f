\ maki/target/toolchain-test.f - toolchain identity owner tests.
\
\ Low-level tests use TOOLCHAIN's bounded audited TEST-* seam; no test reopens the
\ owner or receives a raw refinement, WID, mutable arena span, or owner state cell.
\
\ Regressions cover delimiter-injection aliasing, ids that outlive RESET, mutable
\ projection aliases, and discovery rounds that previously shared partial state.

require lib/test.f
require test/checker-assert.f
require maki/target/toolchain.f

package TOOLCHAIN-TEST

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
   TOOLCHAIN:PTXAS s" 12.6.85" TOOLCHAIN:CUDA s" 580.65.06" s" -arch=sm_87" TOOLCHAIN:DEFINE ;

: DEF-CC-VER ( -- CAD-KIND:toolchain-id )    \ same but a newer assembler
   TOOLCHAIN:PTXAS s" 12.7.0" TOOLCHAIN:CUDA s" 580.65.06" s" -arch=sm_87" TOOLCHAIN:DEFINE ;

: DEF-DRV-VER ( -- CAD-KIND:toolchain-id )   \ same but a newer driver
   TOOLCHAIN:PTXAS s" 12.6.85" TOOLCHAIN:CUDA s" 585.10.01" s" -arch=sm_87" TOOLCHAIN:DEFINE ;

: DEF-CFG ( -- CAD-KIND:toolchain-id )       \ same but a different config
   TOOLCHAIN:PTXAS s" 12.6.85" TOOLCHAIN:CUDA s" 580.65.06" s" -arch=sm_90" TOOLCHAIN:DEFINE ;

: DEF-Z ( -- CAD-KIND:toolchain-id )         \ an unrelated toolchain, to take a reused row
   TOOLCHAIN:PTXAS s" 99.9" TOOLCHAIN:CUDA s" 1.0" s" -arch=sm_90" TOOLCHAIN:DEFINE ;

: DISC-A ( -- TOOLCHAIN:disc )
   PTXAS-PATH$ s" 12.6.85" s" cuda" s" 580.65.06" s" -arch=sm_87"
   TOOLCHAIN-DISC:MAKE ;

: ADOPT-A ( -- CAD-KIND:toolchain-id )  DISC-A TOOLCHAIN:ADOPT ;

\ The proven delimiter-injection pair. Under an unframed `k=v;k=v` rendering both of
\ these render exactly "...;drvver=2;cfg=x;cfg=y", so they digest alike and the second
\ collapses onto the first - an identity reporting a driver version and a config that
\ it was never defined with. Framed by length, the drvver field is 0007 in one and
\ 0001 in the other, so the forms cannot coincide.
: DEF-INJ-A ( -- CAD-KIND:toolchain-id )     \ the driver version carries the delimiter
   TOOLCHAIN:PTXAS s" 1" TOOLCHAIN:CUDA s" 2;cfg=x" s" y" TOOLCHAIN:DEFINE ;

: DEF-INJ-B ( -- CAD-KIND:toolchain-id )     \ ...and here the config carries the tail
   TOOLCHAIN:PTXAS s" 1" TOOLCHAIN:CUDA s" 2" s" x;cfg=y" TOOLCHAIN:DEFINE ;

\ a descriptor with every variable field at its cap: the largest form the owner must
\ still render through SB and still fit in the arena
create MAXBUF TOOLCHAIN:FACT-CAPACITY allot
: MAX-FILL ( -- )  TOOLCHAIN:FACT-CAPACITY 0 ?do  $78 MAXBUF i + c!  loop ;

: DEF-MAX ( -- CAD-KIND:toolchain-id )
   MAX-FILL
   TOOLCHAIN:PTXAS MAXBUF TOOLCHAIN:FACT-CAPACITY
   TOOLCHAIN:CUDA MAXBUF TOOLCHAIN:FACT-CAPACITY
   MAXBUF TOOLCHAIN:FACT-CAPACITY TOOLCHAIN:DEFINE ;

: MAX-ROW ( n -- ) {: ix:n :}                \ a maximal descriptor, distinct per row
   MAX-FILL
   ix $30 + MAXBUF c!
   TOOLCHAIN:PTXAS MAXBUF TOOLCHAIN:FACT-CAPACITY
   TOOLCHAIN:CUDA MAXBUF TOOLCHAIN:FACT-CAPACITY
   MAXBUF TOOLCHAIN:FACT-CAPACITY TOOLCHAIN:DEFINE drop ;

: FILL-MAX-TABLE ( -- )                      \ TOOLCHAIN:ID-CAPACITY maximal rows: arena worst case
   TOOLCHAIN:RESET
   TOOLCHAIN:ID-CAPACITY 0 ?do  i MAX-ROW  loop ;

T-RESET

\ ---- identity is a function of the facts ---------------------------------------
TOOLCHAIN:RESET
DEF-A drop
TOOLCHAIN:IDS 1 T=
DEF-A DEF-A TOOLCHAIN:ID= TTRUE            \ equal facts collapse to one identity
TOOLCHAIN:IDS 1 T=

DEF-A DEF-CC-VER   TOOLCHAIN:ID= TFALSE    \ compiler version change -> distinct identity
DEF-A DEF-DRV-VER  TOOLCHAIN:ID= TFALSE    \ driver version change   -> distinct identity
DEF-A DEF-CFG      TOOLCHAIN:ID= TFALSE    \ config change           -> distinct identity
DEF-CC-VER DEF-CFG TOOLCHAIN:ID= TFALSE
TOOLCHAIN:IDS 4 T=

\ ---- typed projections ----------------------------------------------------------
DEF-A SB-RESET TOOLCHAIN:VERSION+        SB$ s" 12.6.85"    T$=
DEF-A SB-RESET TOOLCHAIN:DRIVER-VERSION+ SB$ s" 580.65.06"  T$=
DEF-A SB-RESET TOOLCHAIN:CONFIG+         SB$ s" -arch=sm_87" T$=
DEF-A TOOLCHAIN:COMPILER@ TOOLCHAIN:PTXAS TOOLCHAIN:COMPILER= TTRUE
DEF-A TOOLCHAIN:DRIVER@   TOOLCHAIN:CUDA  TOOLCHAIN:DRIVER=   TTRUE
DEF-CFG SB-RESET TOOLCHAIN:CONFIG+ SB$ s" -arch=sm_90" T$=

TOOLCHAIN:PTXAS TOOLCHAIN:COMPILER-NAME$ s" ptxas" T$=
TOOLCHAIN:CUDA  TOOLCHAIN:DRIVER-NAME$   s" cuda"  T$=

\ ---- canonical form + digest -----------------------------------------------------
\ every field framed by its exact byte length, so no field's content can be read as
\ another field's structure
DEF-A SB-RESET TOOLCHAIN:CANONICAL+ SB$
   s" cc=0005:ptxas;ver=0007:12.6.85;drv=0004:cuda;drvver=0009:580.65.06;cfg=000B:-arch=sm_87" T$=
DEF-A SB-RESET TOOLCHAIN:DIGEST+ SB$ nip TOOLCHAIN:DIGEST-SIZE T=
128 constant COPY-CAP
create COPY-A COPY-CAP allot
create COPY-B COPY-CAP allot
variable COPY-A-U
variable COPY-B-U
DEF-A COPY-A COPY-CAP TOOLCHAIN:DIGEST-COPY COPY-A-U !
DEF-CFG COPY-B COPY-CAP TOOLCHAIN:DIGEST-COPY COPY-B-U !
COPY-A COPY-A-U @ COPY-B COPY-B-U @ STR= TFALSE \ distinct facts -> distinct digests

\ Copies are caller-owned. Mutating one must not mutate the interned owner row.
DEF-A COPY-A COPY-CAP TOOLCHAIN:VERSION-COPY COPY-A-U !
$78 COPY-A c!
DEF-A SB-RESET TOOLCHAIN:VERSION+ SB$ s" 12.6.85" T$=

: COMPOSE$ ( CAD-KIND:toolchain-id -- ptr u8 n ) {: id:CAD-KIND:toolchain-id :}
   SB-RESET
   s" ver=" SB-APPEND id TOOLCHAIN:VERSION+
   s" |cfg=" SB-APPEND id TOOLCHAIN:CONFIG+
   SB$ ;

DEF-A COMPOSE$ s" ver=12.6.85|cfg=-arch=sm_87" T$=

\ ---- regression: the canonical form is injective -----------------------------------
\ Distinct facts, one rendering, one identity - the defect this pair exists to pin.
TOOLCHAIN:RESET
DEF-INJ-A DEF-INJ-B TOOLCHAIN:ID= TFALSE                          \ distinct identities...
DEF-INJ-A COPY-A COPY-CAP TOOLCHAIN:CANONICAL-COPY COPY-A-U !
DEF-INJ-B COPY-B COPY-CAP TOOLCHAIN:CANONICAL-COPY COPY-B-U !
COPY-A COPY-A-U @ COPY-B COPY-B-U @ STR= TFALSE         \ ...distinct canonical forms...
DEF-INJ-A COPY-A COPY-CAP TOOLCHAIN:DIGEST-COPY COPY-A-U !
DEF-INJ-B COPY-B COPY-CAP TOOLCHAIN:DIGEST-COPY COPY-B-U !
COPY-A COPY-A-U @ COPY-B COPY-B-U @ STR= TFALSE         \ ...distinct digests
TOOLCHAIN:IDS 2 T=                                                \ ...and two rows, not a collapse

\ each identity reports the facts it was actually defined with
DEF-INJ-A SB-RESET TOOLCHAIN:DRIVER-VERSION+ SB$ s" 2;cfg=x" T$=
DEF-INJ-A SB-RESET TOOLCHAIN:CONFIG+         SB$ s" y"       T$=
DEF-INJ-B SB-RESET TOOLCHAIN:DRIVER-VERSION+ SB$ s" 2"       T$=
DEF-INJ-B SB-RESET TOOLCHAIN:CONFIG+         SB$ s" x;cfg=y" T$=

\ the delimiter lands inside a framed field, so it cannot forge a field boundary
DEF-INJ-A SB-RESET TOOLCHAIN:CANONICAL+ SB$
   s" cc=0005:ptxas;ver=0001:1;drv=0004:cuda;drvver=0007:2;cfg=x;cfg=0001:y" T$=
DEF-INJ-B SB-RESET TOOLCHAIN:CANONICAL+ SB$
   s" cc=0005:ptxas;ver=0001:1;drv=0004:cuda;drvver=0001:2;cfg=0007:x;cfg=y" T$=

\ ---- canonical round-trip preserves the identity ---------------------------------
TOOLCHAIN:RESET
DEF-A drop
DEF-CFG drop
DEF-A SB-RESET TOOLCHAIN:DIGEST+ SB$ TOOLCHAIN:LOOKUP DEF-A TOOLCHAIN:ID= TTRUE
DEF-CFG SB-RESET TOOLCHAIN:DIGEST+ SB$ TOOLCHAIN:LOOKUP DEF-CFG TOOLCHAIN:ID= TTRUE
DEF-A SB-RESET TOOLCHAIN:DIGEST+ SB$ TOOLCHAIN:KNOWN? TTRUE
s" 0000000000000000" TOOLCHAIN:KNOWN? TFALSE           \ well-formed, and not interned

\ ---- the audited PTXTC discovery adapter -----------------------------------------
\ ADOPT must land on exactly the identity the typed constructor would have built.
ADOPT-A DEF-A TOOLCHAIN:ID= TTRUE
TOOLCHAIN:IDS 2 T=

\ ---- regression: discovery is one atomic typed value -----------------------------
: DISC-BAD-DRV ( -- TOOLCHAIN:disc )
   PTXAS-PATH$ s" 12.6.85" s" rocm" s" 580.65.06" s" -arch=sm_87"
   TOOLCHAIN-DISC:MAKE ;

: DISC-NO-CFG ( -- TOOLCHAIN:disc )
   PTXAS-PATH$ s" 12.6.85" s" cuda" s" 580.65.06" s" "
   TOOLCHAIN-DISC:MAKE ;

: TN-ADOPT-BAD-DRV ( -- )  DISC-BAD-DRV TOOLCHAIN:ADOPT drop ;
: TN-ADOPT-NO-CFG  ( -- )  DISC-NO-CFG  TOOLCHAIN:ADOPT drop ;

' TN-ADOPT-BAD-DRV TOOLCHAIN:E-KIND TTHROWS
' TN-ADOPT-NO-CFG  TOOLCHAIN:E-FACT TTHROWS
ADOPT-A DEF-A TOOLCHAIN:ID= TTRUE                 \ a failed round leaves no state to contaminate this one

\ ---- audited low-level seam -------------------------------------------------------
TOOLCHAIN:RESET
DEF-A drop
DEF-A TOOLCHAIN:TEST-REFINEMENTS? TTRUE
\ a digest hit is verified against the canonical form it claims to name, so a
\ collision could never quietly hand back the wrong toolchain
DEF-A COPY-A COPY-CAP TOOLCHAIN:CANONICAL-COPY COPY-A-U !
DEF-A COPY-A COPY-A-U @ TOOLCHAIN:TEST-HIT-AGREES? TTRUE
DEF-A s" cc=0005:ptxas;ver=0003:9.9;drv=0004:cuda;drvver=0001:1;cfg=0001:x"
   TOOLCHAIN:TEST-HIT-AGREES? TFALSE

\ ---- regression: RESET retires every id it issued -------------------------------
\ An id used to be a bare row index, so after RESET the next descriptor to take that
\ row answered to it - the old id silently reported the new toolchain's facts. An id
\ now carries the generation it was issued under, and RESET advances it.
TOOLCHAIN:TEST-STALE-RC TOOLCHAIN:E-ID T=
TOOLCHAIN:IDS 1 T=                                \ the new generation has exactly one identity...
DEF-Z SB-RESET TOOLCHAIN:VERSION+ SB$ s" 99.9" T$= \ ...and row 0 is DEF-Z, not the retired DEF-A

\ the row is reused, the id is not: same facts, same row, a new generation
TOOLCHAIN:RESET
DEF-A TOOLCHAIN:RESET DEF-A TOOLCHAIN:ID= TFALSE            \ same row and facts, but a retired generation

\ a forged id is rejected before its row is indexed
TOOLCHAIN:TEST-FORGE-GEN-RC TOOLCHAIN:E-ID T=
TOOLCHAIN:TEST-FORGE-ROW-RC TOOLCHAIN:E-ID T=
TOOLCHAIN:TEST-ROW-NEG-RC   TOOLCHAIN:E-ID T=
TOOLCHAIN:TEST-ROW-HIGH-RC  TOOLCHAIN:E-ID T=

\ the generation does not wrap: it is exhausted, not reissued under live ids
TOOLCHAIN:TEST-EPOCH-RC TOOLCHAIN:E-EPOCH T=
TOOLCHAIN:RESET

\ ---- fail closed: incomplete discovery facts ---------------------------------------
: TN-EMPTY-VER ( -- )
   TOOLCHAIN:PTXAS s" " TOOLCHAIN:CUDA s" 580.65.06" s" -arch=sm_87"
   TOOLCHAIN:DEFINE drop ;

' TN-EMPTY-VER TOOLCHAIN:E-FACT TTHROWS

\ ---- fail closed: an unaudited compiler or driver is not a toolchain ----------------
: TN-ADOPT-BAD-CC ( -- )
   s" /usr/bin/clang" s" 12.6.85" s" cuda" s" 580.65.06" s" -arch=sm_87"
   TOOLCHAIN-DISC:MAKE TOOLCHAIN:ADOPT drop ;

' TN-ADOPT-BAD-CC TOOLCHAIN:E-KIND TTHROWS

\ ---- fail closed: malformed / unknown digests ----------------------------------------
\ KNOWN? answers about a digest. A malformed string is not an unknown toolchain,
\ so it throws rather than reporting a legitimate miss.
: TN-DIG-SHORT ( -- )  s" ff" TOOLCHAIN:LOOKUP drop ;
: TN-DIG-LONG ( -- )   s" 00000000000000000" TOOLCHAIN:LOOKUP drop ;
: TN-DIG-CHAR ( -- )   s" 00000000000000zz" TOOLCHAIN:LOOKUP drop ;
: TN-DIG-MISS ( -- )   s" 0000000000000000" TOOLCHAIN:LOOKUP drop ;
: TN-KNOWN-SHORT ( -- ) s" ff" TOOLCHAIN:KNOWN? drop ;
: TN-KNOWN-LONG ( -- )  s" 00000000000000000" TOOLCHAIN:KNOWN? drop ;
: TN-KNOWN-CHAR ( -- )  s" 00000000000000zz" TOOLCHAIN:KNOWN? drop ;
: TN-KNOWN-EMPTY ( -- ) s" " TOOLCHAIN:KNOWN? drop ;

' TN-DIG-SHORT   TOOLCHAIN:E-DIGEST TTHROWS
' TN-DIG-LONG    TOOLCHAIN:E-DIGEST TTHROWS
' TN-DIG-CHAR    TOOLCHAIN:E-DIGEST TTHROWS
' TN-DIG-MISS    TOOLCHAIN:E-MISS   TTHROWS
' TN-KNOWN-SHORT TOOLCHAIN:E-DIGEST TTHROWS
' TN-KNOWN-LONG  TOOLCHAIN:E-DIGEST TTHROWS
' TN-KNOWN-CHAR  TOOLCHAIN:E-DIGEST TTHROWS
' TN-KNOWN-EMPTY TOOLCHAIN:E-DIGEST TTHROWS

\ ---- fail closed: a digest hit that does not agree with the form it names -------------
TOOLCHAIN:TEST-COLLIDE-RC TOOLCHAIN:E-COLLIDE T=

\ ---- the derived layout: a maximal descriptor is a valid one ---------------------------
\ Every variable field at FACT-CAP must still render through SB and still intern, so a
\ valid bounded fact can never leak a foreign E-STR-CAPACITY instead of a toolchain error.
TOOLCHAIN:RESET
DEF-MAX SB-RESET TOOLCHAIN:VERSION+ SB$ nip TOOLCHAIN:FACT-CAPACITY T=
DEF-MAX SB-RESET TOOLCHAIN:CONFIG+ SB$ nip TOOLCHAIN:FACT-CAPACITY T=
DEF-MAX SB-RESET TOOLCHAIN:CANONICAL+ SB$ nip TOOLCHAIN:CANONICAL-CAPACITY <= TTRUE
TOOLCHAIN:IDS 1 T=

\ and the arena holds TC-CAP of them, which is what makes exhaustion unreachable from
\ DEFINE / ADOPT
FILL-MAX-TABLE
TOOLCHAIN:IDS TOOLCHAIN:ID-CAPACITY T=

\ ---- fail closed: capacity ----------------------------------------------------------------
create CAPBUF $4 allot
: CAP-VER$ ( n -- ptr u8 n ) {: ix:n :}     \ "vNN": a distinct version per row
   $76 CAPBUF c!
   ix $A /   $30 + CAPBUF 1+ c!
   ix $A mod $30 + CAPBUF 2 + c!
   CAPBUF 3 ;

: TN-TAB-FULL ( -- )
   TOOLCHAIN:RESET
   TOOLCHAIN:ID-CAPACITY 1+ 0 ?do
      TOOLCHAIN:PTXAS i CAP-VER$ TOOLCHAIN:CUDA s" 1" s" -arch=sm_87" TOOLCHAIN:DEFINE drop
   loop ;

create BIGBUF $200 allot
: BIG-FILL ( -- )  $200 0 ?do  $78 BIGBUF i + c!  loop ;
: TN-FACT-BIG ( -- )
   BIG-FILL
   TOOLCHAIN:PTXAS BIGBUF TOOLCHAIN:FACT-CAPACITY 1+
   TOOLCHAIN:CUDA s" 1" s" -arch=sm_87" TOOLCHAIN:DEFINE drop ;
: TN-COPY-SMALL ( -- )  TOOLCHAIN:RESET DEF-A COPY-A 1 TOOLCHAIN:VERSION-COPY drop ;

' TN-TAB-FULL   TOOLCHAIN:E-CAP TTHROWS
' TN-FACT-BIG   TOOLCHAIN:E-CAP TTHROWS
' TN-COPY-SMALL TOOLCHAIN:E-CAP TTHROWS
TOOLCHAIN:TEST-ARENA-RC TOOLCHAIN:E-CAP T=

TOOLCHAIN:RESET

\ ---- the checked boundary: identities do not swap and raw cells do not convert ----------
s" TC-POS-VER ( CAD-KIND:toolchain-id -- ) TOOLCHAIN:VERSION+" YES
s" TC-POS-CFG ( CAD-KIND:toolchain-id -- ) TOOLCHAIN:CONFIG+" YES
s" TC-POS-KINDS ( -- TOOLCHAIN:compiler TOOLCHAIN:driver ) TOOLCHAIN:PTXAS TOOLCHAIN:CUDA" YES
s" TC-POS-DISC ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- TOOLCHAIN:disc ) TOOLCHAIN-DISC:MAKE" YES
s" TC-POS-ADOPT ( TOOLCHAIN:disc -- CAD-KIND:toolchain-id ) TOOLCHAIN:ADOPT" YES
s" TC-POS-DEFINE ( TOOLCHAIN:compiler ptr u8 n TOOLCHAIN:driver ptr u8 n ptr u8 n -- CAD-KIND:toolchain-id ) TOOLCHAIN:DEFINE" YES

\ canonical round-trip preserves the family, and the checker proves it
s" TC-POS-RT ( CAD-KIND:toolchain-id ptr u8 n -- CAD-KIND:toolchain-id ) {: id:CAD-KIND:toolchain-id dst:ptr cap:n :} id dst cap TOOLCHAIN:DIGEST-COPY {: u:n :} dst u TOOLCHAIN:LOOKUP" YES

\ a target is not a toolchain, in either direction
s" TC-NEG-TARGET-IN ( CAD-KIND:target-id -- ) TOOLCHAIN:VERSION+" NO
s" TC-NEG-TARGET-OUT ( CAD-KIND:toolchain-id -- CAD-KIND:target-id )" NO
s" TC-NEG-TARGET-DEF ( TOOLCHAIN:compiler ptr u8 n TOOLCHAIN:driver ptr u8 n ptr u8 n -- CAD-KIND:target-id ) TOOLCHAIN:DEFINE" NO
s" TC-NEG-TARGET-ADOPT ( TOOLCHAIN:disc -- CAD-KIND:target-id ) TOOLCHAIN:ADOPT" NO

\ a compiler is not a driver
s" TC-NEG-CC-AS-DRV ( TOOLCHAIN:driver -- ptr u8 n ) TOOLCHAIN:COMPILER-NAME$" NO
s" TC-NEG-DRV-AS-CC ( TOOLCHAIN:compiler -- ptr u8 n ) TOOLCHAIN:DRIVER-NAME$" NO
s" TC-NEG-DEFINE-SWAP ( TOOLCHAIN:driver ptr u8 n TOOLCHAIN:compiler ptr u8 n ptr u8 n -- CAD-KIND:toolchain-id ) TOOLCHAIN:DEFINE" NO

\ no public raw conversions: an `n` is not an identity and an identity is not an `n`
s" TC-NEG-RAW-IN ( n -- ) TOOLCHAIN:VERSION+" NO
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
