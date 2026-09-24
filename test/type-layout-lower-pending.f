\ type-layout-lower-pending.f — TFAM 12 width-aware lowering suite
\ (habu-tfam-12-layout, docs/type-families.md §17-18). Run BY THE ENGINE over
\ stdin, standalone or through the native registry:
\     bin/hb < test/type-layout-lower-pending.f
\ Generated constructors seed bundles which cross the stack, return stack,
\ locals and typed storage before their values are checked.

using TFAM

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

\ ---------------------------------------------------------------------------
\ layout families under test: width 2 (1 payload slot + tag), width 4
\ (3 payload slots + tag).
\ ---------------------------------------------------------------------------
SUMTYPE tlp-res 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
SUMTYPE tlp-mix 2
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE
\ Constructor-produced bundles cross typed storage in the execution rows.
1 LAYOUT-BUFFER TLP-MEM2-BUF tlp-res<n,n>
: TLP-MEM2-P ( -- ptr tlp-res<n,n> ) 0 TLP-MEM2-BUF ;
: TLP-STORE2 ( tlp-res<n,n> -- ) TLP-MEM2-P ! ;
: TLP-FETCH2 ( -- tlp-res<n,n> ) TLP-MEM2-P @ ;
1 LAYOUT-BUFFER TLP-MEM4-BUF tlp-mix<n,n>
: TLP-MEM4-P ( -- ptr tlp-mix<n,n> ) 0 TLP-MEM4-BUF ;
: TLP-STORE4 ( tlp-mix<n,n> -- ) TLP-MEM4-P ! ;
: TLP-FETCH4 ( -- tlp-mix<n,n> ) TLP-MEM4-P @ ;

\ ---------------------------------------------------------------------------
\ execution rows: whole-bundle transports at RUNTIME. The seeds are the REAL
\ generated constructors (item 8/11: `tlp-res` derives package TLP--RES,
\ `tlp-mix` derives TLP--MIX — tail hyphens escape as `--`), so the physical
\ cells (payload, zero pads, tag) come from checked constructor bodies, not
\ trusted raw pushes. TLP-MK2 = 7 TLP--RES:ERR -> (7, tag 1); TLP-MK4 =
\ 91 92 93 TLP--MIX:BIG -> (91, 92, 93, tag 1). Only the UNPACKERS remain a
\ tested TRUSTED boundary: surfacing bundle cells for value asserts needs a
\ destructor, which is item 9's MATCH (dot habu-retire-tlp-mk2-ac7760d2).
\ Both raw unpackers retire with habu-retire-tlp-mk2-ac7760d2 when checked
\ MATCH/destructuring can expose their payload cells.
\ ---------------------------------------------------------------------------
: TLP-MK2 ( -- tlp-res<n,n> ) 7 TLP--RES:ERR ;
: TLP-MK2B ( -- tlp-res<n,n> ) 8 TLP--RES:OK ;
\ Tested boundary (TRUSTED): the matching 2-cell unpack (payload, tag).
TRUSTED: TLP-UN2 ( tlp-res<n,n> -- n n ) ;
: TLP-MK4 ( -- tlp-mix<n,n> ) 91 92 93 TLP--MIX:BIG ;
\ Tested boundary (TRUSTED): the matching 4-cell unpack.
TRUSTED: TLP-UN4 ( tlp-mix<n,n> -- n n n n ) ;

\ Executed memory lowering: constructor-produced bundles cross typed addresses
\ and return with payload, padding, and tag order intact.
: TLPX-STORE2 ( -- ) TLP-MK2 TLP-STORE2 ;
: TLPX-FETCH2 ( -- n n ) TLP-FETCH2 TLP-UN2 ;
TLPX-STORE2
TLPX-FETCH2 1 T= 7 T=
: TLPX-STORE4 ( -- ) TLP-MK4 TLP-STORE4 ;
: TLPX-FETCH4 ( -- n n n n ) TLP-FETCH4 TLP-UN4 ;
TLPX-STORE4
TLPX-FETCH4 1 T= 93 T= 92 T= 91 T=

: TLPX-DUP ( -- n n n n ) TLP-MK2 dup {: a b :} a TLP-UN2 b TLP-UN2 ;
TLPX-DUP 1 T= 7 T= 1 T= 7 T=
: TLPX-DROP ( -- n ) 5 TLP-MK2 drop ;
TLPX-DROP 5 T=
: TLPX-SWAP ( -- n n n ) TLP-MK2 5 swap {: s:n r :} s r TLP-UN2 ;
TLPX-SWAP 1 T= 7 T= 5 T=
: TLPX-OVER ( -- n n n n n ) TLP-MK2 5 over {: r1 s:n r2 :} r1 TLP-UN2 s r2 TLP-UN2 ;
TLPX-OVER 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-NIP ( -- n n ) 5 TLP-MK2 nip TLP-UN2 ;
TLPX-NIP 1 T= 7 T=
: TLPX-TUCK ( -- n n n n n ) 5 TLP-MK2 tuck {: r1 s:n r2 :} r1 TLP-UN2 s r2 TLP-UN2 ;
TLPX-TUCK 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-ROT ( -- n n n n ) TLP-MK2 5 6 rot {: s1:n s2:n r :} s1 s2 r TLP-UN2 ;
TLPX-ROT 1 T= 7 T= 6 T= 5 T=
: TLPX-MROT ( -- n n n n ) 5 6 TLP-MK2 -rot {: r s1:n s2:n :} r TLP-UN2 s1 s2 ;
TLPX-MROT 6 T= 5 T= 1 T= 7 T=
: TLPX-2DUP ( -- n n n n n n ) TLP-MK2 5 2dup {: r1 s1:n r2 s2:n :} r1 TLP-UN2 s1 r2 TLP-UN2 s2 ;
TLPX-2DUP 5 T= 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-2DROP ( -- n ) 6 TLP-MK2 5 2drop ;
TLPX-2DROP 6 T=
: TLPX-2SWAP ( -- n n n n n n n n ) TLP-MK2 5 TLP-MK4 6 2swap {: m s2:n r s1:n :} m TLP-UN4 s2 r TLP-UN2 s1 ;
TLPX-2SWAP 5 T= 1 T= 7 T= 6 T= 1 T= 93 T= 92 T= 91 T=
: TLPX-2OVER ( -- n n n n n n n n n n n ) TLP-MK2 5 TLP-MK4 6 2over {: r1 s1:n m1 s2:n r2 s3:n :} r1 TLP-UN2 s1 m1 TLP-UN4 s2 r2 TLP-UN2 s3 ;
TLPX-2OVER 5 T= 1 T= 7 T= 6 T= 1 T= 93 T= 92 T= 91 T= 5 T= 1 T= 7 T=
: TLPX-TOR ( -- n n n ) TLP-MK2 >r 5 r> TLP-UN2 ;
TLPX-TOR 1 T= 7 T= 5 T=
: TLPX-RAT ( -- n n n n ) TLP-MK2 >r r@ {: c :} r> TLP-UN2 c TLP-UN2 ;
TLPX-RAT 1 T= 7 T= 1 T= 7 T=
: TLPX-2TOR ( -- n n n ) TLP-MK2 5 2>r 2r> {: r s:n :} r TLP-UN2 s ;
TLPX-2TOR 5 T= 1 T= 7 T=
: TLPX-2RAT ( -- n n n n n n ) TLP-MK2 5 2>r 2r@ {: c cs:n :}
   2r> {: r s:n :} c TLP-UN2 cs r TLP-UN2 s ;
TLPX-2RAT 5 T= 1 T= 7 T= 5 T= 1 T= 7 T=
: TLPX-MIX-DUP ( -- n n n n n n n n ) TLP-MK4 dup {: a b :} a TLP-UN4 b TLP-UN4 ;
TLPX-MIX-DUP 1 T= 93 T= 92 T= 91 T= 1 T= 93 T= 92 T= 91 T=
: TLPX-MIX-SWAP ( -- n n n n n ) TLP-MK4 5 swap {: s:n m :} s m TLP-UN4 ;
TLPX-MIX-SWAP 1 T= 93 T= 92 T= 91 T= 5 T=
: TLPX-LOCAL ( -- n n n n n n n n n ) 5 TLP-MK4 {: y:n z :} z TLP-UN4 y z TLP-UN4 ;
TLPX-LOCAL 1 T= 93 T= 92 T= 91 T= 5 T= 1 T= 93 T= 92 T= 91 T=
\ two distinct wide locals in one carve group pin declaration-order bind replay
\ against operand-position-sorted width evidence.
package TLP-LOCAL-TEST
public
: DUAL ( -- n n n n n n )
   TLP-MK2 TLP-MK4 {: r m :} r TLP-UN2 m TLP-UN4 ;
: DEEP ( -- n n n n n n )
   TLP-MK2 1 2 3 4 {: r a:n b:n c:n d:n :}
   a b c d r TLP-UN2 ;
;package
TLP-LOCAL-TEST:DUAL 1 T= 93 T= 92 T= 91 T= 1 T= 7 T=
TLP-LOCAL-TEST:DEEP 1 T= 7 T= 4 T= 3 T= 2 T= 1 T=

\ a wide local bound at TOP LEVEL and REFERENCED inside both arms of a branch.
: TLPX-REF-BRANCH ( n -- n n ) TLP-MK2 {: a :} 0 > if a TLP-UN2 else a TLP-UN2 then ;
5 TLPX-REF-BRANCH 1 T= 7 T=
-3 TLPX-REF-BRANCH 1 T= 7 T=

\ ---------------------------------------------------------------------------
\ branch-scoped bundle locals (habu-tfam-12-pass): a bundle local BOUND inside
\ a branch arm lowers by bind sequence (checker LOCW-HW + the P2-CARVE-W live
\ replay), so sibling arms may reuse the same frame slot — at different widths.
\ Each subject runs BOTH arms; values prove whole-bundle capture + reference.
\ ---------------------------------------------------------------------------
: TLPX-BRIF ( n -- n n ) 0 > if TLP-MK2 {: r :} r TLP-UN2
   else TLP-MK2B {: r :} r TLP-UN2 then ;
5 TLPX-BRIF 1 T= 7 T=
-3 TLPX-BRIF 0 T= 8 T=
: TLPX-BRCASE ( n -- n n ) case
     1 of TLP-MK2 {: r :} r TLP-UN2 endof
     2 of TLP-MK2B {: r :} r TLP-UN2 endof
     0 0 rot
   endcase ;
1 TLPX-BRCASE 1 T= 7 T=
2 TLPX-BRCASE 0 T= 8 T=
9 TLPX-BRCASE 0 T= 0 T=
\ sibling arms reuse frame slot 0 at width 2 vs width 4.
: TLPX-BRW ( n -- n n n n ) 0 > if TLP-MK2 {: r :} r TLP-UN2 0 0
   else TLP-MK4 {: m :} m TLP-UN4 then ;
5 TLPX-BRW 0 T= 0 T= 1 T= 7 T=
-3 TLPX-BRW 1 T= 93 T= 92 T= 91 T=
\ a mixed scalar+wide group inside a branch arm.
: TLPX-BRMIX ( n -- n n n ) 0 > if TLP-MK2 5 {: r s:n :} s r TLP-UN2 else 6 7 8 then ;
1 TLPX-BRMIX 1 T= 7 T= 5 T=
0 TLPX-BRMIX 8 T= 7 T= 6 T=
\ an OUTER wide local below a branch-scoped wide local: the branch carve's
\ cumulative spans the live width-4 entry, and the outer local survives the join.
: TLPX-BROUTER ( n -- n n n n ) TLP-MK4 {: m :}
   0 > if TLP-MK2 {: r :} r TLP-UN2 drop drop then m TLP-UN4 ;
1 TLPX-BROUTER 1 T= 93 T= 92 T= 91 T=
0 TLPX-BROUTER 1 T= 93 T= 92 T= 91 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-layout-lower-pending: failures" 1 die ;
REPORT

;using
