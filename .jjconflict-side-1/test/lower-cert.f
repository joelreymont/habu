\ lower-cert.f — canonical lowering-certificate producer regressions.

require test/checker-assert.f
require test/layout-valid-guard-base.f

package LOWER-CERT-TEST

variable FAILS
variable CASES
variable DEPTH0
variable CERT-N
PTR-VARIABLE SRC-A
variable SRC-U

: FAIL ( -- )
   FAILS @ 1 + FAILS ! ;

: EQ ( n n -- ) {: got:n want:n :}
   CASES @ 1 + CASES !
   got want <> if
      FAIL
      s" expected " type want . s" got " type got . cr
   then ;

: YES ( bool -- ) {: value:bool :}
   CASES @ 1 + CASES !
   value 0= if FAIL then ;

: SNAPSHOT ( -- )
   LOWER-CERT:CELL-COUNT CERT-N ! ;

: BLOB@ ( n -- n )
   LOWER-CERT:CELL@ ;

: HASH ( ptr u8 n -- n ) {: a:ptr u:n :}
   LOWER-CERT:FNV-OFFSET
   0 begin dup u < while
      a over + c@ rot xor LOWER-CERT:FNV-PRIME * swap
      1 +
   repeat drop ;

: HEADER ( n n n n -- ) {: total:n needs:n facts:n fetches:n :}
   CERT-N @ total EQ
   LOWER-CERT:MAGIC-CELL BLOB@ LOWER-CERT:MAGIC EQ
   LOWER-CERT:VERSION-CELL BLOB@ LOWER-CERT:VERSION EQ
   LOWER-CERT:TOTAL-BYTES-CELL BLOB@ total cells EQ
   LOWER-CERT:NEEDS-CELL BLOB@ needs EQ
   LOWER-CERT:WF-COUNT-CELL BLOB@ facts EQ
   LOWER-CERT:BIND-COUNT-CELL BLOB@ 0 EQ
   LOWER-CERT:FETCH-COUNT-CELL BLOB@ fetches EQ
   LOWER-CERT:BODY-LEN-CELL BLOB@ SRC-U @ EQ
   LOWER-CERT:BODY-HASH-CELL BLOB@ SRC-A @ SRC-U @ HASH EQ ;

: SOURCE! ( ptr u8 n -- )
   SRC-U ! SRC-A ! ;

ENUM color red green ;ENUM

PRODUCT scalar 0
   FIELD value n
;PRODUCT

ENUM-DECL:ED-RUN lcnamed 0
   VARIANT empty ;VARIANT
   VARIANT shade FIELD value color ;VARIANT
;ENUM

\ ENUM front-end twin of LAYOUT-VALID-GUARD:lvg-outer (dot habu-migrate-linearity
\ -layout). Same nested shape — an outer sum whose first variant carries the
\ shared inner enum (LAYOUT-VALID-GUARD:lvg-inner) and whose second variant is a
\ scalar — but declared through the unified ENUM front end, so the payload lives
\ in TYPE-FIELD rows with SV.SCH-COUNT=0 instead of positional SUMV slots. A fetch
\ of it must lower to the SAME certificate descriptor as the legacy twin.
ENUM-DECL:ED-RUN fe-outer 0
   VARIANT left FIELD v LAYOUT-VALID-GUARD:lvg-inner ;VARIANT
   VARIANT right FIELD w n ;VARIANT
;ENUM

\ A fetch descriptor for a single-fetch, single-width-fact subject occupies one
\ fetch-index row {key, abs-offset, len} at cells 14..16; cell 15 is the absolute
\ descriptor offset and FETCH-DATA-CELLS-CELL is its length. DESC-SNAP records the
\ legacy descriptor; DESC-EQ pins the front-end descriptor against it cell by cell.
64 constant DESC-CAP
create LEG-DESC DESC-CAP cells allot
variable LEG-DN
variable DPOS
: DESC-BASE ( -- n ) 15 BLOB@ ;
: DESC-LEN  ( -- n ) LOWER-CERT:FETCH-DATA-CELLS-CELL BLOB@ ;
: DESC-SNAP ( -- )
   DESC-LEN LEG-DN !
   DESC-LEN DESC-CAP <= YES                       \ loud: an inflated descriptor must fail here, not overrun LEG-DESC
   DESC-BASE DESC-LEN DESC-CAP min {: base:n lim:n :}  \ clamp so the copy cannot exceed LEG-DESC after a recorded failure
   0 DPOS ! begin DPOS @ lim < while
      base DPOS @ + BLOB@  DPOS @ cells LEG-DESC + !
      DPOS @ 1 + DPOS !
   repeat ;
: DESC-EQ ( -- )
   DESC-LEN LEG-DN @ EQ                          \ length parity — flags a fail-open short descriptor
   DESC-BASE {: base:n :}
   DESC-LEN LEG-DN @ min {: lim:n :}             \ compare only the safe overlap; the length EQ owns size mismatch
   0 DPOS ! begin DPOS @ lim < while
      base DPOS @ + BLOB@  DPOS @ cells LEG-DESC + @ EQ
      DPOS @ 1 + DPOS !
   repeat ;

: CLEARED-CERT ( -- )
   SNAPSHOT
   CERT-N @ 10 EQ
   LOWER-CERT:MAGIC-CELL BLOB@ LOWER-CERT:MAGIC EQ
   LOWER-CERT:VERSION-CELL BLOB@ LOWER-CERT:VERSION EQ
   LOWER-CERT:TOTAL-BYTES-CELL BLOB@ 10 cells EQ
   LOWER-CERT:NEEDS-CELL BLOB@ 0 EQ
   LOWER-CERT:WF-COUNT-CELL BLOB@ 0 EQ
   LOWER-CERT:BIND-COUNT-CELL BLOB@ 0 EQ
   LOWER-CERT:FETCH-COUNT-CELL BLOB@ 0 EQ
   LOWER-CERT:FETCH-DATA-CELLS-CELL BLOB@ 0 EQ
   LOWER-CERT:BODY-LEN-CELL BLOB@ 0 > YES
   LOWER-CERT:BODY-HASH-CELL BLOB@ 0 <> YES ;

: REPORT ( -- )
   FAILS @ 0= if s" ok" type cr exit then
   FAILS @ . s" lower-cert: failures" 1 die ;

\ Whitebox multi-error boundaries: MULTI-ERR-BEGIN/END are engine-internal
\ (DNAME-INT after the seal-time marking pass), so the forced-success
\ publication below reaches them through TRUSTED: wrappers, matching the
\ type-decl suite convention (dot habu-hb-crash-bare-c5be6634).
TRUSTED: LCT-MULTI-ERR-BEGIN ( -- ) MULTI-ERR-BEGIN ;
TRUSTED: LCT-MULTI-ERR-END ( -- n ) MULTI-ERR-END ;

s" LC-ENUM ( ptr color -- color ) @" 2dup SOURCE! CHECK! -1 EQ
SNAPSHOT
21 1 1 1 HEADER
LOWER-CERT:FETCH-DATA-CELLS-CELL BLOB@ 4 EQ
10 BLOB@ 31 EQ
11 BLOB@ 0 EQ
12 BLOB@ 1 EQ
13 BLOB@ LOWER-CERT:FETCH-FLAG EQ
14 BLOB@ 31 EQ
15 BLOB@ 17 EQ
16 BLOB@ 4 EQ
17 BLOB@ 1 EQ
18 BLOB@ 0 EQ
19 BLOB@ 2 EQ
20 BLOB@ 0 EQ

\ A unified named-field enum walks the committed field schema, including the
\ nested enum guard, while its fieldless sibling contributes no payload task.
s" LC-NAMED ( ptr lcnamed -- lcnamed ) @" 2dup SOURCE! CHECK! -1 EQ
SNAPSHOT
CERT-N @ 10 > YES
LOWER-CERT:NEEDS-CELL BLOB@ 1 EQ
LOWER-CERT:WF-COUNT-CELL BLOB@ 1 EQ
LOWER-CERT:FETCH-COUNT-CELL BLOB@ 1 EQ
LOWER-CERT:FETCH-DATA-CELLS-CELL BLOB@ 10 EQ

\ A forced-success multi-error publication must not freeze the prior enum
\ fetch descriptor. The hook replaces it with a canonical empty certificate.
LCT-MULTI-ERR-BEGIN
s" : LC-MULTI-BAD ( n -- n ) drop ;" evaluate
LCT-MULTI-ERR-END 1 EQ
CLEARED-CERT
s" LC-NAMED-BAD ( ptr lcnamed -- color ) @" 2dup SOURCE! CHECK! 0 EQ

s" LC-SCALAR ( ptr scalar -- scalar ) @" 2dup SOURCE! CHECK! -1 EQ
SNAPSHOT
18 0 1 1 HEADER
LOWER-CERT:FETCH-DATA-CELLS-CELL BLOB@ 1 EQ
13 BLOB@ LOWER-CERT:FETCH-FLAG EQ
15 BLOB@ 17 EQ
16 BLOB@ 1 EQ
17 BLOB@ 0 EQ

depth DEPTH0 !
s" LC-NESTED ( ptr LAYOUT-VALID-GUARD:lvg-outer -- LAYOUT-VALID-GUARD:lvg-outer ) @"
   2dup SOURCE! CHECK! -1 EQ
depth DEPTH0 @ EQ
SNAPSHOT
27 1 1 1 HEADER
LOWER-CERT:FETCH-DATA-CELLS-CELL BLOB@ 10 EQ
10 BLOB@ SRC-U @ 1 - EQ
14 BLOB@ SRC-U @ 1 - EQ
15 BLOB@ 17 EQ
16 BLOB@ 10 EQ
17 BLOB@ 2 EQ
18 BLOB@ 1 EQ
19 BLOB@ 2 EQ
20 BLOB@ 0 EQ
21 BLOB@ 0 EQ
22 BLOB@ 2 EQ
23 BLOB@ 1 EQ
24 BLOB@ 1 EQ
25 BLOB@ 0 EQ
26 BLOB@ 2 EQ

\ Guard-row parity: the ENUM front-end twin's fetch lowers to a byte-identical
\ descriptor — same 2 checks (outer tag domain 2, inner tag domain 2) and the
\ same guard triple {offset 1, tag 0, limit 2} gating the inner check on the
\ `left` variant. Before the fix the front-end variant queued no payload walk, so
\ the inner-tag CHECK and its guard were absent (a shorter, fail-open descriptor).
s" LCP-LEG ( ptr LAYOUT-VALID-GUARD:lvg-outer -- LAYOUT-VALID-GUARD:lvg-outer ) @"
   2dup SOURCE! CHECK! -1 EQ
DESC-SNAP
LEG-DN @ 10 EQ
s" LCP-FE ( ptr fe-outer -- fe-outer ) @" 2dup SOURCE! CHECK! -1 EQ
DESC-EQ

s" LC-STORE ( color ptr color -- ) !" 2dup SOURCE! CHECK! -1 EQ
SNAPSHOT
14 0 1 0 HEADER
LOWER-CERT:FETCH-DATA-CELLS-CELL BLOB@ 0 EQ
13 BLOB@ LOWER-CERT:STORE-FLAG EQ

s" LC-EMPTY ( n -- n ) 1 +" 2dup SOURCE! CHECK! -1 EQ
SNAPSHOT
10 0 0 0 HEADER
LOWER-CERT:FETCH-DATA-CELLS-CELL BLOB@ 0 EQ

REPORT

;package
