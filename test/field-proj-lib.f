\ field-proj-lib.f - the field-projection fixture test/field-proj-suite.f and
\ test/field-proj-boundary-child.f share: the trusted arming forwarder, the
\ family and field lookups, and accessors generated inside the armed window for
\ a cell field, a byte-offset field and a generic field, with the words that
\ store a bundle and read each field back by value. It asserts nothing; each
\ file stores and reads what it claims.
\
\ The families are declared outside the package: the suite's candidates name
\ them bare, and FAM-ID finds them as top-level families.

PRODUCT fprec 0
  FIELD a n
  FIELD b n
;PRODUCT

\ generic-substituted field: `fpg<a>` field v:a projects as `ptr a`, and at a
\ concrete instantiation reads the stored value back.
PRODUCT fpg 1
  FIELD v a
;PRODUCT

package FIELD-PROJ-LIB
using TFAM
public

\ --- sealed friend boundary (dot habu-hb-crash-bare-c5be6634 idiom): the
\ field-projection window is armed only by the generative crossing, so its arming
\ word is a pre-hook internal that the seal marks non-executable, and a
\ trusted-only primitive (src/core/checker.f FIELD-PROJ!). This file reaches it
\ through the TRUSTED forwarder FP-ARM, which test/field-proj-suite.f and
\ test/field-proj-boundary-child.f call. FAM-ID calls the family lookup
\ TFAM-FIND-IN itself, checked against its recorded row: the unsealed engine
\ binds it, and test/field-proj-boundary-prepare.f declares it in the boundary
\ child's window.
TRUSTED: FP-ARM ( ptr u8 n n n -- ) FIELD-PROJ! ;
: FP-CLEAR ( -- ) FIELD-PROJ-CLEAR ;

\ field-id lookup helper (TYPE-FIELD:FIND is a public sealed-package API).
: FLD-ID ( n ptr u8 n -- n ) {: fam:n na:ptr nu:n :}   \ fam name$ -> committed field id
   fam TYPE-FIELD:NO-VARIANT na nu TYPE-FIELD:FIND 0= if
      s" field-proj-lib: field not found" 76 die then ;
: FAM-ID ( ptr u8 n -- n ) {: na:ptr nu:n :}   \ top-level family name -> id
   s" " na nu TFAM-FIND-IN 0= if s" field-proj-lib: family not found" 76 die then ;

variable FID-A
variable FID-V

private

\ `field-project` is the engine's word (src/core/structure-make.f); the checker
\ admits it only inside the armed window.

2 LAYOUT-BUFFER FP-BUF fprec
variable FPREC-FAM   variable FID-B
s" fprec" FAM-ID FPREC-FAM !
FPREC-FAM @ s" a" FLD-ID FID-A !
FPREC-FAM @ s" b" FLD-ID FID-B !

\ cell field at offset 0
s" FPX-A" FID-A @ 0 FP-ARM
: FPX-A ( ptr fprec -- ptr n ) 0 field-project ;
\ byte-offset field at offset CELL
s" FPX-B" FID-B @ CELL FP-ARM
: FPX-B ( ptr fprec -- ptr n ) CELL field-project ;

1 LAYOUT-BUFFER FPG-BUF fpg<n>
variable FPG-FAM
s" fpg" FAM-ID FPG-FAM !
FPG-FAM @ s" v" FLD-ID FID-V !
s" FPX-V" FID-V @ 0 FP-ARM
: FPX-V ( ptr fpg<a> -- ptr a ) 0 field-project ;

public

\ MAKE + whole-bundle store happen in compiled words (a layout value cannot sit
\ on the interpret stack); the projected reads are scalars, so they surface fine.
: FP-STORE ( n n n -- ) {: av:n bv:n idx:n :} av bv FPREC:MAKE idx FP-BUF ! ;
: FP-GETA ( n -- n ) FP-BUF FPX-A @ ;
: FP-GETB ( n -- n ) FP-BUF FPX-B @ ;
: FPG-STORE ( n n -- ) {: vv:n idx:n :} vv FPG:MAKE idx FPG-BUF ! ;
: FPG-GET ( n -- n ) FPG-BUF FPX-V @ ;

;using
;package
