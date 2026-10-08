\ field-proj-arm.f - FIELD-PROJ!'s owner row, src/core/checker.f
\ `PPRIM: TYPE-DECL FIELD-PROJ!`, types this checked caller; it must load before
\ a window's seal. FAM-ID needs no owner: its family lookup TFAM-FIND-IN is a
\ prefix word a window binds only before its seal as well.
package TYPE-DECL
public
: FP-ARM ( ptr u8 n n n -- ) FIELD-PROJ! ;
;package

package FIELD-PROJ-ARM
public
: FAM-ID ( ptr u8 n -- n )   \ top-level family name -> id
   {: na:ptr nu:n :}
   s" " na nu TFAM:TFAM-FIND-IN 0= if
      s" field-proj-arm: family not found" 76 die then ;
;package
