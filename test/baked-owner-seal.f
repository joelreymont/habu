\ baked-owner-seal.f - seal every package the running engine bakes, the way
\ dot habu-seal-every-captured-c550102f is to seal a captured engine's.
\
\ Every namespace record below the engine's seal floor is a package the engine
\ baked. Both of its wordlists get the protected bit, so `package NAME` on one
\ exits ENGINE-ERROR:SEAL-PACKAGE naming it, and a definition into one is
\ refused, exactly as src/habu/habu2.f C-PACKAGE-PROT-GUARD refuses a sealed
\ engine package. test/baked-owner-child.f and the stage source
\ test/baked-owner.f builds call SEAL-BAKED before they load anything.

package BAKED-OWNER-SEAL

: SEAL-WID ( n -- )
   dup 0= if drop exit then
   prot-wid-add ;

: SEAL-PACKAGE ( ptr n -- ) {: rec:ptr :}
   rec XREF-PKG-PUBLIC SEAL-WID
   rec XREF-PKG-PRIVATE SEAL-WID ;

public

: SEAL-BAKED ( -- )
   SEAL-NDICT@ 0 ?do
      i XREF-REC dup XREF-WORDLIST XREF-NAMESPACE-WL = if
         SEAL-PACKAGE
      else
         drop
      then
   loop ;

;package
