\ internal-mark.f - seal the packages a capture ships. It changes no record an
\ earlier file made: a word is restricted by its package's private section, and
\ the checker refuses a checked call it has no row for.
\ prefix-boundary.f installs the checker's FIRST-RECORD-XT as it loads.
require src/core/prefix-boundary.f
require src/habu/boot-cells.f

package ENGINE-INTERNAL
public

\ Seal every package the capture ships: both wordlists of every namespace row at
\ or above `first` take the protected bit, so `package NAME` and a definition
\ into either exit ENGINE-ERROR:SEAL-PACKAGE, and the capture keeps none of their
\ private symbols (checker-surface.f KEEP?). The Wasm build calls it at capture
\ (tools/wasm-build.f). A package with no private wordlist holds 0 there, and
\ the capture refuses an image that protects WID 0 (src/habu/aot-capture.f
\ ACAP-PWIN-CAPTURE).
: SEAL-PACKAGES ( n -- )
   {: first:n :}
   ndict@ first ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST BOOT:NAMESPACE-WL = IF
         rec XREF-PKG-PUBLIC prot-wid-add
         rec XREF-PKG-PRIVATE dup 0= IF drop ELSE prot-wid-add THEN
      THEN
   loop ;

seal-package
;package
