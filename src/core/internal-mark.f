\ internal-mark.f - classify the image. It changes no record an earlier file
\ made: a word is restricted by its package's private section, and the checker
\ refuses a checked call it has no row for.
\ prefix-boundary.f installs the checker's FIRST-RECORD-XT as it loads.
require src/core/prefix-boundary.f
require src/habu/boot-cells.f

package ENGINE-INTERNAL

\ ---- the whitebox image ----------------------------------------------------
\ A whitebox build sets ENGINE-UNSEAL (src/core/util.f) before this file loads
\ (tools/native-build-core.f TARGET-ENGINE-UNSEAL); every other load is sealed.

\ The pass writes its verdict into IMK-CLASS and the capture bakes it, so later
\ readers (the build's smoke run in tools/native-build-core.f, SEAL-PACKAGES
\ below) read the image, never the request.
0 constant IMAGE-SEALED         \ the pass ran: the capture seals the packages
1 constant IMAGE-WHITEBOX       \ the pass stood down: they stay open
variable IMK-CLASS

: IMK-PASS ( -- )
   ENGINE-UNSEAL? IF IMAGE-WHITEBOX IMK-CLASS ! EXIT THEN
   IMAGE-SEALED IMK-CLASS ! ;

public

\ The two class values are defined above because IMK-PASS assigns them; they are
\ published here because IMAGE-CLASS's answer is meaningless without them.
EXPORT IMAGE-SEALED
EXPORT IMAGE-WHITEBOX

\ IMAGE-SEALED or IMAGE-WHITEBOX, as the pass that shaped this image left it.
: IMAGE-CLASS ( -- n )
   IMK-CLASS @ ;

\ Seal every package the capture ships: both wordlists of every namespace row at
\ or above `first` take the protected bit, so `package NAME` and a definition
\ into either exit ENGINE-ERROR:SEAL-PACKAGE, and the capture keeps none of their
\ private symbols (checker-surface.f KEEP?). IMK-PASS runs before the compiler,
\ the REPL and the manifest's packages exist, so the native build calls this at
\ capture (tools/native-build-core.f PREPARE-TARGET), before
\ NATIVE-RUNTIME:CAPTURE-PREPARE sweeps; an application image (APP-IMAGE:SAVE)
\ keeps its packages reopenable. The whitebox image stands down here too. A
\ package with no private wordlist holds 0 there, and the capture refuses an
\ image that protects WID 0 (src/habu/aot-capture.f ACAP-PWIN-CAPTURE).
: SEAL-PACKAGES ( n -- )
   {: first:n :}
   IMK-CLASS @ IMAGE-WHITEBOX = IF EXIT THEN
   ndict@ first ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST BOOT:NAMESPACE-WL = IF
         rec XREF-PKG-PUBLIC prot-wid-add
         rec XREF-PKG-PRIVATE dup 0= IF drop ELSE prot-wid-add THEN
      THEN
   loop ;

private

seal-package
' IMK-PASS
;package
execute
