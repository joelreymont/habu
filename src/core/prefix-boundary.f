split-str
\ The first source-prefix record in the running engine's dictionary.
require src/core/exit-status.f
require src/habu/xref.f

package CORE-PREFIX

: MISSING ( -- )
   s" prefix boundary: first source record is missing" EXIT-STATUS:INTERNAL die ;

\ hb's own CORE-PREFIX private wordlist: a build repoints BOOT:NAMESPACE-WL at
\ the target's packages, so the probe goes through WL:NAMESPACE.
: OWN-WL ( -- n )
   s" CORE-PREFIX" WL:NAMESPACE CHECKER-RESOLVE:WL-PROBE {: pkg:^ :}
   pkg NULL-PTR = if MISSING then
   pkg XREF-PKG-PRIVATE ;

public

: FIRST-RECORD ( -- n )
   \ A replayed prefix can hold another marker; the earliest row in hb's own
   \ CORE-PREFIX private wordlist is the original boundary.
   OWN-WL {: wl:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:^ :}
      rec XREF-WORDLIST wl = if
         rec XREF-NAME$ s" IMK-NDICT0" CORE-STR=CI if i unloop exit then
      then
   loop
   MISSING ;

private

EXPORT CHECK:FIRST-RECORD-XT
\ checker.f CHECKER-OVERLAY asks for the boundary when a replay opens; it loads
\ before this file, so it asks through the defer FIRST-RECORD-XT.
: INSTALL ( -- ) [: FIRST-RECORD ;] is FIRST-RECORD-XT ;
INSTALL
undefine INSTALL

;package
