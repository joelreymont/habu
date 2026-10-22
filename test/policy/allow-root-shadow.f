\ Confine the case to its root, as allow-root.f does, and also admit a package
\ whose public word is named require: a case that uses it meets the shadow
\ check at the static require the root admits.
require src/lib/vocabulary.f
require test/policy/dep.f

package PUSER
public
: require ( -- n ) 42 ;
;package

using VOCABULARY

: RUN ( -- )
   s" test/policy/confined" SOURCE-ROOT!
   s" PDEP" ALLOW-PACKAGE
   s" PUSER" ALLOW-PACKAGE
   RESTRICT
   0 SCRIPT-ARGV$ script-required ;
RUN
;using
