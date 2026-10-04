\ The source-loaded package exporter uses alias-record after the engine has
\ captured its prefix. Its alias must retain a constant's fixed value so a
\ later x86 shadow caller can compile without the original body.
require lib/test.f
require src/compiler/native/dict.f

1 set-tier

package NATIVE-EXPORT-FIXED
private
7 constant HIDDEN
public
export HIDDEN
: USE ( -- n ) HIDDEN ;
;package

package NATIVE-EXPORT-FIXED-TEST
public
: RUN ( -- )
   T-RESET
   s" a source-loaded export keeps its fixed value kind" T-LABEL
   s" NATIVE-EXPORT-FIXED:HIDDEN" NDICT:SPELL-FIXED 1 T=
   NATIVE-EXPORT-FIXED:HIDDEN 7 T=
   NATIVE-EXPORT-FIXED:USE 7 T=
   T-REPORT ;
;package

NATIVE-EXPORT-FIXED-TEST:RUN
