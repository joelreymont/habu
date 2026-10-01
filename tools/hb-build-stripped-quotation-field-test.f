\ Build and run the fixed quotation field subject through the product hb-build.
\ Its whole-record replacements happen during source load, before the link.
require tools/hb-build-test-lib.f

package HB-BUILD-CLI

: HBT-STRIPPED-QUOT-FIELD ( -- )
   HBT-AOT-OUT HBT-REMOVE-FILE?
   s" test/stripped-quotation-field-subject.f" HBT-AOT-OUT
   HBT-HBB-PREPARE-AOT HBT-HBB-BUILD-OUT
   HBT-AOT-OUT FILE? TTRUE
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN
   HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N
   {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn S\" quotation fields=ok\n" T$=
   HBT-AOT-OUT HBT-REMOVE-FILE? ;

: HBT-STRIPPED-QUOT-LOOKALIKE ( -- )
   s" test/stripped-quotation-field-lookalike.f" HBT-RUN-MAKER
   {: outn:n errn:n rcn:n :}
   rcn 70 T=
   outn 0 T=
   HBB-ERR-BUF errn
      s" stripped AOT persistent data holds an undeclared code/dict pointer"
      CONTAINS? TTRUE
   HBB-ERR-BUF errn s" word=SCALAR-ROW#base" CONTAINS? TTRUE ;

public
: HBT-STRIPPED-QUOT-FIELD-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-STRIPPED-QUOT-FIELD
   HBT-STRIPPED-QUOT-LOOKALIKE
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-stripped-quotation-field-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-STRIPPED-QUOT-FIELD-MAIN
