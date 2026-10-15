\ A FILE unit keeps a retired dictionary row and the code its old caller uses.
\ bin/hb --load test/native-file-retired-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-RETIRED-TEST

create APP-BUF FS-PATH-CAP allot
create PROD-BUF FS-PATH-CAP allot

: PREPARE ( -- )
   s" cache" AT MAKE-DIR
   s" unit.f"
   S\" package FILE-RETIRED\npublic\n: VALUE ( -- n ) 1 ;\n: OLD-CALLER ( -- n ) VALUE ;\nundefine VALUE\n: VALUE ( -- n ) 2 ;\n;package\ns\" retire-source-ran\" type cr\n" PUT
   s" app.f"
   S\" require unit.f\n: MAIN ( -- ) FILE-RETIRED:VALUE . FILE-RETIRED:OLD-CALLER . ;\n" PUT
   s" prefix.f"
   S\" package FILE-PREFIX-RETIRED\npublic\n: VALUE ( -- n ) 1 ;\n: OLD-CALLER ( -- n ) VALUE ;\n;package\n" PUT
   s" drop.f"
   S\" require prefix.f\npackage FILE-PREFIX-RETIRED\npublic\nundefine VALUE\n;package\ns\" prefix-retire-source-ran\" type cr\n" PUT
   s" absent-app.f"
   S\" require drop.f\n: MAIN ( -- ) s\" FILE-PREFIX-RETIRED:VALUE\" XREF-FIND XREF-FOUND? if 1 else 0 then . FILE-PREFIX-RETIRED:OLD-CALLER . ;\n" PUT
   s" late.f"
      S\" require prefix.f\npackage FILE-PREFIX-RETIRED\npublic\n: NEW-CALLER ( -- n ) VALUE ;\nundefine VALUE\n;package\ns\" late-retire-source-ran\" type cr\n" PUT
   s" late-app.f"
      S\" require late.f\n: MAIN ( -- ) FILE-PREFIX-RETIRED:NEW-CALLER . ;\n" PUT ;

: BUILD ( ptr u8 n ptr u8 n -- n )
   {: source:ptr sourceu:n product:ptr productu:n :}
   ROOT$ source sourceu APP-BUF JOIN-PATH APP-BUF swap
   ROOT$ product productu PROD-BUF JOIN-PATH PROD-BUF swap
   s" cache" AT
      NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: RUN-PRODUCT ( ptr u8 n ptr u8 n -- )
   {: product:ptr productu:n expected:ptr expectedu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   product productu AT s" " CAPTURE SUCCESS
   OUT$ expected expectedu CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" retired FILE row keeps old calls and warm import" T-LABEL
   s" native-file-retired" TREE
   PREPARE
   s" app.f" s" cold-hb" BUILD SUCCESS
   OUT$ s" file unit hits 0" CONTAINS? TTRUE
   OUT$ s" retire-source-ran" CONTAINS? TTRUE
   s" app.f" s" warm-hb" BUILD SUCCESS
   OUT$ s" file unit hits " CONTAINS? TTRUE
   OUT$ s" file unit hits 0" CONTAINS? 0= TTRUE
   OUT$ s" retire-source-ran" CONTAINS? 0= TTRUE
   s" cold-hb" S\" 2\n1\n" RUN-PRODUCT
   s" warm-hb" S\" 2\n1\n" RUN-PRODUCT
   s" absent-app.f" s" absent-cold-hb" BUILD SUCCESS
   OUT$ s" prefix-retire-source-ran" CONTAINS? TTRUE
   s" absent-app.f" s" absent-warm-hb" BUILD SUCCESS
   OUT$ s" file unit hits 3" CONTAINS? TTRUE
   OUT$ s" compiled 0" CONTAINS? TTRUE
   OUT$ s" prefix-retire-source-ran" CONTAINS? 0= TTRUE
   s" absent-cold-hb" S\" 0\n1\n" RUN-PRODUCT
   s" absent-warm-hb" S\" 0\n1\n" RUN-PRODUCT
   s" new caller keeps its retired target on warm import" T-LABEL
   s" late-app.f" s" late-cold-hb" BUILD SUCCESS
   OUT$ s" late-retire-source-ran" CONTAINS? TTRUE
   s" late-app.f" s" late-warm-hb" BUILD SUCCESS
   OUT$ s" file unit hits 0" CONTAINS? 0= TTRUE
   OUT$ s" late-retire-source-ran" CONTAINS? 0= TTRUE
   s" late-cold-hb" S\" 1\n" RUN-PRODUCT
   s" late-warm-hb" S\" 1\n" RUN-PRODUCT
   T-REPORT
   s" native file retired artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-RETIRED-TEST:MAIN
