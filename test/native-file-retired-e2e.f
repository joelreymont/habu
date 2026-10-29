\ A FILE unit keeps a retired dictionary row and the code its old caller uses.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-RETIRED-TEST

: PREPARE ( -- )
   s" cache" AT FS:MAKE-DIR
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
      S\" require late.f\n: MAIN ( -- ) FILE-PREFIX-RETIRED:NEW-CALLER . ;\n" PUT
   \ These definers advance CP past SAVE's retired CALL site without NART publication.
   s" called.f"
      S\" package FILE-CALLED-RETIRED\nvariable RESULT\npublic\n: OLD ( -- n ) 1 ;\n: SAVE ( -- ) OLD RESULT ! ;\nSAVE\ns\" FILE-CALLED-RETIRED:OLD\" FORGET-DEFS-FROM\nvariable FILL-A\nvariable FILL-B\nvariable FILL-C\nvariable FILL-D\nvariable FILL-E\nvariable FILL-F\nvariable FILL-G\nvariable FILL-H\n: VALUE ( -- n ) RESULT @ ;\n;package\ns\" called-retire-source-ran\" type cr\n" PUT
   s" called-app.f"
      S\" require called.f\n: MAIN ( -- ) FILE-CALLED-RETIRED:VALUE . ;\n" PUT
   s" replaced.f"
      S\" require src/habu/xref.f\npackage FILE-REPLACED-OWNER\nvariable HITS\nCAST: TRACK-N ( [ n -- ] -- n )\npublic\n: TRACK ( n -- ) drop 1 HITS +! ;\n: HITS@ ( -- n ) HITS @ ;\n: INSTALLED? ( -- bool ) data-base NATIVE-OBS-CELLS:INVALIDATE + @ ['] TRACK TRACK-N = ;\n: SEVEN ( -- n ) 7 ;\n: EARLIER ( -- n ) SEVEN ;\n: QUOTED ( -- n ) [: SEVEN ;] execute ;\n;package\n' FILE-REPLACED-OWNER:TRACK CODE-RECLAIM:INVALIDATE!\nrequire replaced-child.f\npackage FILE-REPLACED-OWNER\npublic\n: LATER ( -- n ) EARLIER ;\n;package\ns\" replaced-source-ran\" type cr\n" PUT
   s" replaced-child.f"
      S\" package FILE-REPLACED-CHILD\nvariable RESULT\npublic\n: OLD ( -- n ) 1 ;\n: SAVE ( -- ) [: OLD ;] execute RESULT ! ;\nSAVE\ns\" FILE-REPLACED-CHILD:OLD\" FORGET-DEFS-FROM\nvariable FILL-A\nvariable FILL-B\nvariable FILL-C\nvariable FILL-D\nvariable FILL-E\nvariable FILL-F\nvariable FILL-G\nvariable FILL-H\n: VALUE ( -- n ) RESULT @ ;\n;package\n" PUT
   s" replaced-app.f"
      S\" require replaced.f\n: MAIN ( -- ) FILE-REPLACED-OWNER:LATER . FILE-REPLACED-OWNER:QUOTED . FILE-REPLACED-CHILD:VALUE . FILE-REPLACED-OWNER:HITS@ 0 > if 1 else 0 then . FILE-REPLACED-OWNER:INSTALLED? if 1 else 0 then . ;\n" PUT
   s" failed.f"
      S\" require src/habu/xref.f\npackage FILE-FAILED-OWNER\npublic\n: TRACK ( n -- ) drop ;\n;package\n' FILE-FAILED-OWNER:TRACK CODE-RECLAIM:INVALIDATE!\n7277 throw\n" PUT
   s" failed-app.f"
      S\" require failed.f\n: MAIN ( -- ) ;\n" PUT
   s" failed-origin.f"
      S\" require src/habu/xref.f\npackage FAILED-ORIGIN\npublic\nvariable OLD-XT\n: VANISH ( -- n ) 9 ;\ns\" FAILED-ORIGIN:VANISH\" XREF-FIND XREF-START OLD-XT !\ns\" FAILED-ORIGIN:VANISH\" FORGET-DEFS-FROM\n: REUSED ( -- n ) 4 ;\n: CHECK-REUSE ( -- ) s\" FAILED-ORIGIN:REUSED\" XREF-FIND XREF-START OLD-XT @ <> if 70 throw then 0 OLD-XT ! ;\nCHECK-REUSE\n: MAKE ( n -- ) create , does> ( -- ptr n ) ;\n: CHECK-FAILURE ( -- ) [: s\" : FAILED ( n -- ) MAKE does> ( -- ptr n ) [: 1 ;] drop ;\" evaluate-closed ;] catch E-NELAB-QUOT <> if 70 throw then ;\nCHECK-FAILURE\n: AFTER ( -- n ) 5 ;\n;package\ns\" failed-origin-ran\" type cr\n" PUT
   s" failed-origin-app.f"
      S\" require failed-origin.f\n: MAIN ( -- ) FAILED-ORIGIN:REUSED . FAILED-ORIGIN:AFTER . ;\n" PUT ;

: BUILD ( ptr u8 n ptr u8 n -- n )
   {: source:ptr sourceu:n product:ptr productu:n :}
   ROOT$ source sourceu product productu NATIVE-FILE-APP:BUILD-AT
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
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   OUT$ s" retire-source-ran" CONTAINS? TTRUE
   s" app.f" s" warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   OUT$ s" retire-source-ran" CONTAINS? TTRUE
   s" cold-hb" S\" 2\n1\n" RUN-PRODUCT
   s" warm-hb" S\" 2\n1\n" RUN-PRODUCT
   s" absent-app.f" s" absent-cold-hb" BUILD SUCCESS
   OUT$ s" prefix-retire-source-ran" CONTAINS? TTRUE
   s" absent-app.f" s" absent-warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   OUT$ s" prefix-retire-source-ran" CONTAINS? TTRUE
   s" absent-cold-hb" S\" 0\n1\n" RUN-PRODUCT
   s" absent-warm-hb" S\" 0\n1\n" RUN-PRODUCT
   s" new caller keeps its retired target on warm import" T-LABEL
   s" late-app.f" s" late-cold-hb" BUILD SUCCESS
   OUT$ s" late-retire-source-ran" CONTAINS? TTRUE
   s" late-app.f" s" late-warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   OUT$ s" late-retire-source-ran" CONTAINS? TTRUE
   s" late-cold-hb" S\" 1\n" RUN-PRODUCT
   s" late-warm-hb" S\" 1\n" RUN-PRODUCT
   s" retired load-time call survives FILE capture" T-LABEL
   s" called-app.f" s" called-cold-hb" BUILD SUCCESS
   OUT$ s" called-retire-source-ran" CONTAINS? TTRUE
   s" called-app.f" s" called-warm-hb" BUILD SUCCESS
   OUT$ s" called-retire-source-ran" CONTAINS? TTRUE
   s" called-cold-hb" S\" 1\n" RUN-PRODUCT
   s" called-warm-hb" S\" 1\n" RUN-PRODUCT
   s" source owner survives FILE capture and later reclamation" T-LABEL
   s" replaced-app.f" s" replaced-cold-hb" BUILD SUCCESS
   OUT$ s" replaced-source-ran" CONTAINS? TTRUE
   s" replaced-app.f" s" replaced-warm-hb" BUILD SUCCESS
   OUT$ s" replaced-source-ran" CONTAINS? TTRUE
   s" replaced-cold-hb" S\" 7\n7\n1\n1\n1\n" RUN-PRODUCT
   s" replaced-warm-hb" S\" 7\n7\n1\n1\n1\n" RUN-PRODUCT
   s" a source error survives callback replacement" T-LABEL
   s" failed-app.f" s" failed-hb" BUILD 0<> TTRUE
   ERR$ s" 7277" CONTAINS? TTRUE
   ERR$ s" 7243" CONTAINS? 0= TTRUE
   s" a reused slot and a failed definition keep their own origins on warm import" T-LABEL
   s" failed-origin-app.f" s" failed-origin-cold-hb" BUILD SUCCESS
   OUT$ s" failed-origin-ran" CONTAINS? TTRUE
   s" failed-origin-app.f" s" failed-origin-warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   OUT$ s" failed-origin-ran" CONTAINS? TTRUE
   s" failed-origin-cold-hb" S\" 4\n5\n" RUN-PRODUCT
   s" failed-origin-warm-hb" S\" 4\n5\n" RUN-PRODUCT
   T-REPORT ;
;package
;using

NATIVE-FILE-RETIRED-TEST:MAIN
