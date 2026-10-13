\ Generated-source certification checks every body despite provided markers.
require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/build.f
require src/lib/codesign.f
require src/lib/test.f
require tools/build-fixpoint.f

package CERTIFY-GENERATED-TEST

create SOURCE FS-PATH-CAP allot variable SOURCE-U

: CHECK ( ptr u8 n n -- ) {: text:ptr size:n want:n :}
   SOURCE SOURCE-U @ text size WRITE-ALL
   [: BUILD-FIXPOINT:BF-CERTIFY-STDIN ;] catch want T= ;

public

: RUN ( -- )
   T-RESET CLEANUP-RESET
   s" habu-certify-generated" HB-TMP-MKDIR {: root:ptr u:n :}
   root u CLEANUP-TREE+
   root u BUILD-FIXPOINT:BF-TMP!
   root u s" stage2-src" SOURCE JOIN-PATH SOURCE-U !
   S\" s\" src/lib/string.f\" provided\n: BAD ( -- ) drop ;\ns\" src/lib/string.f\" provided\n"
      E-BUILD-CERTIFY CHECK
   S\" s\" src/lib/string.f\" provided\n: BAD ( -- ) drop ;\n"
      E-BUILD-CERTIFY CHECK
   S\" s\" src/lib/string.f\" provided\n: BAD ( -- ) drop ;\ns\" src/lib/adt/option.f\" provided\n"
      E-BUILD-CERTIFY CHECK
   \ The actual stdlib constructor shape must certify against the core prefix.
   s" package GENERATED-CHECK public ENUM result<a> VARIANT ok FIELD value a ;VARIANT VARIANT no ;VARIANT ;ENUM : WRAP ( a -- result<a> ) GENERATED--CHECK-RESULT:OK ; ;package"
      0 CHECK
   \ A second child starts from the same prefix; the first did not alter parent.
   s" package GENERATED-CHECK public ENUM result yes no ;ENUM ;package"
      0 CHECK
   s" package GENERATED-CHECK public ENUM result yes no ;ENUM ENUM result yes no ;ENUM ;package"
      E-BUILD-CERTIFY CHECK
   BUILD-FIXPOINT:BF-TMP-RESET
   CLEANUP-RUN T-REPORT ;

;package

CERTIFY-GENERATED-TEST:RUN
