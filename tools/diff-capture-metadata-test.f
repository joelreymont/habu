\ diff-capture-metadata-test.f - metadata parser contracts.

require lib/errors.f
require lib/test.f
require tools/diff-capture-metadata.f

package DIFF-META
public

: TEST-PARSE-ONE ( ptr u8 n -- ) {: a:ptr u:n :}
   1 REC-N !
   REC-CELLS >COUNT MEM-ALLOC-CELLS REC-A !
   u 0= if 1 else u then MEM-ALLOC-BYTES drop POOL-A !
   u POOL-CAP !
   0 POOL-U !
   a u 0 PARSE-ROW ;

;package

package DIFF-META-TEST
private

: PARSE-ADDED ( -- )
   S\" [\qadded\q,\q\q,\q\q,false,false,\qnew.f\q,\qfile\q,false,false]"
   DIFF-META:TEST-PARSE-ONE ;

: PARSE-ABSENT-PATH ( -- )
   S\" [\qadded\q,\qold.f\q,\q\q,false,false,\qnew.f\q,\qfile\q,false,false]"
   DIFF-META:TEST-PARSE-ONE ;

: PARSE-PRESENT-EMPTY ( -- )
   S\" [\qadded\q,\q\q,\qfile\q,false,false,\qnew.f\q,\qfile\q,false,false]"
   DIFF-META:TEST-PARSE-ONE ;

: PARSE-MODIFIED-PATHS ( -- )
   S\" [\qmodified\q,\qold.f\q,\qfile\q,false,false,\qnew.f\q,\qfile\q,false,false]"
   DIFF-META:TEST-PARSE-ONE ;

: PARSE-RENAMED-SAME ( -- )
   S\" [\qrenamed\q,\qsame.f\q,\qfile\q,false,false,\qsame.f\q,\qfile\q,false,false]"
   DIFF-META:TEST-PARSE-ONE ;

: PARSE-ABSENT-EXEC ( -- )
   S\" [\qadded\q,\q\q,\q\q,true,false,\qnew.f\q,\qfile\q,false,false]"
   DIFF-META:TEST-PARSE-ONE ;

: PARSE-NUL-PATH ( -- )
   S\" [\qadded\q,\q\q,\q\q,false,false,\qbad\\u0000.f\q,\qfile\q,false,false]"
   DIFF-META:TEST-PARSE-ONE ;

: MAIN ( -- )
   T-RESET
   [: PARSE-ADDED ;] 0 TTHROWSQ
   DIFF-META:COUNT 1 T=
   0 DIFF-META:OLD? TFALSE
   0 DIFF-META:NEW? TTRUE
   0 DIFF-META:NEW$ s" new.f" T$=
   [: PARSE-ABSENT-PATH ;] E-DIFF-SYNTAX TTHROWSQ
   [: PARSE-PRESENT-EMPTY ;] E-DIFF-SYNTAX TTHROWSQ
   [: PARSE-MODIFIED-PATHS ;] E-DIFF-SYNTAX TTHROWSQ
   [: PARSE-RENAMED-SAME ;] E-DIFF-SYNTAX TTHROWSQ
   [: PARSE-ABSENT-EXEC ;] E-DIFF-SYNTAX TTHROWSQ
   [: PARSE-NUL-PATH ;] E-DIFF-SYNTAX TTHROWSQ
   T-REPORT
   s" diff-capture-metadata-test: ok" type cr ;

MAIN

;package
