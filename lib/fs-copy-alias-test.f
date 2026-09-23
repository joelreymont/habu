require lib/test.f
require lib/fs-mutate.f

package FS-COPY-ALIAS-TEST

PROCESS-SYMBOLS
FUNCTION: LINK-CALL link ( ptr u8 ptr u8 -- n ) ;FUNCTION

create ROOT FS-PATH-CAP allot
variable ROOT-U
create PATHS 4 FS-PATH-CAP * allot
create TEXT 64 allot

: PATH ( n ptr u8 n -- ptr u8 n ) {: index name bytes :}
   PATHS index FS-PATH-CAP * + {: out :}
   out ROOT ROOT-U @ name bytes out JOIN-PATH ;
: SOURCE ( -- ptr u8 n ) 0 s" source" PATH ;
: HARD ( -- ptr u8 n ) 1 s" hard" PATH ;
: SYMBOLIC ( -- ptr u8 n ) 2 s" symbolic" PATH ;
: DEST ( -- ptr u8 n ) 3 s" dest" PATH ;

: CONTENT ( ptr u8 n -- )
   TEXT 64 READ-ALL TEXT swap s" must survive" T$= ;

: CHECKS ( -- )
   [: SOURCE SOURCE COPY-FILE-STREAM ;] E-FS-OPEN TTHROWSQ
   SOURCE CONTENT
   [: SOURCE HARD COPY-FILE-STREAM ;] E-FS-OPEN TTHROWSQ
   SOURCE CONTENT HARD CONTENT
   [: SOURCE SYMBOLIC COPY-FILE-STREAM ;] E-FS-OPEN TTHROWSQ
   SOURCE CONTENT SYMBOLIC CONTENT
   \ A distinct existing destination is truncated, including its old suffix.
   DEST s" old destination longer than source" WRITE-ALL
   SOURCE DEST COPY-FILE-STREAM DEST CONTENT SOURCE CONTENT
   DEST REMOVE-FILE
   SOURCE DEST COPY-FILE-STREAM DEST CONTENT SOURCE CONTENT ;

: SETUP ( -- )
   s" habu-copy-alias" HB-TMP-MKDIR {: a u :}
   a ROOT u BYTE-COPY u ROOT-U !
   SOURCE s" must survive" WRITE-ALL
   SOURCE FS-PATHZ HARD FS-MUT-PATHZ2 LINK-CALL 0 T=
   s" source" SYMBOLIC MAKE-SYMLINK ;

: CLEAN ( -- ) ROOT ROOT-U @ REMOVE-TREE ;
: RUN ( -- ) T-RESET SETUP [: CHECKS ;] [: CLEAN ;] finally T-REPORT ;
RUN
;package
