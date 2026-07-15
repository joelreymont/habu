\ diff-frame-test.f - framed artifact integrity and path-identity tests.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require tools/lint/diff-frame-write.f
require tools/lint/diff-frame.f

package DIFF-FRAME-TEST
private

$10000 constant ART-CAP
$20 constant DIGEST-U
$70 constant FIRST-SECTION
$71 constant STATUS-OFF
$72 constant FORM-OFF
$73 constant BODY-OFF
$78 constant OLD-LEN-OFF

create ART ART-CAP allot
create COPY ART-CAP allot
create DIGEST DIGEST-U allot
create PATH-BUF $200 allot

variable ART-U
variable PATH-U
variable PREFIX-U

: FROM$ ( -- ptr u8 n )
   s" 1111111111111111111111111111111111111111" ;

: TO$ ( -- ptr u8 n )
   s" 2222222222222222222222222222222222222222" ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u $200 > if E-DIFF-FRAME-CAP throw then
   a PATH-BUF u BYTE-COPY
   u PATH-U ! ;

: LF ( -- )
   $0A SB-APPEND-C ;

: TEXT-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND
   s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" index 1111111111..2222222222 100644" SB-APPEND LF
   s" --- a/" SB-APPEND PATH$ SB-APPEND LF
   s" +++ b/" SB-APPEND PATH$ SB-APPEND LF
   s" @@ -1,1 +1,1 @@" SB-APPEND LF
   s" -old" SB-APPEND LF
   s" +new" SB-APPEND LF
   SB$ ;

: MODE-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND
   s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" old mode 100644" SB-APPEND LF
   s" new mode 100755" SB-APPEND LF
   SB$ ;

: EMPTY-ADD-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND
   s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" new file mode 100644" SB-APPEND LF
   s" index 0000000000..e69de29bb2" SB-APPEND LF
   SB$ ;

: GITLINK-ADD-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND
   s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" new file mode 040000" SB-APPEND LF
   s" index 0000000000..1234567890" SB-APPEND LF
   SB$ ;

: BINARY-RAW ( -- ptr u8 n )
   SB-RESET
   s" diff --git a/" SB-APPEND PATH$ SB-APPEND
   s"  b/" SB-APPEND PATH$ SB-APPEND LF
   s" index 1111111111..2222222222 100644" SB-APPEND LF
   s" Binary files a/" SB-APPEND PATH$ SB-APPEND
   s"  and b/" SB-APPEND PATH$ SB-APPEND s"  differ" SB-APPEND LF
   SB$ ;

: REPLACE-RAW ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: old:ptr oldu:n new:ptr newu:n kind:ptr kindu:n :}
   SB-RESET
   s" diff --git a/" SB-APPEND old oldu SB-APPEND
   s"  b/" SB-APPEND new newu SB-APPEND LF
   s" similarity index 100%" SB-APPEND LF
   kind kindu SB-APPEND s"  from " SB-APPEND old oldu SB-APPEND LF
   kind kindu SB-APPEND s"  to " SB-APPEND new newu SB-APPEND LF
   SB$ ;

: MODIFIED-REPLACE-RAW ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: old:ptr oldu:n new:ptr newu:n kind:ptr kindu:n :}
   SB-RESET
   s" diff --git a/" SB-APPEND old oldu SB-APPEND
   s"  b/" SB-APPEND new newu SB-APPEND LF
   s" similarity index 90%" SB-APPEND LF
   kind kindu SB-APPEND s"  from " SB-APPEND old oldu SB-APPEND LF
   kind kindu SB-APPEND s"  to " SB-APPEND new newu SB-APPEND LF
   s" index 1111111111..2222222222 100644" SB-APPEND LF
   s" --- a/" SB-APPEND old oldu SB-APPEND LF
   s" +++ b/" SB-APPEND new newu SB-APPEND LF
   s" @@ -1,1 +1,1 @@" SB-APPEND LF
   s" -old" SB-APPEND LF
   s" +new" SB-APPEND LF
   SB$ ;

: START ( -- )
   ART ART-CAP FROM$ TO$ DIFF-WRITE:START ;

: SAVE ( ptr u8 n -- ) {: a:ptr u:n :}
   a ART = 0= if a ART u BYTE-COPY then
   u ART-U ! ;

: ONE-TEXT ( -- )
   START
   DIFF-STATUS:MODIFIED DIFF-FORM:TEXT true
   true PATH$ true PATH$ TEXT-RAW DIFF-WRITE:SECTION
   DIFF-WRITE:FINISH SAVE ;

: CHECK-CURRENT-PATH ( -- )
   ONE-TEXT
   ART ART-U @ DIFF:OPEN ;

: PATH-CASE ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n label:ptr labelu:n :}
   a u PATH!
   [: CHECK-CURRENT-PATH ;] catch label labelu T-LABEL 0 T= ;

: TEST-PATH-MATRIX ( -- )
   s" plain" s" path-plain" PATH-CASE
   S\" line\nfeed" s" path-lf" PATH-CASE
   S\" carriage\rreturn" s" path-cr" PATH-CASE
   s" has space" s" path-space" PATH-CASE
   S\" has\ttab" s" path-tab" PATH-CASE
   S\" has\"quote" s" path-quote" PATH-CASE
   S\" has\\slash" s" path-slash" PATH-CASE
   s" has b/ split" s" path-b" PATH-CASE
   s" has and split" s" path-and" PATH-CASE ;

: EVENT# ( DIFF:event -- n )
   MATCH DIFF:event
      none    OF 0 ENDOF
      file    OF 1 ENDOF
      hunk    OF 2 ENDOF
      add     OF 3 ENDOF
      context OF 4 ENDOF
      delete  OF 5 ENDOF
   ;MATCH ;

: EXPECT-NEXT ( n -- ) {: want:n :}
   DIFF:NEXT? {: a:ptr u:n meta:n kind:DIFF:event present:bool :}
   kind EVENT# {: got:n :}
   a u 2drop meta drop
   present if 1 else 0 then want 0<> if 1 else 0 then T=
   got want T= ;

: READ-SECTION-INDEX ( -- )
   DIFF:SECTION-INDEX drop ;

: READ-FROM ( -- )
   DIFF:FROM$ 2drop ;

: DROP-NEXT ( -- )
   DIFF:NEXT? drop drop drop 2drop ;

: TEST-PATH ( -- )
   S\" line\ncarriage\r space\tquote\"slash\\ b/ and end" PATH!
   ONE-TEXT
   ART ART-U @ DIFF:OPEN
   [: READ-SECTION-INDEX ;] E-DIFF-SYNTAX TTHROWSQ
   DIFF:NEXT? {: a:ptr u:n meta:n event:DIFF:event present:bool :}
   event EVENT# {: kind:n :}
   present TTRUE
   kind 1 T=
   a u PATH$ T$=
   meta 0 T=
   1 EXPECT-NEXT
   2 EXPECT-NEXT
   5 EXPECT-NEXT
   3 EXPECT-NEXT
   0 EXPECT-NEXT
   [: READ-FROM ;] E-DIFF-SYNTAX TTHROWSQ
   [: DROP-NEXT ;] E-DIFF-SYNTAX TTHROWSQ ;

: TEST-FORMS ( -- )
   s" a.f" PATH!
   START
   DIFF-STATUS:MODIFIED DIFF-FORM:MODE false
   true PATH$ true PATH$ MODE-RAW DIFF-WRITE:SECTION
   DIFF-STATUS:ADDED DIFF-FORM:EMPTY false
   false s" " true PATH$ EMPTY-ADD-RAW DIFF-WRITE:SECTION
   DIFF-STATUS:MODIFIED DIFF-FORM:BINARY true
   true PATH$ true PATH$ BINARY-RAW DIFF-WRITE:SECTION
   DIFF-STATUS:ADDED DIFF-FORM:GITLINK false
   false s" " true PATH$ GITLINK-ADD-RAW DIFF-WRITE:SECTION
   DIFF-WRITE:FINISH SAVE
   ART ART-U @ DIFF:OPEN ;

: TEST-REPLACEMENTS ( -- )
   START
   DIFF-STATUS:RENAMED DIFF-FORM:PURE false
   true s" old name" true s" new name"
   s" old name" s" new name" s" rename" REPLACE-RAW DIFF-WRITE:SECTION
   DIFF-STATUS:COPIED DIFF-FORM:PURE false
   true s" copy old" true s" copy new"
   s" copy old" s" copy new" s" copy" REPLACE-RAW DIFF-WRITE:SECTION
   DIFF-STATUS:RENAMED DIFF-FORM:TEXT true
   true s" mod old" true s" mod new"
   s" mod old" s" mod new" s" rename" MODIFIED-REPLACE-RAW DIFF-WRITE:SECTION
   DIFF-STATUS:COPIED DIFF-FORM:TEXT true
   true s" cmod old" true s" cmod new"
   s" cmod old" s" cmod new" s" copy" MODIFIED-REPLACE-RAW DIFF-WRITE:SECTION
   DIFF-WRITE:FINISH SAVE
   ART ART-U @ DIFF:OPEN ;

: OPEN-PREFIX ( -- )
   ART PREFIX-U @ DIFF:OPEN ;

: TEST-TRUNCATION ( -- )
   s" a.f" PATH!
   ONE-TEXT
   0 begin dup ART-U @ < while
      dup PREFIX-U !
      [: OPEN-PREFIX ;] E-DIFF-SYNTAX TTHROWSQ
      1+
   repeat drop ;

: COPY-ART ( -- )
   ART COPY ART-U @ BYTE-COPY ;

: RESEAL ( -- )
   COPY ART-U @ DIGEST-U - DIGEST SHA256
   DIGEST COPY ART-U @ DIGEST-U - + DIGEST-U BYTE-COPY ;

: COPY-OPEN ( -- )
   COPY ART-U @ DIFF:OPEN ;

: LE64! ( n ptr u8 -- ) {: value:n dst:ptr :}
   0 begin dup 8 < while
      value over 8 * rshift $FF and dst over + c!
      1+
   repeat drop ;

: TEST-MISMATCHES ( -- )
   s" a.f" PATH!
   ONE-TEXT
   COPY-ART
   $FF COPY FORM-OFF + c!
   RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   COPY-ART
   0 COPY BODY-OFF + c!
   RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   COPY-ART
   99 COPY OLD-LEN-OFF + LE64!
   RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   COPY-ART
   COPY ART-U @ DIGEST-U - 1- + dup c@ 1 xor swap c!
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   COPY-ART
   0 COPY ART-U @ DIGEST-U - 8 - + LE64!
   RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ
   COPY-ART
   2 COPY ART-U @ DIGEST-U - 8 - + LE64!
   RESEAL
   [: COPY-OPEN ;] E-DIFF-SYNTAX TTHROWSQ ;

: RUN ( -- )
   T-RESET
   s" plain" PATH!
   [: ONE-TEXT ;] catch s" write-text" T-LABEL 0 T=
   TEST-PATH-MATRIX
   [: TEST-PATH ;] catch s" TEST-PATH" T-LABEL 0 T=
   [: TEST-FORMS ;] catch s" TEST-FORMS" T-LABEL 0 T=
   [: TEST-REPLACEMENTS ;] catch s" TEST-REPLACEMENTS" T-LABEL 0 T=
   [: TEST-TRUNCATION ;] catch s" TEST-TRUNCATION" T-LABEL 0 T=
   [: TEST-MISMATCHES ;] catch s" TEST-MISMATCHES" T-LABEL 0 T=
   T-REPORT ;

RUN

;package
