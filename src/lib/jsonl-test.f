\ jsonl-test.f - the JSONL memory splitter through the public API.
\ Run: bin/hb --load src/lib/jsonl-test.f

require src/lib/test.f
require src/lib/jsonl.f

package JSONL-TEST
private

\ The line at pos is want, and the position after it is next.
: SPLIT= ( str n str n -- )
   {: s:str pos:n want:str next:n :}
   s pos JSONL:SPLIT-LINE TTRUE {: line:str got:n :}
   line want T$=
   got next T= ;

\ No line starts at pos.
: SPLIT-END ( str n -- )
   {: s:str pos:n :}
   s pos JSONL:SPLIT-LINE TFALSE {: line:str got:n :}
   line STR:LEN 0 T=
   got pos T= ;

: SPLIT ( -- )
   s" " 0 SPLIT-END
   s\" \n" 0 s" " 1 SPLIT=
   s\" \n" 1 SPLIT-END
   s\" a\n" 0 s" a" 2 SPLIT=
   s\" a\n" 2 SPLIT-END
   s\" a\n" 5 SPLIT-END
   s\" a\r\n\nlast" 0 s\" a\r" 3 SPLIT=
   s\" a\r\n\nlast" 3 s" " 4 SPLIT=
   s\" a\r\n\nlast" 4 s" last" 8 SPLIT=
   s\" a\r\n\nlast" 8 SPLIT-END
   s\" \n1\n\n2" 0 s" " 1 SPLIT=
   s\" \n1\n\n2" 1 s" 1" 3 SPLIT=
   s\" \n1\n\n2" 3 s" " 4 SPLIT=
   s\" \n1\n\n2" 4 s" 2" 5 SPLIT=
   s\" \n1\n\n2" 5 SPLIT-END
   [: s" a" STR:DATA -1 >str 0 JSONL:SPLIT-LINE drop drop drop ;] JSONL:E-SOURCE TTHROWSQ
   [: NULL-PTR 1 >str 0 JSONL:SPLIT-LINE drop drop drop ;] JSONL:E-SOURCE TTHROWSQ
   [: s" a" -1 JSONL:SPLIT-LINE drop drop drop ;] JSONL:E-SOURCE TTHROWSQ ;

public
: RUN ( -- )
   SPLIT ;
;package

T-RESET
JSONL-TEST:RUN
T-REPORT
s" jsonl-test: ok" type cr
