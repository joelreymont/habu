\ src/arch/wasm/kernel-words.f on the host: a SAME case runs an engine name and the word WKMAP
\ (kernel-map.f) answers for it, each in a child of its own; both print and exit alike.

require src/lib/test.f
require src/lib/test/subject.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/process.f
require src/lib/ieee754.f
require src/arch/wasm/kernel-words.f
require src/arch/wasm/kernel-map.f

package WKERNEL-TEST
private

$1000 constant CAP
CAP BUFFER: OUT-E
CAP BUFFER: ERR-E
CAP BUFFER: OUT-K
CAP BUFFER: ERR-K

\ The word the map names for an engine name; an empty spelling is a failed case.
: WORD$ ( str -- str )
   WKMAP:WORD-OF {: w:str :}
   w STR:LEN 0= if  s" the map answers a word" T-LABEL  false TTRUE  then
   w ;

\ The text args, w and tail, run in a child with its stdout into o and its
\ stderr into e; answers their lengths and its exit status.
: CHILD ( str str str ^u8 ^u8 -- n n n )
   {: a:str w:str t:str o:^ e:^ :}
   SB-RESET  a SB-APPEND  s"  " SB-APPEND  w SB-APPEND
   s"  " SB-APPEND  t SB-APPEND
   SB$  o CAP >LEN  e CAP >LEN  PROCESS:WAIT-FOREVER  SUBJECT:RUN
   PROCESS:ENDING>RC RC>N {: ou:len eu:len rc:n :}
   ou LEN>N  eu LEN>N  rc ;

\ A rejection's first line, its code and message: the place below it quotes the
\ case's own spelling.
: HEAD ( str -- str ) {: s:str :}
   s 10 STR:INDEX-OF MATCH option
      some OF IDX>N s STR:DATA swap >str ENDOF
      none OF s ENDOF
   ;MATCH ;

\ The engine name on args, its results consumed by tail, against the word the
\ map names for it; both exit 0, so two alike rejections fail the case.
: SAME ( str str str -- )
   {: a:str nm:str t:str :}
   SB-RESET  a SB-APPEND  s"  " SB-APPEND  nm SB-APPEND  SB$ T-LABEL
   a nm t OUT-E ERR-E CHILD {: oe:n ee:n re:n :}
   a  nm WORD$  t OUT-K ERR-K CHILD {: ok:n ek:n rk:n :}
   OUT-K ok >str OUT-E oe >str T$=
   ERR-K ek >str HEAD ERR-E ee >str HEAD T$=
   re 0 T=  rk 0 T= ;

: INTEGERS ( -- )
   s" 0" s" ." s" " SAME
   s" 7" s" ." s" " SAME
   s" -7" s" ." s" " SAME
   s" 1234567890" s" ." s" " SAME
   s" $8000000000000000" s" ." s" " SAME
   s" $7FFFFFFFFFFFFFFF" s" ." s" " SAME
   s" 0" s" u." s" " SAME
   s" 10" s" u." s" " SAME
   s" -1" s" u." s" " SAME
   s" $8000000000000000" s" u." s" " SAME ;

\ Each real by its bits.
: REALS ( -- )
   s" $3FF8000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ 1.5
   s" $C004000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ -2.5
   s" $4008800000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ 3.0625, a fraction that pads
   s" $405EDD2F1A9FBE77 IEEE754:BITS>F64" s" f." s" " SAME   \ 123.456, one that truncates
   s" $3EB0C6F7A0B5ED8D IEEE754:BITS>F64" s" f." s" " SAME   \ 1e-6, at the sixth digit
   s" $0000000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ +0.0
   s" $8000000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ -0.0
   s" $7FF8000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ a NaN
   s" $FFF8000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ a NaN, bit 63 set
   s" $7FF0000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ +inf
   s" $FFF0000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ -inf
   s" $43E0000000000000 IEEE754:BITS>F64" s" f." s" " SAME ; \ 2^63, past MAX-N

: TEXT ( -- )
   s\" s\" kernel words\" " s" type" s" " SAME
   s\" s\" \" " s" type" s" " SAME
   s" " s" cr" s" " SAME
   s" " s" space" s" " SAME ;

: NUMBERS ( -- )
   s" 5" s" negate" s" ." SAME
   s" $8000000000000000" s" negate" s" ." SAME
   s" -7" s" abs" s" ." SAME
   s" 7" s" abs" s" ." SAME
   s" $8000000000000000" s" abs" s" ." SAME
   s" 3 5" s" min" s" ." SAME
   s" 5 3" s" min" s" ." SAME
   s" $8000000000000000 $7FFFFFFFFFFFFFFF" s" min" s" ." SAME
   s" 7 2" s" /mod" s" . ." SAME
   s" -7 2" s" /mod" s" . ." SAME
   s" 7 -2" s" /mod" s" . ." SAME
   s" -7 -2" s" /mod" s" . ." SAME
   s" $8000000000000000 -1" s" /mod" s" . ." SAME
   s" variable V 5 V" s" +!" s" V @ ." SAME ;

false [if] \ off until dot habu-add-the-checks-a49d054d, then called by RUN
: ZERO-DIVISOR ( -- )
   s" 5 0" s" /mod" s" . ." SAME ;
[then]

: TESTS ( -- )
   s" -1" s" 0<" s" bool>n ." SAME
   s" 0" s" 0<" s" bool>n ." SAME
   s" $8000000000000000" s" 0<" s" bool>n ." SAME
   s" 0" s" 0<>" s" bool>n ." SAME
   s" -1" s" 0<>" s" bool>n ." SAME
   s" " s" true" s" bool>n ." SAME
   s" " s" false" s" bool>n ." SAME ;

public

: RUN ( -- )
   T-RESET
   INTEGERS
   REALS
   TEXT
   NUMBERS
   TESTS
   T-REPORT ;

;package

WKERNEL-TEST:RUN
