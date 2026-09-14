\ The first source-prefix record in the running engine's dictionary.
require src/habu/xref.f

package CORE-PREFIX
public

: FIRST-RECORD ( -- n )
   \ Retained/replayed prefixes can contain another marker. The earliest global
   \ row owns the original source boundary, as in the recovery rewind.
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST 0= if
         rec XREF-NAME$ s" IMK-NDICT0" CORE-STR=CI if i unloop exit then
      then
   loop
   s" prefix boundary: first source record is missing" 76 die ;

;package
