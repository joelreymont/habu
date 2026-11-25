\ kernel-map.f - WKMAP, the map from an engine word's name to the word that
\ provides it in a Wasm module.

require src/lib/string.f

package WKMAP
public

\ The kernel-words word answering an engine name, or an empty spelling.
: WORD-OF ( str -- str )
   {: s:str :}
   s s" ." STR:=CI if s" WKWORDS:DOT" exit then
   s s" .s" STR:=CI if s" WKWORDS:DOT-S" exit then
   s s" u." STR:=CI if s" WKWORDS:U-DOT" exit then
   s s" f." STR:=CI if s" WKWORDS:F-DOT" exit then
   s s" fdrop" STR:=CI if s" WKWORDS:F-DROP" exit then
   s s" fdup" STR:=CI if s" WKWORDS:F-DUP" exit then
   s s" type" STR:=CI if s" WKWORDS:TYPE-BYTES" exit then
   s s" cr" STR:=CI if s" WKWORDS:NEWLINE" exit then
   s s" space" STR:=CI if s" WKWORDS:BLANK" exit then
   s s" negate" STR:=CI if s" WKWORDS:NEG" exit then
   s s" abs" STR:=CI if s" WKWORDS:ABSOLUTE" exit then
   s s" min" STR:=CI if s" WKWORDS:MINIMUM" exit then
   s s" max" STR:=CI if s" WKWORDS:MAXIMUM" exit then
   s s" /mod" STR:=CI if s" WKWORDS:DIVREM" exit then
   s s" 0<" STR:=CI if s" WKWORDS:ZERO-NEG?" exit then
   s s" 0<>" STR:=CI if s" WKWORDS:NONZERO?" exit then
   s s" true" STR:=CI if s" WKWORDS:YES" exit then
   s s" false" STR:=CI if s" WKWORDS:NO" exit then
   s s" NULL-PTR" STR:=CI if s" WKWORDS:NIL" exit then
   s s" data-base" STR:=CI if s" WKWORDS:DATA-START" exit then
   s s" +!" STR:=CI if s" WKWORDS:ADD-STORE" exit then
   s s" BYTE-COPY-LEN" STR:=CI if s" WKWORDS:COPY-LEN" exit then
   s s" BYTE-COPY" STR:=CI if s" WKWORDS:COPY-BYTES" exit then
   s s" sha256-blocks" STR:=CI if s" WKWORDS:SHA-BLOCKS" exit then
   s" " ;

;package
