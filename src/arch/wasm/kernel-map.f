\ kernel-map.f - WKMAP, the map from an engine word's name to the word of
\ src/arch/wasm/kernel-words.f that provides it in a Wasm module. WKERNEL's
\ PROVIDER and the linker's reach (src/arch/wasm/link-kernel.f) both read it.

require src/lib/string.f

package WKMAP
public

\ The kernel-words word answering an engine name, or an empty spelling.
: WORD-OF ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   a u s" ." STR=CI if s" WKWORDS:DOT" exit then
   a u s" u." STR=CI if s" WKWORDS:U-DOT" exit then
   a u s" f." STR=CI if s" WKWORDS:F-DOT" exit then
   a u s" fdrop" STR=CI if s" WKWORDS:F-DROP" exit then
   a u s" fdup" STR=CI if s" WKWORDS:F-DUP" exit then
   a u s" type" STR=CI if s" WKWORDS:TYPE-BYTES" exit then
   a u s" cr" STR=CI if s" WKWORDS:NEWLINE" exit then
   a u s" space" STR=CI if s" WKWORDS:BLANK" exit then
   a u s" negate" STR=CI if s" WKWORDS:NEG" exit then
   a u s" abs" STR=CI if s" WKWORDS:ABSOLUTE" exit then
   a u s" min" STR=CI if s" WKWORDS:MINIMUM" exit then
   a u s" /mod" STR=CI if s" WKWORDS:DIVREM" exit then
   a u s" 0<" STR=CI if s" WKWORDS:ZERO-NEG?" exit then
   a u s" 0<>" STR=CI if s" WKWORDS:NONZERO?" exit then
   a u s" true" STR=CI if s" WKWORDS:YES" exit then
   a u s" false" STR=CI if s" WKWORDS:NO" exit then
   a u s" f<=" STR=CI if s" WKWORDS:F-AT-MOST?" exit then
   a u s" f>=" STR=CI if s" WKWORDS:F-AT-LEAST?" exit then
   a u s" NULL-PTR" STR=CI if s" WKWORDS:NIL" exit then
   a u s" field-project" STR=CI if s" WKWORDS:PROJECT-FIELD" exit then
   a u s" data-base" STR=CI if s" WKWORDS:DATA-START" exit then
   a u s" +!" STR=CI if s" WKWORDS:ADD-STORE" exit then
   a u s" BYTE-COPY-LEN" STR=CI if s" WKWORDS:COPY-LEN" exit then
   a u s" BYTE-COPY" STR=CI if s" WKWORDS:COPY-BYTES" exit then
   a u s" DYNAMIC-STORAGE:BOUNDS" STR=CI if s" WKWORDS:DYN-BOUNDS" exit then
   a u s" DYNAMIC-STORAGE:ADDRESS" STR=CI if s" WKWORDS:DYN-ADDRESS" exit then
   a u s" DYNAMIC-STORAGE:RESERVE" STR=CI if s" WKWORDS:DYN-RESERVE" exit then
   a u s" DYNAMIC-STORAGE:RELEASE" STR=CI if s" WKWORDS:DYN-RELEASE" exit then
   s" " ;

;package
