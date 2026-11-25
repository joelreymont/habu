\ link-kernel.f - the Wasm part of the linker. A Wasm module has no hb kernel:
\ a kernel word it calls is either provided by a word of
\ src/arch/wasm/kernel-words.f, which WKMAP names, or left to the write step.
require src/lib/string.f
require src/lib/string-option.f
require src/habu/xref.f
require src/habu/boot-cells.f
require src/arch/wasm/kernel-map.f

package LINKER
private

package WASM

\ The name of the dictionary's first record entering at host address a, or an
\ empty spelling.
: NAME-AT ( n -- str )
   {: a:n :}
   ndict@ 0 ?do
      i XREF-REC XREF-START a = if i XREF-REC XREF-NAME$ unloop exit then
   loop
   s" " ;


\ NAME-AT's reverse: the host entry of the dictionary's first record so named,
\ or 0.
: NAMED-ENTRY ( str -- n )
   {: name:str :}
   ndict@ 0 ?do
      i XREF-REC XREF-NAME$ name STR:=CI if i XREF-REC XREF-START unloop exit then
   loop
   0 ;


\ The host entry of the public word tail of the package named p, or 0. A build
\ that never loaded the package provides nothing.
: PUBLIC-ENTRY ( str str -- n )
   {: p:str t:str :}
   p BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:^ :}
   pkg XREF-FOUND? 0= if 0 exit then
   t pkg XREF-PKG-PUBLIC CHECKER-RESOLVE:WL-PROBE {: rec:^ :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-START ;

public

\ The host entry of the kernel-words word that provides the kernel word
\ entering at host address a, or 0.
: PROVIDER ( n -- n )
   NAME-AT WKMAP:WORD-OF {: w:str :}
   w STR:LEN 0= if 0 exit then
   w [char] : STR:INDEX-OF MATCH option
      none OF 0 ENDOF
      some OF IDX>N {: c:n :}
         w STR:DATA c >str
         w STR:DATA c + 1+  w STR:LEN c - 1-  >str
         PUBLIC-ENTRY ENDOF
   ;MATCH ;

;package
;package
