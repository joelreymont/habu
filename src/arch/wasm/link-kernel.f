\ link-kernel.f - the Wasm part of the linker. A Wasm module has no hb kernel:
\ a kernel word it calls is either provided by a word of
\ src/arch/wasm/kernel-words.f, which WKMAP names, or left to the write step.
require src/lib/string.f
require src/habu/xref.f
require src/habu/boot-cells.f
require src/arch/wasm/kernel-map.f

package LINKER
private

package WASM

\ The name of the dictionary's first record entering at host address a, or an
\ empty spelling.
: NAME-AT ( n -- ptr u8 n )
   {: a:n :}
   ndict@ 0 ?do
      i XREF-REC XREF-START a = if i XREF-REC XREF-NAME$ unloop exit then
   loop
   s" " ;


\ NAME-AT's reverse: the host entry of the dictionary's first record so named,
\ or 0.
: NAMED-ENTRY ( ptr u8 n -- n )
   {: name:ptr u:n :}
   ndict@ 0 ?do
      i XREF-REC XREF-NAME$ name u STR=CI if i XREF-REC XREF-START unloop exit then
   loop
   0 ;


\ The host entry of the public word tail of the package named p, or 0. A build
\ that never loaded the package provides nothing.
: PUBLIC-ENTRY ( ptr u8 n ptr u8 n -- n )
   {: p:ptr pu:n t:ptr tu:n :}
   p pu BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? 0= if 0 exit then
   t tu pkg XREF-PKG-PUBLIC CHECKER-RESOLVE:WL-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-START ;

public

\ The host entry of the kernel-words word that provides the kernel word
\ entering at host address a, or 0.
: PROVIDER ( n -- n )
   NAME-AT WKMAP:WORD-OF {: w:ptr wu:n :}
   wu 0= if 0 exit then
   w wu [char] : INDEX-OF MATCH option
      none OF 0 ENDOF
      some OF IDX>N {: c:n :} w c  w c + 1+  wu c - 1-  PUBLIC-ENTRY ENDOF
   ;MATCH ;

;package
;package
