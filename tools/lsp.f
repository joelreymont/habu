\ lsp.f - the Habu language server, speaking LSP on stdin and stdout.

\ An absolute --load entry may run from a different Habu tree. For this
\ load, the engine's baked files belong to the tree containing this entry.
package LSP-ENTRY
create ROOT PATH-CAP allot   variable ROOT-U
create OLD PATH-CAP allot    variable OLD-U

: SAVE ( -- )
   SOURCE-ROOT:ENGINE$ {: old:str :}
   old STR:DATA OLD old STR:LEN BYTE-COPY  old STR:LEN OLD-U !
   SOURCE-ROOT:ACTIVE-SOURCE drop drop
   SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME {: root:str :}
   root STR:DATA ROOT root STR:LEN BYTE-COPY  root STR:LEN ROOT-U ! ;

: LOAD ( -- ) s" tools/lsp-core.f" required ;
: LOAD-ROOT ( -- ) ROOT ROOT-U @ >str [: LOAD ;] SOURCE-ROOT:WITH ;

public
: START ( -- )
   SAVE
   ROOT ROOT-U @ >str SOURCE-ROOT:ENGINE!
   [: LOAD-ROOT ;] catch {: rc:n :}
   OLD OLD-U @ >str SOURCE-ROOT:ENGINE!
   rc 0<> if rc throw then ;
;package

LSP-ENTRY:START

LSP:MAIN
