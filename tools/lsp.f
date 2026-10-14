\ lsp.f - the Habu language server, speaking LSP on stdin and stdout.
\ Run: bin/hb --load tools/lsp.f

\ An absolute --load entry may run from a different Habu tree. For this
\ load, the engine's baked files belong to the tree containing this entry.
package LSP-ENTRY
create ROOT PATH-CAP allot   variable ROOT-U
create OLD PATH-CAP allot    variable OLD-U

: SAVE ( -- )
   SOURCE-ROOT:ENGINE$ {: a:ptr u:n :}
   a OLD u BYTE-COPY  u OLD-U !
   SOURCE-ROOT:ACTIVE-SOURCE 2drop 2drop
   SOURCE-ROOT:DIRNAME SOURCE-ROOT:DIRNAME {: root:ptr rootu:n :}
   root ROOT rootu BYTE-COPY  rootu ROOT-U ! ;

: LOAD ( -- ) s" tools/lsp-core.f" required ;
: LOAD-ROOT ( -- ) ROOT ROOT-U @ [: LOAD ;] SOURCE-ROOT:WITH ;

public
: START ( -- )
   SAVE
   ROOT ROOT-U @ SOURCE-ROOT:ENGINE!
   [: LOAD-ROOT ;] catch {: rc:n :}
   OLD OLD-U @ SOURCE-ROOT:ENGINE!
   rc 0<> if rc throw then ;
;package

LSP-ENTRY:START

LSP:MAIN
