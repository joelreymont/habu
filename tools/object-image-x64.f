\ x86-64 cached-object image writer. Its code is already linked for the fixed
\ text VA; the shared writer restores the ELF and fixed PT_LOAD layout.
require src/habu/image-x64.f

package OBJIMG

public

: WRITE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   OBJLINK:APPLY
   NONEMPTY-TEXT
   path pathu PATH0 1537 493 open {: fd:n :}
   fd 0 < if s" objimg: cannot open output" 74 die then
   OBJLINK:TEXT$ {: code:ptr codeu:n :}
   code codeu code 0 code 0 fd X64IMAGE:WRITE-FD
   fd close-rc 0 <> if s" objimg: output close failed" 74 die then ;

;package
