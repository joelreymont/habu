\ x86-64 cached-object image writer. tools/object-image.f loads it for an
\ x86-64 Linux target, after it has decoded the object. The text is already
\ linked for the fixed text VA and carries its DATA runs itself
\ (src/habu/aot-x64.f X64-WRITE-OBJ), so an object with a DATA payload is not
\ this target's; the shared writer restores the ELF and fixed PT_LOAD layout.
require src/habu/image-x64.f

package OBJIMG

public

: WRITE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   TEXT-U @ 0 <= if E-OBJ-SCHEMA throw then
   DATA-U @ 0 <> if E-OBJ-SCHEMA throw then
   path pathu PATH0 1537 493 open {: fd:n :}
   fd 0 < if s" objimg: cannot open output" 74 die then
   TEXT-BUF TEXT-U @ {: code:ptr codeu:n :}
   code codeu code 0 code 0 fd X64IMAGE:WRITE-FD
   fd close-rc 0 <> if s" objimg: output close failed" 74 die then ;

;package
