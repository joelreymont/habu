require lib/test.f
require lib/fs-mutate.f
require lib/fs-identity.f

package FS-IDENTITY-TEST
using FFI

8 constant PATHS
create ROOT FS-PATH-CAP allot
variable ROOT-SIZE
create PATH-BUFFERS PATHS FS-PATH-CAP * allot

: PATH ( n ptr u8 n -- ptr u8 n )
   {: index:n name bytes:n :}
   index 0 < index PATHS >= or if E-FS-PATH throw then
   PATH-BUFFERS index FS-PATH-CAP * + {: output :}
   output ROOT ROOT-SIZE @ name bytes output JOIN-PATH ;


\ Fixed test-fixture link signature, needed to exercise aliases beyond spelling.
TRUSTED: LINK-CALL ( n -- n ) >r ARGS REG-LENS 2 r> ffi-call-bounded ;


: HARD-LINK ( ptr u8 n ptr u8 n -- )
   {: source source-size:n target target-size:n :}
   HB-TARGET-MACOS? if -2 else 0 then s\" link\z" drop DLSYM {: link-fn:n :}
   link-fn 0<> TTRUE
   RESET source source-size FS-PATHZ 0 READABLE!
   target target-size FS-MUT-PATHZ2 1 READABLE!
   link-fn LINK-CALL 0 T= ;


: SETUP ( -- )
   s" fs-identity" HB-TMP-MKDIR {: directory bytes:n :}
   directory ROOT bytes BYTE-COPY bytes ROOT-SIZE !
   0 s" original" PATH s" same content" WRITE-ALL
   1 s" different" PATH s" same content" WRITE-ALL
   0 s" original" PATH 2 s" hard" PATH HARD-LINK
   s" original" 3 s" symbolic" PATH MAKE-SYMLINK
   s" absent" 4 s" dangling" PATH MAKE-SYMLINK
   s" loop" 5 s" loop" PATH MAKE-SYMLINK ;


: CHECKS ( -- )
   0 s" original" PATH 0 s" original" PATH FS:SAMEFILE TTRUE
   0 s" original" PATH 1 s" different" PATH FS:SAMEFILE TFALSE
   0 s" original" PATH 2 s" hard" PATH FS:SAMEFILE TTRUE
   2 s" hard" PATH 3 s" symbolic" PATH FS:SAMEFILE TTRUE
   0 s" original" PATH 6 s" ./original" PATH FS:SAMEFILE TTRUE
   0 s" original" PATH 7 s" absent" PATH FS:SAMEFILE TFALSE
   7 s" absent" PATH 0 s" original" PATH FS:SAMEFILE TFALSE
   0 s" original" PATH 4 s" dangling" PATH FS:SAMEFILE TFALSE
   [: 0 s" original" PATH 5 s" loop" PATH FS:SAMEFILE drop ;] E-FS-STAT TTHROWSQ
   [: 7 s" absent" PATH 5 s" loop" PATH FS:SAMEFILE drop ;] E-FS-STAT TTHROWSQ
   [: 0 s" original" PATH 6 s" original/child" PATH FS:SAMEFILE drop ;] E-FS-STAT TTHROWSQ
   [: s\" bad\zsuffix" 0 s" original" PATH FS:SAMEFILE drop ;] E-FS-PATH TTHROWSQ
   [: s" " 0 s" original" PATH FS:SAMEFILE drop ;] E-FS-PATH TTHROWSQ ;


: TEST ( -- )
   T-RESET SETUP [: CHECKS ;] catch {: code:n :}
   ROOT ROOT-SIZE @ REMOVE-TREE
   code 0<> if code throw then T-REPORT ;

TEST
;using
;package
