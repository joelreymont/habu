\ A compacted C2 code window is still a partial capture. Its entries cannot
\ become checked scope authorities without the complete runtime.
\ Bind the x86 writer before ARM icode adds its global CODE-CAP-BYTES.
require tools/native-emit.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-arm.f
require src/habu/aot-decl.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f

1 set-tier
AOT-ARM:WINDOW-OPEN
package C2-PARTIAL
public
: PROBE ( -- n ) 7 ;
;package
AOT-ARM:WINDOW-CLOSE

AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE

\ Even a populated compact window cannot become a complete C2 runtime.
package NATIVE-EMIT
private
: PARTIAL-RUN ( -- )
   AOT-FILE:OWN dup AOT-FILE:IMPORT
   AOT-BUF:AOT-BLOB-LEN @ 0 <= if s" c2-partial: empty code window" 79 die then
   s" c2-partial: code bytes " type AOT-BUF:AOT-BLOB-LEN @ . cr
   AOT-RUNTIME:COMPLETE? if s" c2-partial: unexpectedly complete" 79 die then
   dup NATIVE-LAYOUT:CURRENT 0 SCRIPT-ARGV$ WRITE-C2
   AOT-OWNED:CLOSE ;

' PARTIAL-RUN
;package

execute
