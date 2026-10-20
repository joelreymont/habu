\ Construction facts returned by the child that actually loaded support.
require src/lib/engine-id.f
require src/lib/le.f
require src/lib/memory.f
require tools/native-file-inputs.f

package NATIVE-BUILD-RESULT
private

88 constant HEADER-U
DYNAMIC-BUFFER RECORD u8

public

: WRITE ( -- )
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   NFILE-INPUTS:BYTES$ {: rows:ptr rowsu:n :}
   HEADER-U rootu + rowsu + RECORD-RESERVE
   0 RECORD {: rec:ptr :}
   s" HBNSUP1!" drop rec 8 BYTE-COPY
   ENGINE-ID:KEY$ drop rec 8 + 64 BYTE-COPY
   rootu rec 72 + LE:U64!
   rowsu rec 80 + LE:U64!
   root rec HEADER-U + rootu BYTE-COPY
   rows rec HEADER-U + rootu + rowsu BYTE-COPY
   1 SCRIPT-ARGV$ rec HEADER-U rootu + rowsu + WRITE-ALL
   RECORD-RELEASE ;

;package
