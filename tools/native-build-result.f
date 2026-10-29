\ Construction facts returned by the child that actually loaded support.
\ The record: donor engine key (64 hex) | root length u64 | rows length u64 |
\ root | rows (NATIVE-BUILD-IMAGE ACCEPT-RESULT reads it).
require src/lib/engine-id.f
require src/lib/le.f
require src/lib/memory.f
require tools/native-file-inputs.f

package NATIVE-BUILD-RESULT
private

80 constant HEADER-U
DYNAMIC-BUFFER RECORD u8

public

: WRITE ( -- )
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   NFILE-INPUTS:BYTES$ {: rows:ptr rowsu:n :}
   HEADER-U rootu + rowsu + RECORD-RESERVE
   0 RECORD {: rec:ptr :}
   ENGINE-ID:KEY$ drop rec 64 BYTE-COPY
   rootu rec 64 + LE:U64!
   rowsu rec 72 + LE:U64!
   root rec HEADER-U + rootu BYTE-COPY
   rows rec HEADER-U + rootu + rowsu BYTE-COPY
   1 SCRIPT-ARGV$ rec HEADER-U rootu + rowsu + FS:WRITE-ALL
   RECORD-RELEASE ;

;package
