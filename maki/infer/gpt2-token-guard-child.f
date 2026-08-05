\ Guard-page child for the production GPT2:ENCODE chunk reader.

require lib/fs-path.f
require lib/memory.f
require maki/infer/gpt2-generate.f

package GPT2
private

create TG-APOSTROPHE $27 c,
create TG-TRUNC-2-1 $C2 c,
create TG-TRUNC-3-1 $E1 c,
create TG-TRUNC-3-2 $E1 c, $80 c,
create TG-TRUNC-4-1 $F1 c,
create TG-TRUNC-4-2 $F1 c, $80 c,
create TG-TRUNC-4-3 $F1 c, $80 c, $80 c,

: TG-CODE ( result<n,n> -- n )
   MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

: TG-FIRST ( n n -- n ) {: first:n next:n :}
   first 0<> if first else next then ;

: TG-OPEN ( -- GPU:session GPT2:model )
   SCRIPT-ARGC 1 <> if E-STR-BOUNDS throw then
   GPU:OPEN
   MATCH result
      err OF throw ENDOF
      ok OF
         0 SCRIPT-ARGV$ FS-PATH:MAKE OPEN
         MATCH result
            ok OF ENDOF
            err OF
               {: primary:n :}
               GPU:CLOSE TG-CODE
               primary swap TG-FIRST throw
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: TG-CLOSE ( GPU:session GPT2:model -- )
   CLOSE
   TG-CODE {: model-code:n :}
   GPU:CLOSE TG-CODE {: session-code:n :}
   model-code session-code TG-FIRST
   dup 0<> if throw then drop ;

: TG-GUARD! ( ptr u8 -- ) {: region:ptr :}
   region MEM-64K + MEM-64K munmap
   dup 0<> if E-MEM-UNMAP throw then
   drop ;

: TG-PLACE ( ptr u8 ptr u8 n -- ptr u8 )
   {: region:ptr bytes:ptr length:n :}
   region MEM-64K + length - {: source:ptr :}
   bytes source length BYTE-COPY
   source ;

: TG-MAPPED
   ( GPU:session GPT2:model ptr u8 n ptr u8 CAD-NUM:alloc-byte-len -- GPU:session GPT2:model )
   {: bytes:ptr length:n region:ptr region-len:CAD-NUM:alloc-byte-len :}
   region-len drop
   region TG-GUARD!
   region bytes length TG-PLACE {: source:ptr :}
   source length BYTE-CAP ENCODE
   MATCH result
      ok OF IC>N drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: TG-RUN
   ( GPU:session GPT2:model ptr u8 n -- GPU:session GPT2:model )
   MEM-64K 2 * MEM:BYTES-ALLOC-LEN
   [: TG-MAPPED ;] MEM:WITH-BYTES ;

: TG-CASES ( GPU:session GPT2:model -- GPU:session GPT2:model )
   TG-APOSTROPHE 1 TG-RUN
   TG-TRUNC-2-1 1 TG-RUN
   TG-TRUNC-3-1 1 TG-RUN
   TG-TRUNC-3-2 2 TG-RUN
   TG-TRUNC-4-1 1 TG-RUN
   TG-TRUNC-4-2 2 TG-RUN
   TG-TRUNC-4-3 3 TG-RUN ;

TG-OPEN TG-CASES TG-CLOSE

;package
