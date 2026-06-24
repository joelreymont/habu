\ check-hook.f - default native source checker hook.

70 constant HOOK-CHECK-RC

: HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> if HOOK-CHECK-RC throw then ;
