\ check-hook.f - default native source checker hook.

70 constant HOOK-CHECK-RC

: HOOK-REPORT-UNCHECKABLE ( n -- n )
   dup 1 = DIAGXT @ 0 <> and if DIAGXT @ execute then ;

: HOOK ( ptr u8 n -- n )
   CHECK! HOOK-REPORT-UNCHECKABLE dup -1 <> if HOOK-CHECK-RC throw then ;

' HOOK set-check
