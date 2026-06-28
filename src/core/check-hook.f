\ check-hook.f - default native source checker hook.

70 constant HOOK-CHECK-RC

: HOOK-REPORT-UNCHECKABLE ( n -- n )
   dup 1 = DIAGXT @ 0 <> and if DIAGXT @ execute then ;

: HOOK ( ptr u8 n -- n )
   CHECK! HOOK-REPORT-UNCHECKABLE dup -1 <> if HOOK-CHECK-RC throw then ;

' HOOK set-check

s" CORE-STR=" s" ptr u8 n ptr u8 n -- bool" TRUST
s" PATHZ" s" ptr u8 n ptr u8 --" TRUST
s" PATH0" s" ptr u8 n -- ptr u8" TRUST
s" RD32" s" ptr u8 -- n" TRUST
s" DIAG-FILE!" s" ptr u8 n --" TRUST
s" DIAG-ORIGIN!" s" n n n --" TRUST
s" DIAG-JSON!" s" bool --" TRUST
s" CHECK-CANDIDATE!" s" ptr u8 n -- n" TRUST
s" CHECKER-CANDIDATE-SCOPE-START" s" --" TRUST
s" CHECKER-CANDIDATE-SCOPE-DONE" s" --" TRUST
s" CHECKER-USIGS-TRUNCATE-FROM" s" ptr u8 n --" TRUST
s" CHECKER-UNDEFINE" s" ptr u8 n --" TRUST
s" CHECKER-DEFER" s" ptr u8 n --" TRUST
s" CHECKER-PACKAGE" s" ptr u8 n --" TRUST
s" CHECKER-PUBLIC" s" --" TRUST
s" CHECKER-PRIVATE" s" --" TRUST
s" CHECKER-END-PACKAGE" s" --" TRUST
