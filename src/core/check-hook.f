\ check-hook.f - default native source checker hook.

70 constant HOOK-CHECK-RC

: HOOK-REPORT-UNCHECKABLE ( n -- n )
   dup 1 = JSON-DIAGS @ 0= and DIAGXT @ 0 <> and if DIAGXT @ execute then ;

\ In multi-error mode CHECK already emitted the diagnostic, counted the reject,
\ and trusted the declared signature. Return -1 so the native publishes the
\ definition (a non-zero hook return commits it; zero rejects and unpublishes
\ it) — the name must resolve for later definitions to keep checking. The body
\ is compiled but never run on a check-only load; the driver exits nonzero via
\ MULTI-ERR-END.
: HOOK ( ptr u8 n -- n )
   CHECK! HOOK-REPORT-UNCHECKABLE
   MULTI-ERR? if drop -1 exit then
   dup -1 <> if HOOK-CHECK-RC throw then ;

' HOOK set-check
