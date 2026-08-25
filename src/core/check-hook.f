\ check-hook.f - default native source checker hook and certificate checkpoint.

package LOWER-CERT-HOOK

70 constant CHECK-RC

: REPORT-UNCHECKABLE ( n -- n )
   dup 1 = JSON-DIAGS @ 0= and DIAG-QUIET @ 0= and if DIAGXT then ;

: PREFLIGHT ( ptr u8 n ptr u8 n bool -- )
   {: ba:ptr bu:n ta:ptr tu:n trusted:bool :}
   trusted if exit then
   ta tu CHECKER-PREFLIGHT:MODELED? if exit then
   ba bu ta tu CHECKER-PREFLIGHT:CHECK! 0 <> if
      s" checker: preflight did not reject unmodeled immediate" 76 die
   then
   CHECK-RC throw ;

\ In multi-error mode CHECK already emitted the diagnostic, counted the reject,
\ and trusted the declared signature. Return -1 so the native publishes the
\ definition (a non-zero hook return commits it; zero rejects and unpublishes
\ it) — the name must resolve for later definitions to keep checking. The body
\ is compiled but never run on a check-only load; the driver exits nonzero via
\ MULTI-ERR-END.
public

: HOOK ( ptr u8 n -- n )
   CHECK! REPORT-UNCHECKABLE
   MULTI-ERR? if drop -1 exit then
   dup -1 <> if
      \ never die bare: with no DIAGXT installed an uncheckable (verdict 1)
      \ definition used to exit rc 70 with NO diagnostic - an opaque exit in
      \ fork/gate captures. Name the definition and its failing token to fd 2.
      2 s" hook: non-certified definition: " write drop
      2 NMB NMU @ write drop
      2 s"  at '" write drop
      2 FAILTK FAILTU @ write drop
      2 S\" '\n" write drop
      CHECK-RC throw then ;

\ Dynamic preflight/hook installation asserts the canonical checker identities;
\ ordinary checked code cannot type execution-token installation.
\ Retirement: cap:checker-hook-identity.
TRUSTED: INSTALL ( -- )
   ['] PREFLIGHT set-preflight
   ['] HOOK set-check ;

\ The bare INSTALL call sits between the two unique PTD-HOOK-BLANK sentinels so
\ test/pre-trust-defer.f can blank exactly this region and reach the RUNTIME
\ backstop it is testing. Blanking the pre-trust drain alone never reaches that
\ backstop: the checker rejects the first checked `is` on an undrained pre-trust
\ defer long before it (src/habu/xref.f INSTALL, exit 70). Removing the hook for
\ that one case isolates the runtime property — a non-empty pending table at
\ SEAL-CAPTURE refuses the boot at exit 73 — from the checker's earlier refusal,
\ which that fixture asserts as its own case. Keep the sentinels contiguous with
\ the call.
\ PTD-HOOK-BLANK-BEGIN
INSTALL
\ PTD-HOOK-BLANK-END

;package
