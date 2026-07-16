\ check-hook.f - default native source checker hook and certificate checkpoint.

package LOWER-CERT-HOOK

70 constant CHECK-RC

: REPORT-UNCHECKABLE ( n -- n )
   dup 1 = JSON-DIAGS @ 0= and DIAGXT @ 0 <> and if DIAGXT @ execute then ;

1 constant REJECT-ACTION
2 constant EXECUTE-ACTION
3 constant SKIP-ACTION

public

: PREFLIGHT ( ptr u8 n n n bool -- n ) {: body:ptr bodyu:n xt:n gen:n replay:bool :}
   xt gen UNMODELED-IMM-XT? if
      body bodyu CHECK 0= if REJECT-ACTION exit then
      s" immediate hook: classifier/checker mismatch" 76 die
   then
   replay if
      xt gen REPLAY-IMM-XT? if EXECUTE-ACTION else SKIP-ACTION then
      exit
   then
   EXECUTE-ACTION ;

\ In multi-error mode CHECK already emitted the diagnostic, counted the reject,
\ and trusted the declared signature. Return -1 so the native publishes the
\ definition (a non-zero hook return commits it; zero rejects and unpublishes
\ it) — the name must resolve for later definitions to keep checking. The body
\ is compiled but never run on a check-only load; the driver exits nonzero via
\ MULTI-ERR-END.
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

TRUSTED: INSTALL ( -- )
   ['] LOWER-CERT-HOOK:PREFLIGHT ['] LOWER-CERT-HOOK:HOOK set-checks ;

INSTALL

;package
