\ lower-cert-effects.f - checker rows for the lowering-certificate ABI.
\
\ package LOWER-CERT is written in two files. src/core/lower-cert-base.f holds
\ the ABI constants, the certificate buffer and the boot-safe dispatcher, and it
\ must load BEFORE src/core/check-hook.f because the checker asks it for a
\ certificate at every publish. src/core/layout-valid.f holds the full producer
\ and loads AFTER the hook, with the type registry it reads. So the package's own
\ words cross the hook, and the half that is checked needs rows for the half that
\ is not. Every row is copied from the owning definition's own header.
\
\ These rows are package-scoped: they sit before `public`, so nothing outside
\ LOWER-CERT gains any visibility it did not already have.
\
\ Retirement: habu-primitive-effect-axiom-1119f176, or sooner if the certificate
\ dispatcher stops having to be armed before the hook.

package LOWER-CERT

s" MAGIC-V" s" -- n" TRUST
s" VERSION-V" s" -- n" TRUST
s" HEADER-N" s" -- n" TRUST
s" WF-NCELLS" s" -- n" TRUST
s" FETCH-NCELLS" s" -- n" TRUST
s" FETCH-FLAG" s" -- n" TRUST
s" GROW-CAP" s" n n -- n" TRUST
s" BUF-ENSURE" s" n --" TRUST
s" BUF," s" n --" TRUST
s" BUF-N@" s" -- n" TRUST
s" BUF-REWIND" s" --" TRUST
s" SOURCE!" s" ptr u8 n --" TRUST
s" BODY-LEN@" s" -- n" TRUST
s" BODY-HASH@" s" -- n" TRUST
s" EMPTY" s" ptr u8 n --" TRUST
s" FULL-INSTALL" s" [ ptr u8 n n -- ] --" TRUST

;package
