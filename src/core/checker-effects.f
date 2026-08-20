\ checker-effects.f - checker rows for the checker words the type registry calls.
\
\ src/core/checker.f loads before src/core/check-hook.f installs the hook, so
\ none of its definitions records a signature of its own. The type foundation
\ (type-schema.f, type-family.f, sumtype.f, layout-buffer.f, layout-valid.f) now
\ loads AFTER the hook and is checked, so every checker word it calls needs a row
\ before it can be called from checked source. Each row below is copied from the
\ owning definition's own header in checker.f - that header is the single
\ authority, and a row that disagrees with it is a bug in this file.
\
\ These rows replace 58 one-token TRUSTED: forwarders that structure-decl.f and
\ enum-decl.f used to carry for the same purpose, each re-declaring a signature
\ its own file did not own.
\
\ Retirement: habu-seal-the-checker-5314c0ab. Once the checker has real package
\ owners these become PPRIM: axioms under those packages, published by the owner
\ rather than republished here.

\ --- the verdict and its diagnostics.
s" RES-TRUE" s" -- bool" TRUST
s" RES-FALSE" s" -- bool" TRUST
s" CHECK-REJECT!" s" --" TRUST
s" MULTI-ERR?" s" -- bool" TRUST
s" MULTI-ERR-COUNT+" s" --" TRUST
s" MDIAG!" s" n --" TRUST
s" MD-CON-FAM" s" -- n" TRUST
s" MD-CON-KIND" s" -- n" TRUST
s" MD-CON-VAR" s" -- n" TRUST
s" MD-FAM-UNKNOWN" s" -- n" TRUST
s" MD-FAM-KIND" s" -- n" TRUST
s" CONSTRUCT-WIDE-STAGED-REJECT" s" --" TRUST

\ --- type terms: minting, projecting, and stepping the checked stack.
s" FRESH" s" -- n" TRUST
s" MK-CON" s" n -- n" TRUST
s" MK-VAR" s" n -- n" TRUST
s" MK-PTR" s" n -- n" TRUST
s" MK-ROW" s" n -- n" TRUST
s" MK-PARAM" s" n ptr u8 n n -- n" TRUST
s" MK-QUOT" s" n n n n -- n" TRUST
s" PUSH-LOGICAL" s" n n -- n" TRUST
s" T-RES" s" n -- n" TRUST
s" T-WIDTH" s" n -- n" TRUST
s" TYPE-CLOSED?" s" n -- bool" TRUST
s" PARAM>FAM" s" n -- n" TRUST
s" PARAM>ARG" s" n n -- n" TRUST
s" PARAM>ARGC" s" n -- n" TRUST
s" PARAM-SCR+" s" n --" TRUST
s" PARAM-SCR-N@" s" -- n" TRUST
s" CHECKER-STEP" s" n n --" TRUST

\ --- the signature lexer and the shared token fold.
s" SIG-SCAN!" s" ptr u8 n --" TRUST
s" NEXT-SIG-TOK" s" -- ptr u8 n" TRUST
s" DELIM?" s" ptr u8 n -- bool" TRUST
s" PK!" s" ptr u8 n --" TRUST
s" PKRESET" s" --" TRUST
s" TOKFOLD" s" ptr u8 n -- bool" TRUST
s" TKF$" s" -- ptr u8 n" TRUST
s" CON-OF" s" ptr u8 n -- n" TRUST
s" CC-N" s" -- n" TRUST
s" CC-BOOL" s" -- n" TRUST
s" CC-R" s" -- n" TRUST
s" CT-INT?" s" n -- bool" TRUST
s" CT-LIVE?" s" n -- bool" TRUST
s" CT-LINEAR?" s" n -- bool" TRUST
s" CT-NAME$" s" n -- ptr u8 n" TRUST
s" ATOM-TOK?" s" ptr u8 n -- bool" TRUST
s" FRESH-ATOM-TOK?" s" ptr u8 n -- bool" TRUST
s" VREC-FIND" s" ptr u8 n -- n bool" TRUST

\ --- registry storage and the shared symbol index.
s" REG-GROW1" s" ptr a n n --" TRUST
s" REG-PERSIST-BUF" s" ptr a ptr a n -- bool" TRUST
s" ARENA-CELLS-ZERO" s" ptr a n n --" TRUST
s" ARENA-BYTES-GROW" s" ptr a n n -- ptr a" TRUST
s" USIGS-COPY" s" ptr a ptr a n --" TRUST
s" HIDX-ENSURE" s" --" TRUST
s" HIDX-GEN@" s" -- n" TRUST
s" HIDX-FNV-PRIME" s" -- n" TRUST
s" HIDX-FNV-BASIS" s" -- n" TRUST
s" HT-SVX" s" -- n" TRUST
s" IDX-HEAD@" s" n n -- n" TRUST
s" IDX-HEAD!" s" n n n --" TRUST
s" IDX-SYM-OK" s" n --" TRUST
s" IDX-HEADS-CLEAR" s" n --" TRUST
s" CHECKER-RECORD-SYM" s" ptr u8 n -- n" TRUST

\ --- the wide-layout fact surface.
s" MWIN-CELLS!" s" n --" TRUST
s" WF-XPAD-FLAG" s" -- n" TRUST
s" WF-ADD-FULL" s" n n n n n --" TRUST
s" CONSTRUCT-DECL-LAYOUT" s" n -- n bool" TRUST
s" WF-XPAD?" s" -- bool" TRUST
s" LAYOUT-PARAM?" s" n -- bool" TRUST
s" LBUF-PEND!" s" ptr u8 n --" TRUST
s" LBUF-PEND-CLEAR" s" --" TRUST

\ --- the generated-constructor plan.
s" CTOR-PEND-N@" s" -- n" TRUST
s" CTOR-PEND-CLEAR" s" --" TRUST
s" CTOR-PEND-SYM+" s" n n --" TRUST
s" CTOR-PEND-REQUIRE-DONE" s" --" TRUST
s" CTOR-PEND-REWIND" s" --" TRUST
s" CTOR-PEND-ARENA-BOOT" s" --" TRUST

\ --- package scope and registry housekeeping.
s" CHECKER-PACKAGE-PUBLIC" s" -- n" TRUST
s" FIELD-FAM!" s" n --" TRUST
s" EXT-FREE-CLEAR" s" --" TRUST
s" RBF-SNAP-RESET" s" --" TRUST
