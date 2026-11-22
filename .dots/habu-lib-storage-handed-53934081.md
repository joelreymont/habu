---
title: Library storage handed to callers as a byte size becomes a typed record
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T08:06:18.025080+02:00"
---

Problem: libraries make callers allot storage by a byte size they export, against docs/design.md:19-21. Left after the token lanes: src/lib/fs-list.f FS-LIST:STORAGE-BYTES (:40) and the caller-owned context API it sizes (INIT :214, which checks the span's size and alignment at run time, EACH-IN :223, EACH-TYPED-IN :232, EACH-TYPED-AT :243, NAMES-IN :252 and the context type); no program calls them (rg -i over every repository on spark finds only fs-list-test.f), while EACH, EACH-TYPED and NAMES map their own. SSE:STORAGE-BYTES and xmodem SESSION-BYTES go with habu-give-the-xmodem-5152c3f9, SIPHASH:STATE-CELLS with habu-let-lib-tokens-e95db608/habu-give-an-xml-4cef65cb. Acceptance: the context API, STORAGE-BYTES's export and their tests are deleted; no exported byte size for caller storage remains in src/lib (`rg -n -e '^EXPORT .*(BYTES|CELLS)' -e 'constant [A-Z-]*STORAGE-BYTES' src/lib` finds only module-private task-local rows); the fs-list tests pass. Depends: none. Claim: unassigned.
