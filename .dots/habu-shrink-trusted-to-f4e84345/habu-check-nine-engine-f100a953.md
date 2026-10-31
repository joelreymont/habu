---
title: Check six engine words that need no trust
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:08:03.776422+02:00"
closed-at: "2026-10-31T14:16:13.348088+02:00"
close-reason: "Landed b4404618: REGISTER-TRUST, POSITION and DEF-HERE are :, ENTRY-BYTES, RAW-PTR and CREATED-XT private casts. MULTI-ERR-BEGIN and CHECK-BODY wait on habu-publish-json-diags-116b6142. Full suite 510/510 and x86-64 cross-build rc 0 on the batch stack."
---

Problem: six engine TRUSTED: words check as : or CAST: (census tc2): REGISTER-TRUST, POSITION, DEF-HERE (definition.f, source-exec.f, definers.f), and ENTRY-BYTES, RAW-PTR, CREATED-XT as private casts (shadow.f, text-sites-x64.f, cell-body.f). MULTI-ERR-BEGIN and CHECK-BODY stay TRUSTED: until the checker owns their verdict and recovery decisions (habu-publish-json-diags-116b6142); MULTI-ERROR? moved to habu-row-the-checker-6eccc828. Acceptance: the six are : or CAST:, engine build and x86-64 cross-build pass, rows pass. Files: definition.f, source-exec.f, definers.f, shadow.f, text-sites-x64.f, cell-body.f. Verify: engine build; x86-64 cross-build (definers.f is not in the arm64 engine); rows cell-body, compiler-shadow, compiler-native-create-does, compiler-native-generated-constructor-aot, program-diagnostics, tool-boundary-check-repair, check-verify-batch, check-verify. Depends: none. Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tc5
