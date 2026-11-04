---
title: "Refuse removing a reserved-name package's word after the seal"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T19:21:05.257379+02:00"
---

Problem: packages reserved by name only (src/habu/habu2.f RESTAB-BUF :2019-2031: tfam, type, match, checker-cert, lower-cert, lower-cert-hook, engine-error) hold no checker entry, so after the seal their words still undefine, while the switch design (design-switch.md Revision 8 item 1, amended: the refusal in XREF:RETIRE, src/habu/xref.f:525, for a record whose wordlist is protected) refuses removal from every sealed package (DECISIONS 2026-11-03, refuse every removal). Acceptance: after the seal, removing a word of a reserved-name package is E-SEALED-PACKAGE on every removal path, with a rejected-program row; RESTAB-BUF's lower-cert-hook entry follows the switch lane's rename of LOWER-CERT-HOOK to CHECK. Files: src/habu/habu2.f, src/habu/xref.f. Verify: test/package-seal.f and the undefine rows of tools/check-verify-test-lib.f. Depends: the switch lane that adds the refusal to XREF:RETIRE (dot habu-remove-the-checker-4425bcef). Ownership: those files. Claim: unassigned.
