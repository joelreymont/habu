---
title: "SOURCE-ROOT:CD in a loaded file's top level says only at top level"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T13:08:07.017636+02:00"
---

Problem: SOURCE-ROOT:CD /tmp at the top level of a file loaded with --load fails with 'CD: only at top level' and rc 74, though the call is at that file's top level (probe p21 in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases7/card/ex). Found by the review of habu-cut-the-card-6526717c. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the message names the condition CD really requires, or CD accepts a loaded file's top level if that is what its rule means; the hunt says which from the code. Files: the file defining SOURCE-ROOT:CD. Verify: the probe. Depends: none. Ownership: SOURCE-ROOT. Claim: agent=bob workspace=.jj-ws.noindex/habu-src-root-cd-610b1c3d.

Hunt 2026-11-10 (bob): src/core/include.f TOP-ONLY (:567) refuses CD, PUSHPATH and POPPATH inside any open scope, a load's among them, by design (:623: a file scopes a root with WITH); only the message misled. The lane rewords the three messages to name SOURCE-ROOT:WITH and updates test/source-root-exe-test.f:561-563.
