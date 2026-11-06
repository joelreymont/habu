---
title: "File objects L2d: write each piece's DATA and STRINGS"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T13:06:11.994603+02:00"
---

Problem: a linked file must re-lay its DATA and strings (design section 2 items 2-3; section 3 DATA, STRINGS). Acceptance: each piece's DATA holds the bytes it laid from a cell boundary (src/core/include.f:2001-2007) and its address cells (offset; xt, data or scratch; reference: a DATA cell by (file, offset in that file's DATA) or a header offset); STRINGS lists the offset and length of each string laid in that DATA (src/habu/intern.f:232-237); no address appears. Files: src/habu/linker.f, test/file-objects.f. Verify: test/file-objects.f (two cold builds in two fresh hb processes write byte-identical objects; the program still prints Hi and ok), test/linker.f, test/compiler/arm64-link-run.f. Depends: L2b. Ownership: carl (lead), worker lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-fobj-l2d.
