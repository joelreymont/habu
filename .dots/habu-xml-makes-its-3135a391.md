---
title: XML makes its own storage and speaks str and vectors
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:55:06.564946+01:00"
---

Problem: XML:SOURCE-INIT (src/lib/xml/source.f:380) takes caller storage sized by XML:SOURCE-BYTES, so Zen (xlsx.f:830, structure.f:2701, opc.f:451) and Tender (tenderd/eforms.f:390) carve raw cells and cast them; other public XML words take or answer ^u8 n. Joel 2026-11-19: no ^u8 n, text is str, filled bytes a VEC:vector<u8>. Tender's storage dot (stuart, /tmp/stuart-xml-source.md) waits on it. Acceptance: no caller sizes, allocates or casts storage for an XML source; no public XML word takes or answers ^u8 n; Zen and Tender are told the new effects before the install. Design: .out/designs/design-xml-storage.md. Depends: none. Claim: agent=jerry workspace=none (design).
