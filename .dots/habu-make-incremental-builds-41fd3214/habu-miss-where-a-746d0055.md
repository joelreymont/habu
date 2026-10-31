---
title: "Miss where a child changed a file's own namespace"
status: active
priority: 1
issue-type: task
created-at: "2026-10-31T12:12:25.136936+02:00"
---

Lane 11 finding (frame STALE-CON): after wlt.f is edited to give WLS its private wordlist, the stale warm build reuses wls.f's pre-edit object (2646 bytes) while a cold build writes a different one under the same key (2558 bytes); wls.f's second segment read the package decision on its own namespace that the child changed, and no frame row recorded it. wlsk=7 in warm and cold (behaviour matched by luck of install). Fix lane brief-t11-wlsfix.md, worker in .jj-ws.noindex/carl-t11t @ syoqyuny on utnlqywz; squashes into lane 11. Result: no fix needed. With a lookup in the reopened namespace (wls.f's second segment `: B ( -- n ) WLV ;`, the edited wlt.f adding a WLV to WLS's private wordlist), the stale warm build misses wls.f at head B, compiles it whole and prints wlsb=4 as cold does; without a lookup the reused object differs from cold's but the product matches (warm product == fresh-cache product holds). Test commit syoqyuny "Test a lookup in a namespace a child edited" (frame e2e: wlsb asserts, wls.f NOT-REUSED kept) is stacked in carl-l11r and squashes into lane 11; close at landing. Claim: agent=carl workspace=.jj-ws.noindex/carl-l11r.
