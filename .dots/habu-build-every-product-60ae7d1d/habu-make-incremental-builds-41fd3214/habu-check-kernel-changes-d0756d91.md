---
title: Check kernel changes a warm build may not see
status: closed
priority: 3
issue-type: task
created-at: "2026-11-10T06:39:04.477962+02:00"
closed-at: "2026-11-10T10:58:54.640847+02:00"
close-reason: "landed; 1. a primitive renamed or removed: warm fails exactly as cold (rc 74, the same 'link: X names Y, neither a product word nor a kernel primitive') for rmdir renamed, rmdir removed and write renamed, the last with its caller util.f's object reused; a row left without its kernel body is refused alike when the kernel object is written. 2. the kernel's data bound lowered to 5,300,000: cold died with 'checker: facts text outside DATA' because FACTS@ bounded hb's own records by the build's ceiling; fixed (CHECK:OPEN keeps hb's ceiling), and cold and warm now both die with 'hb: data space out of range' at end.f. LINK-ADDRESS on no match returns -1 and TARGET-ADDRESS dies (link-band.f:390-401), seen in case 1. No test: the cases need an edited kernel."
---

Problem: two kernel changes were left out of the warm-equals-cold check (dot e9e3bae2). 1. A primitive a saved object calls is removed or renamed: the link maps kernel calls by name (src/arch/arm64/link-kernel.f KEEP :260), and what it does on a miss was not read. 2. DATA-SIZE or the DP ceiling lowered so a fresh compile would overflow: the compile area's bounds come from the kernel (src/habu/linker.f:133-143) and are not in a file object's key, so a warm build may pass where a cold one fails. Also unread: LINK-ADDRESS (src/habu/link-band.f:385) given a host address that matches no primitive entry. Acceptance: for each, a warm build fails or succeeds exactly as a cold build of the edited tree does, or the cause is fixed. Workspace: none yet.

Claim: agent=jerry workspaces=.jj-ws.noindex/jerry-kern1 (a primitive removed) and .jj-ws.noindex/jerry-kern2 (the data size lowered): two root-cause lanes, 2026-11-10; a fix lane follows each cause.
