---
title: Check a warm build after a kernel change equals a cold one
status: closed
priority: 1
issue-type: task
created-at: "2026-11-10T06:34:27.590816+02:00"
closed-at: "2026-11-10T06:39:04.493055+02:00"
close-reason: warm equals cold after a kernel size change and a kernel value change; uncovered cases moved to their own dot
---

Problem: nothing in a file object's key names the kernel object (src/habu/linker.f KEY-SUM :369), so after an edit that changes the kernel object a warm build may link objects made against the old kernel. Not examined by the kernel reuse review (2026-11-10). Acceptance: after an edit that changes the kernel object's bytes, a warm build equals a cold build of the edited tree before LC_CODE_SIGNATURE and in its objects, or the cause is reported with path:line. Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-kchange (root-cause hunt).

Result (carl worker, 2026-11-10, base 4cbd8bf3, installed hb): warm equals cold after both kernel edits. Edit 1 (a NOP in src/arch/arm64/boot.f ENGINE-START, kernel object 8 bytes longer) and edit 2 (DP-CEILING-OFF lowered in kernel-object.f, 33 bytes differ): program equal before LC_CODE_SIGNATURE, 336 shared objects equal, maps equal, hb-w prints 3; the warm build rewrote 2 and 1 objects. Why: an object records a kernel call as an offset into the building hb's kernel (linker.f:1013), which the key covers, and each link maps it to the new kernel by primitive name and wordlist (link-kernel.f:260, :480; link-band.f:385); symbols and presets reach the product only at link time (link-data.f:470-504). Not covered: see habu-check-kernel-changes.
