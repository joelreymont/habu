---
title: Guard owner name rewind
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T06:25:42.138854+02:00"
---

Problem: raw cp! can rewind below external name bytes still referenced by a live owner marker, then later emission overwrites the package identity while NDICT survives. Result: consume OWNER-GUARD:REC? from the marker leaf. BCPSET scans live marker records with DNAME-EXT and rejects when requested CP lies below the aligned end of a referenced name span. The marker and canonical namespace share that pointer, so one check protects both. Preserve the task-live and protected-code guards in their current order. Owner: native cp! sink only; bootstrap uses the same builder path. Production red: close a directly flagged package whose name exceeds DNAME-INL, rewind CP below its name with cp!, emit code over it, and reopen the package. Acceptance: the real child path fails closed before CP, name bytes, permissions, or checker state change; CP equal to the aligned end and every later pointer remains legal; inline owner names and unmarked cp!/FORGET behavior remain exact; source, snapshot, refresh, and fixpoint pass. Forbidden: name-length limit, copied name bytes, registry, hash, general rewind policy, AOT DNAME-EXT work, compatibility, or lint. Smallest owning check: direct cp! one byte below and exactly at the long-name floor rejects then succeeds without other mutation. Claim: unassigned.
