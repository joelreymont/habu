---
title: Resolve qualified TRUST rows against the dictionary
status: closed
priority: 2
issue-type: task
created-at: "2026-10-03T21:50:03.223386+03:00"
closed-at: "2026-10-04T09:04:01.020336+02:00"
close-reason: "Resolved by 46f25064 (qualtrust): a qualified TRUST row resolves against its package; NOPKG:NOSUCH refuses with E-TRUST-UNRESOLVED 7143, rc 70; test/trust-row-test.f rc 0"
---

Found by lane 537 (dot 8a8912cc; owner 3913fe54 was superseded into a campaign with no child): src/core/checker.f TRUST-RESOLVES? (~:17008, comment ~:16995) accepts every qualified spelling because PKG:TAIL has no resolver in the boot prefix. Measured on 9084b558: `s" NOPKG:NOSUCH" s" -- n" TRUST` rc 0, while the bare `s" NOSUCHWORD" s" -- n" TRUST` is refused E-TRUST-UNRESOLVED rc 67 (probes $HOME/.cache/tmp/kestrel-jerry-defectcmt/p2/tr1.f, tr2.f). A trust row for a word that does not exist is a soundness gap. Acceptance: an unknown qualified row (missing package, or missing or private tail of a real package) is refused as a bare one is; a real public of a closed package is admitted; the probe pair seen failing first through the real load path; the comment at ~:16995 names no missing dot; baked: rebuild, g1 == g2, two-generation build.
