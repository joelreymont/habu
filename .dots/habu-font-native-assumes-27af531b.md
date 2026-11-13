---
title: "font-native assumes Ubuntu's font paths"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T21:32:01.690317+01:00"
---

Problem: test/font-native.f FONT-PATH$, LIGATURE-PATH$, CFF-PATH$ and COLOR-PATH$ (:17-39) name Debian/Ubuntu paths under /usr/share/fonts/truetype and /usr/share/fonts/opentype; on Arch Linux the same fonts sit under /usr/share/fonts/TTF, gsfonts and noto, so the test throws -2101 with no output. Acceptance: font-native finds its fonts on any Linux that has them installed and passes on Ubuntu and Arch. Depends: none. Claim: unassigned.
