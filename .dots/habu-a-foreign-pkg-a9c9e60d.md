---
title: A foreign package forges or drops a linear token through a private pointer mint
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T22:36:06.170443+02:00"
---

Problem: docs/forth-card.md:446-450 says only the package that declared a DEFLINEAR type mints or erases its token, but another package does both through memory: a private CAST: >CELLS ( n -- ptr n ) applied to HK NULL-PTR - (HK a TYPED-VARIABLE typed [ -- Q:tok ]) stores the xt of a quotation cast to n (CAST: ( [ -- n ] -- n )), and HK @ execute then yields a Q:tok the package never minted. Probes ~/.cache/tmp/carl-vcast/design/e1.f (prints 42: a forged token reaches Q:CLOSE) and e2.f (prints lost: a token is dropped without Q's ERASE). Acceptance: both probes are refused at load, with a message naming the laundering step; a package's own private mints and its own linear rows still load. Depends: none. Claim: unassigned, owner carl.
