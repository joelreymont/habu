---
title: "Convert the Mac's Habu install to a tree behind a link"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T00:16:17.472532+01:00"
---

Problem: tools/install.f CHECK (:103-106) refuses a host whose ~/.local/lib/habu is still a directory; spark is converted, the Mac is not, so its next install stops there. Acceptance: on the Mac ~/.local/lib/habu is a link into habu-trees and an install from a qualified tree succeeds there; with no host left to meet it, CHECK's link test and its message are deleted. Depends: none. Claim: unassigned.
