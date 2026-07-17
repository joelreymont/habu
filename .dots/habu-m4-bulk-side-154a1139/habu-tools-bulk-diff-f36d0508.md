---
title: "Tools: bulk diff content scan"
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T12:09:51.775407+02:00"
blocks:
  - habu-m3-authenticated-modular-ed51c697
---

Full context: the framed-diff producer must independently declare content/body/form, but jj 0.37 TreeEntry templates expose no content ID, size, or bytes. Per-file jj file show needs two child processes per changed row (5+2N total), creating unbounded commit-gate startup scaling. Implement one checked Habu external directory-diff scanner invoked once by jj diff --tool: accept materialized left/right roots plus the ordered JSON metadata file, preserve arbitrary Jj RepoPath bytes through parsed length-bearing JSON paths, read file/symlink sides exactly, skip absent/gitlink sides, compute SHA-256, byte size, binary-NUL classification, and emit a deterministic ordered framed side-content artifact with magic/version/count/digest and strict bounds. Match and propagate every FS:stream-outcome; do not leave outcomes on the stack. Add real jj tests for empty/text/binary/symlink/gitlink/absent, duplicate rows, LF/CR and adversarial path bytes, truncation/corruption/count/order/path escape, and prove constant child-process count for N>=2. Register the focused tests in the gate. No Python/shell parser, unstable jj debug tree, line delimiters for paths/content, compatibility API, or edits to diff-capture consumers. Files owned: scanner core/CLI/tests/docs only; leave producer integration and FILEMAP to M5. Claim: agent=bulk-diff-scan workspace=.jj-ws/habu-tools-bulk-diff-f36d0508. Milestone routing: M4 only. Claim released for milestone sequencing; preserve the existing workspace until M4.
