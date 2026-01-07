---
title: Generate dSYM bundles for lldb debugging
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-08T14:20:18.530907+02:00"
closed-at: "2025-12-08T14:23:05.004965+02:00"
close-reason: ""
---

Implement proper dSYM bundle generation that wraps DWARF sections in a Mach-O file. Currently write-dsym-bundle just writes raw DWARF data. Need to create a proper dSYM bundle structure:
- output.dSYM/Contents/Info.plist
- output.dSYM/Contents/Resources/DWARF/binary-name (Mach-O with DWARF sections)
