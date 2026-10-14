---
title: Avoid repeated filesystem path checks
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T23:29:12.535638+03:00"
---

Otto measured a Dots walk of about1800 Tender dot files: FS-PATHZ-INTO10.4% and FS-PATH-UNSAFE?8.1% of walktime revalidate paths built from a validateddirectory and kernel-listedname. Syscalls remain lstat34.6%,open20.6%,getdirentries17.5%; C controls readdir56.0M,+lstat79.2M,+mapping82.2M instructions. First measure remaining path-validation cost after reviewed typedentries/context changes; d_type removes many lstat calls. If still material, establish path facts once at responsible public filesystem boundary and reuse them without racy reopen, caller workaround, sharedscratch, certificate/seal/hash framework or unchecked blanket bypass. Preserve NUL/length/error rules and exact failurecause. Own FS library interface; Dots owner migrates. Existing real path rejection/success and listing downstream tests before code, same-tree operation/count/instruction comparison, independent Astra review and proportional gates. Queue after current incremental/CELL release; no speculative implementation before remaining cost established.
