---
title: Avoid repeated filesystem path checks
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T23:29:12.535638+03:00"
---

Otto measured a Dots walk of about 1,800 Tender dot files: FS-PATHZ-INTO
10.4%, FS-PATH-UNSAFE? 8.1%, lstat 34.6%, open 20.6% and getdirentries
17.5% of walk time. The path percentages may include the same work. The
measurement precedes typed directory entries and reusable listing contexts,
which src/lib/fs-list.f now has (INIT, EACH-TYPED-IN).

Source review found one NUL scan and one copy per lstat, not two validations.
Dots joins copied entry names into a mutable path; its join does not validate
NUL. FS:path carries a pointer and length, without ownership or immutability,
and a traversal callback can mutate the path before stat, so skipping
validation or caching a validated flag would be unsound. Do not introduce a
validated-path type or an unchecked bypass.

Next, the Dots owner migrates the walk to typed entries and reusable contexts
and reprofiles the same tree. Known d_type entries remove lstat; unknown
entries keep its checks. Keep this task open until that measurement
establishes whether a useful residual optimization exists. Other repeated path
checks in READLINK, writes and WRITABLE-ROOT are outside the measured walk.

Any justified change must preserve path length limits, embedded-NUL refusal
before copying, capacity/terminator error distinctions, empty-path OS behavior,
copy overlap semantics and exact syscall failure cause. If validation is
reused across mutation, a successful call followed by an interior-NUL mutation
must refuse rather than open a truncated prefix; traversal callback mutation
must also refuse. Habu owns the library; Dots owns caller migration. Queue
after the incremental/CELL release.
