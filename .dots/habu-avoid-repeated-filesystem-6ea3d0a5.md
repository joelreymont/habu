---
title: Avoid repeated filesystem path checks
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T23:29:12.535638+03:00"
---

Otto measured a Dots walk of about 1,800 Tender dot files: FS-PATHZ-INTO
10.4%, FS-PATH-UNSAFE? 8.1%, lstat 34.6%, open 20.6% and getdirentries
17.5% of walk time. The path percentages may include the same work. C controls
used 56.0M instructions for readdir, 79.2M with lstat and 82.2M with listing
map/unmap. These measurements precede typed directory entries and reusable
listing contexts.

Independent source review found one NUL scan and one copy per current lstat,
not two validations. Dots joins copied entry names into a mutable path; its
join does not validate NUL. FS:path carries a pointer and length, without
ownership or immutability. A traversal callback can mutate the path before
stat. Skipping validation or caching a validated flag would therefore be
unsound. Do not introduce a validated-path type or an unchecked bypass.

First qualify and publish typed entries (habu-expose-dir-entry-228824ad) and
reusable contexts (habu-reduce-dir-listing-e14f5e55), then have the Dots owner
migrate and reprofile the same tree. Known d_type entries remove lstat; unknown
entries retain its current checks. Keep this task open until that measurement
establishes whether a useful residual optimization exists. Other repeated path
checks in READLINK, writes and WRITABLE-ROOT are outside the measured walk.

Any justified change must preserve path length limits, embedded-NUL refusal
before copying, capacity/terminator error distinctions, empty-path OS behavior,
copy overlap semantics and exact syscall failure cause. Inspect existing FS,
mutation, listing and cause tests before adding coverage. If validation is
reused across mutation, a successful call followed by an interior-NUL mutation
must refuse rather than open a truncated prefix; traversal callback mutation
must also refuse. Measure the changed real operation and outputs, obtain
independent Astra review and run proportional gates. Habu owns the library;
Dots owns caller migration. Queue after the incremental/CELL release.
