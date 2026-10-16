---
title: Classify non-ZIP input as open failure
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T10:48:09.188390+03:00"
closed-at: "2026-10-16T11:32:58.467714+03:00"
close-reason: "Qualified source-only ZIP E-OPEN fix: ZIP, removal, ZIP/XML source checks and Tom 21/23-byte repro pass; stripped public removal consumer builds and runs."
---

Public 600eb464-libs-b42eccf6 returns E-OPEN for a17byte nonarchive but E-READ for23bytes because raw FIND-END runs before libzip. Tom retained Habu-only repro ~/.cache/tmp/tom/56094501-repro/zip-nonarchive.f. Nonarchives of any length must refuse E-OPEN, preserving genuine read/corrupt-entry errors and public ZIP semantics. Extend existing nonarchive ZIP behavior case with22+bytes before fixing, run focused ZIP and source/native consumer checks, independent Astra review, then source-only qualified release.
