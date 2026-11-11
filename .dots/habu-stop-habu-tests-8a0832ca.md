---
title: Stop Habu tests leaking temp dirs into TMPDIR
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T20:11:15.463080+02:00"
---

Problem: on 2026-10-31 Joel's /tmp held 116 GiB in 5,750 entries, 5,591 created Oct 27-31, and many were Habu test leftovers. Named families: native-build-edit-e2e-*-0 (test/compiler/native-build-edit-e2e.f, up to 1.6 GiB each, five in one agent's TMPDIR), native-file-types-* (11-15 copies of each fixture), native-file-retired-* (test/native-file-retired-e2e.f), hb-lsp-entry-* (58), habu-zip-source-* (64), hb-generations-* (41), habu-lsp-test-* (39), habu-aio-* (31), hb-build-native-* (25), hb-native-smoke-* (14), prefix-provided-unfilled-* (33), check-drivers-* (13) and .tmp-<n>-0 files. Agents also trashed d/d/d fixture trees 450-2,713 levels deep, which Finder cannot delete (their scratch held fs-mutate-test.f). It is not known which of these leak from passing runs and which from failed or killed runs. Acceptance: (1) a census naming each test that leaves anything in its TMPDIR after a passing run, a failing run and a killed run; (2) the owning layer fixed so that a complete test run, pass or fail, leaves its TMPDIR as it found it, and a killed run's leftovers are removed by the next run instead of accumulating; (3) a check in the suite that fails when a run leaves anything behind. Files: test harness and the tests the census names. Verify: run the gate with a fresh empty TMPDIR and list it afterwards; repeat with one failing and one killed test. Depends: none. Ownership: as the census names. Claim: agent=carl workspace=.jj-ws.noindex/carl-xos3 (census: ~/.cache/tmp/carl-census/run1).

Note 2026-11-05: test/compiler/native-build-edit-e2e.f, test/native-file-retired-e2e.f and test/prefix-provided-unfilled.f are deleted (dot habu-keep-only-the-4b091d0e), so their families no longer accumulate.

Cause of the habu-zip-source-* family (2026-11-10, /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/seal-resolution-review.md finding 1): a build whose top level keeps a zip archive open leaves the archive's snapshot in TMPDIR. src/lib/zip.f:33-37 makes it with FS:MKTEMP, which registers nothing for cleanup (src/lib/fs-mutate.f:879-886); it is unlinked only when the archive is discarded (src/lib/zip-ffi.f:65 DISCARD-NODE). Reproducer: `TMPDIR=<empty dir> HABU_OUT=<empty dir> bin/hb --load tools/hb-build.f -- test/fixtures/file-objects/kept-main.f -o <dir>/prog </dev/null` ends rc 0 and leaves habu-zip-source-<n>-0 (682 bytes) in TMPDIR. test/file-objects.f's BUILD-RC now sets TMPDIR to its own removed directory, so a direct run of that test no longer shows it.

Acceptance case 2026-11-10 (bob; merged from habu-census-test-temp-99730692 "Census test temp roots that no cleanup removes", one cause with this dot): 262 FS:MKTEMP-DIR sites under test/ and tools/ against 26 FS:REMOVE-TREE and about 20 FS:CLEANUP-RUN users; test/native-file-e2e.f leaked 9 GB in a day before registering its root (habu-remove-the-native). List each helper or row whose temp root reaches process exit unregistered, and fix each at its helper with FS:CLEANUP-TREE+.


Merged from habu-remove-fixture-directories-75e60ce7 (Remove fixture directories tests leave under TMPDIR), 2026-11-10:

stuart observed (2026-10-31) that src/lib/jsonl-test.f, tools/lsp-test.f and tools/lsp-entry-test.f leave their fixture directories under TMPDIR after a passing run. Each test removes what it creates. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-xos3 (census: ~/.cache/tmp/carl-census/run1).

Census of passing runs (carl, tree 26790de5; ~/.cache/tmp/carl-census/run1/result.tsv): each of the suite's 317 files run directly with its own empty TMPDIR, all rc 0. Nine leave their temp root: test/xml-owner-e2e.f, src/lib/aio-test.f, test/zip-temp-e2e.f, test/zlib-source.f (24 MB), src/lib/jsonl-test.f, test/deep-cwd-e2e.f, test/include-refusal-e2e.f, test/boot-relocation-e2e.f (6 MB), test/structure-opaque-e2e.f. The other 308 leave nothing. Lane: workspace .jj-ws.noindex/carl-tmp, brief .jj-ws.noindex/carl-layout/.out/briefs/tmp-nine-tests.md. Still to census: a failing run and a killed run; then acceptance (2) for killed runs and (3) the suite's check.
