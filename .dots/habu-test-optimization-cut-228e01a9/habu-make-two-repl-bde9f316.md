---
title: Make two --repl app builds byte-identical
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-30T18:14:02.912090+02:00\\\"\""
closed-at: "2026-10-01T12:54:48.053191+02:00"
close-reason: Fixed by ypmuxysm 21655bcb (r4-repl lane, review 127 ACCEPT)
---

Problem: two `bin/hb --load tools/hb-build.f -- --repl prog.f -o out` builds from one tree, one engine (d40cc36d), the same HB_TMP, HABU_BUILD_CACHE and output paths, on one host, differ in 158 bytes: heap-address-like cells in __TEXT near offsets 4376747-4377028 and 4390890-4391948, one byte near 4904276, a 13-byte run near 5319852, and the code-signature hashes after 5628571 (which follow from the rest). Stripped (AOT), object-relink and cache-restore images from the same runs are byte-identical, and docs/bootstrap.md holds engine builds to the same rule ("Two builds by the same host are byte-identical"; dot habu-make-the-engine-9db99082 fixed one process-local cell there). A process-local value baked into an image is either stale on load or a reproducibility break. Seen by the hb-build tier-0 lane (change qzxmwqpo), present on the parent. Acceptance: each differing cell is named (the word or record that writes it and why its value is process-local); every one that has a reader in the restored image is shown safe or fixed at the layer that writes it; two --repl builds of the same program are byte-identical; every path the image embeds (source, output, HB_TMP, tree root) is named with its reader in the restored image, and one with no reader (a build-scratch path) is not stored: a review of qzxmwqpo saw two builds into output/HB_TMP paths one character apart differ in 278,993 bytes; a gate row builds one program twice with --repl and compares the images. Files: lib/app-image.f, src/habu/aot-capture.f and whatever writes the cells; the test row. Verify: the new row, the app-image and hb-build rows, tools/two-generation-build.f if capture changes. Depends: none. Ownership: --repl image contents. Claim: agent=kestrel workspace=.jj-ws/r4-repl.
