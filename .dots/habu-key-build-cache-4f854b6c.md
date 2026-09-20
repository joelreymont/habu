---
title: Key build-cache artifacts on tree-relative paths
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T13:24:25.055509+03:00"
---

Problem: lib/content-key.f FILE+ folds the absolute pathname (CK-FILE-TAG a u) into the preimage, so every workspace of the same tree keys a different whitebox and cold host and pays the 70 s build again; now that the whitebox key folds the builder's 159-file closure (7967a5b8) the cost is paid per workspace (whitebox-key lane, 2026-09-17). Acceptance: the key folds tree-relative paths plus content, two workspaces at the same tree content resolve the same artifact path in ~/.cache/habu-build, an edited file still moves the key, and the existing key fixtures pass. Files: lib/content-key.f, test/cold-engine.f, test/whitebox-engine.f, test/whitebox-engine-key-test.f. Verify: the fixtures; two workspaces at one commit share the host. Depends: none. Ownership: gate harness. Claim: unassigned.

Claim: alder, .jj-ws/alder-relative-cache on 1afd910c. Separate a file's logical
key name from the physical path used for reading and the digest cache; retain
FILE+'s existing identity contract for other callers. The closure already owns
each file's resolution root, so expose its relative name and use that for the
whitebox/cold keys. The host input is named consistently and keyed by bytes.
Fixture: the existing copied-closure key equals the original's; content edits
and restores still move and restore that key. No engine/compiler edits.

Implemented: FILE-NAMED+ separates the logical key name from the physical read
and digest-cache path; FILE+ keeps its old contract. EC:NAME$ is relative to the
root that resolved each member. Whitebox/cold keys use those names and a stable
host name plus the host bytes, with bumped cache versions.

Proof: the copied-closure equality fixture failed before the change (assert 5)
and passes after it; the existing edit/restore/unloaded-file fixtures pass. Two
identical private trees and separately copied hosts resolve the same whitebox
and cold artifacts; the second lookup takes 0.45 s and preserves both files'
size, mtime and inode. Content-key fixtures also pin distinct logical names,
changed bytes and the unchanged FILE+ contract. All 21 focused registry rows
pass: content-key-cache, event-closure, source-discovery, object-image-writer,
source-root, hb-build-fixtures, whitebox-engine-key, whitebox-engine,
cold-argv-separator, aot-named-cells-image and all eleven aot-wid modes.
Evidence: /tmp/alder-relative-cache. Independent Astra review: no findings.
The build-fixpoint-fixtures row invokes install --force and remains for Hazel's
serial gate under the no-install rule; no separate full gate was run.
