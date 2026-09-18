---
title: Make build-fixpoint install produce a usable engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T13:38:13.305569+03:00"
---

Problem: three lanes on 2026-09-18 rebuilt with 'tools/build-fixpoint.f -- install --force' (private host, HABU_FIXPOINT_ENGINE private) and got a ~5.4 MB engine that fails where the tools/native-build.f product image (3,997,888 bytes) passes: test/gate-stdlib.f dies at load with 'duplicate definition: STORE' rc 78 because the engine does not provide src/core/quotation-storage.f the way the release engine does (span lane handoff), tier-1 compilation dies 'bad arch tag' even from pristine sources (div-trap lane), and ncomp reports unresolvable CC-N (field-accessors lane). docs/bootstrap.md 'What the engine carries' still calls the install route the reference native chain and the fresh-workspace recipe. Acceptance: measure the two routes from one pristine tree and one host (release engine): sizes, provided-file lists (src/habu/native-runtime.f manifest vs what install bakes), the STORE duplicate, a tier-1 ': INC 1 + ;' run; fix the responsible verb in tools/build-fixpoint.f / build-fixpoint-refresh.f so an installed engine provides the same closure as the product image, or, if the install route is meant only for the warm dev snapshot, make it refuse to install over a product engine and rewrite docs/bootstrap.md so native-build.f is the documented refresh; a regression in tools/build-fixpoint-test.f that loads test/gate-stdlib.f on the installed engine. Files: tools/build-fixpoint.f, tools/build-fixpoint-refresh.f, tools/build-fixpoint-test.f, docs/bootstrap.md. Verify: the fixtures; three generations with cmp; test/run.f. Depends: none. Ownership: build tooling. Claim: unassigned.
