---
title: Bind explicit FILE capture producer
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T08:39:15.849882+03:00"
---

Independent Astra review of48a1a8f8 finds fresh explicit --export-file/--import-file fails before loading prefix: NATIVE-FILE-EXPLICIT:SOURCE-LOAD calls START-FILE-PREFIX -> ARM-OWNERS -> TARGET-XT while the new producer binding table is empty. Automatic definition-cache entry binds it, explicit driver does not. Bind the emitting producer at the explicit operation entry, before its first captured owner call; preserve exact target binding and explicit source/action semantics without name fallbacks. Existing native-file-explicit-reclaim-e2e is selected before implementation to demonstrate failing export and verify export/import retained action behavior. Source driver repair only, independent focused follow-up review and integrated required gates; no new framework.
