---
title: Rename NUNIT and NFILE packages to OBJ and OBJCACHE
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T11:49:22.971832+02:00"
---

Joel 2026-10-29: one package OBJ ("the package should be named OBJ") for tools/native-unit-{file,object,dependency,capture,origin}.f and tools/native-file-{reference,scope}.f (files become tools/obj-*.f); NDEF-CACHE (tools/native-definition-cache.f) becomes OBJCACHE, a name to reconsider with the fasl redesign (dot habu-spike-the-fasl-16b76e28). NFILE-INPUTS stays. Public collisions on merge: BEGIN-DEFINITION, CLOSE, DICT-CUT, RESET. Waits for the refusal census, which may delete much of these files first.
