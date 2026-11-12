---
title: Name every duplicate the engine refuses silently
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:18:22.276655+03:00"
---

Duplicates the checker sees but no check names. A CAST: of a checker-only name exits 78 with nothing on stderr under --load and in every check.f form (REGISTER-CAST route). defer and TRUSTED: of a checker-only name exit 0. A does> child's duplicate is never refused. A TYPED-VARIABLE or DEFER-LAYOUT-BUFFER duplicate of a generated name exits 78 silently under --load (printf 'PRODUCT ckdprod 0 FIELD x n ;PRODUCT\nTYPED-VARIABLE CKDPROD:MAKE n\n'). check.f locates the last two; the engine's wall should say 'duplicate definition: NAME at FILE:LINE' for all of them. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix it in the code that stays, never in the machinery the parent replaces.
