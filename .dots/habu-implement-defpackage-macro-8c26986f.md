---
title: Implement defpackage macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:34.110689+02:00"
---

stdlib.habu: Add (defmacro defpackage (name &key nicknames use export shadow ...) ...). Expand to make-package + export + use calls. Dependencies: habu-implement-in-pkg-de5e114e. Verify: (defpackage :foo (:use :cl)).
