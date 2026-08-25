---
title: Begin a memory order without a data-stack take
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T11:58:25.973428+02:00"
---

src/compiler/native/select.f gives hir.mem no instruction: it binds the source memory order to the token a64.dtake mints, because a routine under the data-stack convention already begins its generic memory order there. A routine whose convention names only registers has no such beginning, so a memory operation in one is refused with E-A64SEL-MEM. Wanted: either a machine form that mints the generic order on its own, or the argument written down that no register-convention routine of this chain can reach memory. Until then the refusal is named and tested in test/compiler/native-select.f.
