---
title: Test a T0 unit past 4096 bytes and shadowed locals
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T05:44:50.043954+02:00"
---

From the qsonklvt review (/tmp/jerry-t0ld/rev): no T0 row emits a unit over 4096 bytes, so a span taken before W32+'s BUF:GROW (mutant M6, SIGSEGV on the k=1100 body scripts/big.tmpl, 8860 bytes) passes both suites; no row tests local shadowing (M5: FIND-LOCAL oldest first; ': L3 ( n n -- n ) {: ab:n :} {: ab:n :} ab ;' on 5 7 must give 7). Add one rows case (NEMIT:SIZE 8860, a word past 4096, the patched if branch) and one t0-install row.
