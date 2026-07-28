---
title: Seal SAFET and SAFET-MAP wordlists
status: closed
priority: 1
issue-type: task
created-at: "2026-07-28T15:14:19.159166+02:00"
closed-at: "2026-07-28T17:14:27.164299+02:00"
close-reason: "Landed in source commit 0f0e75fbfbf039d6841ba67dfc549a203e9886f7 on master: both SAFET and SAFET-MAP wordlists protected with six independent child probes (private set-current, bare package open, qualified publication per package) and a one-to-one protection-line mutation matrix; verified by independent review and destruction review."
---

Problem: SAFET and SAFET-MAP remain mutable after publication because their
public and private wordlists are not protected. A later source can therefore
publish new words into either package and bypass the package boundary.

Owner: `maki/infer/safetensors.f`, in packages SAFET and SAFET-MAP.

Dependencies: none.

Contract: before each existing `;package`, protect both that package's private
wordlist and its public wordlist. Do not change any existing public word,
visibility, or behavior.

Acceptance: `maki/infer/safetensors-test.f` runs exactly six distinct
production-loaded child probes:

1. Publish into SAFET's private wordlist through XREF and `set-current`.
2. Publish into SAFET-MAP's private wordlist through XREF and `set-current`.
3. Open SAFET with a bare `package SAFET`.
4. Open SAFET-MAP with a bare `package SAFET-MAP`.
5. Publish a qualified public word into SAFET.
6. Publish a qualified public word into SAFET-MAP.

Each publication child exits with `ENGINE-ERROR:SEAL-PACKAGE` when its target
wordlist is protected and changes by itself to clean publication when only that
protection line is removed. Each bare-open child exits with the same error and
changes by itself to a clean open when only the package-open guard is disabled.
One shared private `SUBJECT-EXITS` helper owns child execution and outcome
cleanup.

Forbidden: bounded-reader code; API or visibility changes; redundant
leak-specific probes that duplicate the six structural probes; compatibility
surface; a separate guard module; FILEMAP, census, or governance work.

Source order: this generic seal is the first of two serial source commits. The
second commit may then add the bounded read against packages whose publication
boundary is already closed.

Verify: `bin/hb --load maki/infer/safetensors-test.f`.
