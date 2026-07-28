---
title: Seal SAFET and SAFET-MAP wordlists
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T15:14:19.159166+02:00"
---

Problem: SAFET and SAFET-MAP remain mutable after publication because their
public and private wordlists are not protected. A later source can therefore
publish new words into either package and bypass the package boundary.

Owner: `maki/infer/safetensors.f`, in packages SAFET and SAFET-MAP.

Dependencies: none.

Contract: before each existing `;package`, protect both that package's private
wordlist and its public wordlist. Do not change any existing public word,
visibility, or behavior.

Acceptance: `maki/infer/safetensors-test.f` runs exactly four independent
production-loaded child probes, one for each protected wordlist:

1. Publish into SAFET's private wordlist through XREF and `set-current`.
2. Publish into SAFET-MAP's private wordlist through XREF and `set-current`.
3. Publish a qualified public word into SAFET.
4. Publish a qualified public word into SAFET-MAP.

Each child exits with `ENGINE-ERROR:SEAL-PACKAGE` when its own protection line
is present and changes by itself to clean publication when only that protection
line is removed. One shared private `SUBJECT-EXITS` helper owns child execution
and outcome cleanup.

Forbidden: bounded-reader code; API or visibility changes; duplicated package
reopen or leak probes; compatibility surface; a separate guard module; FILEMAP,
census, or governance work.

Source order: this generic seal is the first of two serial source commits. The
second commit may then add the bounded read against packages whose publication
boundary is already closed.

Verify: `bin/hb --load maki/infer/safetensors-test.f`.

Claim: agent=claude workspace=.jj-ws/habu-add-bounded-little-189c4aa9
