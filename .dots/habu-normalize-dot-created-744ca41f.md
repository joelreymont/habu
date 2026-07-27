---
title: Normalize dot created-at quoting
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T17:35:07.823890+02:00"
---

Measured on master cd743607: 304 dot files under .dots/ carry their created-at timestamp double-quoted, as created-at: "\"2026-07-27T00:09:32.400720+02:00\"" instead of created-at: "2026-07-27T00:09:32.400720+02:00". The value is a quoted string containing a quoted string. It parses, so no lint currently objects, but dot show prints the timestamp with the inner quotes and backslashes visible, which is simply wrong output, and any future consumer that parses the timestamp as a date rather than as an opaque string will fail on exactly these 304 files and not on the others.

This was found while carrying restored dot contracts forward on 2026-07-27. The files were copied verbatim on purpose, so the commit that found it deliberately did not fix it: a mechanical change across 304 files does not belong inside a contract-correction commit, and fixing the data without fixing whatever writes it would just let the form come back.

Owned result, two parts, and the second is the one that matters:
(1) One mechanical normalization commit that rewrites every doubled created-at value to the single-quoted form, changing nothing else - not the timestamp, not field order, not any other line. Prove it is mechanical: the set of changed lines must be exactly the created-at lines in exactly the 304 files, and every timestamp must be byte-identical after the quote characters are removed.
(2) Find and fix whatever produces the doubled form so it cannot recur. The dot command-line tool writes these files, so start there: something on the add or edit path is quoting an already-quoted string. Until that is found, part (1) is a cleanup with a known source, which is the pattern this repository keeps rejecting. If the writer turns out to be correct today and the 304 files came from a fixed older version, prove that with a fresh dot add on the current tool and say so - then part (2) becomes a regression that fails if the doubled form ever appears again.

First measurement already taken, at the moment this dot was minted: the current
dot command-line tool wrote THIS file's own created-at in the correct
single-quoted form. So the plain add path on the current tool is not the
culprit, and part (2) should look next at the edit, claim-rewrite, and status
paths - dot on and dot off both rewrite front matter - and at whether the 304
affected files share an origin, such as having been created by an older tool
version or by a bulk-minting script. Do not assume; the counts are easy to
partition by comparing which files carry the doubled form against when and how
they were added.

Acceptance: zero created-at lines tree-wide carry an embedded quote character; dot show renders the timestamp of a previously affected dot correctly; a freshly added dot writes the single-quoted form; dot-dep-lint reports 0 finding(s) and stale-status-lint reports 0 finding(s) on the resulting tree; the diff touches only created-at lines.
