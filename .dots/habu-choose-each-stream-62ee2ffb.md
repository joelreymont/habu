---
title: Choose each stream of a captured run
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:17:37.881461+02:00"
---

Problem: Habu's capture words (src/lib/process-argv.f:127-170, src/lib/process-env.f:316-392) pipe both stdout and stderr into caller storage, either fixed, where overflow of either stream throws one E-PROC-TRUNCATED (src/lib/process.f:688-708), or growing without a bound; none sends a stream to /dev/null or leaves it the parent's. Omabox runs secret-tool with stdout bounded at 8 KiB and stderr discarded (Rust's Stdio::null), and today grows both without limit, so a flooding child can exhaust the daemon; its systemctl runs need stdin/stdout/stderr as own/own/own, own/null/null, null/pipe/null and null/null/pipe, so it keeps spawn code of its own. Acceptance: a run whose stdin is bytes, /dev/null or the parent's and whose stdout and stderr are each captured within a bound, discarded or the parent's; a stream past its bound is named and its child ended and reaped; both Omabox uses run through it. Depends: none. Claim: unassigned.
