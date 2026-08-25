---
title: "Tracker GC: close provably-landed dots"
status: closed
priority: 2
issue-type: task
created-at: "2026-08-21T09:27:55.441027+02:00"
closed-at: "2026-08-21T10:30:00.000000+02:00"
close-reason: "Done in four commits: 413c6726 (five proved closures), 8ec6f9bf (99 dead claims released, the landed mid-row leaf closed), 81eaeae3 (349 satisfied blocker edges swept), 1ee5bde3 (four stray leaves deleted, 523 quote layers unwrapped). dot-dep-lint 0 finding(s) and both diff lints rc 0 on every batch; no .f file changed in any of them. Arithmetic from master cd7d96c0 to here: leaves 2045 -> 2041, blocker edges 1120 -> 772 (zero now point at a closed leaf, from 349), active 106 -> 5 (all five name a workspace that exists), open 1395 -> 1487, closed 542 -> 547, dot ready 908 -> 990. Signal: 93 percent of the active set was lying about ownership and 31 percent of the dependency graph constrained nothing; both are now zero. The work descriptions themselves were sound - six leaves out of 1395 could be disproved, so the rot was in the ownership and dependency layers, not in the contracts."
---

Problem: the tracker holds 2045 dots / 1120 blocker edges (dot-dep-lint baseline on master cd7d96c0), including work that has provably landed - so dot ready and the blocker graph no longer describe real open work. Acceptance: every closure carries a proof in its close-reason - either a commit id verified in master ancestry, or a re-run command plus rc showing the leaf's demanded behavior now holds, or an exact-duplicate cross-reference to the surviving leaf. No design questions, unlanded capability dots, actively-claimed dots, or dots blocked by an open dot are closed. Files: .dots only, no .f changes. Verify: HB_TMP=<private> bin/hb --load tools/dot-dep-lint.f exit 0 and 0 finding(s) per batch. Depends: none. Ownership: .dots leaf frontmatter.

Claim: agent=gc-1 workspace=.jj-ws/habu-effstore (DONE 2026-08-21)

## Lessons from this GC, for LESSONS.md

- **`blocks:` in this tracker means blocked-BY, and the reading is decidable,
  not a matter of taste.** `dot ready` is exactly the open set minus the leaves
  whose `blocks:` names an open or active leaf: 1390 open, 482 so blocked, 908
  ready, zero contradictions. The opposite reading contradicts `dot ready` 90
  times. Check a graph convention against the tool that consumes it before
  editing 349 edges, because both readings look plausible in the prose - some
  leaves write `Depends: X` right next to `blocks: - X`.

- **`jj log -r 'master & <id>'` is not an ancestry test.** `master` is a single
  commit, so the intersection is empty for every id but master's own and the
  command silently answers "not in master" for work that landed. The test is
  `jj log -r '<id> & ::master'`. Four spot checks were wrong before this was
  caught, and the wrong answer is the safe-looking one.

- **A leaf that shouts LANDED almost never means itself.** 322 open leaves
  contained a landed/superseded word; reading them, the word nearly always
  attached to a *blocker* that got satisfied, a *sub-slice*, or a *duplicate
  merged into* this leaf. Three that read as decisive - `DELIVERED AND MERGED`,
  `SATISFIED and removed`, `MERGED 2026-08-04` - were all correctly open.
  Grep finds candidates; only the sentence around the banner decides.

- **Corruption that compounds must be measured for depth, not assumed to be one
  layer.** The `dot on` re-quoting bug wraps an already-wrapped value, so leaves
  sat at two, three, four and five layers. "Strip one layer" would have left 151
  of 361 still broken. Decode to the underlying value and re-serialize once,
  then prove it by running the fixer twice and diffing.

- **A frontmatter key only exists if the tool's parser reaches it.** One leaf
  carried a blocker entry with no `blocks:` header above it, so no tool ever
  read the dependency - the files held 1121 edges where dot-dep-lint counted
  1120. Another leaf held two whole dot documents concatenated; the CLI reads
  the first, so the second block's `status: active` was invisible to a sweep
  that edited frontmatter. Both were found by counting the same thing two ways
  and chasing the difference of one.

- **Dead claims, not stale work, are what rots a tracker.** 99 of 106 active
  leaves named a workspace that no longer existed, hiding them from `dot ready`
  while their contracts stayed accurate. Of 1395 open leaves only six could be
  disproved. Sweep ownership and dependency metadata on a schedule; the prose
  looks after itself.

- **`dot ready` did not move when 349 dead edges were swept.** The CLI already
  treated a closed prerequisite as satisfied, which is exactly why those edges
  rotted unnoticed for months - nothing ever surfaced them. Hygiene that no
  tool can feel needs its own audit, because it will never be reported.
