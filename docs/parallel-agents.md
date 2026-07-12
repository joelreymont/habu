# Parallel Agent Pipeline

This is the map-reduce protocol for using agents on Habu dots without corrupting
the main workspace. It has a required map phase for discovery, a work phase for
isolated implementation, and a reduce phase for review, merge, and the final
gate.

## Roles

- The orchestrator owns the current working tree, dot state, final review, merge
  order, and the final gate.
- Read-only scouts map broad areas into concrete findings or dots. They must not
  edit the current working tree.
- Workers implement one concrete dot each in a separate jj workspace unless their
  write sets are known to be disjoint.

## Map Phase

1. Start from `dot ready` or `dot list`. Pick independent dots before dependent
   dots, and prefer hard blockers over easy cleanup.
2. For broad work, launch read-only scouts first. Their prompt must say
   `read-only` and `must not edit the current working tree`.
3. Scouts return findings, file paths, tests, and proposed dots. The orchestrator
   creates missing work with `dot add "Title" -d "Problem: ... Acceptance: ... Files: ... Verify: ... Depends: ... Ownership: ... Claim: unassigned."`.
4. The orchestrator chooses only unblocked leaves printed by `dot ready`, checks
   each full brief with `dot show <id>`, and proves the ownership sets disjoint.

## Work Phase

1. Give every worker one dot, one ownership scope, and a disjoint file set.
2. If a worker will edit files, create it from the verified integration base:
   `jj workspace add .jj-ws/<dot-id> --name <name> -r <verified-base>`. Verify
   `@- == <verified-base>` with `jj -R .jj-ws/<dot-id> log -r '@-'`, plus
   `jj workspace list` and clean `jj -R .jj-ws/<dot-id> st`.
3. Record `Claim: agent=<name> workspace=.jj-ws/<dot-id>` in the exact leaf,
   run `dot on <exact-id>`, verify the claim and `Status: active`, then commit it
   on a feature change and fetch/rebase. On the exact rebased tree run
   `maki/test.f`, ptx-stdlib plus touched native slices, `host-lint`,
   `filemap-lint`, and `dot-dep-lint`; then fast-forward `master` and push with
   `jj git push --bookmark master --remote origin`.
   A competing claim aborts dispatch; preserve its owner and release the losing
   local claim. Verify the remote claim with
   `jj file show -r master@origin <dot-file>`, then rebase the empty workspace
   with `jj rebase -s <workspace-name>@ -d master@origin`. Immediately before
   spawning, verify from `.jj-ws/<dot-id>` that its parent is `master@origin`,
   the same claim is active, and `jj st` is clean. Do not rerun `dot on` on an
   active dot because it rewrites metadata. Never claim a parent epic, queued
   dot, or read-only scout.
4. The worker commits its own completed work with `jj commit -m "<short title>"`
   and reports changed files, tests run, and unresolved risks.
5. The main workspace may continue only on non-overlapping files while workers
   run. Do not let two workers edit the same files in parallel.

## Reduce Phase

1. Wait for completed workers only when their result is needed. Close agents that
   are no longer needed.
2. Review each worker with `jj show --stat` and targeted diffs before merging.
3. Merge one worker at a time with `jj rebase -s <workspace>@ -d @`.
4. Resolve conflicts in the main workspace, rerun the relevant focused tests,
   then move to the next worker.
5. After all merges, run the full native gate command from `docs/bootstrap.md`.
6. Keep each dot active through fresh destruction review, integration, and the
   owning gates. Only then close it with
   `dot off <id> -r "implemented, reviewed, merged, gates green: <summary>"`.
7. Rerun `dot-dep-lint` and every required master gate on the closure tree,
   commit it on a feature change, fast-forward and push `master`, then fetch and
   verify the remote has no active/open copy.
8. Clean up extra workspaces and temporary logs.

## Conflict Rules

- A worker may not revert user changes or unrelated agent changes.
- If a worker discovers a larger dependency, it reports the dependency instead of
  broadening its write scope.
- If a dot is too large, split it immediately with `dot add` child dots before
  implementation continues.
- Every dot add, description/dependency/claim edit, reopen, closure, or removal
  must pass gates on the exact post-mutation tree, be committed, pushed to green
  `master`, and verified at `master@origin` before another lane relies on it.
- The orchestrator never closes a dot until the merged main workspace passes the
  relevant tests.
