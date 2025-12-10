# Parallel Agents

**ALWAYS launch 2-4 agents in parallel when possible.**

## Before launching code-writing agents:
1. Commit current changes
2. Create git worktrees in `/tmp/habu-*` directories, branched from HEAD
3. Merge agent work after completion

## Read-only agents (search, research) don't need worktrees.

## Example:
```
git add -A && git commit -m "WIP before parallel work"
# Launch agents with separate worktrees
# Merge results when done
```
