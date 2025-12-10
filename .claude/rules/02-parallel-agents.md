# Parallel Agents

**ALWAYS launch 2-4 agents in parallel when possible.**

**NEVER idle waiting for agents. Continue working on other tasks while agents run.**

## Before launching code-writing agents:
1. Commit current changes
2. Agents create git worktrees in `/tmp/habu-*` directories, branched from HEAD
3. Merge agent work after completion

## Read-only agents (search, research) don't need worktrees.

## While agents run:
- Work on unrelated tasks
- Check agent status with `block=false`
- Only block-wait when truly nothing else to do
