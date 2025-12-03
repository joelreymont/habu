#!/bin/bash
# Debug wrapper for Habu MCP server
LOG=/tmp/habu-mcp-debug.log

exec 2>>"$LOG"
echo "=== Started $(date) ===" >&2
echo "PWD: $(pwd)" >&2

# Just run sbcl directly - stderr goes to log
exec /opt/homebrew/bin/sbcl --noinform --load /Users/joel/Work/habu/mcp-server/habu-mcp.lisp
