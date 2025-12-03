#!/bin/bash
# Helper script to call MCP server tools
# Usage: ./mcp-call.sh tool_name 'json_args'
# Example: ./mcp-call.sh lisp_eval '{"code":"(+ 1 2)"}'

TOOL="$1"
ARGS="$2"

if [ -z "$TOOL" ]; then
    echo "Usage: $0 tool_name 'json_args'"
    echo "Tools: lisp_eval, lisp_compile, lisp_disasm, lisp_jit, lisp_trace, lisp_inspect, lisp_apropos"
    exit 1
fi

cd /Users/joel/Work/habu

echo "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"$TOOL\",\"arguments\":$ARGS}}" | \
    timeout 30 sbcl --noinform --load mcp-server/habu-mcp.lisp 2>/dev/null | \
    python3 -c "import sys,json; r=json.load(sys.stdin); print(r.get('result',{}).get('content',[{}])[0].get('text','Error: '+str(r)))"
