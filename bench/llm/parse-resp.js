// parse-resp.js — extract the completion text + GENERATION token count from a
// claude -p response. Handles `--output-format json` (real claude) and raw text
// (stubs). Usage: node parse-resp.js <resp-file> <out-text-file>  -> prints tokens.
// We count OUTPUT tokens only: input_tokens is dominated by Claude Code harness
// overhead (~7-22K/call) and distorted by prompt caching, so it is not a fair
// cross-call signal; output_tokens = what the model generated for the task.
const fs = require('fs');
const raw = fs.readFileSync(process.argv[2], 'utf8');
let result = raw, toks = 0;
try {
  const j = JSON.parse(raw);
  if (j && typeof j.result === 'string') {
    result = j.result;
    toks = (j.usage && j.usage.output_tokens) || 0;
  }
} catch (e) { /* raw text stub */ }
fs.writeFileSync(process.argv[3], result);
process.stdout.write(String(toks));
