// parse-resp.js — extract completion text + generation token count from a model
// response. Usage:
//   node parse-resp.js <resp-file> <out-text-file> [parser] [token-fields]
// Parsers: claude-json (default), openai-json, raw. Token fields are comma-
// separated JSON paths; `*` fans out over object values or array elements.
// We count OUTPUT tokens only: input tokens are dominated by harness overhead and
// prompt caching, so they are not a fair cross-call signal.
const fs = require('fs');
const raw = fs.readFileSync(process.argv[2], 'utf8');
const parser = process.argv[4] || 'claude-json';
const tokenFields = process.argv[5] || 'usage.output_tokens,modelUsage.*.outputTokens';
let result = raw, toks = 0;

function valuesAt(x, path) {
  if (!path.length) return [x];
  if (x == null) return [];
  const [h, ...rest] = path;
  if (h === '*') {
    if (Array.isArray(x)) return x.flatMap(v => valuesAt(v, rest));
    if (typeof x === 'object') return Object.values(x).flatMap(v => valuesAt(v, rest));
    return [];
  }
  return valuesAt(x[h], rest);
}

function tokenSum(j) {
  return tokenFields.split(',')
    .map(s => s.trim()).filter(Boolean)
    .flatMap(p => valuesAt(j, p.split('.')))
    .reduce((sum, v) => {
      const n = Number(v);
      return sum + (Number.isFinite(n) && n > 0 ? n : 0);
    }, 0);
}

function textFromJson(j) {
  if (parser === 'claude-json') {
    if (j && typeof j.result === 'string') return j.result;
    if (Array.isArray(j && j.content)) {
      return j.content.map(c => c && c.text).filter(Boolean).join('');
    }
  }
  if (parser === 'openai-json') {
    if (typeof (j && j.output_text) === 'string') return j.output_text;
    const msg = j && j.choices && j.choices[0] && j.choices[0].message;
    if (msg && typeof msg.content === 'string') return msg.content;
  }
  return null;
}
try {
  const j = JSON.parse(raw);
  if (parser !== 'raw') {
    const text = textFromJson(j);
    if (typeof text === 'string') result = text;
  }
  toks = tokenSum(j);
} catch (e) { /* raw text stub */ }
fs.writeFileSync(process.argv[3], result);
process.stdout.write(String(toks));
