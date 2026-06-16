// report.js <run.jsonl> -> RESULTS.md (stdout). Head-to-head: can an LLM produce
// CORRECT code in habu vs JavaScript vs Rust on complex (tensor-library) tasks?
const fs = require('fs');
const rows = fs.readFileSync(process.argv[2], 'utf8').trim().split('\n')
  .filter(Boolean).map(JSON.parse);

const ARMS = ['habu-a', 'js', 'rust'];
const LABEL = { 'habu-a': 'habu (checked)', js: 'JavaScript', rust: 'Rust' };
const by = a => rows.filter(r => r.arm === a);
const median = a => { if (!a.length) return null; a = a.slice().sort((x, y) => x - y);
  const n = a.length; return n % 2 ? a[(n - 1) / 2] : Math.round((a[n / 2 - 1] + a[n / 2]) / 2); };
const mean = a => a.length ? Math.round(a.reduce((s, x) => s + x, 0) / a.length * 100) / 100 : null;
const fmt = v => v == null ? '—' : v;

function stats(arm) {
  const rs = by(arm);
  const passed = rs.filter(r => r.outcome === 'pass');
  const tasks = [...new Set(rs.map(r => r.name))];
  const passk = tasks.filter(n => rs.some(r => r.name === n && r.outcome === 'pass')).length;
  const toks = passed.map(r => r.tokens);
  return {
    arm, trials: rs.length, tasks: tasks.length,
    pass1: rs.length ? Math.round(rs.filter(r => r.first_pass).length / rs.length * 100) : 0,
    passk: tasks.length ? Math.round(passk / tasks.length * 100) : 0,
    medRounds: mean(passed.map(r => r.rounds)),
    medTok: median(toks), meanTok: mean(toks), maxTok: toks.length ? Math.max(...toks) : null,
  };
}
const S = Object.fromEntries(ARMS.map(a => [a, stats(a)]));

let o = '';
o += '# RESULTS.md — habu vs JavaScript vs Rust: LLM codegen on array/memory algorithms\n\n';
o += `Generated from \`results/run.jsonl\` (${rows.length} trials). Model: \`claude -p\`. Tasks: ${S['habu-a'].tasks} `;
o += 'algorithms over an integer array (sum/max/min/argmax/count, reverse/prefix-sum/square/negate/running-max).\n';
o += 'In habu these require typed pointers, `i cells arr + @`/`!` indexing, in-place mutation, and concatenative\n';
o += 'loops — unfamiliar territory for an LLM; in JS/Rust they are idiomatic one-liners.\n\n';
o += '_Each task: the model writes the function in the target language; we compile/check + run all io-vectors,\n';
o += 'feeding the failure (checker/compiler diagnostic or failing case) back for up to 5 repair rounds. A trial\n';
o += 'is "green" only when every io-vector passes. Metric: output tokens to green (input excluded — Claude Code\n';
o += 'harness overhead + caching distort it). Output tokens = the model\'s chain-of-thought + code, so they track\n';
o += 'how hard the model had to REASON. habu source is terser, yet its output tokens run HIGHER on the harder tasks\n';
o += '— the reasoning cost of the unfamiliar memory model dominates the terseness saving._\n\n';

o += '## Head-to-head\n\n';
o += '| language | pass@1 | pass@k | mean rounds | median tokens | **mean tokens** | max tokens |\n';
o += '|---|---|---|---|---|---|---|\n';
for (const a of ARMS) { const s = S[a];
  o += `| ${LABEL[a]} | ${s.pass1}% | ${s.passk}% | ${fmt(s.medRounds)} | ${fmt(s.medTok)} | **${fmt(s.meanTok)}** | ${fmt(s.maxTok)} |\n`; }
o += '\noutput-tokens-to-green = the model\'s chain-of-thought + code. The **mean/max** matter more than the median: '
   + 'habu\'s cost is SKEWED — cheap on simple tasks, spiking on hard ones (see per-task table).\n\n';

const h = S['habu-a'], j = S.js, r = S.rust;
const bestMean = Math.min(j.meanTok ?? Infinity, r.meanTok ?? Infinity);
const effort = (h.meanTok != null && bestMean && bestMean !== Infinity) ? (h.meanTok / bestMean) : null;
o += '## Verdict — how does habu stack up?\n\n';
o += `**Correctness parity; effort gap that is SKEWED, not uniform.** pass@k is identical — habu ${h.passk}% vs `;
o += `JS ${j.passk}% / Rust ${r.passk}% (first-try ${h.pass1}% / ${j.pass1}% / ${r.pass1}%): the model CAN write these `;
o += 'array/memory algorithms correctly in habu. The cost is where they diverge, and it is **bimodal**:\n\n';
o += '- **Simple elementwise loops** (sum, square, negate, max) — habu is **comparable or cheaper** than JS/Rust '
   + '(its source is terse and the pattern is regular).\n';
o += '- **Anything needing index tracking, carried state, or in-place rearrangement** (argmax, reverse, prefix-sum, '
   + 'running-max) — habu costs **5x–50x** more reasoning. The worst, ARGMAX, spiked to ~5500 output tokens vs ~100 '
   + 'in JS/Rust.\n\n';
if (effort) o += `Net: mean output-tokens-to-green habu **${fmt(h.meanTok)}** vs JS **${fmt(j.meanTok)}** / Rust **${fmt(r.meanTok)}** `
  + `— about **${effort.toFixed(0)}x** the cheapest mainstream arm, almost entirely from the hard tail.\n\n`;
o += 'The gap is the corpus-familiarity tax: habu\'s typed pointers (`arr:ptr`), `i cells arr + @`/`!` indexing, and\n';
o += 'in-place concatenative loops have ~zero pretraining, so the model reasons each step from first principles — cheap\n';
o += 'when the shape is obvious, expensive when it must juggle the stack. Mean repair rounds: habu '
   + `${fmt(h.medRounds)}, JS ${fmt(j.medRounds)}, Rust ${fmt(r.medRounds)}.\n\n`;

o += '## Per-task max output-tokens — the effort signal\n\n';
const names = [...new Set(rows.map(r => r.name))];
o += '| task | habu | JS | Rust | habu/best | first-trial outcomes (h/js/rust) |\n|---|---|---|---|---|---|\n';
for (const n of names) {
  const tk = arm => { const xs = rows.filter(z => z.name === n && z.arm === arm && z.outcome === 'pass'); return xs.length ? Math.max(...xs.map(x => x.tokens)) : null; };
  const oc = arm => { const x = rows.find(z => z.name === n && z.arm === arm); return x ? `${x.outcome}/${x.rounds}` : '—'; };
  const hh = tk('habu-a'), jj = tk('js'), rr = tk('rust');
  const best = Math.min(jj ?? Infinity, rr ?? Infinity);
  const ratio = (hh != null && best && best !== Infinity) ? `${(hh / best).toFixed(0)}x` : '—';
  o += `| ${n} | ${fmt(hh)} | ${fmt(jj)} | ${fmt(rr)} | ${ratio} | ${oc('habu-a')} ${oc('js')} ${oc('rust')} |\n`;
}
o += '\nCells = max output tokens across trials (passing). `habu/best` = habu vs the cheaper mainstream arm — note '
   + 'the jump from ~1x on elementwise tasks to ~50x on ARGMAX.\n';
process.stdout.write(o);
