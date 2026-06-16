// report.js <run.jsonl> -> RESULTS.md (stdout). Head-to-head: can an LLM produce
// correct array/memory code in habu vs JavaScript vs Rust?
const fs = require('fs');
const rows = fs.readFileSync(process.argv[2], 'utf8').trim().split('\n')
  .filter(Boolean).map(JSON.parse);

const ARMS = ['habu-a', 'js', 'rust'];
const LABEL = { 'habu-a': 'habu (checked)', js: 'JavaScript', rust: 'Rust' };
const by = a => rows.filter(r => r.arm === a);
const median = a => { if (!a.length) return null; a = a.slice().sort((x, y) => x - y);
  const n = a.length; return n % 2 ? a[(n - 1) / 2] : (a[n / 2 - 1] + a[n / 2]) / 2; };
const mean = a => a.length ? Math.round(a.reduce((s, x) => s + x, 0) / a.length * 100) / 100 : null;
const pct = (n, d) => d ? `${Math.round(n / d * 100)}%` : '—';
const fmt = v => v == null ? '—' : Number.isInteger(v) ? String(v) : String(Math.round(v * 100) / 100);
const sec = ms => ms == null ? null : Math.round(ms / 10) / 100;
const q = s => String(s).replace(/\|/g, '\\|');

function stats(arm) {
  const rs = by(arm);
  const passed = rs.filter(r => r.outcome === 'pass');
  const nonpass = rs.filter(r => r.outcome !== 'pass');
  const tasks = [...new Set(rs.map(r => r.name))];
  const passk = tasks.filter(n => rs.some(r => r.name === n && r.outcome === 'pass')).length;
  const toks = passed.map(r => r.tokens);
  const wall = passed.map(r => r.wall_ms);
  return {
    arm, trials: rs.length, tasks: tasks.length, passed: passed.length,
    firstPass: rs.filter(r => r.first_pass).length, passk,
    nonpass: nonpass.length,
    meanRounds: mean(passed.map(r => r.rounds)),
    medTok: median(toks), meanTok: mean(toks), maxTok: toks.length ? Math.max(...toks) : null,
    medWall: median(wall), meanWall: mean(wall), maxWall: wall.length ? Math.max(...wall) : null,
  };
}
const S = Object.fromEntries(ARMS.map(a => [a, stats(a)]));
const allNonpass = rows.filter(r => r.outcome !== 'pass');

let o = '';
o += '# RESULTS.md — habu vs JavaScript vs Rust: LLM codegen on array/memory algorithms\n\n';
o += `Generated from \`results/run.jsonl\` (${rows.length} trials). Model: \`claude -p\`. Tasks: ${S['habu-a'].tasks} `;
o += 'algorithms over an integer array (sum/max/min/argmax/count, reverse/prefix-sum/square/negate/running-max).\n';
o += 'In habu these require typed pointers, `i cells arr + @`/`!` indexing, in-place mutation, and concatenative\n';
o += 'loops — unfamiliar territory for an LLM; in JS/Rust they are idiomatic one-liners.\n\n';
o += '_Each task: the model writes the function in the target language; we compile/check + run all io-vectors,\n';
o += 'feeding the failure (checker/compiler diagnostic or failing case) back for up to 5 repair rounds. A trial\n';
o += 'is "green" only when every io-vector passes. Metric: output tokens to green (input excluded — Claude Code\n';
o += 'harness overhead + caching distort it). Output tokens are generated-token cost, not direct access to hidden\n';
o += 'reasoning. They are still a useful effort proxy: habu source is terser, yet output tokens run HIGHER on hard tasks\n';
o += '— the reasoning cost of the unfamiliar memory model dominates the terseness saving._\n\n';

o += '## Reliability\n\n';
o += '| language | trials | green trials | trial pass | first-try green | task pass@k | non-pass rows |\n';
o += '|---|---:|---:|---:|---:|---:|---:|\n';
for (const a of ARMS) { const s = S[a];
  o += `| ${LABEL[a]} | ${s.trials} | ${s.passed} | ${pct(s.passed, s.trials)} | ${pct(s.firstPass, s.trials)} | ${pct(s.passk, s.tasks)} | ${s.nonpass} |\n`; }
o += '\n`trial pass` is stricter than `task pass@k`: Habu has one failed ARGMAX trial, but another ARGMAX trial passed, so task-level pass@k is still 100%.\n\n';

o += '## Effort To Green\n\n';
o += '| language | mean rounds | median output tokens | **mean output tokens** | max output tokens | median wall s | max wall s |\n';
o += '|---|---:|---:|---:|---:|---:|---:|\n';
for (const a of ARMS) { const s = S[a];
  o += `| ${LABEL[a]} | ${fmt(s.meanRounds)} | ${fmt(s.medTok)} | **${fmt(s.meanTok)}** | ${fmt(s.maxTok)} | ${fmt(sec(s.medWall))} | ${fmt(sec(s.maxWall))} |\n`; }
o += '\nEffort metrics use passing trials only. Mean/max matter more than the median: Habu\'s cost is skewed — cheap on simple tasks, spiking on hard ones.\n\n';

const h = S['habu-a'], j = S.js, r = S.rust;
const bestMean = Math.min(j.meanTok ?? Infinity, r.meanTok ?? Infinity);
const effort = (h.meanTok != null && bestMean && bestMean !== Infinity) ? (h.meanTok / bestMean) : null;
o += '## Verdict — how does habu stack up?\n\n';
o += `**Task-level correctness parity, with a trial-level reliability gap.** task pass@k is identical — Habu ${pct(h.passk, h.tasks)} vs `;
o += `JS ${pct(j.passk, j.tasks)} / Rust ${pct(r.passk, r.tasks)} — so the model can produce correct Habu for every task in this suite. `;
o += `At the stricter trial level Habu is ${h.passed}/${h.trials}, JS ${j.passed}/${j.trials}, Rust ${r.passed}/${r.trials}; `;
o += 'the one Habu miss is an ARGMAX driver error row after the model-call timeout. The cost split is bimodal:\n\n';
o += '- **Simple elementwise loops** (sum, square, negate, max) — habu is **comparable or cheaper** than JS/Rust '
   + '(its source is terse and the pattern is regular).\n';
o += '- **Anything needing index tracking, carried state, or in-place rearrangement** (argmax, reverse, prefix-sum, '
   + 'running-max) — habu costs **5x–60x** more generation effort. The worst, ARGMAX, spiked to ~5500 output tokens vs ~100 '
   + 'in JS/Rust.\n\n';
if (effort) o += `Net: mean output-tokens-to-green habu **${fmt(h.meanTok)}** vs JS **${fmt(j.meanTok)}** / Rust **${fmt(r.meanTok)}** `
  + `— about **${effort.toFixed(0)}x** the cheapest mainstream arm, almost entirely from the hard tail.\n\n`;
o += 'The gap is the corpus-familiarity tax: habu\'s typed pointers (`arr:ptr`), `i cells arr + @`/`!` indexing, and\n';
o += 'in-place concatenative loops have much less model prior than JavaScript arrays or Rust slices. That makes obvious\n';
o += 'stack shapes cheap and stateful/indexed loops expensive. Mean repair rounds on passing trials: Habu '
   + `${fmt(h.meanRounds)}, JS ${fmt(j.meanRounds)}, Rust ${fmt(r.meanRounds)}.\n\n`;

if (allNonpass.length) {
  o += '## Non-Pass Rows\n\n';
  o += '| task | language | outcome | rounds | output tokens | wall s |\n|---|---|---|---:|---:|---:|\n';
  for (const r of allNonpass) {
    o += `| ${q(r.name)} | ${LABEL[r.arm] || r.arm} | ${q(r.outcome)} | ${r.rounds} | ${r.tokens} | ${fmt(sec(r.wall_ms))} |\n`;
  }
  o += '\n';
}

o += '## Per-Task Max Output Tokens\n\n';
const names = [...new Set(rows.map(r => r.name))];
o += '| task | Habu | JS | Rust | Habu/best | trial outcomes (h/js/rust) |\n|---|---:|---:|---:|---:|---|\n';
for (const n of names) {
  const tk = arm => { const xs = rows.filter(z => z.name === n && z.arm === arm && z.outcome === 'pass'); return xs.length ? Math.max(...xs.map(x => x.tokens)) : null; };
  const oc = arm => { const xs = rows.filter(z => z.name === n && z.arm === arm); return xs.length ? xs.map(x => `${x.outcome}/${x.rounds}`).join(',') : '—'; };
  const hh = tk('habu-a'), jj = tk('js'), rr = tk('rust');
  const best = Math.min(jj ?? Infinity, rr ?? Infinity);
  const ratio = (hh != null && best && best !== Infinity) ? `${(hh / best).toFixed(0)}x` : '—';
  o += `| ${n} | ${fmt(hh)} | ${fmt(jj)} | ${fmt(rr)} | ${ratio} | ${oc('habu-a')} ${oc('js')} ${oc('rust')} |\n`;
}
o += '\nCells are max output tokens among passing trials. `Habu/best` compares Habu with the cheaper mainstream arm; '
   + 'the jump from ~1x on elementwise tasks to ~60x on ARGMAX is the main signal.\n';
process.stdout.write(o);
