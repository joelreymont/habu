// report.js <run.jsonl> -> RESULTS.md (stdout). Head-to-head: can an LLM produce
// correct array/memory code in raw Habu, library-assisted Habu, JavaScript, and Rust?
const fs = require('fs');
const input = fs.readFileSync(process.argv[2], 'utf8').trim();
const rows = input.split('\n')
  .filter(Boolean).map(JSON.parse);
const perfPath = process.argv[3] || process.env.BENCH_PERF_JSON || null;

const ARMS = ['habu-a', 'habu-lib', 'js', 'rust'];
const LABEL = {
  'habu-a': 'Habu raw',
  'habu-lib': 'Habu + array helpers',
  js: 'JavaScript',
  rust: 'Rust',
};
const median = a => { if (!a.length) return null; a = a.slice().sort((x, y) => x - y);
  const n = a.length; return n % 2 ? a[(n - 1) / 2] : (a[n / 2 - 1] + a[n / 2]) / 2; };
const mean = a => a.length ? Math.round(a.reduce((s, x) => s + x, 0) / a.length * 100) / 100 : null;
const pct = (n, d) => d ? `${Math.round(n / d * 100)}%` : '—';
const fmt = v => v == null ? '—' : Number.isInteger(v) ? String(v) : String(Math.round(v * 100) / 100);
const sec = ms => ms == null ? null : Math.round(ms / 10) / 100;
const q = s => String(s).replace(/\|/g, '\\|');
const modelKey = r => r.model_id || r.model || 'unknown';
const modelLabel = key => rows.find(r => modelKey(r) === key)?.model || key;
const ratio = (n, d) => {
  if (n == null || d == null || d === 0 || d === Infinity) return '—';
  const x = n / d;
  return `${x < 10 ? Math.round(x * 10) / 10 : Math.round(x)}x`;
};
const tokenKnown = r => Number.isFinite(Number(r.tokens)) && Number(r.tokens) > 0;
const runtimeKnown = r => r.runtime_ms != null && Number.isFinite(Number(r.runtime_ms)) && Number(r.runtime_ms) >= 0;
const trialTaskKey = r => `${modelKey(r)}\t${r.task_id ?? r.name}`;

function readPerf(path) {
  if (!path) return null;
  const perf = JSON.parse(fs.readFileSync(path, 'utf8'));
  if (!perf || perf.bench !== 'llm-perf' || !Array.isArray(perf.results)) {
    throw new Error(`invalid llm-perf JSON: ${path}`);
  }
  return perf;
}

const models = [...new Set(rows.map(modelKey))].sort((a, b) => modelLabel(a).localeCompare(modelLabel(b)) || a.localeCompare(b));
const by = (a, model = null) => rows.filter(r => r.arm === a && (model == null || modelKey(r) === model));
const perf = readPerf(perfPath);

function stats(arm, model = null) {
  const rs = by(arm, model);
  const passed = rs.filter(r => r.outcome === 'pass');
  const nonpass = rs.filter(r => r.outcome !== 'pass');
  const taskUnits = new Map();
  for (const r of rs) {
    const k = trialTaskKey(r);
    taskUnits.set(k, (taskUnits.get(k) || false) || r.outcome === 'pass');
  }
  const passk = [...taskUnits.values()].filter(Boolean).length;
  const tokenRows = passed.filter(tokenKnown);
  const runtimeRows = passed.filter(runtimeKnown);
  const toks = tokenRows.map(r => r.tokens);
  const runtime = runtimeRows.map(r => Number(r.runtime_ms));
  const wall = passed.map(r => r.wall_ms);
  return {
    arm, trials: rs.length, tasks: taskUnits.size, passed: passed.length,
    firstPass: rs.filter(r => r.first_pass).length, passk,
    nonpass: nonpass.length,
    missingTokens: passed.length - tokenRows.length,
    missingRuntime: passed.length - runtimeRows.length,
    meanRounds: mean(passed.map(r => r.rounds)),
    medTok: median(toks), meanTok: mean(toks), maxTok: toks.length ? Math.max(...toks) : null,
    medRuntime: median(runtime), maxRuntime: runtime.length ? Math.max(...runtime) : null,
    medWall: median(wall), meanWall: mean(wall), maxWall: wall.length ? Math.max(...wall) : null,
  };
}
const S = Object.fromEntries(ARMS.map(a => [a, stats(a)]));
const allNonpass = rows.filter(r => r.outcome !== 'pass');
const missingTokenRows = rows.filter(r => r.outcome === 'pass' && !tokenKnown(r));
const missingRuntimeRows = rows.filter(r => r.outcome === 'pass' && !runtimeKnown(r));
const taskNames = [...new Set(rows.map(r => r.name))];
const hasLib = S['habu-lib'].trials > 0;

function taskTokenMax(name, arm) {
  const xs = rows.filter(z => z.name === name && z.arm === arm && z.outcome === 'pass' && tokenKnown(z));
  return xs.length ? Math.max(...xs.map(x => x.tokens)) : null;
}

function taskRatioRows(arm) {
  return taskNames.map(name => {
    const habu = taskTokenMax(name, arm);
    const js = taskTokenMax(name, 'js');
    const rust = taskTokenMax(name, 'rust');
    const best = Math.min(js ?? Infinity, rust ?? Infinity);
    if (habu == null || best === Infinity || best === 0) return null;
    return { name, habu, best, ratio: habu / best };
  }).filter(Boolean);
}

let o = '';
o += '# RESULTS.md — Habu vs JavaScript vs Rust: LLM codegen on array/memory algorithms\n\n';
o += `Generated from \`results/run.jsonl\` (${rows.length} trials). Models: ${models.map(m => `\`${modelLabel(m)}\``).join(', ')}. Tasks: ${taskNames.length} `;
o += 'algorithms over an integer array (sum/max/min/argmax/count, reverse/prefix-sum/square/negate/running-max).\n';
o += 'Raw Habu requires typed pointers, `i cells arr + @`/`!` indexing, in-place mutation, and concatenative\n';
o += 'loops — unfamiliar territory for an LLM. The Habu + array helpers arm exposes checked helpers for array access and\n';
o += 'common index patterns; JS/Rust use idiomatic array/slice APIs.\n';
if (!hasLib) {
  o += '\n**Habu + array helpers data is missing from this committed run.** The harness now runs the `habu-lib` arm, but\n';
  o += 'the checked-in `results/run.jsonl` predates that arm; re-run `sh bench/llm/run-bench.sh 2` to fill it.\n';
}
o += '\n';
o += '_Each task: the model writes the function in the target language; we compile/check + run all io-vectors,\n';
o += 'feeding the failure (checker/compiler diagnostic or failing case) back for up to 5 repair rounds. A trial\n';
o += 'is "green" only when every io-vector passes. Metric: output tokens to green (input excluded — Claude Code\n';
o += 'harness overhead + caching distort it). Output tokens are generated-token cost, not direct access to hidden\n';
o += 'reasoning. They are still a useful effort proxy: habu source is terser, yet output tokens run HIGHER on hard tasks\n';
o += '— the reasoning cost of the unfamiliar memory model dominates the terseness saving._\n\n';

o += '## Evidence Contract\n\n';
o += 'V2 live rows are identified by `run_id`, `model_id`, `arm`, `task_id`, and `trial_id`; duplicate full keys are invalid while multiple trials for the same task are expected.\n';
o += 'Replayable rows retain `prompt`, `raw_response`, `extracted_candidate`, `checker_diagnostics`, `repair_packet`, `test_output`, and `final_bundle`, each with a `*_sha256` field so artifacts can be matched to archived files or inline payloads.\n\n';

o += '## Limitations\n\n';
o += '- **nondeterminism**: model sampling, provider scheduling, local load, and transient tool latency can change individual rows.\n';
o += '- **k/N confidence**: pass rates are point estimates for the recorded k trials over N selected tasks, not confidence intervals.\n';
o += '- **token proxy limits**: output tokens exclude input, hidden reasoning, prompt-cache effects, and harness overhead.\n';
o += '- **scaffold fairness**: each arm gets the same repair budget, but language prompts, compilers, and diagnostics differ.\n';
o += '- **library comparability**: `habu-lib` measures a checked helper surface, while JS/Rust use their familiar standard library idioms.\n';
o += '- **task selection**: the suite stresses integer array and memory algorithms; it does not represent every programming workload.\n';
o += '- **environment**: wall/runtime timings are tied to the local machine, OS, toolchain, and current `bin/hb` build.\n';
o += '- **deterministic-vs-live boundary**: shell fixtures verify the harness deterministically; benchmark claims require archived live V2 rows.\n\n';

o += '## Reliability\n\n';
o += '| language | trials | green trials | trial pass | first-try green | task pass@k | non-pass rows |\n';
o += '|---|---:|---:|---:|---:|---:|---:|\n';
for (const a of ARMS) { const s = S[a];
  o += `| ${LABEL[a]} | ${s.trials} | ${s.passed} | ${pct(s.passed, s.trials)} | ${pct(s.firstPass, s.trials)} | ${pct(s.passk, s.tasks)} | ${s.nonpass} |\n`; }
o += '\n`trial pass` is passed trials over k. `task pass@k` is any green trial per task+arm+model; a task can have a failed trial and still pass at task level when another trial is green for the same model.\n\n';

o += '## Per-Model Reliability\n\n';
o += '| model | language | trials | green trials | trial pass | first-try green | task pass@k | non-pass rows |\n';
o += '|---|---|---:|---:|---:|---:|---:|---:|\n';
for (const model of models) {
  for (const a of ARMS) {
    const s = stats(a, model);
    if (!s.trials) continue;
    o += `| ${q(modelLabel(model))} | ${LABEL[a]} | ${s.trials} | ${s.passed} | ${pct(s.passed, s.trials)} | ${pct(s.firstPass, s.trials)} | ${pct(s.passk, s.tasks)} | ${s.nonpass} |\n`;
  }
}
o += '\nAggregate language tables above pool rows only after this per-model breakdown makes each model family visible.\n\n';

o += '## Effort To Green\n\n';
o += '| language | mean rounds | median output tokens | **mean output tokens** | max output tokens | median runtime ms | max runtime ms | median wall s | max wall s |\n';
o += '|---|---:|---:|---:|---:|---:|---:|---:|---:|\n';
for (const a of ARMS) { const s = S[a];
  o += `| ${LABEL[a]} | ${fmt(s.meanRounds)} | ${fmt(s.medTok)} | **${fmt(s.meanTok)}** | ${fmt(s.maxTok)} | ${fmt(s.medRuntime)} | ${fmt(s.maxRuntime)} | ${fmt(sec(s.medWall))} | ${fmt(sec(s.maxWall))} |\n`; }
o += '\nEffort metrics use passing trials with a positive output-token count. Runtime metrics use `runtime_ms`, a warmed candidate execution over fixed vectors and repetitions; wall time remains model/checker/compiler/feedback latency. Mean/max matter more than the median: Habu\'s cost is skewed — cheap on simple tasks, spiking on hard ones.\n\n';
if (missingTokenRows.length) {
  const miss = ARMS.map(a => [a, S[a].missingTokens]).filter(([, n]) => n > 0)
    .map(([a, n]) => `${LABEL[a]} ${n}`).join(', ');
  o += `Output-token metrics exclude ${missingTokenRows.length} passing row(s) with missing/zero token counts (${miss}). `;
  o += 'Reliability, repair-round, and wall-time metrics still include those rows.\n\n';
}
if (missingRuntimeRows.length) {
  const miss = ARMS.map(a => [a, S[a].missingRuntime]).filter(([, n]) => n > 0)
    .map(([a, n]) => `${LABEL[a]} ${n}`).join(', ');
  o += `Runtime metrics exclude ${missingRuntimeRows.length} passing row(s) without measured runtime (${miss}). `;
  o += 'Reliability, repair-round, token, and wall-time metrics still include those rows.\n\n';
}

o += '## LLM Feedback Latency\n\n';
o += 'Source: `bench/llm/perf.sh --json`; these timings measure local checker/test/report feedback latency, not model inference latency.\n\n';
if (perf) {
  o += '| check | wall ms | wall s |\n|---|---:|---:|\n';
  for (const r of perf.results) {
    o += `| ${q(r.name)} | ${fmt(r.wall_ms)} | ${fmt(sec(r.wall_ms))} |\n`;
  }
  o += '\n';
} else {
  o += 'No perf JSON artifact was supplied with this report run.\n\n';
}

const h = S['habu-a'], hl = S['habu-lib'], j = S.js, r = S.rust;
const bestMean = Math.min(j.meanTok ?? Infinity, r.meanTok ?? Infinity);
const effort = ratio(h.meanTok, bestMean);
const libEffort = ratio(hl.meanTok, bestMean);
const libVsRaw = ratio(hl.meanTok, h.meanTok);
const rawRatios = taskRatioRows('habu-a');
const rawWorst = rawRatios.reduce((best, x) => !best || x.ratio > best.ratio ? x : best, null);
o += '## Verdict — how does Habu stack up?\n\n';
o += `Task pass@k is Habu raw ${pct(h.passk, h.tasks)}, `;
o += hasLib ? `Habu + array helpers ${pct(hl.passk, hl.tasks)}, ` : 'Habu + array helpers —, ';
o += `JS ${pct(j.passk, j.tasks)}, Rust ${pct(r.passk, r.tasks)}. `;
o += `At the stricter trial level: Habu raw ${h.passed}/${h.trials}, `;
o += hasLib ? `Habu + array helpers ${hl.passed}/${hl.trials}, ` : 'Habu + array helpers has no live rows, ';
o += `JS ${j.passed}/${j.trials}, Rust ${r.passed}/${r.trials}. `;
const notes = [];
if (h.nonpass) notes.push(`Raw Habu has ${h.nonpass} non-pass row(s); see the table below.`);
if (hasLib && hl.nonpass) notes.push(`The helper arm has ${hl.nonpass} non-pass row(s); see the table below.`);
if (notes.length) o += notes.join(' ');
o += '\n\nThe raw-Habu cost split is bimodal:\n\n';
o += '- **Simple elementwise loops** (sum, square, negate, max) — raw Habu is **comparable or cheaper** than JS/Rust '
   + '(its source is terse and the pattern is regular).\n';
if (rawWorst) {
  o += '- **Anything needing index tracking, carried state, or in-place rearrangement** (argmax, reverse, prefix-sum, '
     + `running-max) remains the hard tail. In this run the worst measured raw-Habu task is ${rawWorst.name} at `
     + `about **${rawWorst.ratio.toFixed(0)}x** (${fmt(rawWorst.habu)} vs ${fmt(rawWorst.best)} output tokens).\n\n`;
}
if (h.meanTok != null && bestMean !== Infinity) o += `Net: mean output-tokens-to-green Habu raw **${fmt(h.meanTok)}** vs JS **${fmt(j.meanTok)}** / Rust **${fmt(r.meanTok)}** `
  + `— about **${effort}** the cheapest mainstream arm, almost entirely from the hard tail.\n\n`;
if (hasLib && hl.meanTok != null && bestMean !== Infinity) {
  o += `Habu + array helpers mean output-tokens-to-green is **${fmt(hl.meanTok)}**, about **${libEffort}** the cheapest mainstream arm `;
  o += `and **${libVsRaw}** of raw Habu's mean cost.\n\n`;
}
if (!hasLib) o += 'A raw-vs-library conclusion is intentionally withheld until `habu-lib` rows are collected in a live run.\n\n';
o += 'The raw-Habu gap is the corpus-familiarity tax: Habu\'s typed pointers (`arr:ptr`), `i cells arr + @`/`!` indexing, and\n';
o += 'in-place concatenative loops have much less model prior than JavaScript arrays or Rust slices. That makes obvious\n';
o += 'stack shapes cheap and stateful/indexed loops expensive. Mean repair rounds on passing trials: Habu raw '
   + `${fmt(h.meanRounds)}, Habu + array helpers ${fmt(hl.meanRounds)}, JS ${fmt(j.meanRounds)}, Rust ${fmt(r.meanRounds)}.\n\n`;

if (allNonpass.length) {
  o += '## Non-Pass Rows\n\n';
  o += '| task | language | outcome | rounds | output tokens | wall s |\n|---|---|---|---:|---:|---:|\n';
  for (const r of allNonpass) {
    o += `| ${q(r.name)} | ${LABEL[r.arm] || r.arm} | ${q(r.outcome)} | ${r.rounds} | ${r.tokens} | ${fmt(sec(r.wall_ms))} |\n`;
  }
  o += '\n';
}

o += '## Per-Task Max Output Tokens\n\n';
o += '| task | Habu raw | Habu + helpers | JS | Rust | raw/best | helpers/best | trial outcomes (raw/helpers/js/rust) |\n|---|---:|---:|---:|---:|---:|---:|---|\n';
for (const n of taskNames) {
  const oc = arm => { const xs = rows.filter(z => z.name === n && z.arm === arm); return xs.length ? xs.map(x => `${x.outcome}/${x.rounds}`).join(',') : '—'; };
  const hh = taskTokenMax(n, 'habu-a'), ll = taskTokenMax(n, 'habu-lib'), jj = taskTokenMax(n, 'js'), rr = taskTokenMax(n, 'rust');
  const best = Math.min(jj ?? Infinity, rr ?? Infinity);
  const rawRatio = ratio(hh, best);
  const libRatio = ratio(ll, best);
  const outcomes = `raw ${oc('habu-a')}; helpers ${oc('habu-lib')}; js ${oc('js')}; rust ${oc('rust')}`;
  o += `| ${q(n)} | ${fmt(hh)} | ${fmt(ll)} | ${fmt(jj)} | ${fmt(rr)} | ${rawRatio} | ${libRatio} | ${q(outcomes)} |\n`;
}
o += '\nCells are max output tokens among passing trials with positive output-token counts. `raw/best` and `helpers/best` compare each Habu arm with the cheaper mainstream arm; '
   + 'the jump from ~1x on elementwise tasks to the hard-task tail is the main raw-Habu signal.\n';
process.stdout.write(o);
