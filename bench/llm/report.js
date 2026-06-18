// report.js <run.jsonl> -> RESULTS.md (stdout). Head-to-head: can an LLM produce
// correct array/memory code across Habu arms, JavaScript, Python, TypeScript,
// and Rust?
const fs = require('fs');
const input = fs.readFileSync(process.argv[2], 'utf8').trim();
const rows = input.split('\n')
  .filter(Boolean).map(JSON.parse);
const perfPath = process.argv[3] || process.env.BENCH_PERF_JSON || null;

const HABU_ARMS = ['habu-a', 'habu-lib', 'habu-stdlib', 'habu-skeleton'];
const BASELINE_ARMS = ['js', 'python', 'ts', 'rust'];
const ARMS = [...HABU_ARMS, ...BASELINE_ARMS];
const LABEL = {
  'habu-a': 'Habu raw',
  'habu-lib': 'Habu + array helpers',
  'habu-stdlib': 'Habu + stdlib',
  'habu-skeleton': 'Habu + skeleton',
  js: 'JavaScript',
  python: 'Python',
  ts: 'TypeScript',
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
const rowCategory = r => r.task_family || r.category || 'unknown';
const diagFields = [
  'diagnostic_token',
  'diagnostic_span',
  'diagnostic_expected',
  'diagnostic_actual',
  'diagnostic_code',
  'diagnostic_repair_class',
  'all_errors_stable',
];
const diagComplete = r => diagFields.every(k => r[k] === true);
const passRateValue = s => s.tasks ? s.passk / s.tasks : null;
const passDelta = (a, b) => {
  const av = passRateValue(a), bv = passRateValue(b);
  if (av == null || bv == null) return '—';
  const pp = Math.round((av - bv) * 100);
  return `${pp > 0 ? '+' : ''}${pp}pp`;
};

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

function stats(arm, model = null, category = null) {
  const rs = by(arm, model).filter(r => category == null || rowCategory(r) === category);
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
  const diagOk = rs.filter(diagComplete).length;
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
    diagOk,
  };
}
const S = Object.fromEntries(ARMS.map(a => [a, stats(a)]));
const allNonpass = rows.filter(r => r.outcome !== 'pass');
const missingTokenRows = rows.filter(r => r.outcome === 'pass' && !tokenKnown(r));
const missingRuntimeRows = rows.filter(r => r.outcome === 'pass' && !runtimeKnown(r));
const taskNames = [...new Set(rows.map(r => r.name))];
const categories = [...new Set(rows.map(rowCategory))].sort((a, b) => a.localeCompare(b));
const hasLib = S['habu-lib'].trials > 0;

function taskTokenMax(name, arm) {
  const xs = rows.filter(z => z.name === name && z.arm === arm && z.outcome === 'pass' && tokenKnown(z));
  return xs.length ? Math.max(...xs.map(x => x.tokens)) : null;
}

function bestBaselineTokenMax(name) {
  const xs = BASELINE_ARMS
    .map(arm => taskTokenMax(name, arm))
    .filter(x => x != null);
  return xs.length ? Math.min(...xs) : Infinity;
}

function taskRatioRows(arm) {
  return taskNames.map(name => {
    const habu = taskTokenMax(name, arm);
    const best = bestBaselineTokenMax(name);
    if (habu == null || best === Infinity || best === 0) return null;
    return { name, habu, best, ratio: habu / best };
  }).filter(Boolean);
}

let o = '';
o += '# RESULTS.md — Habu vs JavaScript, Python, TypeScript, and Rust: LLM codegen on array/memory algorithms\n\n';
o += `Generated from \`results/run.jsonl\` (${rows.length} trials). Models: ${models.map(m => `\`${modelLabel(m)}\``).join(', ')}. Tasks: ${taskNames.length} `;
o += 'algorithms over an integer array (sum/max/min/argmax/count, reverse/prefix-sum/square/negate/running-max).\n';
o += 'Raw Habu requires typed pointers, `i cells arr + @`/`!` indexing, in-place mutation, and concatenative\n';
o += 'loops — unfamiliar territory for an LLM. The Habu + array helpers arm exposes checked helpers for array access and\n';
o += 'common index patterns; JS, Python, TypeScript, and Rust use idiomatic array/list/slice APIs.\n';
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
o += 'Rows also carry `task_family`, `model_version`, `model_date`, trial/order metadata, outcome and repair counters, diagnostic-quality booleans, `source_chars`, and warmed-runtime fields. Unknown model version/date are recorded as `unknown` rather than omitted.\n';
o += 'Replayable rows retain `prompt`, `raw_response`, `extracted_candidate`, `checker_diagnostics`, `repair_packet`, `test_output`, and `final_bundle`, each with a `*_sha256` field so artifacts can be matched to archived files or inline payloads.\n\n';

o += '## Limitations\n\n';
o += '- **nondeterminism**: model sampling, provider scheduling, local load, and transient tool latency can change individual rows.\n';
o += '- **k/N confidence**: pass rates are point estimates for the recorded k trials over N selected tasks, not confidence intervals.\n';
o += '- **token proxy limits**: output tokens exclude input, hidden reasoning, prompt-cache effects, and harness overhead.\n';
o += '- **scaffold fairness**: each arm gets the same repair budget, but language prompts, compilers, and diagnostics differ.\n';
o += '- **library comparability**: `habu-lib` and `habu-stdlib` measure checked helper surfaces, `habu-skeleton` measures scaffold help, while JS, Python, TypeScript, and Rust use their familiar standard library idioms.\n';
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

o += '## Category Reliability And Effort\n\n';
o += '| category | language | trials | green trials | trial pass | task pass@k | mean rounds | mean output tokens | median runtime ms | diagnostic complete |\n';
o += '|---|---|---:|---:|---:|---:|---:|---:|---:|---:|\n';
for (const category of categories) {
  for (const arm of ARMS) {
    const s = stats(arm, null, category);
    if (!s.trials) continue;
    o += `| ${q(category)} | ${LABEL[arm]} | ${s.trials} | ${s.passed} | ${pct(s.passed, s.trials)} | ${pct(s.passk, s.tasks)} | ${fmt(s.meanRounds)} | ${fmt(s.meanTok)} | ${fmt(s.medRuntime)} | ${pct(s.diagOk, s.trials)} |\n`;
  }
}
o += '\nCategory rows keep the same trial pass, task pass@k, repair-round, token, runtime, and diagnostic-quality semantics as the aggregate tables, but make weak task families visible.\n\n';

o += '## Habu Arm Deltas By Category\n\n';
o += '| category | raw task pass@k | stdlib task pass@k | skeleton task pass@k | stdlib - raw pass | skeleton - stdlib pass | stdlib/raw tokens | skeleton/stdlib tokens | stdlib/raw runtime | skeleton/stdlib runtime |\n';
o += '|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n';
for (const category of categories) {
  const raw = stats('habu-a', null, category);
  const stdlib = stats('habu-stdlib', null, category);
  const skeleton = stats('habu-skeleton', null, category);
  if (!raw.trials && !stdlib.trials && !skeleton.trials) continue;
  o += `| ${q(category)} | ${pct(raw.passk, raw.tasks)} | ${pct(stdlib.passk, stdlib.tasks)} | ${pct(skeleton.passk, skeleton.tasks)} | ${passDelta(stdlib, raw)} | ${passDelta(skeleton, stdlib)} | ${ratio(stdlib.meanTok, raw.meanTok)} | ${ratio(skeleton.meanTok, stdlib.meanTok)} | ${ratio(stdlib.medRuntime, raw.medRuntime)} | ${ratio(skeleton.medRuntime, stdlib.medRuntime)} |\n`;
}
o += '\nPositive pass deltas mean the later Habu arm solved more tasks in that category. Token and runtime ratios below 1x mean the later arm was cheaper among passing trials with measured values.\n\n';

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

const h = S['habu-a'], hl = S['habu-lib'], hs = S['habu-stdlib'], hk = S['habu-skeleton'];
const bestMean = Math.min(...BASELINE_ARMS.map(a => S[a].meanTok ?? Infinity));
const effort = ratio(h.meanTok, bestMean);
const libEffort = ratio(hl.meanTok, bestMean);
const libVsRaw = ratio(hl.meanTok, h.meanTok);
const rawRatios = taskRatioRows('habu-a');
const rawWorst = rawRatios.reduce((best, x) => !best || x.ratio > best.ratio ? x : best, null);
const baselinePass = BASELINE_ARMS.map(a => `${LABEL[a]} ${pct(S[a].passk, S[a].tasks)}`).join(', ');
const baselineTrials = BASELINE_ARMS.map(a => `${LABEL[a]} ${S[a].passed}/${S[a].trials}`).join(', ');
const baselineMeans = BASELINE_ARMS.map(a => `${LABEL[a]} **${fmt(S[a].meanTok)}**`).join(' / ');
const baselineRounds = BASELINE_ARMS.map(a => `${LABEL[a]} ${fmt(S[a].meanRounds)}`).join(', ');
o += '## Verdict — how does Habu stack up?\n\n';
o += `Task pass@k is Habu raw ${pct(h.passk, h.tasks)}, `;
o += hasLib ? `Habu + array helpers ${pct(hl.passk, hl.tasks)}, ` : 'Habu + array helpers —, ';
o += `Habu + stdlib ${pct(hs.passk, hs.tasks)}, Habu + skeleton ${pct(hk.passk, hk.tasks)}, `;
o += `${baselinePass}. `;
o += `At the stricter trial level: Habu raw ${h.passed}/${h.trials}, `;
o += hasLib ? `Habu + array helpers ${hl.passed}/${hl.trials}, ` : 'Habu + array helpers has no live rows, ';
o += `Habu + stdlib ${hs.passed}/${hs.trials}, Habu + skeleton ${hk.passed}/${hk.trials}, `;
o += `${baselineTrials}. `;
const notes = [];
if (h.nonpass) notes.push(`Raw Habu has ${h.nonpass} non-pass row(s); see the table below.`);
if (hasLib && hl.nonpass) notes.push(`The helper arm has ${hl.nonpass} non-pass row(s); see the table below.`);
if (hs.nonpass) notes.push(`The stdlib arm has ${hs.nonpass} non-pass row(s); see the table below.`);
if (hk.nonpass) notes.push(`The skeleton arm has ${hk.nonpass} non-pass row(s); see the table below.`);
for (const a of BASELINE_ARMS) {
  if (S[a].nonpass) notes.push(`${LABEL[a]} has ${S[a].nonpass} non-pass row(s); see the table below.`);
}
if (notes.length) o += notes.join(' ');
o += '\n\nThe raw-Habu cost split is bimodal:\n\n';
o += '- **Simple elementwise loops** (sum, square, negate, max) — raw Habu is **comparable or cheaper** than baseline languages '
   + '(its source is terse and the pattern is regular).\n';
if (rawWorst) {
  o += '- **Anything needing index tracking, carried state, or in-place rearrangement** (argmax, reverse, prefix-sum, '
     + `running-max) remains the hard tail. In this run the worst measured raw-Habu task is ${rawWorst.name} at `
     + `about **${rawWorst.ratio.toFixed(0)}x** (${fmt(rawWorst.habu)} vs ${fmt(rawWorst.best)} output tokens).\n\n`;
}
if (h.meanTok != null && bestMean !== Infinity) o += `Net: mean output-tokens-to-green Habu raw **${fmt(h.meanTok)}** vs ${baselineMeans} `
  + `— about **${effort}** the cheapest mainstream arm, almost entirely from the hard tail.\n\n`;
if (hasLib && hl.meanTok != null && bestMean !== Infinity) {
  o += `Habu + array helpers mean output-tokens-to-green is **${fmt(hl.meanTok)}**, about **${libEffort}** the cheapest mainstream arm `;
  o += `and **${libVsRaw}** of raw Habu's mean cost.\n\n`;
}
if (!hasLib) o += 'A raw-vs-library conclusion is intentionally withheld until `habu-lib` rows are collected in a live run.\n\n';
o += 'The raw-Habu gap is the corpus-familiarity tax: Habu\'s typed pointers (`arr:ptr`), `i cells arr + @`/`!` indexing, and\n';
o += 'in-place concatenative loops have much less model prior than JavaScript arrays, Python lists, TypeScript arrays, or Rust slices. That makes obvious\n';
o += 'stack shapes cheap and stateful/indexed loops expensive. Mean repair rounds on passing trials: Habu raw '
   + `${fmt(h.meanRounds)}, Habu + array helpers ${fmt(hl.meanRounds)}, Habu + stdlib ${fmt(hs.meanRounds)}, `
   + `Habu + skeleton ${fmt(hk.meanRounds)}, ${baselineRounds}.\n\n`;

if (allNonpass.length) {
  o += '## Non-Pass Rows\n\n';
  o += '| task | language | outcome | rounds | output tokens | wall s |\n|---|---|---|---:|---:|---:|\n';
  for (const r of allNonpass) {
    o += `| ${q(r.name)} | ${LABEL[r.arm] || r.arm} | ${q(r.outcome)} | ${r.rounds} | ${r.tokens} | ${fmt(sec(r.wall_ms))} |\n`;
  }
  o += '\n';
}

o += '## Per-Task Max Output Tokens\n\n';
o += '| task | Habu raw | Habu + helpers | Habu + stdlib | Habu + skeleton | JS | Python | TypeScript | Rust | raw/best | helpers/best | stdlib/best | skeleton/best | trial outcomes (raw/helpers/stdlib/skeleton/js/python/ts/rust) |\n|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|\n';
for (const n of taskNames) {
  const oc = arm => { const xs = rows.filter(z => z.name === n && z.arm === arm); return xs.length ? xs.map(x => `${x.outcome}/${x.rounds}`).join(',') : '—'; };
  const hh = taskTokenMax(n, 'habu-a'), ll = taskTokenMax(n, 'habu-lib'), ss = taskTokenMax(n, 'habu-stdlib'), kk = taskTokenMax(n, 'habu-skeleton'), jj = taskTokenMax(n, 'js'), pp = taskTokenMax(n, 'python'), tt = taskTokenMax(n, 'ts'), rr = taskTokenMax(n, 'rust');
  const best = bestBaselineTokenMax(n);
  const rawRatio = ratio(hh, best);
  const libRatio = ratio(ll, best);
  const stdlibRatio = ratio(ss, best);
  const skeletonRatio = ratio(kk, best);
  const outcomes = `raw ${oc('habu-a')}; helpers ${oc('habu-lib')}; stdlib ${oc('habu-stdlib')}; skeleton ${oc('habu-skeleton')}; js ${oc('js')}; python ${oc('python')}; ts ${oc('ts')}; rust ${oc('rust')}`;
  o += `| ${q(n)} | ${fmt(hh)} | ${fmt(ll)} | ${fmt(ss)} | ${fmt(kk)} | ${fmt(jj)} | ${fmt(pp)} | ${fmt(tt)} | ${fmt(rr)} | ${rawRatio} | ${libRatio} | ${stdlibRatio} | ${skeletonRatio} | ${q(outcomes)} |\n`;
}
o += '\nCells are max output tokens among passing trials with positive output-token counts. Habu ratios compare each Habu arm with the cheaper mainstream arm; '
   + 'the jump from ~1x on elementwise tasks to the hard-task tail is the main raw-Habu signal.\n';
process.stdout.write(o);
