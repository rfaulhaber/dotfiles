#!/usr/bin/env python3
"""Print Markdown usage tables from extract.py output and ~/.claude/history.jsonl.

usage: summarize.py EXTRACT.json [--since YYYY-MM-DD]
"""
import argparse, collections, datetime, json, os, re, statistics
HISTORY = os.path.expanduser('~/.claude/history.jsonl')

def iso_date(v):
    if not re.fullmatch(r'\d{4}-\d{2}-\d{2}', v):
        raise argparse.ArgumentTypeError(f'expected YYYY-MM-DD, got {v!r}')
    return v

ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
ap.add_argument('extract', help='JSON written by extract.py')
ap.add_argument('--since', type=iso_date, help='drop sessions that started, and history rows made, before this date')
args = ap.parse_args()

with open(args.extract) as fh: sessions = json.load(fh)
if args.since:
    sessions = [s for s in sessions if s.get('start') and s['start'] >= args.since]

def subs(s): return s.get('subagents') or []

def files():
    for s in sessions:
        yield s, False
        for x in subs(s): yield x, True

def short_dir(name): return re.sub(r'^-home-[^-]+-(Projects-)?', '', name or '?') or '~'
def short_path(path): return re.sub(r'^/home/[^/]+/(Projects/)?', '', path or '?') or '~'

def usd(x): return f'${x:,.2f}'
def pct(a, b): return f'{100 * a / b:.1f}%' if b else '-'

def cell(v):
    if isinstance(v, int): v = f'{v:,}'
    return str(v).replace('|', '\\|').replace('\n', ' ')

def section(title): print(f'## {title}\n')
def subsection(title): print(f'### {title}\n')

def table(headers, rows, align=None):
    align = align or 'l' + 'r' * (len(headers) - 1)
    if not rows:
        print('_no data_\n')
        return
    print('| ' + ' | '.join(headers) + ' |')
    print('|' + '|'.join('---:' if a == 'r' else '---' for a in align) + '|')
    for r in rows: print('| ' + ' | '.join(cell(v) for v in r) + ' |')
    print()

def split_counts(key):
    """Counter of `key` (a dict of name -> count) summed over main sessions and over subagents."""
    main, sub = collections.Counter(), collections.Counter()
    for f, is_sub in files(): (sub if is_sub else main).update(f.get(key) or {})
    return main, sub

def split_list(extract):
    """Same as split_counts for per-file lists; `extract` maps one list item to a hashable key."""
    main, sub = collections.Counter(), collections.Counter()
    for f, is_sub in files():
        for k, v in extract(f): (sub if is_sub else main)[k] += v
    return main, sub

def cost_of(f): return (f.get('cost') or {}).get('totalCostUSD') or 0.0

def errors_of(f): return [e for e in f.get('errors') or [] if isinstance(e, dict)]

print('# Claude Code usage summary\n')
starts = sorted(s['start'] for s in sessions if s.get('start'))
n_sub = sum(len(subs(s)) for s in sessions)
print(f'- Sessions: {len(sessions):,} ({n_sub:,} subagent files)')
if starts: print(f'- Session starts: {starts[0][:10]} to {starts[-1][:10]}')
if args.since: print(f'- Filtered to sessions and prompts since {args.since}')
print()

# a. sessions and cost
section("Sessions and estimated cost by project and month (Claude Code's list-price estimates, not billed amounts)")
by_proj, by_month = {}, {}
for s in sessions:
    for agg, key in ((by_proj, short_dir(s.get('project'))), (by_month, (s.get('start') or '')[:7] or '(no timestamp)')):
        a = agg.setdefault(key, [0, 0, 0.0])
        a[0] += 1; a[1] += len(subs(s)); a[2] += cost_of(s)
total_cost = sum(a[2] for a in by_proj.values())
no_cost = sum(1 for s in sessions if not s.get('cost'))
print(f'Total estimated cost {usd(total_cost)}; {no_cost} sessions have no cost record and count as $0.\n')
subsection('By project')
rows = sorted(by_proj.items(), key=lambda kv: -kv[1][2])
table(['Project', 'Sessions', 'Subagent files', 'Est. cost', 'Share'], [(k, a[0], a[1], usd(a[2]), pct(a[2], total_cost)) for k, a in rows])
subsection('By month')
table(['Month', 'Sessions', 'Subagent files', 'Est. cost', 'Share'], [(k, a[0], a[1], usd(a[2]), pct(a[2], total_cost)) for k, a in sorted(by_month.items())])

# b. modelUsage
section('modelUsage by model (main sessions and subagents)')
TOKEN_COLS = (('inputTokens', 'Input'), ('outputTokens', 'Output'), ('cacheReadInputTokens', 'Cache read'),
              ('cacheCreationInputTokens', 'Cache write'), ('thinkingTokens', 'Thinking'), ('webSearchRequests', 'Web searches'))
usage = collections.defaultdict(collections.Counter)
for f, _ in files():
    for model, u in ((f.get('cost') or {}).get('modelUsage') or {}).items():
        for k, v in (u or {}).items():
            if isinstance(v, (int, float)): usage[model][k] += v
model_total = sum(u['costUSD'] for u in usage.values())
cols = [(k, h) for k, h in TOKEN_COLS if any(u[k] for u in usage.values())]
rows = [(m, usd(u['costUSD']), pct(u['costUSD'], model_total), *(int(u[k]) for k, _ in cols))
        for m, u in sorted(usage.items(), key=lambda kv: -kv[1]['costUSD'])]
table(['Model', 'Est. cost', 'Share', *(h for _, h in cols)], rows)

# c. tools
section('Top 20 tools, main sessions vs subagents')
tool_main, tool_sub = split_counts('tools')
rows = sorted(((t, tool_main[t], tool_sub[t], tool_main[t] + tool_sub[t]) for t in set(tool_main) | set(tool_sub)), key=lambda r: -r[3])
table(['Tool', 'Main', 'Subagent', 'Total'], rows[:20])

# d. Bash command classes
section('Top 25 Bash command classes')
bash_main, bash_sub = split_list(lambda f: ((b[1], 1) for b in f.get('bash') or []))
err_main, err_sub = split_list(lambda f: ((e.get('command_class'), 1) for e in errors_of(f) if e.get('tool') == 'Bash'))
def bash_rows(keys): return sorted(((k, bash_main[k], bash_sub[k], err_main[k], err_sub[k]) for k in keys), key=lambda r: -(r[1] + r[2]))
table(['Class', 'Main', 'Subagent', 'Main errors', 'Subagent errors'], bash_rows(set(bash_main) | set(bash_sub))[:25])
subsection('ssh calls by host')
table(['Host', 'Main', 'Subagent', 'Main errors', 'Subagent errors'], bash_rows(k for k in set(bash_main) | set(bash_sub) if k.startswith('ssh:'))[:25])

# e. Agent calls
section('Agent calls by subagent_type and model')
agent_calls = collections.defaultdict(lambda: [0, 0])
for f, _ in files():
    for a in f.get('agents') or []:
        c = agent_calls[(a.get('type') or '(unset)', a.get('model') or '(unset)')]
        c[0] += 1; c[1] += a.get('plen') or 0
n_agents = sum(c[0] for c in agent_calls.values())
n_unset = sum(c[0] for (_, m), c in agent_calls.items() if m == '(unset)')
print(f'{n_agents:,} Agent calls; {n_unset:,} ({pct(n_unset, n_agents)}) set no model.\n')
rows = sorted(((t, m, c[0], c[1] // c[0]) for (t, m), c in agent_calls.items()), key=lambda r: -r[2])
table(['subagent_type', 'model', 'Calls', 'Avg prompt chars'], rows, 'llrr')

# f. Skills
section('Skill invocations by name')
skill_main, skill_sub = split_list(lambda f: ((k or '(unset)', 1) for k in f.get('skills') or []))
rows = sorted(((k, skill_main[k], skill_sub[k], skill_main[k] + skill_sub[k]) for k in set(skill_main) | set(skill_sub)), key=lambda r: -r[3])
table(['Skill', 'Main', 'Subagent', 'Total'], rows)

# g. Tool-result errors
def norm_line(text):
    lines = [l.strip() for l in (text or '').splitlines() if l.strip()]
    if not lines: return '(empty)'
    line = lines[0]
    # Bash errors open with a bare exit code, which says nothing about the cause.
    if re.fullmatch(r'Exit code \d+', line) and len(lines) > 1: line = f'{line}: {lines[1]}'
    line = re.sub(r'https?://\S+', '<path>', line)
    line = re.sub(r'(?<![\w<])(?:~|\.{1,2})?(?:/[^\s:\'"`<>()]+)+', '<path>', line)
    line = re.sub(r'\b(?:[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}|0x[0-9a-f]+|(?=[0-9a-f]*\d)[0-9a-f]{7,})\b', '<hex>', line, flags=re.I)
    return re.sub(r'\d+', '<n>', line)

section('Tool-result errors')
by_line, line_tools = collections.Counter(), collections.defaultdict(collections.Counter)
for f, _ in files():
    for e in errors_of(f):
        k = norm_line(e.get('text'))[:110]
        by_line[k] += 1; line_tools[k][e.get('tool') or 'unknown'] += 1
print(f'{sum(by_line.values()):,} errors across {sum(tool_main.values()) + sum(tool_sub.values()):,} tool calls.\n')
subsection('Top 20 by normalised first line')
table(['Count', 'Top tool', 'First line'], [(n, line_tools[k].most_common(1)[0][0], k) for k, n in by_line.most_common(20)], 'rll')
tool_err = collections.Counter(e.get('tool') or 'unknown' for f, _ in files() for e in errors_of(f))
subsection('By tool')
table(['Tool', 'Errors', 'Calls', 'Error rate'], [(t, n, tool_main[t] + tool_sub[t], pct(n, tool_main[t] + tool_sub[t])) for t, n in tool_err.most_common(15)])
subsection('By Bash command class')
cls_err = err_main + err_sub
table(['Class', 'Errors', 'Calls', 'Error rate'], [(k, n, bash_main[k] + bash_sub[k], pct(n, bash_main[k] + bash_sub[k])) for k, n in cls_err.most_common(15)])

# h. Workflows
section('Workflow calls')
wf = [w for f, _ in files() for w in f.get('workflows') or []]
# Inline calls are the only ones with a measurable script body; path calls record 0 bytes.
inline = [w for w in wf if w.get('bytes')]
saved = [w for w in wf if w.get('saved')]
sizes = [w['bytes'] for w in inline]
table(['Metric', 'Value'], [
    ('Calls', len(wf)), ('Saved (by name)', len(saved)), ('Inline script', len(inline)),
    ('Script file (scriptPath)', len(wf) - len(saved) - len(inline)),
    ('Median inline script bytes', int(statistics.median(sizes)) if sizes else 0), ('Total inline script bytes', sum(sizes))])
subsection('Top names')
names = collections.defaultdict(lambda: [0, 0])
for w in wf:
    n = names[w.get('name') or '(unnamed)']
    n[0] += 1; n[1] += w.get('bytes') or 0
table(['Name', 'Calls', 'Script bytes'], [(k, v[0], v[1]) for k, v in sorted(names.items(), key=lambda kv: (-kv[1][0], -kv[1][1]))[:15]])

# i. Edits
section('Edits by top-level directory, per project (top 15)')
def top_dir(path, cwd):
    if not path.startswith('/'): return '?'
    root = (cwd or '').rstrip('/')
    if root and path.startswith(root + '/'):
        rel = path[len(root) + 1:]
        return rel.split('/')[0] + '/' if '/' in rel else '(root files)'
    parts = re.sub(r'^/home/[^/]+/', '~/', path).split('/')
    # Sibling checkouts and worktrees under ~/Projects are told apart by their own directory.
    return '(outside) ' + '/'.join(parts[:3 if parts[:2] == ['~', 'Projects'] else 2])
edits = collections.Counter()
for f, _ in files():
    for path, n in (f.get('edits') or {}).items(): edits[(short_dir(f.get('project')), top_dir(path, f.get('cwd')))] += n
table(['Project', 'Directory', 'Edits'], [(p, d, n) for (p, d), n in edits.most_common(15)], 'llr')

# j. Hooks
section('Hook attachments by name')
hook_main, hook_sub = split_counts('hooks')
rows = sorted(((k, hook_main[k], hook_sub[k], hook_main[k] + hook_sub[k]) for k in set(hook_main) | set(hook_sub)), key=lambda r: -r[3])
table(['Hook', 'Main', 'Subagent', 'Total'], rows[:25])

# k. history.jsonl
section('Prompt history (~/.claude/history.jsonl)')
since_ms = datetime.datetime.fromisoformat(args.since).replace(tzinfo=datetime.timezone.utc).timestamp() * 1000 if args.since else 0
per_month, per_project, usage_month = collections.Counter(), collections.Counter(), collections.Counter()
try:
    with open(HISTORY, errors='replace') as fh:
        for line in fh:
            try: d = json.loads(line)
            except Exception: continue
            ts = d.get('timestamp')
            if not isinstance(ts, (int, float)) or ts < since_ms: continue
            month = datetime.datetime.fromtimestamp(ts / 1000, datetime.timezone.utc).strftime('%Y-%m')
            per_month[month] += 1
            per_project[short_path(d.get('project'))] += 1
            # Whole-word match so a skill named /usage-review is not counted as /usage.
            if re.match(r'/usage(\s|$)', (d.get('display') or '').lstrip()): usage_month[month] += 1
except OSError as e:
    print(f'_cannot read history: {e}_\n')
subsection('Prompts per month')
table(['Month', 'Prompts'], sorted(per_month.items()))
subsection('Prompts per project (top 20)')
table(['Project', 'Prompts'], per_project.most_common(20))
subsection('`/usage` invocations per month')
table(['Month', 'Invocations'], sorted(usage_month.items()))
