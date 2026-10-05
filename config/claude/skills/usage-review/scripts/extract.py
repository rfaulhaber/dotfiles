#!/usr/bin/env python3
"""Condense Claude Code transcripts into one per-session summary JSON.

usage: extract.py OUT.json [--since YYYY-MM-DD]
"""
import argparse, json, os, re, glob, collections
ROOT = os.path.expanduser('~/.claude/projects')

def iso_date(v):
    if not re.fullmatch(r'\d{4}-\d{2}-\d{2}', v):
        raise argparse.ArgumentTypeError(f'expected YYYY-MM-DD, got {v!r}')
    return v

ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
ap.add_argument('out', help='output JSON path')
ap.add_argument('--since', type=iso_date, help='skip sessions that ended before this date')
args = ap.parse_args()

PRELUDE = re.compile(r'''^(?:cd\s+(?:"[^"]*"|'[^']*'|\S+)|[A-Za-z_]\w*=(?:"[^"]*"|'[^']*'|\$\([^)]*\)|\S*))\s*(?:&&|;|\n)\s*(.*)$''', re.S)
META_NAME = re.compile(r'''export\s+const\s+meta\s*=\s*\{\s*name:\s*(['"`])(.+?)\1''')

def strip_prelude(cmd):
    # Leading `cd DIR` / `VAR=value` statements say nothing about what the command does.
    cmd = cmd.strip()
    while m := PRELUDE.match(cmd): cmd = m.group(1).strip()
    return cmd

def classify_bash(cmd):
    c = strip_prelude(cmd)
    toks = c.split()
    if not toks: return 'empty'
    t0 = os.path.basename(toks[0])
    if t0 == 'ssh':
        host = next((t for t in toks[1:] if not t.startswith('-')), '?')
        return f'ssh:{host.split("@")[-1]}'
    if t0 == 'nix' and len(toks) > 1:
        if toks[1] == 'run' and len(toks) > 2: return f'nix run {toks[2].split("--")[0]}'
        if toks[1] == 'shell' and len(toks) > 2: return f'nix shell {toks[2]}'
        return f'nix {toks[1]}'
    if t0 in ('git','gh','systemctl','journalctl','podman','zfs','zpool','cargo','npm','pnpm','nu','python3','sops','ip','curl','deploy-rs'):
        sub = toks[1] if len(toks) > 1 else ''
        return f'{t0} {sub}'
    if t0 == 'sudo' and len(toks) > 1: return f'sudo {toks[1]}'
    return t0

def text_of(content):
    if isinstance(content, str): return content
    out = []
    for c in content or []:
        if isinstance(c, dict) and c.get('type') == 'text': out.append(c.get('text',''))
    return '\n'.join(out)

def workflow_call(inp):
    # Transcripts carry a script body (inline) or a scriptPath (file written earlier, or a resume).
    # A path call has no body to measure, so bytes stays 0 and it is not a saved workflow.
    script = inp.get('script')
    if isinstance(script, str):
        m = META_NAME.search(script)
        return dict(name=m.group(2) if m else None, bytes=len(script), saved=False)
    if inp.get('name'):
        return dict(name=inp['name'], bytes=0, saved=True)
    path = inp.get('scriptPath')
    if path:
        base = re.sub(r'\.js$', '', os.path.basename(path))
        return dict(name=re.sub(r'-wf_[0-9a-f-]+$', '', base), bytes=0, saved=False)
    return dict(name=None, bytes=0, saved=False)

def process_file(path, is_sub, parent, project):
    s = dict(path=path, is_sub=is_sub, parent=parent, project=project, start=None, end=None, versions=set(), cwd=None,
             branches=set(), titles=[], prompts=[], tools=collections.Counter(), bash=[], errors=[],
             agents=[], skills=[], edits=collections.Counter(), models=collections.Counter(),
             cost=None, compacts=0, perm_modes=collections.Counter(), hooks=collections.Counter(),
             ask_user=0, lines=0, slash=[], effort=collections.Counter(), sidechain_lines=0,
             mcp=collections.Counter(), task_notifs=0, webfetch=[], thinking_blocks=0, workflows=[])
    uses = {}  # tool_use id -> (tool, command_class), to attribute errors that arrive in later lines
    with open(path, errors='replace') as fh:
        for line in fh:
            s['lines'] += 1
            try: d = json.loads(line)
            except Exception: continue
            t = d.get('type')
            ts = d.get('timestamp')
            if isinstance(ts, str):
                if s['start'] is None or ts < s['start']: s['start'] = ts
                if s['end'] is None or ts > s['end']: s['end'] = ts
            if d.get('version'): s['versions'].add(d['version'])
            if d.get('cwd') and not s['cwd']: s['cwd'] = d['cwd']
            if d.get('gitBranch'): s['branches'].add(d['gitBranch'])
            if d.get('isSidechain'): s['sidechain_lines'] += 1
            if t == 'ai-title': s['titles'].append(d.get('aiTitle'))
            elif t == 'permission-mode': s['perm_modes'][d.get('permissionMode')] += 1
            elif t == 'cost-state': s['cost'] = {k: d.get(k) for k in ('totalCostUSD','totalAPIDuration','totalToolDuration','totalLinesAdded','totalLinesRemoved','totalDuration','modelUsage')}
            elif t == 'attachment':
                a = d.get('attachment') or {}
                if a.get('hookName'): s['hooks'][a['hookName']] += 1
            elif t == 'queue-operation':
                if '<task-notification>' in (d.get('content') or ''): s['task_notifs'] += 1
            elif t == 'user':
                if d.get('isCompactSummary'): s['compacts'] += 1
                m = d.get('message') or {}
                content = m.get('content')
                if isinstance(content, list):
                    for c in content:
                        if isinstance(c, dict) and c.get('type') == 'tool_result':
                            if c.get('is_error'):
                                body = c.get('content')
                                if isinstance(body, list): body = text_of(body)
                                tool, cls = uses.get(c.get('tool_use_id'), ('unknown', None))
                                s['errors'].append(dict(tool=tool, command_class=cls, text=str(body)[:240]))
                txt = text_of(content).strip()
                if txt and not d.get('isMeta') and not txt.startswith('<task-notification>') and not txt.startswith('<local-command') and not d.get('isCompactSummary'):
                    # user-authored prompt (may include command-name tags for skills)
                    cm = re.search(r'<command-name>([^<]+)</command-name>', txt)
                    if cm: s['slash'].append(cm.group(1).strip())
                    if not txt.startswith('<system-reminder>') and 'tool_result' not in txt[:20]:
                        s['prompts'].append((ts, txt[:500]))
            elif t == 'assistant':
                m = d.get('message') or {}
                if m.get('model'): s['models'][m['model']] += 1
                if d.get('effort'): s['effort'][str(d['effort'])] += 1
                for c in m.get('content') or []:
                    if not isinstance(c, dict): continue
                    ct = c.get('type')
                    if ct == 'thinking': s['thinking_blocks'] += 1
                    if ct != 'tool_use': continue
                    name = c.get('name'); inp = c.get('input') or {}
                    cls = None
                    s['tools'][name] += 1
                    if name and name.startswith('mcp__'): s['mcp'][name] += 1
                    if name == 'Bash':
                        cmd = inp.get('command','')
                        cls = classify_bash(cmd)
                        s['bash'].append((c.get('id'), cls, cmd[:300].replace('\n',' ')))
                    elif name == 'Agent':
                        s['agents'].append(dict(id=c.get('id'), type=inp.get('subagent_type'), model=inp.get('model'), desc=inp.get('description'), plen=len(inp.get('prompt') or '')))
                    elif name == 'Skill':
                        s['skills'].append(inp.get('skill'))
                    elif name in ('Edit','Write','MultiEdit','NotebookEdit'):
                        s['edits'][inp.get('file_path','?')] += 1
                    elif name == 'AskUserQuestion':
                        s['ask_user'] += 1
                    elif name in ('WebFetch',):
                        s['webfetch'].append((inp.get('url') or '')[:120])
                    elif name == 'Workflow':
                        s['workflows'].append(workflow_call(inp))
                    uses[c.get('id')] = (name, cls)
    s['versions'] = sorted(s['versions']); s['branches'] = sorted(s['branches'])
    for k in ('tools','edits','models','perm_modes','hooks','effort','mcp'): s[k] = dict(s[k])
    return s

def collect_session(f, project):
    sid = os.path.basename(f)[:-6]
    s = process_file(f, False, None, project); s['sid'] = sid
    # Subagents are only parsed for sessions that survive the cutoff; they are most of the bytes.
    if args.since and s['end'] and s['end'] < args.since: return None
    # Workflow-spawned agents live a level deeper (subagents/workflows/wf_*/agent-*.jsonl).
    pattern = os.path.join(os.path.dirname(f), sid, 'subagents', '**', 'agent-*.jsonl')
    s['subagents'] = [process_file(sf, True, sid, project) for sf in sorted(glob.glob(pattern, recursive=True))]
    return s

sessions = []
for proj in sorted(os.listdir(ROOT)):
    pdir = os.path.join(ROOT, proj)
    if not os.path.isdir(pdir): continue
    # nested project dirs (e.g. sigha/-home-ryan-Projects-sfdc-formula-analyzer)
    dirs = [(pdir, proj)] + [(n, os.path.basename(n)) for n in sorted(glob.glob(os.path.join(pdir, '-home-*')))]
    for d, name in dirs:
        for f in sorted(glob.glob(os.path.join(d, '*.jsonl'))):
            s = collect_session(f, name)
            if s: sessions.append(s)
with open(args.out, 'w') as fh: json.dump(sessions, fh)
print('sessions', len(sessions), 'subagent files', sum(len(s['subagents']) for s in sessions))
