#!/usr/bin/env python3
"""Aggregate keyboard statistics for layout analysis.

Collects ONLY aggregates from a ZSA keyboard's evdev nodes: per-key press
counts bucketed by held modifiers, adjacent-key (bigram) counts with summed
inter-key latency, and autorepeat counts. No key sequences or per-event
timestamps are stored, so the data cannot reconstruct typed text beyond
pair frequencies. Create <state-dir>/pause to suspend recording (e.g.
before typing a password), remove it to resume.
"""

import argparse
import glob
import json
import os
import select
import signal
import struct
import sys
import time
from collections import Counter
from datetime import datetime, timezone

DEVICE_GLOB = "/dev/input/by-id/*ZSA*event-kbd*"
EVENT_FMT = "llHHi"  # struct input_event on 64-bit: timeval + type + code + value
EVENT_SIZE = struct.calcsize(EVENT_FMT)
EV_KEY = 0x01
BIGRAM_MAX_GAP_MS = 1500  # pairs across longer pauses aren't typing rolls
SAVE_INTERVAL_S = 60
RESCAN_INTERVAL_S = 30

# keycode -> modifier bit (left/right share a bit)
MOD_BITS = {42: 1, 54: 1, 29: 2, 97: 2, 56: 4, 100: 4, 125: 8, 126: 8}
MOD_NAMES = {1: "Shift", 2: "Ctrl", 4: "Alt", 8: "Gui"}

KEYNAMES = {
    1: "Esc", 12: "-", 13: "=", 14: "Bspc", 15: "Tab", 26: "[", 27: "]",
    28: "Enter", 29: "LCtrl", 39: ";", 40: "'", 41: "`", 42: "LShift",
    43: "\\", 51: ",", 52: ".", 53: "/", 54: "RShift", 55: "KP*",
    56: "LAlt", 57: "Space", 58: "Caps", 69: "NumLk", 96: "KPEnter",
    97: "RCtrl", 98: "KP/", 100: "RAlt", 102: "Home", 103: "Up",
    104: "PgUp", 105: "Left", 106: "Right", 107: "End", 108: "Down",
    109: "PgDn", 110: "Ins", 111: "Del", 113: "Mute", 114: "Vol-",
    115: "Vol+", 117: "KP=", 119: "Pause", 125: "LGui", 126: "RGui",
    127: "Menu", 163: "Next", 164: "Play", 165: "Prev", 166: "Stop",
}
KEYNAMES.update({i + 1: c for i, c in enumerate("1234567890", start=1)})
for i, c in enumerate("qwertyuiop"):
    KEYNAMES[16 + i] = c
for i, c in enumerate("asdfghjkl"):
    KEYNAMES[30 + i] = c
for i, c in enumerate("zxcvbnm"):
    KEYNAMES[44 + i] = c
for i, name in enumerate(["KP7", "KP8", "KP9", "KP-", "KP4", "KP5", "KP6",
                          "KP+", "KP1", "KP2", "KP3", "KP0", "KP."]):
    KEYNAMES[71 + i] = name
for i in range(10):
    KEYNAMES[59 + i] = f"F{i + 1}"
KEYNAMES[87] = "F11"
KEYNAMES[88] = "F12"
for i in range(12):
    KEYNAMES[183 + i] = f"F{i + 13}"

US_BASE = {12: "-", 13: "=", 26: "[", 27: "]", 39: ";", 40: "'", 41: "`",
           43: "\\", 51: ",", 52: ".", 53: "/", 57: "␣", 28: "⏎", 15: "⇥"}
US_BASE.update({i + 1: c for i, c in enumerate("1234567890", start=1)})
US_SHIFT = {2: "!", 3: "@", 4: "#", 5: "$", 6: "%", 7: "^", 8: "&", 9: "*",
            10: "(", 11: ")", 12: "_", 13: "+", 26: "{", 27: "}", 39: ":",
            40: '"', 41: "~", 43: "|", 51: "<", 52: ">", 53: "?"}
for code, name in KEYNAMES.items():
    if len(name) == 1 and name.isalpha():
        US_BASE[code] = name
        US_SHIFT[code] = name.upper()


def keyname(code):
    return KEYNAMES.get(code, f"KEY_{code}")


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


class Aggregator:
    def __init__(self, state):
        self.state = state
        self.counts = state.setdefault("counts", {})
        self.bigrams = state.setdefault("bigrams", {})
        self.repeats = state.setdefault("repeats", {})
        self.held = set()
        self.prev_code = None
        self.prev_ms = 0
        self.paused = False
        self.dirty = False

    def key_event(self, t_ms, code, value):
        if code in MOD_BITS:
            if value == 1:
                if not self.paused:
                    self._count_press(code)
                self.held.add(code)
            elif value == 0:
                self.held.discard(code)
            return
        if self.paused:
            self.prev_code = None
            return
        if value == 1:
            self._count_press(code)
            gap = t_ms - self.prev_ms
            if self.prev_code is not None and 0 <= gap <= BIGRAM_MAX_GAP_MS:
                pair = self.bigrams.setdefault(f"{self.prev_code}:{code}", [0, 0])
                pair[0] += 1
                pair[1] += int(gap)
            self.prev_code = code
            self.prev_ms = t_ms
        elif value == 2:
            key = str(code)
            self.repeats[key] = self.repeats.get(key, 0) + 1
            self.state["total_repeats"] = self.state.get("total_repeats", 0) + 1
            self.dirty = True

    def _count_press(self, code):
        mask = 0
        for held in self.held:
            mask |= MOD_BITS[held]
        key = f"{mask}:{code}"
        self.counts[key] = self.counts.get(key, 0) + 1
        self.state["total_presses"] = self.state.get("total_presses", 0) + 1
        self.dirty = True


def load_state(path):
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, ValueError):
        return {"version": 1, "started": now_iso()}


def save_state(path, state):
    state["updated"] = now_iso()
    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        json.dump(state, f)
    os.replace(tmp, path)


def collect(sdir):
    os.makedirs(sdir, exist_ok=True)
    state_path = os.path.join(sdir, "stats.json")
    pause_path = os.path.join(sdir, "pause")
    state = load_state(state_path)
    agg = Aggregator(state)

    stopping = []
    for sig in (signal.SIGTERM, signal.SIGINT):
        signal.signal(sig, lambda *_: stopping.append(True))

    fds = {}  # fd -> path
    last_scan = 0.0
    last_save = time.time()
    print(f"keystats: collecting into {state_path}", flush=True)
    while not stopping:
        now = time.time()
        if now - last_scan >= RESCAN_INTERVAL_S or not fds:
            last_scan = now
            for path in glob.glob(DEVICE_GLOB):
                real = os.path.realpath(path)
                if real in fds.values():
                    continue
                try:
                    fd = os.open(real, os.O_RDONLY | os.O_NONBLOCK)
                except OSError as e:
                    print(f"keystats: cannot open {path}: {e}", flush=True)
                    continue
                fds[fd] = real
                print(f"keystats: watching {path}", flush=True)
            if not fds:
                time.sleep(5)
                continue
        was_paused = agg.paused
        agg.paused = os.path.exists(pause_path)
        if agg.paused != was_paused:
            print(f"keystats: {'paused' if agg.paused else 'resumed'}", flush=True)

        ready, _, _ = select.select(list(fds), [], [], 1.0)
        for fd in ready:
            try:
                data = os.read(fd, EVENT_SIZE * 256)
            except OSError:
                data = b""
            if not data:
                print(f"keystats: lost {fds.pop(fd)}", flush=True)
                os.close(fd)
                last_scan = 0.0
                continue
            for off in range(0, len(data) - EVENT_SIZE + 1, EVENT_SIZE):
                sec, usec, etype, code, value = struct.unpack_from(EVENT_FMT, data, off)
                if etype == EV_KEY:
                    agg.key_event(sec * 1000 + usec // 1000, code, value)
        if agg.dirty and now - last_save >= SAVE_INTERVAL_S:
            save_state(state_path, state)
            agg.dirty = False
            last_save = now
    if agg.dirty:
        save_state(state_path, state)
    print("keystats: stopped", flush=True)


def report(sdir, top):
    state = load_state(os.path.join(sdir, "stats.json"))
    total = state.get("total_presses", 0)
    if not total:
        print("no data collected yet")
        return

    def pm(n):
        return f"{1000 * n / total:7.2f}"

    by_code = Counter()
    by_char = Counter()
    chords = Counter()
    chorded_keys = Counter()
    shifted = 0
    for key, n in state["counts"].items():
        mask, code = (int(x) for x in key.split(":"))
        by_code[code] += n
        if mask == 1:
            shifted += n
        if mask == 0 and code in US_BASE:
            by_char[US_BASE[code]] += n
        elif mask == 1 and code in US_SHIFT:
            by_char[US_SHIFT[code]] += n
        if mask & ~1:
            mods = "+".join(name for bit, name in sorted(MOD_NAMES.items()) if mask & bit)
            chords[mods] += n
            chorded_keys[f"{mods}+{keyname(code)}"] += n

    print(f"presses: {total:,}   repeats: {state.get('total_repeats', 0):,}   "
          f"span: {state.get('started', '?')} → {state.get('updated', '?')}")
    print(f"shift-only share: {100 * shifted / total:.1f}%\n")

    print(f"top {top} keys (per-mille of presses):")
    for code, n in by_code.most_common(top):
        print(f"  {keyname(code):>6} {pm(n)}  ({n:,})")

    print(f"\ntop {top} effective characters:")
    for ch, n in by_char.most_common(top):
        print(f"  {ch:>3} {pm(n)}  ({n:,})")

    print("\nmodifier chords (non-shift):")
    for mods, n in chords.most_common(10):
        print(f"  {mods:>14} {pm(n)}  ({n:,})")
    print("\ntop chorded keys:")
    for name, n in chorded_keys.most_common(15):
        print(f"  {name:>20} ({n:,})")

    pairs = [(k, n, ms) for k, (n, ms) in state["bigrams"].items()]
    print(f"\ntop {top} bigrams (count, mean gap ms):")
    for key, n, ms in sorted(pairs, key=lambda p: -p[1])[:top]:
        a, b = (keyname(int(x)) for x in key.split(":"))
        print(f"  {a:>6} → {b:<6} {n:6,}  {ms / n:6.0f}ms")

    slow = [(k, n, ms) for k, n, ms in pairs if n >= 30]
    print("\nslowest common bigrams (n ≥ 30) — awkwardness candidates:")
    for key, n, ms in sorted(slow, key=lambda p: -p[2] / p[1])[:15]:
        a, b = (keyname(int(x)) for x in key.split(":"))
        print(f"  {a:>6} → {b:<6} {ms / n:6.0f}ms  (n={n})")

    print("\ntop held keys (autorepeats):")
    for code, n in Counter({int(k): v for k, v in state["repeats"].items()}).most_common(10):
        print(f"  {keyname(code):>6} ({n:,})")


def selftest():
    agg = Aggregator({})
    events = [
        (0, 30, 1), (30, 30, 0),        # a
        (100, 42, 1),                   # shift down
        (150, 10, 1), (160, 10, 0),     # shift+9 = (
        (170, 42, 0),                   # shift up
        (200, 1, 1), (210, 1, 2), (220, 1, 2), (230, 1, 0),  # esc + repeats
        (5000, 30, 1), (5030, 30, 0),   # gap > max: no bigram
    ]
    for ev in events:
        agg.key_event(*ev)
    assert agg.state["total_presses"] == 5, agg.state
    assert agg.counts == {"0:30": 2, "0:42": 1, "1:10": 1, "0:1": 1}, agg.counts
    assert agg.bigrams == {"30:10": [1, 150], "10:1": [1, 50]}, agg.bigrams
    assert agg.repeats == {"1": 2} and agg.state["total_repeats"] == 2
    agg.paused = True
    agg.key_event(5100, 31, 1)
    assert agg.state["total_presses"] == 5
    print("selftest ok")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["collect", "report", "pause", "resume", "status", "selftest"])
    parser.add_argument("--state-dir", default=os.environ.get("STATE_DIRECTORY", "/var/lib/keystats"))
    parser.add_argument("--top", type=int, default=25)
    args = parser.parse_args()
    sdir = args.state_dir

    if args.command == "collect":
        collect(sdir)
    elif args.command == "report":
        report(sdir, args.top)
    elif args.command == "pause":
        open(os.path.join(sdir, "pause"), "w").close()
        print("paused (rm the pause file or run `keystats resume` to resume)")
    elif args.command == "resume":
        try:
            os.remove(os.path.join(sdir, "pause"))
        except FileNotFoundError:
            pass
        print("resumed")
    elif args.command == "status":
        state = load_state(os.path.join(sdir, "stats.json"))
        paused = os.path.exists(os.path.join(sdir, "pause"))
        print(f"presses: {state.get('total_presses', 0):,}  "
              f"updated: {state.get('updated', 'never')}  "
              f"{'PAUSED' if paused else 'recording'}")
    elif args.command == "selftest":
        selftest()


if __name__ == "__main__":
    main()
