"""Build a full-text-searchable SQLite database from an archivr --json archive.

Every post file is NPF (Tumblr's block format). The text a blog wrote itself
is the top-level `content`; reblogged text lives in `trail`, one entry per
earlier hop, root first. They are indexed as separate columns so a search can
rank or filter on the blog's own words.

The build always starts from scratch into <output>.tmp and is renamed over
<output> only once complete, so readers never see a half-built database.
"""

import argparse
import json
import os
import sqlite3
import sys
from concurrent.futures import ThreadPoolExecutor
from datetime import UTC, datetime

SCHEMA = """
CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);

CREATE TABLE posts (
    id INTEGER PRIMARY KEY,
    timestamp INTEGER NOT NULL,
    date TEXT NOT NULL,
    original_type TEXT,
    post_url TEXT,
    short_url TEXT,
    slug TEXT,
    summary TEXT,
    note_count INTEGER,
    is_reblog INTEGER NOT NULL,
    parent_post_url TEXT,
    body TEXT NOT NULL,
    trail_text TEXT NOT NULL,
    tags TEXT NOT NULL,
    authors TEXT NOT NULL,
    json_path TEXT NOT NULL,
    html_path TEXT
);
CREATE INDEX posts_timestamp ON posts (timestamp);

CREATE TABLE tags (
    post_id INTEGER NOT NULL REFERENCES posts (id),
    position INTEGER NOT NULL,
    tag TEXT NOT NULL,
    PRIMARY KEY (post_id, position)
);
CREATE INDEX tags_tag ON tags (tag COLLATE NOCASE);

CREATE TABLE trail (
    post_id INTEGER NOT NULL REFERENCES posts (id),
    position INTEGER NOT NULL,
    blog TEXT,
    source_post_id TEXT,
    is_root INTEGER NOT NULL,
    text TEXT NOT NULL,
    PRIMARY KEY (post_id, position)
);
CREATE INDEX trail_blog ON trail (blog);

CREATE TABLE media (
    post_id INTEGER NOT NULL REFERENCES posts (id),
    position INTEGER NOT NULL,
    in_trail INTEGER NOT NULL,
    kind TEXT NOT NULL,
    path TEXT,
    url TEXT,
    mime TEXT,
    alt_text TEXT,
    PRIMARY KEY (post_id, position)
);
CREATE INDEX media_kind ON media (kind);

CREATE VIRTUAL TABLE posts_fts USING fts5 (
    body, trail_text, tags, authors,
    content = 'posts',
    content_rowid = 'id',
    tokenize = 'porter unicode61 remove_diacritics 2',
    prefix = '2 3'
);
"""


def block_text(block):
    kind = block.get("type")
    if kind == "text":
        parts = [block.get("text")]
    elif kind == "image":
        parts = [block.get("alt_text"), block.get("caption")]
    elif kind == "link":
        parts = [block.get(k) for k in
                 ("title", "description", "site_name", "author", "url")]
    elif kind == "audio":
        parts = [block.get(k) for k in ("title", "artist", "album")]
    elif kind == "poll":
        parts = [block.get("question")]
        parts += [a.get("answer_text") for a in block.get("answers") or []]
    else:
        parts = []
    return "\n".join(p for p in parts if isinstance(p, str) and p)


def media_entries(block):
    media = block.get("media")
    if isinstance(media, dict):
        return [media]
    if isinstance(media, list):
        return [m for m in media if isinstance(m, dict)]
    return []


def block_media(block, json_dir):
    """Return (local path, remote url, mime) for a media block, or None.

    archivr rewrites the URL of each media item it downloaded to a path
    relative to the archive, leaving the other renditions remote.
    """
    if block.get("type") not in ("image", "video", "audio"):
        return None
    entries = media_entries(block)
    local = remote = mime = None
    for entry in entries:
        url = entry.get("url") or ""
        if not url:
            continue
        if url.startswith(("http://", "https://")):
            remote = remote or url
        elif local is None:
            local = os.path.join(json_dir, url)
            mime = entry.get("type")
    remote = remote or block.get("url")
    if mime is None and entries:
        mime = entries[0].get("type")
    if local is None and remote is None:
        return None
    return local, remote, mime


def trail_blog(item):
    blog = item.get("blog")
    if isinstance(blog, dict) and blog.get("name"):
        return blog["name"]
    return item.get("broken_blog_name")


def askers(layouts):
    for layout in layouts or []:
        if layout.get("type") != "ask":
            continue
        attribution = layout.get("attribution") or {}
        name = (attribution.get("blog") or {}).get("name")
        if name:
            yield name


def unique(names):
    return list(dict.fromkeys(n for n in names if n))


def extract(path, json_dir, html_ids, html_dir):
    with open(path, encoding="utf-8") as f:
        post = json.load(f)

    post_id = int(post["id"])
    ts = int(post.get("timestamp") or 0)
    content = post.get("content") or []
    trail = post.get("trail") or []
    tags = [t for t in post.get("tags") or [] if isinstance(t, str)]

    media = []

    def collect_media(blocks, in_trail):
        for block in blocks:
            found = block_media(block, json_dir)
            if found:
                media.append((post_id, len(media), in_trail, block["type"],
                              *found, block.get("alt_text")))

    collect_media(content, 0)
    body = "\n\n".join(filter(None, map(block_text, content)))

    trail_rows = []
    for position, item in enumerate(trail):
        blocks = item.get("content") or []
        collect_media(blocks, 1)
        trail_rows.append((
            post_id,
            position,
            trail_blog(item),
            (item.get("post") or {}).get("id"),
            int(bool(item.get("is_root_item"))),
            "\n\n".join(filter(None, map(block_text, blocks))),
        ))

    authors = unique(
        [r[2] for r in trail_rows]
        + list(askers(post.get("layout")))
        + [a for item in trail for a in askers(item.get("layout"))]
        + [post.get("post_author"), post.get("anonymous_name")]
    )

    html_path = None
    if str(post_id) in html_ids:
        html_path = os.path.join(html_dir, f"{post_id}.html")

    row = (
        post_id,
        ts,
        datetime.fromtimestamp(ts, UTC).strftime("%Y-%m-%dT%H:%M:%SZ"),
        post.get("original_type"),
        post.get("post_url"),
        post.get("short_url"),
        post.get("slug") or None,
        post.get("summary") or None,
        post.get("note_count"),
        int(bool(trail) or bool(post.get("parent_post_url"))),
        post.get("parent_post_url"),
        body,
        "\n\n".join(r[5] for r in trail_rows if r[5]),
        "\n".join(tags),
        "\n".join(authors),
        path,
        html_path,
    )
    tag_rows = [(post_id, i, t) for i, t in enumerate(tags)]
    return row, tag_rows, trail_rows, media


def post_files(json_dir):
    for name in os.listdir(json_dir):
        stem, ext = os.path.splitext(name)
        if ext == ".json" and stem.isdigit():
            yield os.path.join(json_dir, name)


def html_index(html_dir):
    if html_dir is None:
        return set()
    return {
        name[:-5] for name in os.listdir(html_dir) if name.endswith(".html")
    }


def build(args):
    json_dir = os.path.abspath(args.json_dir)
    html_dir = os.path.abspath(args.html_dir) if args.html_dir else None
    output = os.path.abspath(args.output)
    tmp = output + ".tmp"

    files = sorted(post_files(json_dir))
    html_ids = html_index(html_dir)
    print(f"indexing {len(files)} posts from {json_dir}", file=sys.stderr)

    if os.path.exists(tmp):
        os.remove(tmp)
    db = sqlite3.connect(tmp)
    # Nothing reads the temporary file until it is renamed into place, and a
    # crash leaves it to be deleted by the next run, so durability buys
    # nothing during the build.
    db.execute("PRAGMA journal_mode = OFF")
    db.execute("PRAGMA synchronous = OFF")
    db.executescript(SCHEMA)

    failures = 0
    done = 0

    def work(path):
        try:
            return extract(path, json_dir, html_ids, html_dir)
        except (OSError, ValueError, KeyError, TypeError) as e:
            return e

    # Post files are a few KB each in one huge directory on spinning disks:
    # the build waits on reads, not parsing, so overlapping them is the win.
    with ThreadPoolExecutor(max_workers=args.jobs) as pool, db:
        for path, result in zip(files, pool.map(work, files)):
            done += 1
            if isinstance(result, Exception):
                failures += 1
                print(f"skipped {path}: {result!r}", file=sys.stderr)
                continue
            row, tag_rows, trail_rows, media = result
            db.execute(f"INSERT INTO posts VALUES ({','.join('?' * 17)})",
                       row)
            db.executemany("INSERT INTO tags VALUES (?,?,?)", tag_rows)
            db.executemany("INSERT INTO trail VALUES (?,?,?,?,?,?)",
                           trail_rows)
            db.executemany("INSERT INTO media VALUES (?,?,?,?,?,?,?,?)",
                           media)
            if done % 10000 == 0:
                print(f"  {done}/{len(files)}", file=sys.stderr)

    print("building full-text index", file=sys.stderr)
    with db:
        db.execute("INSERT INTO posts_fts (posts_fts) VALUES ('rebuild')")
        db.execute("INSERT INTO posts_fts (posts_fts) VALUES ('optimize')")
        indexed = done - failures
        db.executemany("INSERT INTO meta VALUES (?, ?)", [
            ("built_at", datetime.now(UTC).strftime("%Y-%m-%dT%H:%M:%SZ")),
            ("json_dir", json_dir),
            ("html_dir", html_dir or ""),
            ("posts", str(indexed)),
            ("skipped", str(failures)),
        ])
    db.execute("ANALYZE")
    db.close()
    os.replace(tmp, output)

    print(f"wrote {indexed} posts to {output}"
          + (f" ({failures} skipped)" if failures else ""), file=sys.stderr)
    return 1 if failures else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--json-dir", required=True,
                        help="archive written by archivr --json")
    parser.add_argument("--html-dir",
                        help="matching HTML archive, to link each post's page")
    parser.add_argument("--output", required=True, help="database to write")
    parser.add_argument("--jobs", type=int, default=16,
                        help="concurrent file reads (default: 16)")
    sys.exit(build(parser.parse_args()))


if __name__ == "__main__":
    main()
