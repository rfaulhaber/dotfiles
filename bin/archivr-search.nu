# Search the Tumblr archive database written by archivr-index.
#
# Terms are FTS5 query syntax: bare words must all match, "quoted phrases",
# OR, NOT, prefix*, NEAR(a b, 5), and column filters on body (what the blog
# wrote), trail_text (what it reblogged), tags and authors, e.g. tags:art or
# authors:someblog. Matching is stemmed and diacritic-insensitive.
def main [
  ...terms: string        # FTS5 query; omit to list by the other filters alone
  --db: path              # database to search (default: $env.ARCHIVR_DB)
  --limit (-n): int = 25  # maximum rows to return
  --tag (-t): string      # only posts carrying this exact tag, ignoring case
  --type: string          # original post type: photo, regular, video, quote, note, link, audio, conversation
  --since: datetime       # only posts published at or after this time
  --until: datetime       # only posts published before this time
  --own (-o)              # match only text the blog wrote, not reblogged text
  --newest                # order by publish date instead of relevance
  --literal (-l)          # treat terms as plain words rather than FTS5 syntax
] {
  let db = $db | default $env.ARCHIVR_DB?
  if $db == null {
    error make {msg: "no database: pass --db or set ARCHIVR_DB"}
  }

  let match = if ($terms | is-empty) {
    null
  } else if $literal {
    $terms | each {|t| $'"($t | str replace --all '"' '""')"' } | str join " "
  } else {
    $terms | str join " "
  }
  let match = if $match != null and $own { $"body : \(($match)\)" } else { $match }

  mut where = []
  if $match != null {
    $where ++= [$"posts_fts MATCH (sql-quote $match)"]
  }
  if $tag != null {
    $where ++= [$"p.id IN \(SELECT post_id FROM tags WHERE tag = (sql-quote $tag) COLLATE NOCASE\)"]
  }
  if $type != null {
    $where ++= [$"p.original_type = (sql-quote $type)"]
  }
  if $since != null {
    $where ++= [$"p.timestamp >= (($since | into int) // 1_000_000_000)"]
  }
  if $until != null {
    $where ++= [$"p.timestamp < (($until | into int) // 1_000_000_000)"]
  }

  let hl = [(ansi yellow_bold) (ansi reset)]
  let select = if $match != null {
    # The blog's own words and its tags outrank text quoted from a reblog.
    $"SELECT p.id, p.date, p.original_type AS type,
        snippet\(posts_fts, -1, '($hl.0)', '($hl.1)', '…', 16\) AS snippet,
        p.post_url AS url, p.html_path AS html
      FROM posts_fts JOIN posts p ON p.id = posts_fts.rowid"
  } else {
    "SELECT p.id, p.date, p.original_type AS type,
        substr(coalesce(nullif(p.body, ''), p.trail_text), 1, 120) AS snippet,
        p.post_url AS url, p.html_path AS html
      FROM posts p"
  }
  let order = if $newest or $match == null {
    "p.timestamp DESC"
  } else {
    "bm25(posts_fts, 4.0, 1.0, 3.0, 0.5)"
  }
  let where_sql = if ($where | is-empty) { "" } else { $"WHERE ($where | str join ' AND ')" }
  let sql = $"($select) ($where_sql) ORDER BY ($order) LIMIT ($limit)"

  # Not `query db`: it reports errors raised while stepping a statement as an
  # empty result, and FTS5 only parses the MATCH expression then, so a typo in
  # the query would be indistinguishable from no matches.
  let out = $sql | ^sqlite3 -bail -readonly -json $db
  if ($out | str trim | is-empty) { return [] }
  $out
  | from json
  | update date { into datetime }
  | update snippet { str replace --all --regex '\s+' ' ' }
}

# SQLite string literals have a single escape: a doubled quote.
def sql-quote [s: string] {
  $"'($s | str replace --all "'" "''")'"
}
