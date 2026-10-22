import { Database } from "bun:sqlite";
import { mkdirSync } from "fs";
import { homedir } from "os";
import { join } from "path";
import { SessionRef, type StoredSession } from "./sessions";

const stateDir = join(
  process.env.XDG_STATE_HOME || join(homedir(), ".local", "state"),
  "cl-tmux",
);
const databasePath = join(stateDir, "sessions.sqlite");

type DbRow = {
  harness: StoredSession["harness"];
  sid: string;
  cwd: string;
  started_at: number | null;
  active_at: number | null;
  title: string | null;
  bookmark_name: string | null;
};

type CachedConversation = {
  harness: StoredSession["harness"];
  sid: string;
  fingerprint: string;
  index_version: number;
};

let database: Database | undefined;

export const SEARCH_INDEX_VERSION = 2;

function db(): Database {
  if (database) return database;
  mkdirSync(stateDir, { recursive: true });
  const opened = new Database(databasePath);
  opened.run("PRAGMA busy_timeout = 5000");
  opened.run("PRAGMA journal_mode = WAL");
  opened.run(`
    CREATE TABLE IF NOT EXISTS sessions (
      harness TEXT NOT NULL,
      sid TEXT NOT NULL,
      cwd TEXT NOT NULL,
      started_at INTEGER,
      active_at INTEGER,
      title TEXT,
      bookmark_name TEXT,
      PRIMARY KEY (harness, sid)
    )
  `);
  opened.run(`
    CREATE TABLE IF NOT EXISTS indexed_conversations (
      harness TEXT NOT NULL,
      sid TEXT NOT NULL,
      fingerprint TEXT NOT NULL,
      index_version INTEGER NOT NULL,
      PRIMARY KEY (harness, sid)
    )
  `);
  opened.run(`
    CREATE VIRTUAL TABLE IF NOT EXISTS conversation_search USING fts5(
      text,
      harness UNINDEXED,
      sid UNINDEXED,
      tokenize='unicode61'
    )
  `);
  database = opened;
  return opened;
}

export function cachedConversations(): Map<string, CachedConversation> {
  const rows = db().query("SELECT * FROM indexed_conversations").all();
  return new Map(
    (rows as CachedConversation[]).map((row) => [SessionRef.key(row), row]),
  );
}

function deleteConversationMessages(opened: Database, ref: SessionRef): void {
  opened
    .query("DELETE FROM conversation_search WHERE harness = ? AND sid = ?")
    .run(ref.harness, ref.sid);
}

export function replaceConversation(
  ref: SessionRef,
  fingerprint: string,
  messages: ReadonlyArray<string>,
): void {
  const opened = db();
  const insert = opened.query(
    "INSERT INTO conversation_search (text, harness, sid) VALUES (?, ?, ?)",
  );
  const track = opened.query(`
    INSERT INTO indexed_conversations (harness, sid, fingerprint, index_version)
    VALUES (?, ?, ?, ?)
    ON CONFLICT (harness, sid) DO UPDATE SET
      fingerprint = excluded.fingerprint,
      index_version = excluded.index_version
  `);

  opened.transaction(() => {
    deleteConversationMessages(opened, ref);
    for (const message of messages) insert.run(message, ref.harness, ref.sid);
    track.run(ref.harness, ref.sid, fingerprint, SEARCH_INDEX_VERSION);
  })();
}

export function removeConversation(ref: SessionRef): void {
  const opened = db();
  opened.transaction(() => {
    deleteConversationMessages(opened, ref);
    opened
      .query("DELETE FROM indexed_conversations WHERE harness = ? AND sid = ?")
      .run(ref.harness, ref.sid);
  })();
}

export function searchConversations(
  query: string,
): Array<SessionRef & { score: number }> {
  const opened = db();
  const search = query.trim();
  // FTS tokenizes URL punctuation, so also require the literal URL in the message.
  const url = /^https?:\/\/\S+$/i.test(search) ? search : undefined;
  const terms = url ? [url] : [...new Set(search.split(/\s+/).filter(Boolean))];
  if (!terms.length) return [];

  const expression = terms
    .map((term) => `"${term.replaceAll('"', '""')}"`)
    .join(" AND ");
  return opened
    .query(
      `
      SELECT conversation_search.harness, conversation_search.sid, MIN(rank) AS score
      FROM conversation_search
      JOIN sessions USING (harness, sid)
      WHERE conversation_search MATCH ? ${url ? "AND instr(text, ?) > 0" : ""}
      GROUP BY conversation_search.harness, conversation_search.sid
      ORDER BY score, MAX(sessions.active_at) DESC
    `,
    )
    .all(...(url ? [expression, url] : [expression])) as Array<
    SessionRef & { score: number }
  >;
}

function fromDbRow(row: DbRow): StoredSession {
  return {
    harness: row.harness,
    sid: row.sid,
    cwd: row.cwd,
    startedAt: row.started_at ?? undefined,
    activeAt: row.active_at ?? undefined,
    title: row.title ?? undefined,
    bookmarkName: row.bookmark_name ?? undefined,
  };
}

export function upsertActivity(
  ref: SessionRef,
  cwd: string,
  timestamp: number,
  active: boolean,
  title?: string,
): void {
  db()
    .prepare(
      `
      INSERT INTO sessions (harness, sid, cwd, started_at, active_at, title)
      SELECT $harness, $sid, $cwd, $timestamp, $activeAt, $title
      -- Only a prompt creates an entry; other hooks may update existing sessions.
      WHERE $title IS NOT NULL OR EXISTS (
        SELECT 1 FROM sessions WHERE harness = $harness AND sid = $sid
      )
      ON CONFLICT (harness, sid) DO UPDATE SET
        cwd = excluded.cwd,
        started_at = COALESCE(MIN(sessions.started_at, excluded.started_at), sessions.started_at, excluded.started_at),
        active_at = COALESCE(MAX(sessions.active_at, excluded.active_at), sessions.active_at, excluded.active_at),
        title = COALESCE(sessions.title, excluded.title)
    `,
    )
    .run({
      $harness: ref.harness,
      $sid: ref.sid,
      $cwd: cwd,
      $timestamp: timestamp,
      $activeAt: active ? timestamp : null,
      $title: title ?? null,
    });
}

export function all(): StoredSession[] {
  return db()
    .query("SELECT * FROM sessions")
    .all()
    .map((row) => fromDbRow(row as DbRow));
}

export function find(ref: SessionRef): StoredSession | undefined {
  const row = db()
    .query("SELECT * FROM sessions WHERE harness = ? AND sid = ?")
    .get(ref.harness, ref.sid) as DbRow | null;
  return row ? fromDbRow(row) : undefined;
}

export function save(ref: SessionRef, name: string): void {
  db()
    .prepare(
      `
      UPDATE sessions SET bookmark_name = ?
      WHERE harness = ? AND sid = ?
    `,
    )
    .run(name, ref.harness, ref.sid);
}

export function remove(ref: SessionRef): void {
  db()
    .prepare("DELETE FROM sessions WHERE harness = ? AND sid = ?")
    .run(ref.harness, ref.sid);
}
