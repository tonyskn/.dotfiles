import { Database } from "bun:sqlite";
import { mkdirSync } from "fs";
import { homedir } from "os";
import { join } from "path";
import type { SessionRef, StoredSession } from "./sessions";

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

let database: Database | undefined;

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
  database = opened;
  return opened;
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
      VALUES (?, ?, ?, ?, ?, ?)
      ON CONFLICT (harness, sid) DO UPDATE SET
        cwd = excluded.cwd,
        started_at = COALESCE(MIN(sessions.started_at, excluded.started_at), sessions.started_at, excluded.started_at),
        active_at = COALESCE(MAX(sessions.active_at, excluded.active_at), sessions.active_at, excluded.active_at),
        title = COALESCE(sessions.title, excluded.title)
    `,
    )
    .run(
      ref.harness,
      ref.sid,
      cwd,
      timestamp,
      active ? timestamp : null,
      title ?? null,
    );
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
