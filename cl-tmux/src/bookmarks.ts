import { mkdir, rename } from "fs/promises";
import { homedir } from "os";
import { dirname, join } from "path";
import { SessionRef, type BookmarkRecord, type SessionRow } from "./model";

const STATE_HOME =
  process.env.XDG_STATE_HOME || join(homedir(), ".local", "state");
const BOOKMARKS_PATH = join(STATE_HOME, "cl-tmux", "bookmarks.jsonl");

let entries: BookmarkRecord[] = [];
let dirty = false;

export async function load(): Promise<void> {
  const file = Bun.file(BOOKMARKS_PATH);
  if (!(await file.exists())) return;

  entries = (await file.text())
    .trim()
    .split("\n")
    .filter(Boolean)
    .map((line) => JSON.parse(line) as BookmarkRecord);
}

export async function flush(): Promise<void> {
  if (!dirty) return;

  const contents =
    entries.map((entry) => JSON.stringify(entry)).join("\n") + "\n";
  const temporaryPath = BOOKMARKS_PATH + ".tmp";
  await mkdir(dirname(BOOKMARKS_PATH), { recursive: true });
  await Bun.write(temporaryPath, contents);
  await rename(temporaryPath, BOOKMARKS_PATH);
  dirty = false;
}

export function all(): ReadonlyArray<BookmarkRecord> {
  return entries;
}

export function find(ref: SessionRef): BookmarkRecord | undefined {
  return entries.find((bookmark) => SessionRef.equals(bookmark, ref));
}

export function addOrSave(ref: SessionRef, name: string, cwd: string): void {
  const bookmark = find(ref);
  if (bookmark) {
    bookmark.name = name;
    bookmark.cwd = cwd;
  } else {
    const timestamp = Math.floor(Date.now() / 1000);
    entries.push({
      ...ref,
      name,
      cwd,
      started: timestamp,
      lastActive: timestamp,
    });
  }
  dirty = true;
}

export function remove(ref: SessionRef): void {
  entries = entries.filter((bookmark) => !SessionRef.equals(bookmark, ref));
  dirty = true;
}

export function rebind(from: SessionRef, to: SessionRef): void {
  const bookmark = find(from);
  if (!bookmark || find(to)) return;

  bookmark.harness = to.harness;
  bookmark.sid = to.sid;
  dirty = true;
}

export function updateActivity(rows: ReadonlyArray<SessionRow>): void {
  const bookmarkBySession = SessionRef.index(entries);

  for (const row of rows) {
    const bookmark = bookmarkBySession.get(SessionRef.key(row));
    if (bookmark && row.lastActive > bookmark.lastActive) {
      bookmark.lastActive = row.lastActive;
      dirty = true;
    }
  }
}
