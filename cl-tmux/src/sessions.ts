import { exists } from "fs/promises";
import { basename } from "path";
import * as Transcripts from "./transcripts";
import * as Store from "./store";
import * as Tmux from "./tmux";

const STATES = {
  idle: "○",
  working: "●",
  waiting: "◐",
} as const;

export type AgentState = keyof typeof STATES;

export const AgentState = {
  is(value: string): value is AgentState {
    return Object.hasOwn(STATES, value);
  },
  icon(state?: AgentState): string {
    return state ? STATES[state] : "";
  },
  aggregateIcon(states: ReadonlyArray<AgentState | undefined>): string {
    if (states.includes("waiting")) return STATES.waiting;
    if (states.includes("working")) return STATES.working;
    if (states.includes("idle")) return STATES.idle;
    return "";
  },
};

export type HarnessId = "claude" | "codex";

export type SessionRef = {
  harness: HarnessId;
  sid: string;
};

export const SessionRef = {
  key(ref: SessionRef): string {
    return `${ref.harness}\0${ref.sid}`;
  },

  index<T extends SessionRef>(items: ReadonlyArray<T>): Map<string, T> {
    const indexed = new Map<string, T>();
    for (const item of items) {
      const key = SessionRef.key(item);
      if (!indexed.has(key)) indexed.set(key, item);
    }
    return indexed;
  },

  equals(a: SessionRef, b: SessionRef): boolean {
    return a.harness === b.harness && a.sid === b.sid;
  },
};

export type StoredSession = SessionRef & {
  cwd: string;
  startedAt?: number;
  activeAt?: number;
  title?: string;
  bookmarkName?: string;
};

export type LivePane = SessionRef & {
  windowId: string;
  paneId: string;
  state?: AgentState;
};

export type SessionRow = StoredSession & {
  name: string;
  bookmarked: boolean;
  pane?: LivePane;
};

export const SessionRow = {
  build(
    stored: ReadonlyArray<StoredSession>,
    panes: ReadonlyArray<LivePane>,
  ): SessionRow[] {
    const paneBySession = SessionRef.index(panes);

    return stored
      .map((entry): SessionRow => {
        const pane = paneBySession.get(SessionRef.key(entry));
        return {
          ...entry,
          name: entry.bookmarkName ?? (basename(entry.cwd) || "unnamed"),
          bookmarked: entry.bookmarkName !== undefined,
          pane,
        };
      })
      .sort((a, b) => (b.activeAt ?? 0) - (a.activeAt ?? 0));
  },

  filter(
    rows: ReadonlyArray<SessionRow>,
    options: { bookmarkedView?: boolean; period?: string },
    now = Date.now() / 1000,
  ): SessionRow[] {
    return rows.filter((row) => {
      if (options.bookmarkedView && !row.bookmarked && !row.pane) return false;
      switch (options.period) {
        case "today":
          return row.activeAt !== undefined && now - row.activeAt < 86400;
        case "week":
          return row.activeAt !== undefined && now - row.activeAt < 604800;
        case "live":
          return row.pane !== undefined;
        default:
          return true;
      }
    });
  },
};

export async function list(
  options?: { bookmarkedView?: boolean; period?: string },
  paneId = "",
  windowId = "",
): Promise<{ rows: SessionRow[]; selectedPaneId?: string }> {
  const panes = await Tmux.livePanes();
  const selectedPaneId = Tmux.selectedPane(panes, paneId, windowId)?.paneId;
  const rows = SessionRow.filter(
    SessionRow.build(Store.all(), panes),
    options ?? {},
  );
  return { rows, selectedPaneId };
}

export function recordHook(
  ref: SessionRef,
  cwd: string,
  update: { active: boolean; prompt?: string },
): void {
  const now = Math.floor(Date.now() / 1000);
  const title = update.prompt?.replace(/\s+/g, " ").trim().slice(0, 240);
  Store.upsertActivity(ref, cwd, now, update.active, title);
}

export async function find(ref: SessionRef): Promise<SessionRow | undefined> {
  return (await list()).rows.find((row) => SessionRef.equals(row, ref));
}

export async function search(
  query: string,
  bookmarkedView = false,
): Promise<SessionRow[]> {
  const { rows } = await list();
  const matches = await Transcripts.search(query, rows);
  const matchingRefs = new Set(matches.map(SessionRef.key));
  return SessionRow.filter(
    rows.filter((row) => matchingRefs.has(SessionRef.key(row))),
    { bookmarkedView },
  );
}

export async function launchError(
  session: SessionRow,
  cwd = session.cwd,
): Promise<string | undefined> {
  if (!(await exists(cwd))) return `Directory no longer exists: ${cwd}`;
  if (!(await Transcripts.hasTranscript(session)))
    return `No session file for '${session.name}'`;
  return undefined;
}
