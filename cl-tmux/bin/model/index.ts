const STATES = {
  idle: { priority: 1, icon: "○" },
  working: { priority: 2, icon: "●" },
  loop: { priority: 3, icon: "∞" },
  waiting: { priority: 4, icon: "◐" },
  attention: { priority: 5, icon: "⚠" },
} as const;

export type AgentState = keyof typeof STATES;

export const AgentState = {
  is(value: string): value is AgentState {
    return Object.hasOwn(STATES, value);
  },

  icon(state?: AgentState): string {
    return state ? STATES[state].icon : "";
  },

  aggregate(states: ReadonlyArray<AgentState | undefined>): string {
    let selected: AgentState | undefined;

    for (const state of states) {
      if (!state) continue;
      const priority = STATES[state].priority;
      const selectedPriority = selected ? STATES[selected].priority : -1;
      if (priority > selectedPriority) selected = state;
    }

    return selected ? STATES[selected].icon : "";
  },
};

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
      const key = this.key(item);
      if (!indexed.has(key)) indexed.set(key, item);
    }
    return indexed;
  },

  equals(a: SessionRef, b: SessionRef): boolean {
    return a.harness === b.harness && a.sid === b.sid;
  },
};

export type BookmarkRecord = SessionRef & {
  name: string;
  cwd: string;
  started: number;
  lastActive: number;
};

export type LivePane = SessionRef & {
  windowId: string;
  paneId: string;
  state?: AgentState;
  activeAt: number;
  cwd: string;
};

export type HistoryEntry = SessionRef & {
  title: string;
  name: string;
  cwd: string;
  cwdExists: boolean;
  forkedFromSid?: string;
};

// Collapse duplicate files and inherited fork matches to one row per relevant session.
export function collapseHistoryMatches(entries: ReadonlyArray<HistoryEntry>): HistoryEntry[] {
  const unique = SessionRef.index(entries);
  const matchedSessions = new Set(unique.keys());
  return [...unique.values()].filter((entry) => {
    if (!entry.forkedFromSid) return true;
    const parent = SessionRef.key({ harness: entry.harness, sid: entry.forkedFromSid });
    return !matchedSessions.has(parent);
  });
}

export type SessionRow = SessionRef & {
  name: string;
  cwd: string;
  lastActive: number;
  saved: boolean;
  pane?: LivePane;
};

// Join persisted bookmarks and live panes into the picker's flat session model.
export function buildSessionRows(
  bookmarks: ReadonlyArray<BookmarkRecord>,
  panes: ReadonlyArray<LivePane>,
  fallbackActiveAt = Math.floor(Date.now() / 1000),
): SessionRow[] {
  const paneBySession = SessionRef.index(panes);
  const savedSessions = new Set(bookmarks.map(SessionRef.key));

  const saved = bookmarks.map((bookmark): SessionRow => {
    const pane = paneBySession.get(SessionRef.key(bookmark));
    return {
      harness: bookmark.harness,
      sid: bookmark.sid,
      name: bookmark.name,
      cwd: bookmark.cwd,
      lastActive: Math.max(bookmark.lastActive, pane?.activeAt ?? 0),
      saved: true,
      pane,
    };
  });

  const live = panes
    .filter((pane) => !savedSessions.has(SessionRef.key(pane)))
    .map((pane): SessionRow => ({
      harness: pane.harness,
      sid: pane.sid,
      name: "unnamed",
      cwd: pane.cwd,
      lastActive: pane.activeAt || fallbackActiveAt,
      saved: false,
      pane,
    }));

  return [...saved, ...live].sort((a, b) => b.lastActive - a.lastActive);
}

export type ParsedHistory = {
  sid: string;
  title: string;
  name: string;
  cwd: string;
  forkedFromSid?: string;
};

export type HarnessId = "claude" | "codex";

export type Harness = {
  id: HarnessId;
  binary: string;
  historyDir: string;
  historyGlobs: string[];
  isProcess(command: string): boolean;
  stateForHook(event: string): AgentState | undefined;
  resume(args: { sid: string; name: string; prompt?: string }): string[];
  fork(args: { sourceSid: string; name: string }): string[];
  sessionGlob(sid: string): string;
  parseHistory(file: string, contents: string): ParsedHistory | undefined;
};

export const Harness = {
  isId(value: string): value is HarnessId {
    return Object.hasOwn(HARNESSES, value);
  },

  get(id: HarnessId): Harness {
    return HARNESSES[id];
  },

  all(): Harness[] {
    return Object.values(HARNESSES);
  },
};

function* jsonRecords(contents: string): Generator<Record<string, unknown>> {
  for (const line of contents.trim().split("\n")) {
    try {
      const record = JSON.parse(line);
      if (record && typeof record === "object" && !Array.isArray(record)) yield record;
    } catch {}
  }
}

const claude: Harness = {
  id: "claude",
  binary: "claude",
  historyDir: ".claude/projects",
  historyGlobs: ["*.jsonl", "!subagents"],

  isProcess(command) {
    return /^\d+\.\d+\.\d+$/.test(command) || ["claude", "node", "bun"].includes(command);
  },

  stateForHook(event) {
    switch (event) {
      case "UserPromptSubmit": return "working";
      case "Stop": return "idle";
      case "Notification": return "waiting";
      default: return undefined;
    }
  },

  resume({ sid, name, prompt }) {
    const args = [this.binary, "--resume", sid, "-n", name];
    if (prompt) args.push(prompt);
    return args;
  },

  fork({ sourceSid, name }) {
    return [
      this.binary,
      "--resume", sourceSid,
      "--fork-session",
      "-n", name,
    ];
  },

  sessionGlob(sid) {
    return `${sid}.jsonl`;
  },

  parseHistory(file, contents) {
    const filename = file.slice(file.lastIndexOf("/") + 1);
    const sid = filename.replace(/\.jsonl$/, "");
    let title = "";
    let name = "";
    let cwd = "";

    for (const record of jsonRecords(contents)) {
      if (record.type === "ai-title" && !title && typeof record.aiTitle === "string") {
        title = record.aiTitle;
      }
      if (record.type === "custom-title" && !name && typeof record.customTitle === "string") {
        name = record.customTitle;
      }
      if (record.type === "user" && !cwd && typeof record.cwd === "string") {
        cwd = record.cwd;
      }
      if (title && name && cwd) break;
    }

    return { sid, title, name, cwd };
  },
};

const codex: Harness = {
  id: "codex",
  binary: "codex",
  historyDir: ".codex/sessions",
  historyGlobs: ["*.jsonl"],

  isProcess(command) {
    return command === this.binary;
  },

  stateForHook(event) {
    switch (event) {
      case "UserPromptSubmit": return "working";
      case "Stop": return "idle";
      default: return undefined;
    }
  },

  resume({ sid, prompt }) {
    const args = [this.binary, "resume", sid];
    if (prompt) args.push(prompt);
    return args;
  },

  fork({ sourceSid }) {
    // The initial turn triggers UserPromptSubmit, which reports the fork's generated SID.
    return [this.binary, "fork", sourceSid, "Wait for further instructions."];
  },

  sessionGlob(sid) {
    return `*${sid}.jsonl`;
  },

  parseHistory(_file, contents) {
    const records = jsonRecords(contents);
    const metadata = records.next().value?.payload as Record<string, unknown> | undefined;
    if (metadata?.source !== "cli") return undefined;

    const sid = typeof metadata.session_id === "string"
      ? metadata.session_id
      : typeof metadata.id === "string" ? metadata.id : "";
    if (!sid) return undefined;

    const cwd = typeof metadata.cwd === "string" ? metadata.cwd : "";
    const forkedFromSid = typeof metadata.forked_from_id === "string"
      ? metadata.forked_from_id
      : undefined;
    let title = "";
    for (const record of records) {
      const payload = record.payload as Record<string, unknown> | undefined;
      if (
        record.type === "event_msg"
        && payload?.type === "user_message"
        && typeof payload.message === "string"
      ) {
        title = payload.message.replace(/\s+/g, " ").trim();
        break;
      }
    }

    const normalizedCwd = cwd.replace(/\/+$/, "");
    const name = normalizedCwd.slice(normalizedCwd.lastIndexOf("/") + 1);
    return { sid, title, name, cwd, forkedFromSid };
  },
};

const HARNESSES: Record<HarnessId, Harness> = { claude, codex };
