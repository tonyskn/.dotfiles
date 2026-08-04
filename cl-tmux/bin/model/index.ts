export type HarnessId = "claude";

const STATES = {
  idle: { priority: 1, icon: "○" },
  working: { priority: 2, icon: "●" },
  loop: { priority: 3, icon: "∞" },
  waiting: { priority: 4, icon: "◐" },
  attention: { priority: 5, icon: "⚠" },
} as const;

export type AgentState = keyof typeof STATES;

export type SessionRef = {
  harness: HarnessId;
  sid: string;
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
};

export type SessionRow = SessionRef & {
  name: string;
  cwd: string;
  started: number;
  lastActive: number;
  saved: boolean;
  pane?: LivePane;
};

export type ParsedHistory = {
  sid: string;
  title: string;
  name: string;
  cwd: string;
};

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
  parseHistory(file: string, contents: string): ParsedHistory;
};

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

    for (const line of contents.trim().split("\n")) {
      try {
        const record = JSON.parse(line) as Record<string, unknown>;
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
      } catch {}
    }

    return { sid, title, name, cwd };
  },
};

const HARNESSES: Record<HarnessId, Harness> = { claude };

export function isAgentState(value: string): value is AgentState {
  return Object.hasOwn(STATES, value);
}

export function isHarnessId(value: string): value is HarnessId {
  return Object.hasOwn(HARNESSES, value);
}

export function getHarness(id: HarnessId): Harness {
  return HARNESSES[id];
}

export function harnesses(): Harness[] {
  return Object.values(HARNESSES);
}

export function iconForState(state?: AgentState): string {
  return state ? STATES[state].icon : "";
}

export function sessionKey(ref: SessionRef): string {
  return `${ref.harness}\0${ref.sid}`;
}

export function sameSession(a: SessionRef, b: SessionRef): boolean {
  return a.harness === b.harness && a.sid === b.sid;
}

export function aggregateIcon(
  states: ReadonlyArray<AgentState | undefined>,
): string {
  let state: AgentState | undefined;

  for (const candidate of states) {
    if (!candidate) continue;
    const priority = STATES[candidate].priority;
    const selectedPriority = state ? STATES[state].priority : -1;
    if (priority > selectedPriority) state = candidate;
  }

  return iconForState(state);
}

export function buildSessionRows(
  bookmarks: ReadonlyArray<BookmarkRecord>,
  panes: ReadonlyArray<LivePane>,
): SessionRow[] {
  const paneBySession = new Map(panes.map((pane) => [sessionKey(pane), pane]));
  const savedSessions = new Set(bookmarks.map(sessionKey));

  const saved = bookmarks.map((bookmark): SessionRow => {
    const pane = paneBySession.get(sessionKey(bookmark));
    return {
      ...bookmark,
      lastActive: Math.max(bookmark.lastActive, pane?.activeAt ?? 0),
      saved: true,
      pane,
    };
  });

  const live = panes
    .filter((pane) => !savedSessions.has(sessionKey(pane)))
    .map((pane): SessionRow => ({
      harness: pane.harness,
      sid: pane.sid,
      name: "unnamed",
      cwd: pane.cwd,
      started: pane.activeAt,
      lastActive: pane.activeAt,
      saved: false,
      pane,
    }));

  return [...saved, ...live].sort((a, b) => b.lastActive - a.lastActive);
}

export function buildHistoryRows(entries: ReadonlyArray<HistoryEntry>): HistoryEntry[] {
  return entries.map((entry) => ({
    ...entry,
    title: entry.title || "untitled",
    name: entry.name || "unnamed",
  }));
}
