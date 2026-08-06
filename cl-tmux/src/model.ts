const STATES = {
  idle: { priority: 1, icon: "○" },
  working: { priority: 2, icon: "●" },
  waiting: { priority: 4, icon: "◐" },
} as const;

const MODES = {
  loop: { priority: 3, icon: "∞" },
  attention: { priority: 5, icon: "⚠" },
} as const;

export type AgentState = keyof typeof STATES;

// Sticky markers set by skills and scripts, independent of harness lifecycle events.
// State and mode still share one presentation priority when choosing an icon.
export type AgentMode = keyof typeof MODES;
export type AgentStatus = AgentState | AgentMode;

export const AgentMode = {
  is(value: string): value is AgentMode {
    return Object.hasOwn(MODES, value);
  },
};

const AGENT_STATUSES = { ...STATES, ...MODES };

export const AgentState = {
  is(value: string): value is AgentState {
    return Object.hasOwn(STATES, value);
  },
};

export const AgentStatus = {
  aggregateIcon(statuses: ReadonlyArray<AgentStatus | undefined>): string {
    let selected: AgentStatus | undefined;

    for (const status of statuses) {
      if (!status) continue;
      const priority = AGENT_STATUSES[status].priority;
      const selectedPriority = selected
        ? AGENT_STATUSES[selected].priority
        : -1;
      if (priority > selectedPriority) selected = status;
    }

    return selected ? AGENT_STATUSES[selected].icon : "";
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
  previousSid?: string;
  state?: AgentState;
  mode?: AgentMode;
  lastActive: number;
  cwd: string;
};

export type SessionMetadata = SessionRef & {
  title: string;
  name: string;
  cwd: string;
  cwdExists: boolean;
  modifiedAt: number;
  forkedFromSid?: string;
};

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
      lastActive: Math.max(bookmark.lastActive, pane?.lastActive ?? 0),
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
      lastActive: pane.lastActive || fallbackActiveAt,
      saved: false,
      pane,
    }));

  return [...saved, ...live].sort((a, b) => b.lastActive - a.lastActive);
}
