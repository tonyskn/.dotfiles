import { describe, expect, test } from "bun:test";
import {
  AgentStatus,
  SessionMetadata,
  SessionRef,
  buildSessionRows,
  type BookmarkRecord,
  type LivePane,
} from "../src/model";

const bookmark: BookmarkRecord = {
  harness: "claude",
  sid: "saved",
  name: "saved session",
  cwd: "/repo",
};

function pane(overrides: Partial<LivePane> = {}): LivePane {
  return {
    harness: "claude",
    sid: "saved",
    windowId: "@1",
    paneId: "%1",
    state: "idle",
    cwd: "/repo",
    ...overrides,
  };
}

describe("AgentStatus.aggregateIcon", () => {
  test("uses deterministic state priority", () => {
    expect(AgentStatus.aggregateIcon(["attention", "working"])).toBe("⚠");
  });

  test("prioritizes loop over ordinary working", () => {
    expect(AgentStatus.aggregateIcon(["working", "loop"])).toBe("∞");
  });

  test("returns an empty presentation for no pane state", () => {
    expect(AgentStatus.aggregateIcon([undefined])).toBe("");
  });
});

describe("SessionMetadata.mergePages", () => {
  test("keeps the session origin and latest page state", () => {
    const original: SessionMetadata = {
      harness: "codex",
      sid: "session",
      title: "Original prompt",
      name: "old-worktree",
      cwd: "/old-worktree",
      cwdExists: false,
      startedAt: 100,
      activeAt: 200,
      forkedFromSid: "parent",
    };
    const continuation: SessionMetadata = {
      ...original,
      title: "Rewind point",
      name: "new-worktree",
      cwd: "/new-worktree",
      cwdExists: true,
      startedAt: 300,
      activeAt: 400,
      forkedFromSid: undefined,
    };

    expect(SessionMetadata.mergePages([continuation, original])).toEqual([
      {
        ...continuation,
        title: original.title,
        startedAt: original.startedAt,
        forkedFromSid: original.forkedFromSid,
      },
    ]);
  });
});

describe("buildSessionRows", () => {
  const metadata: SessionMetadata = {
    ...bookmark,
    title: "Saved session",
    cwdExists: true,
    startedAt: 100,
    activeAt: 300,
  };
  const metadataBySession = new Map([[SessionRef.key(bookmark), metadata]]);

  test("joins transcript timestamps and a live pane to its bookmark", () => {
    expect(buildSessionRows([bookmark], [pane()], metadataBySession)).toEqual([
      {
        harness: bookmark.harness,
        sid: bookmark.sid,
        name: bookmark.name,
        cwd: bookmark.cwd,
        startedAt: metadata.startedAt,
        activeAt: metadata.activeAt,
        saved: true,
        pane: pane(),
      },
    ]);
  });

  test("keeps dormant bookmarks and unbookmarked live sessions flat", () => {
    const orphan = pane({ sid: "orphan", paneId: "%2" });
    const orphanMetadata = {
      ...metadata,
      sid: "orphan",
      name: "orphan session",
      title: "Orphan session title",
      startedAt: 200,
      activeAt: 400,
    };
    const rows = buildSessionRows(
      [bookmark],
      [orphan],
      new Map([
        ...metadataBySession,
        [SessionRef.key(orphan), orphanMetadata] as const,
      ]),
    );

    expect(
      rows.map(({ sid, name, title, saved, pane }) => ({
        sid,
        name,
        title,
        saved,
        paneId: pane?.paneId,
      })),
    ).toEqual([
      {
        sid: "orphan",
        name: "orphan session",
        title: "Orphan session title",
        saved: false,
        paneId: "%2",
      },
      {
        sid: "saved",
        name: "saved session",
        title: undefined,
        saved: true,
        paneId: undefined,
      },
    ]);
  });

  test("keeps sessions whose transcript cannot be found", () => {
    const [row] = buildSessionRows([bookmark], [], new Map());
    expect(row).toMatchObject({ sid: "saved", saved: true });
    expect(row.activeAt).toBeUndefined();
    expect(row.startedAt).toBeUndefined();
  });
});
