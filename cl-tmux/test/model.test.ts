import { describe, expect, test } from "bun:test";
import {
  AgentStatus,
  buildSessionRows,
  collapseHistoryMatches,
  type BookmarkRecord,
  type HistoryEntry,
  type LivePane,
} from "../src/model";

const bookmark: BookmarkRecord = {
  harness: "claude",
  sid: "saved",
  name: "saved session",
  cwd: "/repo",
  started: 100,
  lastActive: 200,
};

function pane(overrides: Partial<LivePane> = {}): LivePane {
  return {
    harness: "claude",
    sid: "saved",
    windowId: "@1",
    paneId: "%1",
    state: "idle",
    activeAt: 300,
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

describe("buildSessionRows", () => {
  test("joins a live pane to its bookmark and uses live activity", () => {
    expect(buildSessionRows([bookmark], [pane()])).toEqual([
      {
        harness: bookmark.harness,
        sid: bookmark.sid,
        name: bookmark.name,
        cwd: bookmark.cwd,
        lastActive: 300,
        saved: true,
        pane: pane(),
      },
    ]);
  });

  test("keeps dormant bookmarks and unbookmarked live sessions flat", () => {
    const orphan = pane({ sid: "orphan", paneId: "%2", activeAt: 400 });
    const rows = buildSessionRows([bookmark], [orphan]);

    expect(
      rows.map(({ sid, saved, pane }) => ({
        sid,
        saved,
        paneId: pane?.paneId,
      })),
    ).toEqual([
      { sid: "orphan", saved: false, paneId: "%2" },
      { sid: "saved", saved: true, paneId: undefined },
    ]);
  });

  test("uses fallback activity for a newly discovered live session", () => {
    const orphan = pane({ sid: "orphan", activeAt: 0 });
    expect(buildSessionRows([bookmark], [orphan], 500)[0].lastActive).toBe(500);
  });
});

test("collapseHistoryMatches removes duplicate files and inherited fork matches", () => {
  const entry = (sid: string, forkedFromSid?: string): HistoryEntry => ({
    harness: "codex",
    sid,
    forkedFromSid,
    title: sid,
    name: "repo",
    cwd: "/repo",
    cwdExists: true,
    modifiedAt: 100,
  });

  expect(
    collapseHistoryMatches([
      entry("fork", "parent"),
      entry("parent"),
      entry("parent"),
      entry("independent-fork", "missing-parent"),
    ]),
  ).toEqual([entry("parent"), entry("independent-fork", "missing-parent")]);
});
