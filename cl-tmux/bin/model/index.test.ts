import { describe, expect, test } from "bun:test";
import {
  aggregateIcon,
  buildSessionRows,
  type BookmarkRecord,
  type LivePane,
} from "./index";

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

describe("aggregateIcon", () => {
  test("uses deterministic state priority", () => {
    expect(aggregateIcon(["attention", "working"])).toBe("⚠");
  });

  test("prioritizes loop over ordinary working", () => {
    expect(aggregateIcon(["working", "loop"])).toBe("∞");
  });

  test("returns an empty presentation for no pane state", () => {
    expect(aggregateIcon([undefined])).toBe("");
  });
});

describe("buildSessionRows", () => {
  test("joins a live pane to its bookmark and uses live activity", () => {
    expect(buildSessionRows([bookmark], [pane()])).toEqual([{
      ...bookmark,
      lastActive: 300,
      saved: true,
      pane: pane(),
    }]);
  });

  test("keeps dormant bookmarks and unbookmarked live sessions flat", () => {
    const orphan = pane({ sid: "orphan", paneId: "%2", activeAt: 400 });
    const rows = buildSessionRows([bookmark], [orphan]);

    expect(rows.map(({ sid, saved, pane }) => ({ sid, saved, paneId: pane?.paneId }))).toEqual([
      { sid: "orphan", saved: false, paneId: "%2" },
      { sid: "saved", saved: true, paneId: undefined },
    ]);
  });
});
