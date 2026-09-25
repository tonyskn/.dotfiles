import { expect, test } from "bun:test";
import { Database } from "bun:sqlite";
import { mkdtemp, rm } from "fs/promises";
import { tmpdir } from "os";
import { join } from "path";

const ROOT = join(import.meta.dir, "..");
const MARKER = join(ROOT, "bin", "tmux-marker");

type TmuxFixture = {
  paneId: string;
  tmux(args: string[]): string;
  mark(
    paneId: string,
    payload: unknown,
    options?: {
      harness?: "claude" | "codex";
      withoutPane?: boolean;
    },
  ): Promise<void>;
  close(windowId: string, paneId: string): Promise<void>;
  format(target: string, format: string): string;
  indexEntries(): Promise<Array<Record<string, unknown>>>;
};

async function withTmux(
  run: (fixture: TmuxFixture) => Promise<void>,
): Promise<void> {
  const server = `cl-tmux-test-${process.pid}-${crypto.randomUUID()}`;
  const stateDir = await mkdtemp(join(tmpdir(), "cl-tmux-state-"));

  function tmux(args: string[]): string {
    const result = Bun.spawnSync(["tmux", "-L", server, ...args], {
      stdout: "pipe",
      stderr: "pipe",
    });
    if (result.exitCode !== 0) throw new Error(result.stderr.toString().trim());
    return result.stdout.toString().trim();
  }

  try {
    tmux([
      "-f",
      "/dev/null",
      "new-session",
      "-d",
      "-s",
      "marker",
      "bun -e 'setInterval(() => {}, 1000)'",
    ]);
    const [socket, paneId] = tmux([
      "display-message",
      "-p",
      "#{socket_path}\t#{pane_id}",
    ]).split("\t");

    async function mark(
      targetPaneId: string,
      payload: unknown,
      options: {
        harness?: "claude" | "codex";
        withoutPane?: boolean;
      } = {},
    ): Promise<void> {
      const child = Bun.spawn(
        [MARKER, "--harness", options.harness ?? "claude"],
        {
          env: {
            ...process.env,
            TMUX: `${socket},0,0`,
            TMUX_PANE: options.withoutPane ? "" : targetPaneId,
            XDG_STATE_HOME: stateDir,
          },
          stdin: "pipe",
          stdout: "pipe",
          stderr: "pipe",
        },
      );
      const sink = child.stdin;
      if (!sink) throw new Error("marker stdin pipe unavailable");
      sink.write(JSON.stringify(payload));
      sink.end();

      const exitCode = await child.exited;
      if (exitCode !== 0) throw new Error(await child.stderr.text());
    }

    async function close(
      windowId: string,
      targetPaneId: string,
    ): Promise<void> {
      const module = JSON.stringify(join(ROOT, "src", "tmux.ts"));
      const target = JSON.stringify({ windowId, paneId: targetPaneId });
      const child = Bun.spawn(
        [
          "bun",
          "-e",
          `import * as Tmux from ${module}; await Tmux.close(${target})`,
        ],
        {
          env: { ...process.env, TMUX: `${socket},0,0` },
          stdout: "pipe",
          stderr: "pipe",
        },
      );
      if ((await child.exited) !== 0)
        throw new Error(await child.stderr.text());
    }

    await run({
      paneId,
      tmux,
      mark,
      close,
      async indexEntries() {
        const path = join(stateDir, "cl-tmux", "sessions.sqlite");
        const file = Bun.file(path);
        if (!(await file.exists())) return [];
        const db = new Database(path, { readonly: true });
        try {
          return db
            .query(
              "SELECT sid, cwd, started_at AS startedAt, title FROM sessions",
            )
            .all() as Array<Record<string, unknown>>;
        } finally {
          db.close();
        }
      },
      format(target, format) {
        return tmux(["display-message", "-p", "-t", target, format]);
      },
    });
  } finally {
    Bun.spawnSync(["tmux", "-L", server, "kill-server"], {
      stdout: "ignore",
      stderr: "ignore",
    });
    await rm(stateDir, { recursive: true, force: true });
  }
}

test("indexes resolved sessions and captures the first prompt", () =>
  withTmux(async (fixture) => {
    await fixture.mark(fixture.paneId, {
      session_id: "tracked-session",
      hook_event_name: "SessionStart",
      source: "startup",
      cwd: "/repo",
    });
    await fixture.mark(fixture.paneId, {
      session_id: "tracked-session",
      hook_event_name: "UserPromptSubmit",
      cwd: "/repo",
      prompt: "Investigate the build",
    });
    const entries = await fixture.indexEntries();
    expect(entries[0]).toMatchObject({
      sid: "tracked-session",
      title: "Investigate the build",
      cwd: "/repo",
      startedAt: expect.any(Number),
    });
  }));

test("publishes hook state and window icon", () =>
  withTmux(async (fixture) => {
    await fixture.mark(fixture.paneId, {
      session_id: "test-session",
      hook_event_name: "UserPromptSubmit",
    });
    expect(
      fixture.format(fixture.paneId, "#{@cl_sid}\t#{@cl_state}\t#{@cl_icon}"),
    ).toBe("test-session\tworking\t●");

    await fixture.mark(fixture.paneId, {
      session_id: "test-session",
      hook_event_name: "Stop",
    });
    expect(
      fixture.format(fixture.paneId, "#{@cl_sid}\t#{@cl_state}\t#{@cl_icon}"),
    ).toBe("test-session\tidle\t○");
  }));

test("tags a session without inventing state", () =>
  withTmux(async (fixture) => {
    await fixture.mark(fixture.paneId, {
      session_id: "started-session",
      hook_event_name: "SessionStart",
    });

    expect(fixture.format(fixture.paneId, "#{@cl_sid}|#{@cl_state}")).toBe(
      "started-session|",
    );
  }));

test("updates the session identity reported by hooks", () =>
  withTmux(async (fixture) => {
    for (const sessionId of ["original", "replacement", "latest"]) {
      await fixture.mark(fixture.paneId, {
        session_id: sessionId,
        hook_event_name: "SessionStart",
      });
    }

    expect(fixture.format(fixture.paneId, "#{@cl_sid}")).toBe("latest");
  }));

test("routes Codex hooks by the session title", () =>
  withTmux(async (fixture) => {
    const sid = "01a0d9e2-c386-7e63-84b7-b8775e010203";
    fixture.tmux([
      "set-option",
      "-p",
      "-t",
      fixture.paneId,
      "@cl_harness",
      "codex",
    ]);
    fixture.tmux([
      "select-pane",
      "-t",
      fixture.paneId,
      "-T",
      `codex | ${sid.slice(0, 29)}...`,
    ]);

    await fixture.mark(
      fixture.paneId,
      {
        session_id: sid,
        hook_event_name: "UserPromptSubmit",
      },
      { harness: "codex", withoutPane: true },
    );
    expect(fixture.format(fixture.paneId, "#{@cl_sid}|#{@cl_state}")).toBe(
      `${sid}|working`,
    );

    const otherPaneId = fixture.tmux([
      "split-window",
      "-d",
      "-t",
      fixture.paneId,
      "-P",
      "-F",
      "#{pane_id}",
      "bun -e 'setInterval(() => {}, 1000)'",
    ]);
    fixture.tmux([
      "set-option",
      "-p",
      "-t",
      otherPaneId,
      "@cl_harness",
      "codex",
    ]);
    fixture.tmux([
      "select-pane",
      "-t",
      otherPaneId,
      "-T",
      `codex | ${sid.slice(0, 29)}...`,
    ]);
    await fixture.mark(
      otherPaneId,
      {
        session_id: sid,
        hook_event_name: "Stop",
      },
      { harness: "codex" },
    );
    expect(fixture.format(fixture.paneId, "#{@cl_state}")).toBe("idle");
    expect(fixture.format(otherPaneId, "#{@cl_state}")).toBe("");
  }));

test("routes a Codex hook through a tagged pane when its title is unavailable", () =>
  withTmux(async (fixture) => {
    const sid = "01a0d9e2-c386-7e63-84b7-b8775e010203";
    fixture.tmux([
      "set-option",
      "-p",
      "-t",
      fixture.paneId,
      "@cl_harness",
      "codex",
    ]);
    fixture.tmux(["set-option", "-p", "-t", fixture.paneId, "@cl_sid", sid]);

    await fixture.mark(
      fixture.paneId,
      { session_id: sid, hook_event_name: "UserPromptSubmit" },
      { harness: "codex" },
    );

    expect(fixture.format(fixture.paneId, "#{@cl_state}")).toBe("working");
  }));

test("reconciles window icons after moving an agent pane", () =>
  withTmux(async (fixture) => {
    const secondPaneId = fixture.tmux([
      "split-window",
      "-d",
      "-t",
      fixture.paneId,
      "-P",
      "-F",
      "#{pane_id}",
      "bun -e 'setInterval(() => {}, 1000)'",
    ]);

    await fixture.mark(fixture.paneId, {
      session_id: "working-session",
      hook_event_name: "UserPromptSubmit",
    });
    await fixture.mark(secondPaneId, {
      session_id: "waiting-session",
      hook_event_name: "Notification",
      notification_type: "permission_prompt",
    });
    expect(fixture.format(fixture.paneId, "#{@cl_icon}")).toBe("◐");

    fixture.tmux(["break-pane", "-d", "-s", secondPaneId]);
    await fixture.mark(secondPaneId, {
      session_id: "waiting-session",
      hook_event_name: "Notification",
      notification_type: "permission_prompt",
    });

    expect(fixture.format(fixture.paneId, "#{@cl_icon}")).toBe("●");
    expect(fixture.format(secondPaneId, "#{@cl_icon}")).toBe("◐");
  }));

test("reconciles the window icon after closing a marked pane", () =>
  withTmux(async (fixture) => {
    const secondPaneId = fixture.tmux([
      "split-window",
      "-d",
      "-t",
      fixture.paneId,
      "-P",
      "-F",
      "#{pane_id}",
      "bun -e 'setInterval(() => {}, 1000)'",
    ]);
    const windowId = fixture.format(fixture.paneId, "#{window_id}");

    await fixture.mark(fixture.paneId, {
      session_id: "working-session",
      hook_event_name: "UserPromptSubmit",
    });
    await fixture.mark(secondPaneId, {
      session_id: "idle-session",
      hook_event_name: "Stop",
    });
    expect(fixture.format(secondPaneId, "#{@cl_icon}")).toBe("●");

    await fixture.close(windowId, fixture.paneId);

    expect(fixture.format(secondPaneId, "#{@cl_icon}")).toBe("○");
  }));
