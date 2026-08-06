import { expect, test } from "bun:test";
import { join } from "path";

const ROOT = join(import.meta.dir, "..");
const MARKER = join(ROOT, "bin", "tmux-marker");

type TmuxFixture = {
  paneId: string;
  tmux(args: string[]): string;
  mark(paneId: string, args?: string[], payload?: unknown): Promise<void>;
  close(windowId: string, paneId: string): Promise<void>;
  format(target: string, format: string): string;
};

async function withTmux(
  run: (fixture: TmuxFixture) => Promise<void>,
): Promise<void> {
  const server = `cl-tmux-test-${process.pid}-${crypto.randomUUID()}`;

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
      args: string[] = [],
      payload?: unknown,
    ): Promise<void> {
      const child = Bun.spawn([MARKER, "--harness", "claude", ...args], {
        env: {
          ...process.env,
          TMUX: `${socket},0,0`,
          TMUX_PANE: targetPaneId,
        },
        stdin: payload === undefined ? "ignore" : "pipe",
        stdout: "pipe",
        stderr: "pipe",
      });
      if (payload !== undefined) {
        const sink = child.stdin;
        if (!sink) throw new Error("marker stdin pipe unavailable");
        sink.write(JSON.stringify(payload));
        sink.end();
      }

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
      format(target, format) {
        return tmux(["display-message", "-p", "-t", target, format]);
      },
    });
  } finally {
    Bun.spawnSync(["tmux", "-L", server, "kill-server"], {
      stdout: "ignore",
      stderr: "ignore",
    });
  }
}

test("keeps explicit modes separate from hook state", () =>
  withTmux(async (fixture) => {
    await fixture.mark(fixture.paneId, ["loop"]);
    await fixture.mark(fixture.paneId, [], {
      session_id: "test-session",
      hook_event_name: "Stop",
    });
    expect(
      fixture.format(
        fixture.paneId,
        "#{@cl_sid}\t#{@cl_state}\t#{@cl_mode}\t#{@cl_icon}",
      ),
    ).toBe("test-session\tidle\tloop\t∞");

    await fixture.mark(fixture.paneId, ["idle"]);
    expect(
      fixture.format(
        fixture.paneId,
        "#{@cl_sid}\t#{@cl_state}\t#{@cl_mode}\t#{@cl_icon}",
      ),
    ).toBe("test-session\tidle\t\t○");
  }));

test("tags a session without inventing activity", () =>
  withTmux(async (fixture) => {
    fixture.tmux([
      "set-option",
      "-p",
      "-t",
      fixture.paneId,
      "@cl_active_at",
      "123",
    ]);

    await fixture.mark(fixture.paneId, [], {
      session_id: "started-session",
      hook_event_name: "SessionStart",
    });

    expect(
      fixture.format(
        fixture.paneId,
        "#{@cl_sid}|#{@cl_state}|#{@cl_active_at}",
      ),
    ).toBe("started-session||123");
  }));

test("preserves the first SID replaced within a pane", () =>
  withTmux(async (fixture) => {
    for (const sessionId of ["original", "replacement", "latest"]) {
      await fixture.mark(fixture.paneId, [], {
        session_id: sessionId,
        hook_event_name: "SessionStart",
      });
    }

    expect(
      fixture.format(fixture.paneId, "#{@cl_sid}|#{@cl_previous_sid}"),
    ).toBe("latest|original");
  }));

test("reconciles window icons after moving a marked pane", () =>
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

    await fixture.mark(fixture.paneId, [], {
      session_id: "working-session",
      hook_event_name: "UserPromptSubmit",
    });
    await fixture.mark(secondPaneId, ["attention"]);
    expect(fixture.format(fixture.paneId, "#{@cl_icon}")).toBe("⚠");

    fixture.tmux(["break-pane", "-d", "-s", secondPaneId]);
    await fixture.mark(secondPaneId, [], {
      session_id: "attention-session",
      hook_event_name: "Stop",
    });

    expect(fixture.format(fixture.paneId, "#{@cl_icon}")).toBe("●");
    expect(fixture.format(secondPaneId, "#{@cl_icon}")).toBe("⚠");
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

    await fixture.mark(fixture.paneId, [], {
      session_id: "working-session",
      hook_event_name: "UserPromptSubmit",
    });
    expect(fixture.format(secondPaneId, "#{@cl_icon}")).toBe("●");

    await fixture.close(windowId, fixture.paneId);

    expect(fixture.format(secondPaneId, "#{@cl_icon}")).toBe("");
  }));
