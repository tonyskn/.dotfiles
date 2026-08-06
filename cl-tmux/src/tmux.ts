import * as Harness from "./harnesses";
import type { Harness as HarnessAdapter } from "./harnesses/types";
import {
  AgentMode,
  AgentState,
  AgentStatus,
  SessionRef,
  type HarnessId,
  type LivePane,
} from "./model";

// Tmux lists every pane, but only panes owned by a recognized harness have session identity.
type Pane = Omit<LivePane, keyof SessionRef> & Partial<SessionRef>;

type PaneTarget = Pick<Pane, "windowId" | "paneId">;

type PaneOptions = {
  "@cl_harness": HarnessId;
  "@cl_sid": string;
  "@cl_previous_sid": string;
  "@cl_state": AgentState | "";
  "@cl_mode": AgentMode | "";
  "@cl_active_at": number;
};

type WindowOptions = {
  "@cl_icon": string;
};

type OptionUpdate =
  | { type: "pane"; id: string; options: Partial<PaneOptions> }
  | { type: "window"; id: string; options: Partial<WindowOptions> };

type CommandResult = {
  ok: boolean;
  stdout: string;
};

async function run(args: string[], stdin?: string): Promise<CommandResult> {
  const child = Bun.spawn(["tmux", ...args], {
    stdin: stdin === undefined ? "ignore" : "pipe",
    stdout: "pipe",
    stderr: "ignore",
  });
  if (stdin !== undefined) {
    const sink = child.stdin;
    if (!sink) throw new Error("tmux stdin pipe unavailable");
    sink.write(stdin);
    sink.end();
  }

  const stdout = child.stdout.text();
  const exitCode = await child.exited;
  return {
    ok: exitCode === 0,
    stdout: (await stdout).trim(),
  };
}

// Pane options are the storage contract shared by the picker and marker.
const PANE_FORMAT = [
  "#{@cl_harness}",
  "#{@cl_sid}",
  "#{@cl_previous_sid}",
  "#{window_id}",
  "#{pane_id}",
  "#{@cl_state}",
  "#{@cl_mode}",
  "#{@cl_active_at}",
  "#{pane_current_command}",
  "#{pane_current_path}",
].join("\t");

async function panes(): Promise<Pane[]> {
  const result = await run(["list-panes", "-a", "-F", PANE_FORMAT]);
  if (!result.ok) return [];

  const found: Pane[] = [];
  for (const line of result.stdout.split("\n")) {
    const [
      harness,
      sid,
      previousSid,
      windowId,
      paneId,
      state,
      mode,
      activeAt,
      command,
      cwd,
    ] = line.split("\t");

    if (!windowId || !paneId) continue;

    const harnessId =
      Harness.isId(harness) && Harness.get(harness).isProcess(command ?? "")
        ? harness
        : undefined;
    found.push({
      windowId,
      paneId,
      harness: harnessId,
      sid: harnessId && sid ? sid : undefined,
      previousSid: harnessId && previousSid ? previousSid : undefined,
      state: harnessId && AgentState.is(state) ? state : undefined,
      mode: harnessId && AgentMode.is(mode) ? mode : undefined,
      lastActive: Number(activeAt) || 0,
      cwd: cwd ?? "",
    });
  }

  return found;
}

export async function livePanes(): Promise<LivePane[]> {
  return (await panes()).filter(
    (pane): pane is Pane & LivePane =>
      pane.harness !== undefined && pane.sid !== undefined,
  );
}

export async function find(ref: SessionRef): Promise<LivePane | undefined> {
  return (await livePanes()).find((pane) => SessionRef.equals(pane, ref));
}

export async function close(pane: PaneTarget): Promise<void> {
  await run(["kill-pane", "-t", pane.paneId]);
  await reconcileWindowIcons([pane.windowId]);
}

export async function rename(ref: SessionRef, name: string): Promise<void> {
  const pane = await find(ref);
  if (pane)
    await run(["rename-window", "-t", pane.windowId, "--", name.toUpperCase()]);
}

async function focus(pane: PaneTarget): Promise<void> {
  await run(["select-window", "-t", pane.windowId]);
  await run(["select-pane", "-t", pane.paneId]);
}

async function paste(pane: PaneTarget, prompt: string): Promise<void> {
  await run(["load-buffer", "-b", "cl-prompt", "-"], prompt);
  await run(["paste-buffer", "-p", "-d", "-b", "cl-prompt", "-t", pane.paneId]);
  await run(["send-keys", "-t", pane.paneId, "Enter"]);
}

async function setOptions(update: OptionUpdate): Promise<boolean> {
  const scope = update.type === "pane" ? "-p" : "-w";
  const entries = Object.entries(update.options).filter(
    ([, value]) => value !== undefined,
  );
  const args = entries.flatMap(([option, value], index) => [
    ...(index ? [";"] : []),
    "set-option",
    scope,
    "-t",
    update.id,
    option,
    String(value),
  ]);
  return (await run(args)).ok;
}

export async function setPaneOptions(
  paneId: string,
  options: Partial<PaneOptions>,
): Promise<boolean> {
  return setOptions({ type: "pane", id: paneId, options });
}

async function launch(
  harness: HarnessAdapter,
  name: string,
  cwd: string,
  command: string[],
  knownSid?: string,
): Promise<PaneTarget> {
  const window = await run([
    "new-window",
    "-d",
    "-c",
    cwd,
    "-n",
    name.toUpperCase(),
    "-PF",
    "#{window_id}",
    "--",
    ...command,
  ]);
  const windowId = window.stdout;

  const listed = await run(["list-panes", "-t", windowId, "-F", "#{pane_id}"]);
  const paneId = listed.stdout.split("\n")[0];
  if (paneId) {
    await setPaneOptions(paneId, {
      "@cl_harness": harness.id,
      "@cl_sid": knownSid,
    });
  }

  return { windowId, paneId };
}

export async function fork(
  source: SessionRef,
  name: string,
  cwd: string,
): Promise<void> {
  const harness = Harness.get(source.harness);
  const command = harness.fork({ sourceSid: source.sid, name });
  const pane = await launch(harness, name, cwd, command);
  await focus(pane);
}

// Ensure a session has a running window, then focus or send a prompt.
export async function open(
  ref: SessionRef,
  options: {
    name: string;
    cwd: string;
    pane?: PaneTarget;
    prompt?: string;
  },
): Promise<void> {
  const { name, cwd, pane, prompt } = options;
  if (!pane) {
    const harness = Harness.get(ref.harness);
    const command = harness.resume({ sid: ref.sid, name, prompt });
    const launchedPane = await launch(harness, name, cwd, command, ref.sid);
    if (!prompt) await focus(launchedPane);
    return;
  }

  if (prompt) await paste(pane, prompt);
  else await focus(pane);
}

export async function paneIdentity(paneId: string): Promise<{
  harness?: string;
  sid?: string;
  previousSid?: string;
}> {
  const format = ["#{@cl_harness}", "#{@cl_sid}", "#{@cl_previous_sid}"].join(
    "\t",
  );
  const result = await run(["display-message", "-p", "-t", paneId, format]);
  const [harness, sid, previousSid] = result.stdout.split("\t");
  return {
    harness: harness || undefined,
    sid: sid || undefined,
    previousSid: previousSid || undefined,
  };
}

async function setWindowOptions(
  windowId: string,
  options: Partial<WindowOptions>,
): Promise<void> {
  await setOptions({ type: "window", id: windowId, options });
}

export async function reconcileWindowIcons(
  windowIds?: Iterable<string>,
): Promise<void> {
  const panesByWindow = Map.groupBy(await panes(), (pane) => pane.windowId);
  const targets = windowIds ? new Set(windowIds) : panesByWindow.keys();

  for (const windowId of targets) {
    const statuses: AgentStatus[] = [];
    for (const { state, mode } of panesByWindow.get(windowId) ?? []) {
      if (state) statuses.push(state);
      if (mode) statuses.push(mode);
    }
    await setWindowOptions(windowId, {
      "@cl_icon": AgentStatus.aggregateIcon(statuses),
    });
  }
}

export function showMessage(message: string): void {
  Bun.spawnSync(["tmux", "display-message", message], {
    stdout: "ignore",
    stderr: "ignore",
  });
}
