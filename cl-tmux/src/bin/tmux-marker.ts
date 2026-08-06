// Publishes agent session state to pane options and derives window status options.
//
//   tmux-marker [--harness <id>] [state | mode]

import { parseArgs } from "util";
import * as Harness from "../harnesses";
import { AgentMode, AgentState } from "../model";
import * as Tmux from "../tmux";

const paneId = process.env.TMUX_PANE;
if (!paneId) process.exit(0);

async function hookPayload(): Promise<unknown> {
  if (process.stdin.isTTY) return undefined;
  try {
    return await Bun.stdin.json();
  } catch {
    return undefined;
  }
}

const { positionals, values } = parseArgs({
  args: process.argv.slice(2),
  allowPositionals: true,
  options: { harness: { type: "string" } },
});
if (values.harness !== undefined && !Harness.isId(values.harness))
  process.exit(0);

const payload = await hookPayload();
const tagged = await Tmux.paneIdentity(paneId);
const harnessId = values.harness ?? tagged.harness;
if (!harnessId || !Harness.isId(harnessId)) process.exit(0);

const harness = Harness.get(harnessId);
const requested = positionals[0] ?? "";
const update = harness.hookUpdate(payload);
const requestedMode = AgentMode.is(requested) ? requested : undefined;
const requestedState = AgentState.is(requested) ? requested : undefined;
const state = requestedState ?? update.state;
// Keep the bookmarked identity across repeated replacements until the picker reconciles it.
const previousSid =
  update.sid && tagged.sid && update.sid !== tagged.sid
    ? (tagged.previousSid ?? tagged.sid)
    : undefined;

// Hook state is transient; only an explicit idle command exits a sticky mode.
const mode = requestedState === "idle" ? "" : requestedMode;
if (
  !(await Tmux.setPaneOptions(paneId, {
    "@cl_harness": harness.id,
    "@cl_state": state,
    "@cl_mode": mode,
    "@cl_previous_sid": previousSid,
    "@cl_active_at":
      state || requestedMode ? Math.floor(Date.now() / 1000) : undefined,
    "@cl_sid": update.sid,
  }))
)
  process.exit(0);

await Tmux.reconcileWindowIcons();
