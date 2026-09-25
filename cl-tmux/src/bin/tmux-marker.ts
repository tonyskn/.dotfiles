// Publishes hook state to pane options and derives window status options.

import { parseArgs } from "util";
import * as Harness from "../harnesses";
import * as Sessions from "../sessions";
import * as Tmux from "../tmux";

async function hookPayload(): Promise<unknown> {
  if (process.stdin.isTTY) return undefined;
  try {
    return await Bun.stdin.json();
  } catch {
    return undefined;
  }
}

const { values } = parseArgs({
  args: process.argv.slice(2),
  options: { harness: { type: "string" } },
});
if (!values.harness || !Harness.isId(values.harness)) process.exit(0);

const payload = await hookPayload();
const harness = Harness.get(values.harness);
if (harness.hookResponse) console.log(harness.hookResponse);
const { sid, state, cwd, prompt } = harness.hookUpdate(payload);
const paneId = await Tmux.resolveHookPaneId(
  harness,
  sid,
  process.env.TMUX_PANE,
);
if (!paneId) process.exit(0);

await Tmux.setPaneOptions(paneId, {
  "@cl_harness": harness.id,
  "@cl_state": state,
  "@cl_sid": sid,
});

if (sid && cwd) {
  Sessions.recordHook({ harness: harness.id, sid }, cwd, {
    active: state === "working" || state === "idle",
    prompt,
  });
}

await Tmux.reconcileWindowIcons();
