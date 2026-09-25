// _cl — data backend for the `cl` agent session manager.
// Lists indexed sessions, searches transcripts, and drives tmux.
// No interactive UI — stdout is structured for fzf consumption.

import { parseArgs } from "util";
import * as Harness from "../harnesses";
import * as IO from "../io";
import * as Sessions from "../sessions";
import { SessionRef } from "../sessions";
import * as Store from "../store";
import * as Tmux from "../tmux";

// Also posts to the tmux status line so the message outlives popups.
function fail(message: string): never {
  console.error(message);
  if (process.env.TMUX) Tmux.showMessage(`cl: ${message}`);
  process.exit(1);
}

// --- commands ---

namespace Cli {
  const USAGE: Record<string, string> = {
    list: "_cl list [<pane-id> <window-id>] [--filter all|live|today|week]",
    save: "_cl save <harness> <sid> [--name <name>]",
    open: "_cl open <harness> <sid> [--prompt <text>]",
    fork: "_cl fork <harness> <sid>",
    close: "_cl close <harness> <sid>",
    remove: "_cl remove <harness> <sid>",
    search: "_cl search <term>",
  };

  const { positionals, values: flags } = parseArgs({
    allowPositionals: true,
    options: {
      prompt: { type: "string" },
      name: { type: "string" },
      filter: { type: "string" },
      bookmarked: { type: "boolean" },
    },
  });
  const [cmd, ...operands] = positionals;

  // Prints the command's own usage if we recognise it, otherwise the full list.
  function die(): never {
    const lines = USAGE[cmd]
      ? [`Usage: ${USAGE[cmd]}`]
      : ["Usage:", ...Object.values(USAGE).map((u) => "  " + u)];
    fail(lines.join("\n"));
  }

  function sessionRef(): SessionRef {
    const [harness, sid] = operands;
    if (!harness || !Harness.isId(harness) || !sid) die();
    return { harness, sid };
  }

  export async function main(): Promise<void> {
    switch (cmd) {
      case "list": {
        if (operands.length !== 0 && operands.length !== 2) die();
        const [paneId, windowId] = operands;
        const { rows, selectedPaneId } = await Sessions.list(
          { bookmarkedView: flags.bookmarked, period: flags.filter },
          paneId,
          windowId,
        );
        if (operands.length === 2) console.log(selectedPaneId ?? "-");
        for (const row of rows) IO.printSession(row);
        break;
      }

      case "save": {
        const target = sessionRef();
        const stored = Store.find(target) ?? fail("Session not indexed");
        const name = flags.name ?? stored.bookmarkName;
        const renamed =
          flags.name !== undefined && stored.bookmarkName !== name;

        if (!name) fail("New bookmark requires --name");

        Store.save(target, name);
        if (renamed) await Tmux.rename(target, name);
        break;
      }

      case "open": {
        const target = sessionRef();
        const existing =
          (await Sessions.find(target)) ?? fail("Session not indexed");
        if (!existing.pane) {
          const error = await Sessions.launchError(existing);
          if (error) fail(error);
        }

        await Tmux.open(target, {
          name: existing.name,
          cwd: existing.cwd,
          pane: existing.pane,
          prompt: flags.prompt,
        });
        break;
      }

      case "fork": {
        const source =
          (await Sessions.find(sessionRef())) ?? fail("Session not found");
        const error = await Sessions.launchError(source);
        if (error) fail(error);
        await Tmux.fork(source, source.name + "-fork", source.cwd);
        break;
      }

      case "close": {
        const selected =
          (await Sessions.find(sessionRef())) ?? fail("Session not found");
        if (selected.pane) await Tmux.close(selected.pane);
        break;
      }

      case "remove": {
        const selected =
          (await Sessions.find(sessionRef())) ?? fail("Session not found");
        if (selected.pane) await Tmux.close(selected.pane);
        Store.remove(selected);
        break;
      }

      case "search": {
        if (!operands[0]?.trim()) die();
        for (const row of await Sessions.search(operands[0], flags.bookmarked))
          IO.printSession(row);
        break;
      }

      default:
        die();
    }
  }
}

await Cli.main();
