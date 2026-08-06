// _cl — data backend for the `cl` agent session manager.
// Manages bookmark JSONL, discovers live sessions, searches session files, and drives tmux.
// No interactive UI — stdout is structured for fzf consumption.

import { exists } from "fs/promises";
import { homedir } from "os";
import { parseArgs } from "util";
import * as Bookmarks from "../bookmarks";
import * as Harness from "../harnesses";
import {
  AgentStatus,
  SessionRef,
  buildSessionRows,
  type SessionMetadata,
  type SessionRow,
} from "../model";
import * as SessionFiles from "../session-files";
import * as Tmux from "../tmux";

const HOME = homedir();

function minutesSince(timestamp: number): number {
  return (Date.now() / 1000 - timestamp) / 60;
}

// Also posts to the tmux status line so the message outlives popups.
function fail(message: string): never {
  console.error(message);
  if (process.env.TMUX) Tmux.showMessage(`cl: ${message}`);
  process.exit(1);
}

// --- output ---

namespace Output {
  function icon(row?: SessionRow): string {
    if (!row?.pane) return "";
    return AgentStatus.aggregateIcon([row.pane.state, row.pane.mode]) || "-";
  }

  function displayName(name: string, row?: SessionRow): string {
    return row && !row.saved ? `* ${name}` : name;
  }

  function formatPath(path: string, marker = "", width = 40): string {
    const short = path.startsWith(HOME) ? "~" + path.slice(HOME.length) : path;
    return col(marker + short, width);
  }

  function col(s: string, n: number): string {
    return s.padEnd(n).slice(0, n);
  }

  function colRight(s: string, n: number): string {
    return s.padStart(n).slice(-n);
  }

  function relTime(mins: number): string {
    if (mins < 1) return "just now";
    if (mins < 60) return `${Math.round(mins)}m ago`;
    if (mins < 1440) return `${Math.round(mins / 60)}h ago`;
    if (mins < 10080) return `${Math.round(mins / 1440)}d ago`;
    return `${Math.round(mins / 10080)}w ago`;
  }

  // Escape codes must wrap the joined row: col() pads by raw length, so coloring
  // individual columns before padding would break alignment.
  function fzfRow(hidden: string[], display: string[], muted = false): void {
    const hiddenFields = hidden.join("\t");
    const displayFields = display.join("  ");
    const rendered = muted
      ? `\x1b[38;5;245m${displayFields}\x1b[0m`
      : displayFields;
    process.stdout.write(`${hiddenFields}\t${rendered}\n`);
  }

  export function printSession(row: SessionRow): void {
    const live = row.pane !== undefined;

    fzfRow(
      [row.harness, row.sid, row.name, row.pane?.paneId ?? ""],
      [
        col(icon(row), 1),
        colRight(relTime(minutesSince(row.lastActive)), 8),
        col(displayName(row.name, row), 30),
        formatPath(row.cwd),
        col(row.harness, 6),
        row.sid,
      ],
      !live,
    );
  }

  export function printSearchResult(
    entry: SessionMetadata,
    row?: SessionRow,
  ): void {
    const name = row?.name ?? entry.name;
    const pathMarker = entry.cwdExists ? "" : "✗ ";
    fzfRow(
      [entry.harness, entry.sid, name, entry.cwd],
      [
        col(icon(row), 1),
        colRight(relTime(minutesSince(entry.modifiedAt)), 8),
        col(entry.title, 50),
        col(displayName(name, row), 30),
        formatPath(entry.cwd, pathMarker),
        col(entry.harness, 6),
      ],
      row === undefined,
    );
  }
}

// --- sessions ---

namespace Sessions {
  export async function list(): Promise<SessionRow[]> {
    const panes = await Tmux.livePanes();
    // Hooks publish identity changes on panes; the picker owns persisted bookmark updates.
    for (const pane of panes) {
      if (!pane.previousSid) continue;

      Bookmarks.rebind({ harness: pane.harness, sid: pane.previousSid }, pane);
      await Tmux.setPaneOptions(pane.paneId, { "@cl_previous_sid": "" });
    }

    const rows = buildSessionRows(Bookmarks.all(), panes);
    Bookmarks.updateActivity(rows);

    return Promise.all(
      rows.map(async (row) => {
        if (row.saved) return row;
        const metadata = await SessionFiles.metadata(row);
        return { ...row, name: metadata?.name ?? row.name };
      }),
    );
  }

  export async function find(ref: SessionRef): Promise<SessionRow | undefined> {
    return (await list()).find((row) => SessionRef.equals(row, ref));
  }

  export async function require(ref: SessionRef): Promise<SessionRow> {
    return (await find(ref)) ?? fail("Session not found");
  }

  export async function ensureLaunchable(session: SessionRow): Promise<void> {
    if (!(await exists(session.cwd))) {
      fail(`Directory no longer exists: ${session.cwd}`);
    }
    if (!(await SessionFiles.metadata(session))) {
      fail(`No session file for '${session.name}'`);
    }
  }
}

// --- commands ---

namespace Cli {
  const USAGE: Record<string, string> = {
    list: "_cl list [--filter all|live|today|week]",
    save: "_cl save <harness> <sid> [--name <name>] [--cwd <dir>]",
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
      cwd: { type: "string" },
      filter: { type: "string" },
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

  function searchTerm(): string {
    return operands[0] ?? die();
  }

  function sessionRef(): SessionRef {
    const [harness, sid] = operands;
    if (!harness || !Harness.isId(harness) || !sid) die();
    return { harness, sid };
  }

  function matchesFilter(row: SessionRow): boolean {
    switch (flags.filter) {
      case "today":
        return minutesSince(row.lastActive) < 1440;
      case "week":
        return minutesSince(row.lastActive) < 10080;
      case "live":
        return row.pane !== undefined;
      default:
        return true;
    }
  }

  export async function main(): Promise<void> {
    switch (cmd) {
      case "list": {
        const sessions = (await Sessions.list()).filter(matchesFilter);
        for (const row of sessions) Output.printSession(row);
        break;
      }

      case "save": {
        const target = sessionRef();
        const bookmark = Bookmarks.find(target);
        const name = flags.name ?? bookmark?.name;
        const cwd =
          flags.cwd ?? bookmark?.cwd ?? (await Tmux.find(target))?.cwd;
        const renamed =
          flags.name !== undefined && bookmark?.name !== flags.name;

        if (!name) fail("New bookmark requires --name");
        if (!cwd)
          fail("New bookmark requires --cwd (no live pane to infer from)");

        Bookmarks.addOrSave(target, name, cwd);
        if (renamed) await Tmux.rename(target, name);
        break;
      }

      case "open": {
        const target = sessionRef();
        const existing = await Sessions.find(target);
        if (existing?.saved && !existing.pane)
          await Sessions.ensureLaunchable(existing);

        const name = existing?.name ?? flags.name ?? "unnamed";
        const cwd = existing?.cwd ?? flags.cwd;
        if (!cwd) fail("Need --cwd for unbookmarked session");

        await Tmux.open(target, {
          name,
          cwd,
          pane: existing?.pane,
          prompt: flags.prompt,
        });
        break;
      }

      case "fork": {
        const source = await Sessions.require(sessionRef());
        await Sessions.ensureLaunchable(source);
        await Tmux.fork(source, source.name + "-fork", source.cwd);
        break;
      }

      case "close": {
        const selected = await Sessions.require(sessionRef());
        if (selected.pane) await Tmux.close(selected.pane);
        break;
      }

      case "remove": {
        const selected = await Sessions.require(sessionRef());
        // Remove first so a tmux failure cannot strand the bookmark.
        Bookmarks.remove(selected);
        if (selected.pane) await Tmux.close(selected.pane);
        break;
      }

      case "search": {
        const [results, sessions] = await Promise.all([
          SessionFiles.search(searchTerm()),
          Sessions.list(),
        ]);
        const sessionsByRef = SessionRef.index(sessions);
        for (const entry of results) {
          Output.printSearchResult(
            entry,
            sessionsByRef.get(SessionRef.key(entry)),
          );
        }
        break;
      }

      default:
        die();
    }
  }
}

await Bookmarks.load();
try {
  await Cli.main();
} finally {
  await Bookmarks.flush();
}
