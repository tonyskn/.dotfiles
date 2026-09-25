import { homedir } from "os";
import { AgentState, type SessionRow } from "./sessions";

const HOME = homedir();
const TITLE_WIDTH = 60;

function col(value: string, width: number): string {
  return value.padEnd(width).slice(0, width);
}

function colRight(value: string, width: number): string {
  return value.padStart(width).slice(-width);
}

function relativeTime(
  timestamp: number | undefined,
  suffix: "ago" | "old",
): string {
  if (timestamp === undefined) return "-";
  const mins = (Date.now() / 1000 - timestamp) / 60;
  if (mins < 1) return "just now";
  if (mins < 60) return `${Math.round(mins)}m ${suffix}`;
  if (mins < 1440) return `${Math.round(mins / 60)}h ${suffix}`;
  if (mins < 10080) return `${Math.round(mins / 1440)}d ${suffix}`;
  return `${Math.round(mins / 10080)}w ${suffix}`;
}

export function printSession(row: SessionRow): void {
  const name = !row.bookmarked && row.title ? row.title : row.name;
  const displayName = row.bookmarked ? name : `* ${name}`;
  const shortPath = row.cwd.startsWith(HOME)
    ? "~" + row.cwd.slice(HOME.length)
    : row.cwd;
  const hidden = [
    row.harness,
    row.sid,
    row.bookmarked ? row.name : "",
    row.pane?.paneId ?? "",
    row.title ?? name,
  ].join("\t");
  const display = [
    col(row.pane ? AgentState.icon(row.pane.state) || "-" : "", 1),
    colRight(relativeTime(row.activeAt, "ago"), 8),
    col(displayName, TITLE_WIDTH),
    col(shortPath, 40),
    col(row.harness, 6),
    colRight(relativeTime(row.startedAt, "old"), 8),
    row.sid,
  ].join("  ");

  // Color the joined row so escape codes cannot affect column padding.
  const rendered = row.pane ? display : `\x1b[38;5;245m${display}\x1b[0m`;
  process.stdout.write(`${hidden}\t${rendered}\n`);
}
