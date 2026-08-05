import { exists } from "fs/promises";
import { homedir } from "os";
import { join } from "path";
import { Harness } from "./harnesses";
import type { Harness as HarnessAdapter } from "./harnesses/types";
import {
  collapseHistoryMatches,
  type HistoryEntry,
  type SessionRef,
} from "./model";

const HOME = homedir();

async function ripgrep(args: string[]): Promise<string> {
  return Bun.$`rg ${args}`.nothrow().quiet().text();
}

async function parse(
  harness: HarnessAdapter,
  file: string,
): Promise<HistoryEntry | undefined> {
  const parsed = harness.parseHistory(file, await Bun.file(file).text());
  if (!parsed) return undefined;

  const cwdExists = Boolean(parsed.cwd) && (await exists(parsed.cwd));
  return {
    harness: harness.id,
    ...parsed,
    title: parsed.title || "untitled",
    name: parsed.name || "unnamed",
    cwdExists,
    modifiedAt: Math.floor(Bun.file(file).lastModified / 1000),
  };
}

async function search(query: string): Promise<HistoryEntry[]> {
  const terms = query.trim().split(/\s+/).filter(Boolean);
  if (!terms.length) return [];

  const searches = Harness.all().map(async (harness) => {
    const globs = harness.historyGlobs.flatMap((glob) => ["-g", glob]);
    const directory = join(HOME, harness.historyDir);
    const filesByTerm = await Promise.all(
      terms.map(async (term) => {
        const files = await ripgrep(["-ilFw", ...globs, "--", term, directory]);
        return new Set(files.trim().split("\n").filter(Boolean));
      }),
    );

    const [firstFiles, ...remainingFiles] = filesByTerm;
    const matches = [...(firstFiles ?? [])].filter((file) =>
      remainingFiles.every((files) => files.has(file)),
    );

    return matches.map((file) => ({ harness, file }));
  });

  const matches = (await Promise.all(searches)).flat();
  matches.sort(
    (a, b) => Bun.file(b.file).lastModified - Bun.file(a.file).lastModified,
  );

  const parsed = await Promise.all(
    matches.map(({ harness, file }) => parse(harness, file)),
  );
  const entries = parsed.filter(
    (entry): entry is HistoryEntry => entry !== undefined,
  );

  return collapseHistoryMatches(entries);
}

async function get(ref: SessionRef): Promise<HistoryEntry | undefined> {
  const harness = Harness.get(ref.harness);
  const directory = join(HOME, harness.historyDir);
  const args = ["--files", "-g", harness.sessionGlob(ref.sid), directory];
  const file = (await ripgrep(args)).trim();
  if (!file) return undefined;

  return parse(harness, file);
}

export const History = { search, get };
