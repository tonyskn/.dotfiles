import { homedir } from "os";
import { join } from "path";
import * as Harness from "./harnesses";
import type { Harness as HarnessAdapter } from "./harnesses/types";
import { asRecord, jsonRecords } from "./harnesses/json";
import { SessionRef, type SessionMetadata } from "./model";

const HOME = homedir();

async function ripgrep(args: string[]): Promise<string> {
  const process = Bun.spawn(["rg", ...args], {
    stdout: "pipe",
    stderr: "ignore",
  });
  const stdout = process.stdout.text();
  await process.exited;
  return stdout;
}

async function filesMatchingTerm(
  harness: HarnessAdapter,
  term: string,
): Promise<Set<string>> {
  const directory = join(HOME, harness.sessionsDir);
  const globs = harness.searchGlobs.flatMap((glob) => ["-g", glob]);
  const ripgrepJson = await ripgrep([
    "--json",
    "-iFw",
    ...globs,
    "--",
    term,
    directory,
  ]);

  // Decode rg match events and keep files whose matched JSONL record is conversational.
  const matchingFiles = jsonRecords(ripgrepJson)
    .map((event) => {
      if (event.type !== "match") return undefined;

      const matchData = asRecord(event.data);
      const filename = asRecord(matchData?.path)?.text;
      const matchedLine = asRecord(matchData?.lines)?.text;
      if (typeof filename !== "string" || typeof matchedLine !== "string")
        return undefined;

      const record = jsonRecords(matchedLine).next().value;
      return record ? { filename, record } : undefined;
    })
    .filter((match) => match !== undefined)
    .filter(({ record }) => harness.isConversationRecord(record))
    .map(({ filename }) => filename);
  return new Set(matchingFiles);
}

async function searchHarness(
  harness: HarnessAdapter,
  terms: string[],
): Promise<SessionMetadata[]> {
  const filesByTerm = await Promise.all(
    terms.map((term) => filesMatchingTerm(harness, term)),
  );
  // Intersect the per-term results so every term occurs in the session file.
  const [firstFiles, ...remainingFiles] = filesByTerm;
  const matchingFiles = [...(firstFiles ?? [])].filter((filename) =>
    remainingFiles.every((files) => files.has(filename)),
  );

  const metadata = await Promise.all(
    matchingFiles.map((filename) => harness.readMetadata(filename)),
  );
  const matches = metadata.filter(
    (entry): entry is SessionMetadata => entry !== undefined,
  );
  return deduplicateForkMatches(matches);
}

// Group matching forks by their oldest matching ancestor, then keep the most recently modified.
function deduplicateForkMatches(
  entries: ReadonlyArray<SessionMetadata>,
): SessionMetadata[] {
  const matchBySid = new Map(entries.map((entry) => [entry.sid, entry]));
  const matchesByRootSid = Map.groupBy(entries, (entry) =>
    rootSid(entry, matchBySid),
  );

  return [...matchesByRootSid.values()].map((matches) =>
    matches.reduce((latest, match) =>
      match.modifiedAt > latest.modifiedAt ? match : latest,
    ),
  );
}

function rootSid(
  entry: SessionMetadata,
  matchesBySid: ReadonlyMap<string, SessionMetadata>,
): string {
  let root = entry;
  while (root.forkedFromSid) {
    const parent = matchesBySid.get(root.forkedFromSid);
    if (!parent) break;
    root = parent;
  }
  return root.sid;
}

// Find sessions containing every whole-word query term in conversation records.
export async function search(query: string): Promise<SessionMetadata[]> {
  const terms = query.trim().split(/\s+/).filter(Boolean);
  if (!terms.length) return [];

  const matchesByHarness = await Promise.all(
    Harness.all().map((harness) => searchHarness(harness, terms)),
  );
  return matchesByHarness.flat().sort((a, b) => b.modifiedAt - a.modifiedAt);
}

export async function metadata(
  ref: SessionRef,
): Promise<SessionMetadata | undefined> {
  const harness = Harness.get(ref.harness);
  const directory = join(HOME, harness.sessionsDir);
  const args = ["--files", "-g", harness.sessionGlob(ref.sid), directory];
  const file = (await ripgrep(args)).trim();
  if (!file) return undefined;

  return harness.readMetadata(file);
}
