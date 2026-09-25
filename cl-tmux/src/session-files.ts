import { homedir } from "os";
import { join } from "path";
import * as Harness from "./harnesses";
import type { Harness as HarnessAdapter } from "./harnesses/types";
import { asRecord, jsonRecords } from "./harnesses/json";
import { SessionMetadata, SessionRef } from "./model";

const HOME = homedir();
const RECENT_LIMIT = 50;

type SessionFile = {
  harness: HarnessAdapter;
  path: string;
  activeAt: number;
};

async function ripgrep(args: string[]): Promise<string> {
  const process = Bun.spawn(["rg", ...args], {
    stdout: "pipe",
    stderr: "ignore",
  });
  const stdout = process.stdout.text();
  await process.exited;
  return stdout;
}

async function scanSessionPaths(
  harness: HarnessAdapter,
  pattern: string,
): Promise<string[]> {
  const glob = new Bun.Glob(pattern);
  return Array.fromAsync(
    glob.scan({
      cwd: join(HOME, harness.sessionsDir),
      absolute: true,
      onlyFiles: true,
    }),
  );
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
  return deduplicateSearchMatches(matches);
}

async function sessionFiles(harness: HarnessAdapter): Promise<SessionFile[]> {
  const paths = await scanSessionPaths(harness, "**/*.jsonl");

  return paths.map((path) => ({
    harness,
    path,
    activeAt: Math.floor(Bun.file(path).lastModified / 1000),
  }));
}

// Merge physical pages, then keep the most recently modified match in each fork tree.
function deduplicateSearchMatches(
  entries: ReadonlyArray<SessionMetadata>,
): SessionMetadata[] {
  const sessions = SessionMetadata.mergePages(entries);
  const matchBySid = new Map(sessions.map((entry) => [entry.sid, entry]));
  const matchesByRootSid = Map.groupBy(sessions, (entry) =>
    rootSid(entry, matchBySid),
  );

  return [...matchesByRootSid.values()].map((matches) =>
    matches.reduce((latest, match) =>
      match.activeAt > latest.activeAt ? match : latest,
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
  if (!terms.length) return recent();

  const matchesByHarness = await Promise.all(
    Harness.all().map((harness) => searchHarness(harness, terms)),
  );
  return matchesByHarness.flat().sort((a, b) => b.activeAt - a.activeAt);
}

async function recent(): Promise<SessionMetadata[]> {
  const pendingFiles = (await Promise.all(Harness.all().map(sessionFiles)))
    .flat()
    .sort((a, b) => b.activeAt - a.activeAt);
  let sessions: SessionMetadata[] = [];

  // Codex subagent rollouts have the same filename shape, so keep going until
  // adapters accept enough user sessions or there are no files left.
  while (sessions.length < RECENT_LIMIT && pendingFiles.length) {
    const batch = pendingFiles.splice(0, RECENT_LIMIT - sessions.length);
    const metadata = await Promise.all(
      batch.map(({ harness, path }) => harness.readMetadata(path)),
    );
    sessions = SessionMetadata.mergePages([
      ...sessions,
      ...metadata.filter(
        (entry): entry is SessionMetadata => entry !== undefined,
      ),
    ]);
  }

  return sessions;
}

export async function readMetadataBySession(
  refs: ReadonlyArray<SessionRef>,
): Promise<Map<string, SessionMetadata>> {
  const uniqueRefs = [...SessionRef.index(refs).values()];
  const refsByHarness = Map.groupBy(uniqueRefs, (ref) => ref.harness);
  const metadataByHarness = await Promise.all(
    [...refsByHarness].map(async ([harnessId, harnessRefs]) => {
      const harness = Harness.get(harnessId);
      const filenames = harnessRefs.map((ref) => harness.sessionGlob(ref.sid));
      const paths = await scanSessionPaths(
        harness,
        `**/{${filenames.join(",")}}`,
      );
      return Promise.all(paths.map((path) => harness.readMetadata(path)));
    }),
  );

  return SessionRef.index(
    SessionMetadata.mergePages(
      metadataByHarness
        .flat()
        .filter((entry): entry is SessionMetadata => entry !== undefined),
    ),
  );
}
