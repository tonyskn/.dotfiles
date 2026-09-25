import { homedir } from "os";
import { basename, join } from "path";
import * as Harness from "./harnesses";
import type { Harness as HarnessAdapter } from "./harnesses/types";
import { asRecord, jsonRecords } from "./harnesses/json";
import { SessionRef } from "./sessions";

async function pathsForRefs(
  harness: HarnessAdapter,
  refs: ReadonlyArray<SessionRef>,
): Promise<string[]> {
  if (!refs.length) return [];
  const filenames = refs.map((ref) => harness.sessionGlob(ref.sid));
  const glob = new Bun.Glob(`**/{${filenames.join(",")}}`);
  return Array.fromAsync(
    glob.scan({
      cwd: join(process.env.HOME || homedir(), harness.sessionsDir),
      absolute: true,
      onlyFiles: true,
    }),
  );
}

export async function hasTranscript(ref: SessionRef): Promise<boolean> {
  return (await pathsForRefs(Harness.get(ref.harness), [ref])).length > 0;
}

async function searchHarness(
  harness: HarnessAdapter,
  terms: string[],
  refs: SessionRef[],
): Promise<SessionRef[]> {
  if (!refs.length) return [];
  const paths = (await pathsForRefs(harness, refs)).filter(
    (path) => !path.includes("/subagents/"),
  );
  if (!paths.length) return [];
  // Multiple transcript files can belong to one indexed session.
  const sidByPath = new Map<string, string>();
  for (const path of paths) {
    const sid = refs.find((ref) => basename(path).includes(ref.sid))?.sid;
    if (sid) sidByPath.set(path, sid);
  }
  if (!sidByPath.size) return [];
  // Recheck visible conversation text: rg searches raw JSON, including metadata.
  // The lookarounds exclude adjacent Unicode letters, digits, and underscores.
  const patterns = terms.map(
    (term) =>
      new RegExp(
        `(?<![\\p{L}\\p{N}_])${term.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}(?![\\p{L}\\p{N}_])`,
        "iu",
      ),
  );
  const child = Bun.spawn(
    [
      "rg",
      "--json",
      "-iFw",
      ...terms.flatMap((term) => ["-e", term]),
      "--",
      ...sidByPath.keys(),
    ],
    { stdout: "pipe", stderr: "ignore" },
  );
  const output = child.stdout.text();
  const exitCode = await child.exited;
  if (exitCode > 1)
    throw new Error(`ripgrep failed with exit code ${exitCode}`);

  const termsBySid = new Map<string, Set<number>>();
  for (const event of jsonRecords(await output)) {
    if (event.type !== "match") continue;
    const data = asRecord(event.data);
    const path = asRecord(data?.path)?.text;
    const line = asRecord(data?.lines)?.text;
    if (typeof path !== "string" || typeof line !== "string") continue;
    const sid = sidByPath.get(path);
    if (!sid) continue;

    const record = jsonRecords(line).next().value;
    const text = record && harness.conversationText(record);
    if (!text) continue;

    // Match decoded messages, not metadata or tool output on the JSONL line.
    const matched = termsBySid.get(sid) ?? new Set<number>();
    patterns.forEach((pattern, index) => {
      if (pattern.test(text)) matched.add(index);
    });
    termsBySid.set(sid, matched);
  }

  return refs.filter((ref) => termsBySid.get(ref.sid)?.size === terms.length);
}

// Find tracked sessions containing every whole-word query term in conversation text.
export async function search(
  query: string,
  refs: ReadonlyArray<SessionRef>,
): Promise<SessionRef[]> {
  const terms = [...new Set(query.trim().split(/\s+/).filter(Boolean))];
  if (!terms.length) return [];
  const matchesByHarness = await Promise.all(
    Harness.all().map((harness) =>
      searchHarness(
        harness,
        terms,
        refs.filter((ref) => ref.harness === harness.id),
      ),
    ),
  );
  return matchesByHarness.flat();
}
