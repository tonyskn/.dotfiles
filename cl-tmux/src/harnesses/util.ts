import { createHash } from "crypto";
import { createReadStream } from "fs";
import { stat } from "fs/promises";
import { homedir } from "os";
import { join } from "path";
import { createInterface } from "readline";
import type { Conversation } from "./types";

export function asRecord(
  payload: unknown,
): Record<string, unknown> | undefined {
  if (!payload || typeof payload !== "object" || Array.isArray(payload))
    return undefined;
  return payload as Record<string, unknown>;
}

function parseRecord(line: string): Record<string, unknown> | undefined {
  try {
    return asRecord(JSON.parse(line));
  } catch {
    return undefined;
  }
}

export async function conversationFiles(
  directory: string,
  filename: string,
  textForRecord: (record: Record<string, unknown>) => string | undefined,
): Promise<Conversation | undefined> {
  const cwd = join(process.env.HOME || homedir(), directory);
  const found = await Array.fromAsync(
    new Bun.Glob(`**/${filename}`).scan({
      cwd,
      absolute: true,
      onlyFiles: true,
    }),
  );
  const paths = found.filter((path) => !path.includes("/subagents/")).sort();
  if (!paths.length) return undefined;

  const files = await Promise.all(
    paths.map(async (path) => {
      const { size, mtimeMs } = await stat(path);
      return [path, size, mtimeMs] as const;
    }),
  );

  const fingerprint = createHash("sha256")
    .update(JSON.stringify(files))
    .digest("hex");
  return { fingerprint, messages: readMessages(paths, textForRecord) };
}

async function* readMessages(
  paths: ReadonlyArray<string>,
  textForRecord: (record: Record<string, unknown>) => string | undefined,
): AsyncGenerator<string> {
  for (const path of paths) {
    const lines = createInterface({ input: createReadStream(path) });
    for await (const line of lines) {
      const record = parseRecord(line);
      if (!record) continue;
      const text = textForRecord(record);
      if (text) yield text;
    }
  }
}
