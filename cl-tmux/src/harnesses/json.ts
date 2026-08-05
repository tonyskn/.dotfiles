export function* jsonRecords(
  contents: string,
): Generator<Record<string, unknown>> {
  for (const line of contents.trim().split("\n")) {
    try {
      const record = JSON.parse(line);
      if (record && typeof record === "object" && !Array.isArray(record))
        yield record;
    } catch {}
  }
}

export function asRecord(
  payload: unknown,
): Record<string, unknown> | undefined {
  if (!payload || typeof payload !== "object" || Array.isArray(payload))
    return undefined;
  return payload as Record<string, unknown>;
}
