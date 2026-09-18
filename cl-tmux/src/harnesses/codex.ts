import { exists } from "fs/promises";
import type { AgentState } from "../model";
import { asRecord, jsonRecords } from "./json";
import type { Harness } from "./types";

function userMessage(record: Record<string, unknown>): string | undefined {
  const payload = asRecord(record.payload);
  if (record.type !== "event_msg") return undefined;
  if (payload?.type === "user_message" && typeof payload.message === "string")
    return payload.message;

  const item = asRecord(payload?.item);
  if (payload?.type !== "item_completed" || item?.type !== "UserMessage")
    return undefined;
  if (!Array.isArray(item.content)) return undefined;

  return item.content
    .map(asRecord)
    .filter((content) => content?.type === "text")
    .map((content) => content?.text)
    .filter((text): text is string => typeof text === "string")
    .join(" ");
}

export const codex: Harness = {
  id: "codex",
  binary: "codex",
  sessionsDir: ".codex/sessions",
  searchGlobs: ["*.jsonl"],

  isConversationRecord(record) {
    const payload = asRecord(record.payload);
    const eventMessage =
      record.type === "event_msg" &&
      (payload?.type === "user_message" || payload?.type === "agent_message");
    const responseMessage =
      record.type === "response_item" &&
      payload?.type === "message" &&
      (payload.role === "user" || payload.role === "assistant");
    return eventMessage || responseMessage;
  },

  isProcess(command) {
    return command === this.binary;
  },

  hookUpdate(payload) {
    const record = asRecord(payload);
    if (!record) return {};

    const event =
      typeof record.hook_event_name === "string" ? record.hook_event_name : "";
    let state: AgentState | undefined;
    switch (event) {
      case "UserPromptSubmit":
        state = "working";
        break;
      case "Stop":
        state = "idle";
        break;
    }
    return {
      sid:
        typeof record.session_id === "string" ? record.session_id : undefined,
      state,
    };
  },

  resume({ sid, prompt }) {
    const args = [this.binary, "resume", sid];
    if (prompt) args.push(prompt);
    return args;
  },

  fork({ sourceSid }) {
    // The initial turn triggers UserPromptSubmit, which reports the fork's generated SID.
    return [this.binary, "fork", sourceSid, "Wait for further instructions."];
  },

  sessionGlob(sid) {
    return `*${sid}.jsonl`;
  },

  async readMetadata(path) {
    const file = Bun.file(path);
    const records = jsonRecords(await file.text());
    const metadata = records.next().value?.payload as
      Record<string, unknown> | undefined;
    if (metadata?.source !== "cli") return undefined;

    const sid =
      typeof metadata.session_id === "string"
        ? metadata.session_id
        : typeof metadata.id === "string"
          ? metadata.id
          : "";
    if (!sid) return undefined;

    const cwd = typeof metadata.cwd === "string" ? metadata.cwd : "";
    const forkedFromSid =
      typeof metadata.forked_from_id === "string"
        ? metadata.forked_from_id
        : undefined;
    const title = records
      .map(userMessage)
      .find((message) => message !== undefined)
      ?.replace(/\s+/g, " ")
      .trim();

    const normalizedCwd = cwd.replace(/\/+$/, "");
    const name = normalizedCwd.slice(normalizedCwd.lastIndexOf("/") + 1);
    return {
      harness: this.id,
      sid,
      title: title || "untitled",
      name: name || "unnamed",
      cwd,
      cwdExists: Boolean(cwd) && (await exists(cwd)),
      modifiedAt: Math.floor(file.lastModified / 1000),
      forkedFromSid,
    };
  },
};
