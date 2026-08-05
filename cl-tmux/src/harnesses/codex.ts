import type { AgentState } from "../model";
import { asRecord, jsonRecords } from "./json";
import type { Harness } from "./types";

export const codex: Harness = {
  id: "codex",
  binary: "codex",
  historyDir: ".codex/sessions",
  historyGlobs: ["*.jsonl"],

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

  parseHistory(_file, contents) {
    const records = jsonRecords(contents);
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
    let title = "";
    for (const record of records) {
      const payload = record.payload as Record<string, unknown> | undefined;
      if (
        record.type === "event_msg" &&
        payload?.type === "user_message" &&
        typeof payload.message === "string"
      ) {
        title = payload.message.replace(/\s+/g, " ").trim();
        break;
      }
    }

    const normalizedCwd = cwd.replace(/\/+$/, "");
    const name = normalizedCwd.slice(normalizedCwd.lastIndexOf("/") + 1);
    return { sid, title, name, cwd, forkedFromSid };
  },
};
