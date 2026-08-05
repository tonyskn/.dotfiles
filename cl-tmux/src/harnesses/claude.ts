import type { AgentState } from "../model";
import { asRecord, jsonRecords } from "./json";
import type { Harness } from "./types";

export const claude: Harness = {
  id: "claude",
  binary: "claude",
  historyDir: ".claude/projects",
  historyGlobs: ["*.jsonl", "!subagents"],

  isProcess(command) {
    return (
      /^\d+\.\d+\.\d+$/.test(command) ||
      ["claude", "node", "bun"].includes(command)
    );
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
      case "Notification":
        state = "waiting";
        break;
    }
    return {
      sid:
        typeof record.session_id === "string" ? record.session_id : undefined,
      state,
    };
  },

  resume({ sid, name, prompt }) {
    const args = [this.binary, "--resume", sid, "-n", name];
    if (prompt) args.push(prompt);
    return args;
  },

  fork({ sourceSid, name }) {
    return [this.binary, "--resume", sourceSid, "--fork-session", "-n", name];
  },

  sessionGlob(sid) {
    return `${sid}.jsonl`;
  },

  parseHistory(file, contents) {
    const filename = file.slice(file.lastIndexOf("/") + 1);
    const sid = filename.replace(/\.jsonl$/, "");
    let title = "";
    let name = "";
    let cwd = "";

    for (const record of jsonRecords(contents)) {
      if (
        record.type === "ai-title" &&
        !title &&
        typeof record.aiTitle === "string"
      ) {
        title = record.aiTitle;
      }
      if (
        record.type === "custom-title" &&
        !name &&
        typeof record.customTitle === "string"
      ) {
        name = record.customTitle;
      }
      if (record.type === "user" && !cwd && typeof record.cwd === "string") {
        cwd = record.cwd;
      }
      if (title && name && cwd) break;
    }

    return { sid, title, name, cwd };
  },
};
