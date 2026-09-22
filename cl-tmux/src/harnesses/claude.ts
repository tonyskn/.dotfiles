import { exists } from "fs/promises";
import type { AgentState } from "../model";
import { asRecord, jsonRecords } from "./json";
import type { Harness } from "./types";

export const claude: Harness = {
  id: "claude",
  binary: "claude",
  sessionsDir: ".claude/projects",
  searchGlobs: ["*.jsonl", "!subagents"],

  isConversationRecord(record) {
    const content = asRecord(record.message)?.content;
    const hasUserText =
      typeof content === "string" ||
      (Array.isArray(content) &&
        content.some((block) => asRecord(block)?.type === "text"));

    return (
      record.type === "assistant" ||
      (record.type === "user" && record.isMeta !== true && hasUserText)
    );
  },

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

  async readMetadata(path) {
    if (path.includes("/subagents/")) return undefined;

    const file = Bun.file(path);
    const filename = path.slice(path.lastIndexOf("/") + 1);
    const sid = filename.replace(/\.jsonl$/, "");
    let startedAt: number | undefined;
    let title = "";
    let name = "";
    let cwd = "";

    for (const record of jsonRecords(await file.text())) {
      // Agent SDK runs also write Claude transcripts, but they are not
      // interactive sessions that cl-tmux should offer for navigation.
      if (
        typeof record.entrypoint === "string" &&
        record.entrypoint !== "cli"
      ) {
        return undefined;
      }
      if (startedAt === undefined && typeof record.timestamp === "string") {
        const timestamp = Date.parse(record.timestamp);
        if (Number.isFinite(timestamp))
          startedAt = Math.floor(timestamp / 1000);
      }
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
      if (startedAt !== undefined && title && name && cwd) break;
    }
    if (startedAt === undefined) return undefined;

    return {
      harness: this.id,
      sid,
      startedAt,
      activeAt: Math.floor(file.lastModified / 1000),
      title: title || "untitled",
      name: name || "unnamed",
      cwd,
      cwdExists: Boolean(cwd) && (await exists(cwd)),
    };
  },
};
