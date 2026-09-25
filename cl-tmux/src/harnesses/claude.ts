import type { AgentState } from "../sessions";
import { asRecord } from "./json";
import type { Harness } from "./types";

export const claude: Harness = {
  id: "claude",
  binary: "claude",
  sessionsDir: ".claude/projects",

  conversationText(record) {
    if (
      (record.type !== "user" && record.type !== "assistant") ||
      record.isMeta === true
    )
      return undefined;
    const content = asRecord(record.message)?.content;
    if (typeof content === "string") return content;
    if (!Array.isArray(content)) return undefined;
    return content
      .map((block) => asRecord(block))
      .filter((block) => block?.type === "text")
      .map((block) => block?.text)
      .filter((text): text is string => typeof text === "string")
      .join("\n");
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
      cwd: typeof record.cwd === "string" ? record.cwd : undefined,
      prompt:
        event === "UserPromptSubmit" && typeof record.prompt === "string"
          ? record.prompt
          : undefined,
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
};
