import type { AgentState } from "../sessions";
import { asRecord } from "./json";
import type { Harness } from "./types";

const FORK_BOOTSTRAP_PROMPT = "Wait for further instructions.";

export const codex: Harness = {
  id: "codex",
  binary: "codex",
  sessionsDir: ".codex/sessions",
  hookResponse: "{}", // Stop hooks require JSON, even when no pane can be resolved.

  conversationText(record) {
    const payload = asRecord(record.payload);
    if (
      record.type === "event_msg" &&
      (payload?.type === "user_message" || payload?.type === "agent_message")
    )
      return typeof payload.message === "string" ? payload.message : undefined;
    if (
      record.type !== "response_item" ||
      payload?.type !== "message" ||
      !Array.isArray(payload.content)
    )
      return undefined;
    const textType =
      payload.role === "user"
        ? "input_text"
        : payload.role === "assistant"
          ? "output_text"
          : undefined;
    if (!textType) return undefined;
    return payload.content
      .map((block) => asRecord(block))
      .filter((block) => block?.type === textType)
      .map((block) => block?.text)
      .filter((text): text is string => typeof text === "string")
      .join("\n");
  },

  isProcess(command) {
    return command === this.binary;
  },

  hookUpdate(payload) {
    const record = asRecord(payload);
    const event =
      typeof record?.hook_event_name === "string" ? record.hook_event_name : "";
    let state: AgentState | undefined;
    switch (event) {
      case "UserPromptSubmit":
        state = "working";
        break;
      case "Stop":
        state = "idle";
        break;
    }
    const prompt =
      event === "UserPromptSubmit" && typeof record?.prompt === "string"
        ? record.prompt
        : undefined;
    return {
      sid:
        typeof record?.session_id === "string" ? record.session_id : undefined,
      state,
      cwd: typeof record?.cwd === "string" ? record.cwd : undefined,
      prompt: prompt === FORK_BOOTSTRAP_PROMPT ? undefined : prompt,
    };
  },

  resume({ sid, prompt }) {
    const args = [this.binary, "resume", sid];
    if (prompt) args.push(prompt);
    return args;
  },

  fork({ sourceSid }) {
    // The initial turn triggers UserPromptSubmit, which reports the fork's generated SID.
    return [this.binary, "fork", sourceSid, FORK_BOOTSTRAP_PROMPT];
  },

  sessionGlob(sid) {
    return `*${sid}*.jsonl`;
  },
};
