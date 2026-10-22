import type { AgentState } from "../sessions";
import { asRecord, conversationFiles } from "./util";
import type { Harness } from "./types";

function conversationText(record: Record<string, unknown>): string | undefined {
  if (record.type !== "message") return undefined;
  const message = asRecord(record.message);
  if (message?.role !== "user" && message?.role !== "assistant")
    return undefined;
  const content = message.content;
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return undefined;
  return content
    .map((block) => asRecord(block))
    .filter((block) => block?.type === "text")
    .map((block) => block?.text)
    .filter((text): text is string => typeof text === "string")
    .join("\n");
}

export const xirp: Harness = {
  id: "xirp",
  binary: "xirp",

  isProcess(command) {
    return ["xirp", "xirp-agent", "snipe", "node", "bun"].includes(command);
  },

  hookUpdate(payload) {
    // Children inherit TMUX_PANE but must not replace their parent's identity.
    if (
      process.env.SNIPE_SUBAGENT_CHILD !== undefined ||
      process.env.SNIPE_BG_TASK_ID !== undefined
    )
      return undefined;
    const record = asRecord(payload);
    if (!record) return undefined;

    let state: AgentState | undefined;
    switch (record.event) {
      case "SessionStart":
        break;
      case "UserPromptSubmit":
        state = "working";
        break;
      case "Stop":
        state = "idle";
        break;
      case "Notification":
        // idle_prompt also fires on startup; Stop already reports completed turns.
        if (record.notification_type !== "permission_prompt") return undefined;
        state = "waiting";
        break;
      default:
        return undefined;
    }
    return {
      sid:
        typeof record.session_id === "string" ? record.session_id : undefined,
      state,
      cwd: typeof record.cwd === "string" ? record.cwd : undefined,
      prompt:
        record.event === "UserPromptSubmit" && typeof record.prompt === "string"
          ? record.prompt
          : undefined,
    };
  },

  resume({ sid, prompt }) {
    const args = [this.binary, "--resume", sid];
    if (prompt) args.push(prompt);
    return args;
  },

  fork({ sourceSid }) {
    return [this.binary, "--fork", sourceSid];
  },

  conversation(sid) {
    return conversationFiles(
      ".snipe/sessions",
      `${sid}.jsonl`,
      conversationText,
    );
  },
};
