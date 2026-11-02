import type { AgentState } from "../sessions";
import { asRecord, conversationFiles } from "./util";
import type { Harness } from "./types";

const FORK_BOOTSTRAP_PROMPT = "Wait for further instructions.";

function isInstructions(text: string): boolean {
  // Older Codex transcripts lack the content-kind tag for injected instructions.
  return (
    text.trimStart().startsWith("# AGENTS.md instructions") &&
    text.includes("<INSTRUCTIONS>")
  );
}

function conversationText(record: Record<string, unknown>): string | undefined {
  const payload = asRecord(record.payload);
  if (
    record.type === "event_msg" &&
    (payload?.type === "user_message" || payload?.type === "agent_message")
  ) {
    if (typeof payload.message !== "string") return undefined;
    return payload.type === "user_message" && isInstructions(payload.message)
      ? undefined
      : payload.message;
  }
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
  const contentKinds = asRecord(
    payload.internal_chat_message_metadata_passthrough,
  )?.content_item_kinds;
  return payload.content
    .map((block, index) => ({ block: asRecord(block), index }))
    .filter(
      ({ block, index }) =>
        block?.type === textType &&
        (!Array.isArray(contentKinds) ||
          ![
            "agents_md.instructions",
            "environments.environment_context",
          ].includes(contentKinds[index])),
    )
    .map(({ block }) => block?.text)
    .filter((text): text is string => typeof text === "string")
    .filter((text) => payload.role !== "user" || !isInstructions(text))
    .join("\n");
}

async function* conversationMessages(
  records: AsyncIterable<Record<string, unknown>>,
): AsyncGenerator<string> {
  // Later Codex pages can replace earlier history, so collect messages before yielding them.
  let messages: Array<{ ordinal: number; text: string }> = [];
  for await (const record of records) {
    if (record.type === "session_meta") {
      const cutoff = asRecord(
        asRecord(record.payload)?.history_base,
      )?.end_ordinal_exclusive;
      if (typeof cutoff === "number")
        messages = messages.filter((message) => message.ordinal < cutoff);
    }
    const text = conversationText(record);
    if (text)
      messages.push({
        ordinal: typeof record.ordinal === "number" ? record.ordinal : -1,
        text,
      });
  }
  yield* messages.map((message) => message.text);
}

export const codex: Harness = {
  id: "codex",
  binary: "codex",
  hookResponse: "{}", // Stop hooks require JSON, even when no pane can be resolved.

  isProcess(command) {
    return command === this.binary;
  },

  hookUpdate(payload) {
    const record = asRecord(payload);
    // Side chats have no persisted transcript and cannot be resumed.
    if (record?.transcript_path === null) return undefined;
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

  resume({ sid, cwd, prompt }) {
    const args = [this.binary, "resume", "-C", cwd, sid];
    if (prompt) args.push(prompt);
    return args;
  },

  fork({ sourceSid }) {
    // The initial turn triggers UserPromptSubmit, which reports the fork's generated SID.
    return [this.binary, "fork", sourceSid, FORK_BOOTSTRAP_PROMPT];
  },

  async conversation(sid) {
    const files = await conversationFiles(".codex/sessions", `*${sid}*.jsonl`);
    if (!files) return undefined;
    return {
      fingerprint: files.fingerprint,
      messages: conversationMessages(files.records),
    };
  },
};
