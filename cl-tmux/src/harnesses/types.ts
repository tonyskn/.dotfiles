import type { AgentState, HarnessId } from "../sessions";

export type HookUpdate = {
  sid?: string;
  state?: AgentState;
  cwd?: string;
  prompt?: string;
};

export type Harness = {
  id: HarnessId;
  binary: string;
  sessionsDir: string;
  conversationText(record: Record<string, unknown>): string | undefined;
  isProcess(command: string): boolean;
  hookResponse?: string;
  hookUpdate(payload: unknown): HookUpdate;
  resume(args: { sid: string; name: string; prompt?: string }): string[];
  fork(args: { sourceSid: string; name: string }): string[];
  sessionGlob(sid: string): string;
};
