import type { AgentState, HarnessId, SessionMetadata } from "../model";

export type HookUpdate = {
  sid?: string;
  state?: AgentState;
};

export type Harness = {
  id: HarnessId;
  binary: string;
  sessionsDir: string;
  searchGlobs: string[];
  isConversationRecord(record: Record<string, unknown>): boolean;
  isProcess(command: string): boolean;
  hookUpdate(payload: unknown): HookUpdate;
  resume(args: { sid: string; name: string; prompt?: string }): string[];
  fork(args: { sourceSid: string; name: string }): string[];
  sessionGlob(sid: string): string;
  readMetadata(filename: string): Promise<SessionMetadata | undefined>;
};
