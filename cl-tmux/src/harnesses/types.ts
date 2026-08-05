import type { AgentState, HarnessId } from "../model";

export type ParsedHistory = {
  sid: string;
  title: string;
  name: string;
  cwd: string;
  forkedFromSid?: string;
};

export type HookUpdate = {
  sid?: string;
  state?: AgentState;
};

export type Harness = {
  id: HarnessId;
  binary: string;
  historyDir: string;
  historyGlobs: string[];
  isProcess(command: string): boolean;
  hookUpdate(payload: unknown): HookUpdate;
  resume(args: { sid: string; name: string; prompt?: string }): string[];
  fork(args: { sourceSid: string; name: string }): string[];
  sessionGlob(sid: string): string;
  parseHistory(file: string, contents: string): ParsedHistory | undefined;
};
