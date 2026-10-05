import type { AgentState, HarnessId } from "../sessions";

export type HookUpdate = {
  sid?: string;
  state?: AgentState;
  cwd?: string;
  prompt?: string;
};

export type Conversation = {
  fingerprint: string;
  messages: AsyncIterable<string>;
};

export type Harness = {
  id: HarnessId;
  /** Executable used to launch this harness. */
  binary: string;
  /** Accepts the foreground command of a pane running this harness. */
  isProcess(command: string): boolean;
  /** Response required by hooks when there is no pane to update. */
  hookResponse?: string;
  /** Normalizes the harness hook payload into session activity. */
  hookUpdate(payload: unknown): HookUpdate | undefined;
  /** Builds the command for reopening a session. */
  resume(args: {
    sid: string;
    name: string;
    cwd: string;
    prompt?: string;
  }): string[];
  /** Builds the command for forking a session. */
  fork(args: { sourceSid: string; name: string }): string[];
  /** Locates a session and lazily exposes its searchable messages. */
  conversation(sid: string): Promise<Conversation | undefined>;
};
