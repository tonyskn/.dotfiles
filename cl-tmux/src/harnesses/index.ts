import type { HarnessId } from "../model";
import { claude } from "./claude";
import { codex } from "./codex";
import type { Harness as HarnessAdapter } from "./types";

const HARNESSES: Record<HarnessId, HarnessAdapter> = { claude, codex };

export const Harness = {
  isId(value: string): value is HarnessId {
    return Object.hasOwn(HARNESSES, value);
  },

  get(id: HarnessId): HarnessAdapter {
    return HARNESSES[id];
  },

  all(): HarnessAdapter[] {
    return Object.values(HARNESSES);
  },
};
