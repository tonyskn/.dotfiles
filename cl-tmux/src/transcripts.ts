import * as Harness from "./harnesses";
import * as Store from "./store";
import { SessionRef } from "./sessions";

export async function hasTranscript(ref: SessionRef): Promise<boolean> {
  return (await Harness.get(ref.harness).conversation(ref.sid)) !== undefined;
}

async function refreshIndex(refs: ReadonlyArray<SessionRef>): Promise<void> {
  const cached = Store.cachedConversations();
  const current = SessionRef.index(refs);
  const conversations = await Promise.all(
    [...current.values()].map(async (ref) => ({
      ref,
      conversation: await Harness.get(ref.harness).conversation(ref.sid),
    })),
  );

  for (const { ref, conversation } of conversations) {
    const previous = cached.get(SessionRef.key(ref));
    if (!conversation) {
      if (previous) Store.removeConversation(ref);
      continue;
    }
    if (
      previous?.fingerprint === conversation.fingerprint &&
      previous.index_version === Store.SEARCH_INDEX_VERSION
    )
      continue;

    Store.replaceConversation(
      ref,
      conversation.fingerprint,
      await Array.fromAsync(conversation.messages),
    );
  }

  for (const [key, previous] of cached) {
    if (!current.has(key)) Store.removeConversation(previous);
  }
}

// Refresh changed indexed sessions, then rank by the best matching message.
export async function search(
  query: string,
  refs: ReadonlyArray<SessionRef>,
): Promise<Array<SessionRef & { score: number }>> {
  if (!query.trim()) return [];
  await refreshIndex(refs);
  return Store.searchConversations(query);
}
