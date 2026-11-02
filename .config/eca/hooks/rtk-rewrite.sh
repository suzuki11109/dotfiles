#!/usr/bin/env bash

# ECA preToolCall adapter for RTK.
# Preserve ECA's own approval policy.

input="$(cat)" || exit 0

command="$(jq -r '.tool_input.command // empty' <<<"$input")"
[[ -n "$command" ]] || exit 0

# Resolve RTK explicitly because ECA may have a different PATH.
RTK_BIN="$(command -v rtk 2>/dev/null || true)"

if [[ -z "$RTK_BIN" ]]; then
  for candidate in /opt/homebrew/bin/rtk /usr/local/bin/rtk; do
    if [[ -x "$candidate" ]]; then
      RTK_BIN="$candidate"
      break
    fi
  done
fi

[[ -n "$RTK_BIN" ]] || exit 0

# Capture output and exit status separately. Exit 3 can be a valid rewrite.
rewritten="$("$RTK_BIN" rewrite "$command" 2>/dev/null)"
rc=$?

# Only accept the documented rewrite outcomes.
case "$rc" in
  0|3) ;;
  *) exit 0 ;;
esac

[[ -n "$rewritten" && "$rewritten" != "$command" ]] || exit 0

# ECA merges updatedInput into the original tool arguments.
# Do not override ECA's approval decision.
jq -cn --arg command "$rewritten" \
  '{updatedInput: {command: $command}}'
