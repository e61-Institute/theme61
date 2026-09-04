#!/bin/bash
# Blocks any `git push` (in any form) that would push to the protected
# dev/main branches. Deterministic guard for CLAUDE.md's "never push
# directly to dev or main" rule - see #392 postmortem.
cmd=$(jq -r '.tool_input.command // empty')

# Strip heredoc bodies and quoted string contents before scanning, so
# literal text like a commit message that merely *mentions* "git push"
# (e.g. `git commit -m "...git push origin dev..."`) isn't mistaken for
# an actual push invocation.
cleaned=$(printf '%s' "$cmd" | perl -0777 -pe '
  s/<<-?([\x27"]?)(\w+)\1.*?\n\2\n/ /gs;
  s/\x27(?:[^\x27\\]|\\.)*\x27/ /gs;
  s/"(?:[^"\\]|\\.)*"/ /gs;
')

seg=$(printf '%s' "$cleaned" | grep -oE 'git[[:space:]]+push[^;&|]*' | head -1)
if [ -n "$seg" ]; then
  rest=${seg#*push}
  blocked=""
  nonflag_count=0
  last_nonflag=""
  for a in $rest; do
    case "$a" in
      -*) ;;
      *) nonflag_count=$((nonflag_count+1)); last_nonflag="$a" ;;
    esac
  done
  if [ "$nonflag_count" -ge 2 ]; then
    check="$last_nonflag"
  else
    check=""
  fi
  if [ -n "$check" ]; then
    for part in $(printf '%s' "$check" | tr '/:' '  '); do
      if [ "$part" = "dev" ] || [ "$part" = "main" ]; then blocked="$part"; fi
    done
  else
    cur=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [ "$cur" = "dev" ] || [ "$cur" = "main" ]; then blocked="$cur"; fi
  fi
  if [ -n "$blocked" ]; then
    printf '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"Blocked: direct push to protected branch '"'"'%s'"'"' is not allowed (CLAUDE.md: never push directly to dev or main, even if the credentials would permit bypassing branch protection). Create a feature branch and open a pull request instead."}}' "$blocked"
    exit 0
  fi
fi
exit 0
