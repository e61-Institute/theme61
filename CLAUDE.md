# Instructions for Claude

- Comments: keep them concise and succinct - a sentence or two, not a
  paragraph. Only comment on non-obvious WHY (a hidden constraint, a subtle
  invariant, a workaround for a specific bug) - never WHAT the code does.
- Agent allocation: before spawning multiple agents to work on separate
  tasks (e.g. issues in a milestone), present the proposed model/task
  allocation - which agent gets which task and which model/effort level -
  and wait for explicit approval before launching any of them.
- Running tests: don't run the full test suite for comment or documentation
  changes. Prioritise running only the tests affected by the code actually
  changed. Only run the full suite when a major change has been made, or a
  significant task has reached completion.
