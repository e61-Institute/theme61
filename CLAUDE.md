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
- Installing dependencies: don't run
  `devtools::install_deps(dependencies = TRUE)` by default - it pulls in the
  entire `Suggests` list (e.g. `sf`/`ggmap`, whose spatial dependency chain
  is a very slow C++ compile) even when the change at hand doesn't touch
  spatial/mapping code. Use the default `devtools::install_deps()`
  (Imports/LinkingTo + testthat/withr) for a targeted fix, and only pull in
  the full `Suggests` set when a change actually needs it or a full-suite
  run is genuinely warranted per the rule above.
- Protected branches: never push directly to `dev` or `main`, even if the
  connected GitHub credentials would technically permit bypassing the
  pull-request-required ruleset on those branches. Always commit to a
  separate branch and open a PR instead, exactly as if the bypass were not
  available.
