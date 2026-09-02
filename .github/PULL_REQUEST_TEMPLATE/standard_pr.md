## Describe your changes


## Do the following before requesting a review

### For feature branches

- [ ] I have written tests covering functions I have added or changed.
- [ ] I have run tests and they all pass.
- [ ] If a graph's rendered output changed, I updated its svg snapshot from a Linux run, not a local Mac/Windows one (see note below).
- [ ] I have ensured all new functions show up in the `_pkgdown.yml` file.
- [ ] I have updated the package documentation with `devtools::document()`.
- [ ] I have updated `DESCRIPTION` with any new package dependencies.

<details>
<summary>Updating an svg snapshot? Read this first.</summary>

The svg snapshot tests (`test-sample-graphs.R`, `test-label_wrapping.R`,
`geom_pointbar visual test`) compare exact rendered glyph output, which
differs by OS even with the same embedded font. The repo's committed
snapshots are Linux-rendered, since that's what CI checks against on every
PR (macOS/Windows CI runners skip these specific tests rather than fail on
an unfixable font mismatch).

If your change intentionally alters a graph's appearance:
1. Push your branch and let the Linux `R-CMD-check.yaml` job run (it will
   fail on the changed snapshot(s), as expected).
2. Pull the new, correctly-rendered snapshots from that run — either
   download its `*-testthat-snapshots` artifact from the Actions tab, or
   run `testthat::snapshot_download_gh("e61-Institute/theme61", "<run-id>")`.
3. Run `testthat::snapshot_accept()`, review the diff, and commit.

Don't run `snapshot_accept()` from a local Mac/Windows R session and commit
the result — it'll write a Windows/Mac-rendered snapshot that then fails
Linux CI (and every other Linux run) for reasons unrelated to your change.
</details>

### Additional steps for the last PR into dev prior to releasing a new version from dev to main

- [ ] I have updated `DESCRIPTION` with the new package version number.
- [ ] I have updated `NEWS.md` with a brief description of my changes.
- [ ] I have ensured the `_pkgdown.yml` file is correctly built by running `pkgdown::check_pkgdown()`.
- [ ] I have run `devtools::build_readme()` to update `README.md`.
- [ ] I have updated the package website with `pkgdown::build_site_github_pages()`.
