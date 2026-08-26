## Describe your changes


## Do the following before requesting a review

### For feature branches

- [ ] I have written tests covering functions I have added or changed.
- [ ] I have run tests and they all pass.
- [ ] I have ensured all new functions show up in the `_pkgdown.yml` file.
- [ ] I have updated the package documentation with `devtools::document()`.
- [ ] I have updated `DESCRIPTION` with any new package dependencies.

### Additional steps for the last PR into dev prior to releasing a new version from dev to main

- [ ] I have updated `DESCRIPTION` with the new package version number.
- [ ] I have updated `NEWS.md` with a brief description of my changes.
- [ ] I have ensured the `_pkgdown.yml` file is correctly built by running `pkgdown::check_pkgdown()`.
- [ ] I have run `devtools::build_readme()` to update `README.md`.
- [ ] I have updated the package website with `pkgdown::build_site_github_pages()`.
