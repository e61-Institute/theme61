## Describe your changes


## Do the following before requesting a review

- [ ] I have written tests covering functions I have added or changed.
- [ ] I have run tests and they all pass.
- [ ] I have ensured all new functions will show up in the `_pkgdown.yml` file.
- [ ] I have updated `DESCRIPTION` with any new package dependencies and the new package version number.
- [ ] I have updated `NEWS.md` with a brief description of my changes.
- [ ] I have updated the package documentation with `devtools::document()`.
- [ ] I have run `devtools::build_readme()` to update `README.md`.
- [ ] I have updated the package website with `pkgdown::build_site_github_pages()`.

## Post-release

- Create a new Release on Github with a copy of the news/changes.
- Check if Github Actions successfully deployed the package.