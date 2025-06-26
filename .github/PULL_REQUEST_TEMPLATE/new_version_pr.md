**This pull request template is for new releases that involve merging changes from dev into main.**

## Check the following before requesting a review
Each of these steps should have been checked in the final pull request into dev _before_ this pull request.

- [ ] All the tests passed.
- [ ] All new functions are listed in the `_pkgdown.yml` file and `pkgdown::check_pkgdown()` does not error out.
- [ ] `DESCRIPTION` contains all new package dependencies and the package version number has been updated.
- [ ] `NEWS.md` is updated with a brief description of all changes.
- [ ] The package documentation was updated by running `devtools::document()`.
- [ ] `README.md` is updated by running `devtools::build_readme()`.
- [ ] The package website is updated by running `pkgdown::build_site_github_pages()`.

## Post-release

- Check if Github Actions successfully deployed the package by looking for the green tick in the latest workflow run [here](https://github.com/e61-Institute/theme61/actions).
- Check if Github Actions successfully deployed the [new website](https://e61-institute.github.io/theme61/) (look at the version number).

### Create a new Release on Github with a copy of the news/changes.

1. Go here: https://github.com/e61-Institute/theme61/releases
2. Click the "Draft new release" button and follow the instructions in the image below.

![image](https://github.com/user-attachments/assets/f29d55c6-0453-42ae-b5b4-2b8f513e1d4e)
