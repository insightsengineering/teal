# Contribution Guidelines

🙏 Thank you for taking the time to contribute!

Your input is deeply valued, whether an issue, a pull request, or even feedback, regardless of size, content or scope.

## Table of contents

[👶 Getting started](#getting-started)

[📔 Code of Conduct](#code-of-conduct)

[🗃 License](#license)

[📜 Issues](#issues)

[🚩 Pull requests](#pull-requests)

[💻 Coding guidelines](#coding-guidelines)

[🏆 Recognition model](#recognition-model)

[❓ Questions](#questions)

## Getting started

Please refer the project [documentation][docs] for a brief introduction. Please also see other [articles][articles] within the project documentation for additional information.

## Code of Conduct

A [Code of Conduct](CODE_OF_CONDUCT.md) governs this project. Participants and contributors are expected to follow the rules outlined therein.

## License

All your contributions will be covered by this project's [license][license].

## Issues

We use GitHub to track issues, feature requests, and bugs. Before submitting a new issue, please check if the issue has already been reported. If the issue already exists, please upvote the existing issue 👍.

For new feature requests, please elaborate on the context and the benefit the feature will have for users, developers, or other relevant personas.

## Pull requests

### Github Flow

This repository uses the [Github Flow](https://docs.github.com/en/get-started/quickstart/github-flow) model for collaboration. To submit a pull request:

1. Create a branch

   Please see the [branch naming convention](#branch-naming-convention) below. If you don't have write access to this repository, please fork it.

2. Make changes

    Make sure your code
    * passes all checks imposed by GitHub Actions
    * is well documented
    * is well tested with unit tests sufficiently covering the changes introduced

3. Create a pull request (PR)

   In the pull request description, please link the relevant issue (if any), provide a detailed description of the change, and include any assumptions.

4. Address review comments, if any

5. Post approval

   Merge your PR if you have write access. Otherwise, the reviewer will merge the PR on your behalf.

6. Pat yourself on the back

   Congratulations! 🎉
   You are now an official contributor to this project! We are grateful for your contribution.

### Branch naming convention

Suppose your changes are related to a current issue in the current project; please name your branch as follows: `<issue_id>_<short_description>`. Please use underscore (`_`) as a delimiter for word separation. For example, `420_fix_ui_bug` would be a suitable branch name if your change is resolving and UI-related bug reported in issue number `420` in the current project.

If your change affects multiple repositories, please name your branches as follows: `<issue_id>_<issue_repo>_<short description>`. For example, `69_awesomeproject_fix_spelling_error` would reference issue `69` reported in project `awesomeproject` and aims to resolve one or more spelling errors in multiple (likely related) repositories.

### `monorepo` and `staged.dependencies`

Sometimes you might need to change upstream dependent package(s) to be able to submit a meaningful change. We are using [`staged.dependencies`](https://github.com/openpharma/staged.dependencies) functionality to simulate a `monorepo` behavior. The dependency configuration is already specified in this project's `staged_dependencies.yaml` file. You need to name the feature branches appropriately. _This is the only exception from the branch naming convention described above_.

Please refer to the [staged.dependencies package documentation](https://openpharma.github.io/staged.dependencies/) for more details.

## Coding guidelines

This repository follows some unified processes and standards adopted by its maintainers to ensure software development is carried out consistently within teams and cohesively across other repositories.

### Style guide

This repository follows the standard [`tidyverse` style guide](https://style.tidyverse.org/) and uses [`lintr`](https://github.com/r-lib/lintr) for lint checks. Customized lint configurations are available in this repository's `.lintr` file.

### Dependency management

Lightweight is the right weight. This repository follows [tinyverse](https://www.tinyverse.org/) recommedations of limiting dependencies to minimum.

### Dependency version management

If the code is not compatible with all (!) historical versions of a given dependenct package, it is required to specify minimal version in the `DESCRIPTION` file. In particular: if the development version requires (imports) the development version of another package - it is required to put `abc (>= 1.2.3.9000)`.

### Recommended development environment & tools

#### R & package versions

We continuously test our packages against the newest R version along with the most recent dependencies from CRAN and BioConductor. We recommend that your working environment is also set up in the same way. You can find the details about the R version and packages used in the `R CMD check` GitHub Action execution log - there is a step that prints out the R `sessionInfo()`.

If you discover bugs on older R versions or with an older set of dependencies, please create the relevant bug reports.

#### `pre-commit`

We highly recommend that you use the [`pre-commit`](https://pre-commit.com/) tool combined with [`R hooks for pre-commit`](https://github.com/lorenzwalthert/precommit) to execute some of the checks before committing and pushing your changes.

Pre-commit hooks are already available in this repository's `.pre-commit-config.yaml` file.

## Recognition model

As mentioned previously, all contributions are deeply valued and appreciated. While all contribution data is available as part of the [repository insights][insights], to recognize a _significant_ contribution and hence add the contributor to the package authors list, the following rules are enforced:

* Minimum 5% of lines of code authored* (determined by `git blame` query) OR
* Being at the top 5 contributors in terms of number of commits OR lines added OR lines removed*

*Excluding auto-generated code, including but not limited to `roxygen` comments or `renv.lock` files.

The package maintainer also reserves the right to adjust the criteria to recognize contributions.

## Questions

If you have further questions regarding the contribution guidelines, please contact the package/repository maintainer.

<!-- urls -->
[docs]: https://insightsengineering.github.io/r.pkg.template/index.html
[articles]: https://insightsengineering.github.io/r.pkg.template/main/articles/index.html
[license]: https://insightsengineering.github.io/r.pkg.template/main/LICENSE-text.html
[insights]: https://github.com/insightsengineering/r.pkg.template/pulse
