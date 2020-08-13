# Contributing Guide

This software package attempts to closely follow modern best-practices for R packages detailed at https://r-pkgs.org. Here we cover basic steps if you'd like to contribute improvements or new functionality.

## GitHub Workflow
We recommend a ["Forking Workflow"](https://www.atlassian.com/git/tutorials/comparing-workflows/forking-workflow) if you’d like to contribute code or documentation to eiCompare !

1. Create a fork of this repository, clone your fork locally this will be your
https://docs.github.com/en/github/getting-started-with-github/fork-a-repo 

```
git clone https://github.com/scottyhq/eiCompare.git
cd eiCompare
```

2. Connect your local repository to ‘upstream’ RPVote/eiCompare:
```
git remote add upstream https://github.com/RPVote/eiCompare.git
# Make sure you have the latest code locally
git pull upstream master
```

3. Create a new feature branch and add modifications
```
git checkout -b new-feature
# Modify files here
# To try out these new features in an R session run `devtools::install_local()`
```

4. Commit, push changes to your fork on GitHub and create a pull request!
https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request
```
git commit -a -m “added new capability to X function”
git push origin
```


## Style convention
eiCompare uses [styler](https://github.com/r-lib/styler) for code style conventions. Using git pre-commit hooks ensures style conventions are followed before committing to GitHub. In your R console just run `precommit::use_precommit() ` to enable, then write R code without worrying about style. When you commit code with git it will be automatically formatted for you:
```
$ git commit -a -m "new function code"
style-files..............................................................Failed
- hook id: style-files
- files were modified by this hook
```

After autoformatting files, you must commit a second time:
```
$ git commit -a -m "new function code"
style-files..............................................................Passed
Check for added large files..............................................Passed
Fix End of Files.........................................................Passed
Don't commit common R artifacts......................(no files to check)Skipped
[styler 1c3e95a] new function code
 2 files changed, 14 insertions(+)
 create mode 100644 R/newfunction.R
```

## Documentation
This package uses [roxygen2](https://github.com/r-lib/roxygen2) for documentation. Functions are documented with inline comments that are automatically turned into .Rd files.

## Testing
This package uses [testthat](https://github.com/r-lib/testthat) "so that you get a visceral satisfaction from writing tests". Please do include tests for new code!


## Continuous integration
This project uses [GitHub Actions](https://docs.github.com/en/actions) for continuous integration. Workflows are adapted from https://github.com/r-lib/actions.

Workflows are run for every commit unless the commit message starts with '[skip-ci]'. For example `git commit -a -m "[skip-ci] added section to readme"`. 
