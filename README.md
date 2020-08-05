# eiCompare
[![R build status](https://github.com/DSSG-eiCompare/eiCompare/workflows/R-CMD-check/badge.svg)](https://github.com/DSSG-eiCompare/eiCompare/actions?workflow=R-CMD-check)
[![Style status](https://github.com/DSSG-eiCompare/eiCompare/workflows/Styler/badge.svg)](https://github.com/DSSG-eiCompare/eiCompare/actions?workflow=Styler)

eiCompare development version

This package helps analysts with Ecological Inference and RxC analysis, to produce plots and tables. 
The goal is to assist people doing voter redistricting work.

## Development

Use branches for new modifications or functionality (change 'newfeature' to something descriptive):
```
git clone https://github.com/DSSG-eiCompare/eiCompare.git
cd eiCompare
# `git pull` to pull new changes from master branch
git checkout -b newfeature
```

eiCompare uses [styler](https://github.com/r-lib/styler) for code style conventions. Using git pre-commit hooks ensures style conventions are followed before committing to GitHub. In your R console just run `precommit::use_precommit() ` to enable, then write R code without worrying about style. When you commit code with git it will be automatically formatted:
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

Now push your changes to github and create a PR from https://github.com/DSSG-eiCompare/eiCompare: 
```
git push --set-upstream origin newfeature 
```

## Continuous integration
This project uses [GitHub Actions](https://docs.github.com/en/actions) for continuous integration. Workflows are run for every commit unless the commit message starts with '[skip-ci]'. For example `git commit -a -m "[skip-ci] added section to readme"`.
