# Contributing Guidelines

Thank you for your interest in contributing to `debiasR`. We welcome code, documentation, examples, tests, methodological ideas, issue reports, and review comments.

This project uses a branch-based workflow in the shared repository. Contributors should normally work from a new branch in `de-bias/debiasR`, not from a personal fork.

## Before You Start

- Make sure you have a GitHub account.
- Ask a maintainer to add you as a collaborator if you do not already have write access.
- Check the issue tracker or project notes before starting larger work.
- Open an issue first for changes that affect public APIs, package structure, data licensing, or statistical method behavior.

## Branch Workflow

1. Clone the shared repository.

```bash
git clone https://github.com/de-bias/debiasR.git
cd debiasR
```

2. Update your local `main` branch.

```bash
git switch main
git pull origin main
```

3. Create a new branch for your change.

Use a short, descriptive branch name. Good examples:

- `docs/contributing-branches`
- `fix/validation-edge-case`
- `feature/stage4-random-effects`
- `codex/package-overview`

```bash
git switch -c docs/my-change
```

4. Make focused changes.

Keep each branch focused on one topic. This makes review easier and reduces merge conflicts.

For R code changes, load the package locally:

```r
devtools::load_all(".")
```

5. Run the relevant checks.

For the fast deterministic tier:

```bash
Rscript scripts/run_fast_tests.R
```

For documentation-only changes, at minimum check that edited links, examples, and file references still make sense.

6. Commit your changes.

Use a clear, concise commit message.

```bash
git status
git add path/to/changed-file
git commit -m "Update branch-based contribution workflow"
```

7. Push your branch to the shared repository.

```bash
git push -u origin docs/my-change
```

8. Open a pull request.

- Use `.github/pull_request_template.md`.
- Target the `main` branch unless a maintainer says otherwise.
- Summarize what changed and why.
- Link related issues, notes, or design discussions.
- Mark the PR as draft if you want early feedback before final review.

9. Respond to review.

- Keep follow-up commits on the same branch.
- Reply to review comments when addressed.
- Ask for clarification if the requested change is unclear.
- Wait for CI and maintainer review before merge.

10. After merge.

- Delete the branch once it is no longer needed.
- Pull the latest `main` before starting your next branch.

```bash
git switch main
git pull origin main
```

## Small Documentation Changes

For small documentation fixes, you may edit directly in GitHub's web interface, but still create a new branch rather than committing directly to `main`.

Examples:

- fixing a typo
- improving a sentence
- updating a broken link
- clarifying a short example

## Larger Changes

Please discuss larger changes before implementation. This includes:

- new adjustment methods
- changes to exported function names
- validation metric design
- Bayesian model behavior
- package data additions
- CRAN or licensing decisions
- workflow or CI changes

For these changes, create or reference a design note in `notes/project-management/` where helpful.

## Testing Expectations

Use the smallest test set that gives useful confidence.

- Documentation-only change: check rendered or linked docs where practical.
- Deterministic R change: run `Rscript scripts/run_fast_tests.R`.
- Bayesian change: run targeted tests locally if optional dependencies are available, and document anything skipped.
- Data or vignette change: check object names, file paths, and any examples that use the changed data.

The current testing notes live in `notes/project-management/TEST_HEALTH.md`.

## Naming Guide

- Use the current exported API in examples and docs: `debiasR_example_data()`, `measure_bias()`, `adjust_*`, `validate_bias_residual_structure()`, `validate_flow_overall()`, `validate_flow_pairs()`, and the other `validate_flow_*` helpers.
- Use `debiasRdata` for user-facing empirical examples and vignettes. The default empirical OD matrices are `msoa_OD_travel2work` for observed MPD flows and `census_msoa_OD_travel2work` for the Census benchmark.
- Treat `simulated_*` data as lightweight test fixtures and compatibility assets, not as the default user-facing vignette data.
- If you need to mention older names for migration context, keep them in `notes/project-management/MIGRATION_MAP.md` rather than in user-facing instructions.
- Prefer current vignette and file naming, for example `adjust-inverse-penetration` and `empirical-methods-walkthrough` rather than legacy `debias-method1` or simulated walkthrough names.

## Pull Request Checklist

Before requesting review, check that:

- the branch is up to date with `main`
- the PR has a clear title and summary
- related issues or notes are linked
- relevant tests or checks are reported
- new exported functions have documentation and tests
- generated documentation is updated when roxygen comments change
- unrelated files are not included

## Contributor Recognition

We use the All Contributors bot to recognise contributions.

After your PR is merged, comment on the issue or PR:

```text
@all-contributors please add @your-username for code, doc
```

Replace `@your-username` and the contribution types as appropriate. See the All Contributors emoji key for available contribution types.

Thank you for helping make `debiasR` clearer, more reliable, and more useful.
