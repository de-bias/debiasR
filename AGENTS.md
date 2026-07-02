# debiasR Codex Instructions

Purpose:
- keep project-specific guidance close to the repository
- reduce repeated setup and task-board discovery work
- protect in-progress migration and documentation edits

Project scope:
- `debiasR` is an R package for origin-destination mobility bias correction and validation.
- The repository is public on GitHub as of 2026-06-04. Treat tracked files,
  docs, vignettes, workflows, issues, and pull requests as public-facing.
- Stable deterministic helpers use the `adjust_*` and `validate_flow_*` naming pattern.
- `adjust_multilevel_bayes()` is an experimental stage-1 prototype unless the task board says otherwise.

Before substantial work:
- Read `notes/project-management/TASK_BOARD.md` and `notes/project-management/STATUS.md`.
- Check `git status --short` and avoid overwriting unrelated modified or untracked files.
- If the task touches validation, also check relevant notes in `notes/project-management/`.

Git and GitHub controls:
- All contributors and automation agents must work on a branch and open a pull
  request into `main`.
- Direct pushes to `main` are not part of the project workflow, including for
  maintainers and automation agents.
- Francisco Rowe (`fcorowe`) and Carmen Cabrera (`carmen-cabrera`) can review
  and merge accepted pull requests.
- If a direct push to `main` appears necessary, stop and ask Francisco to
  approve a PR-based route instead.

Coding defaults:
- Prefer existing package patterns in `R/`, `tests/testthat/`, and roxygen documentation.
- Keep patches narrow and traceable.
- Add or update focused tests for new exported behavior.
- Update `NAMESPACE` and generated `man/` docs when exports or roxygen docs change.
- Do not commit confidential material, credentials, personal local paths beyond
  necessary reproducibility notes, restricted raw data, or development-only
  artifacts that are not appropriate for a public repository.

Validation:
- Use the existing `validate_flow_*` API style.
- Keep validation functions deterministic and tidy-output friendly.
- Document metric interpretation clearly, especially sign conventions and scale.

Testing:
- Prefer the curated fast deterministic test runner when validating broad changes:
  `Rscript scripts/run_fast_tests.R`
- For narrow validation changes, targeted `testthat` runs are acceptable before the full fast tier.

Documentation:
- Keep README, NEWS, status notes, and task board synchronized when user-facing scope changes.
- After changes that affect exported package behavior, public function
  interfaces, vignettes, README guidance, validation interpretation,
  dependency/data requirements, or methodological status decisions, update the
  project notes and software versioning before the change is considered done.
  At minimum, check whether `DESCRIPTION`, `NEWS.md`,
  `notes/project-management/STATUS.md`, and
  `notes/project-management/TASK_BOARD.md` need synchronized updates. Small
  typo fixes, hidden refactors, and test-only edits do not need a version bump
  unless they change user-facing behavior or project status.
- Keep adding release/development notes to `NEWS.md`, but do not display the
  changelog in pkgdown previews or deployed vignette sites unless Francisco
  explicitly asks for it.
- Treat vignettes as user-facing teaching material. Hide setup, helper-loading,
  cached-artifact, and other plumbing chunks unless the code is something users
  should learn from or run directly; show the relevant outputs and interpretive
  text instead.
- For vignettes, match the explanatory style and Aim-section structure of
  nearby vignettes before introducing a new structure.
- Write vignette prose for undergraduate quantitative social scientists,
  students, and policy readers. Prefer familiar, self-explanatory language over
  internal software terms such as contract, artefact, artifact, plumbing,
  cached output, precomputed output, or fast example.
- When a vignette illustrates a `debiasR` function, show outputs using the
  exact column, argument, and attribute names returned by the function.
- When a vignette claims to illustrate empirical use, use the relevant real
  `debiasRdata` or `debiasR_example_data()` route rather than simulated
  examples, unless the vignette is explicitly about simulation.
- Do not treat older migration notes as the current source of truth when `STATUS.md` or `TASK_BOARD.md` disagree.
- When asked to preview vignettes, default to a pkgdown-style preview rather than
  standalone `quarto preview` output. Build the site to a temporary directory
  such as `/private/tmp/debiasr-pkgdown-preview` with
  `trap 'mv /private/tmp/debiasr-NEWS-hidden.md NEWS.md 2>/dev/null || true' EXIT; mv NEWS.md /private/tmp/debiasr-NEWS-hidden.md; RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 Rscript -e "pkgdown::build_site(pkg = '.', override = list(destination = '/private/tmp/debiasr-pkgdown-preview'), new_process = FALSE, install = TRUE)"`,
  serve that directory locally, and open the relevant `articles/*.html` page
  (for example `/articles/v06-adjusting-biases.html`). Use standalone Quarto
  preview only if explicitly requested.
