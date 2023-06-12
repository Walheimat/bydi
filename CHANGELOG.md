# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- Macro `bydi-match-expansion` now has its own `ert-explainer` to show
  the mismatch between actual and expected expansion.

## [v0.2.0]

### Added

- Self-referential test suite.
- Command `bydi-calculate-coverage` to that using the text report file.
- Macro `bydi-with-mock` is now also aliased by just `bydi`.
- `bydi` now accepts `plist`s of a certain shape. You can use `(:mock
  fun :return value)` or `(:mock fun :with other-fun)`.
- `bydi` now accepts shorthand `(:always fun)` and `(:ignore fun)` to
  mock a function with `ignore` or `always` respectively.
- `bydi` now accepts shorthand `(:sometimes fun)` that will have a
  function return `t` by default or `nil` if `(bydi-toggle-sometimes)`
  was called.
- `bydi-was-{not}-called-{n-times, with}` now have an explainer
  function.
- `bydi` now accepts `(:spy fun)` that allows spying on a function
  without replacing it so it can be used in conjunction with
  `bydi-was-{not}-called-{n-times, with}`.
- `bydi` will now issue warnings if a function belonging to
  `bydi--never-mock` is being mocked.

### Changed

- Source code was refactored to group functions (sometimes renaming them).

## [v0.1.0]

Initial version as an extraction of my config package.
