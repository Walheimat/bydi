# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Self-referential test suite.
- Command `bydi-calculate-coverage` to that using the text report file.
- Macro `bydi-with-mock` is now also aliased by just `bydi`.
- `bydi` now accepts `plist`s of a certain shape. You can use `(:mock
  fun :return value)` or `(:mock fun :with other-fun)`.
- `bydi` now accepts shorthand `(:always fun)` and `(:ignore fun)` to
  mock a function with `ignore` or `always` respectively.
- `bydi` now accepts shorthand `(:toggle fun)` that will have a
  function return `t` by default or `nil` if `(bydi-toggle-sometimes)`
  was called.

## [v0.1.0]

Initial version as an extraction of my config package.
