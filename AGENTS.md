# Repository Agent Notes

- Prefer `sbt` for build and test tasks (e.g. `sbt test`, `sbt "coreJVM/compile"`).
- Prefer `bun` for JS tooling when needed, unless a module explicitly uses something else.
- The compile‑time evaluator lives in `modules/comptime`; avoid re‑introducing the old `eval`/`eval2` codepaths.
- Update `modules/comptime/SUPPORTED.md` when adding or removing comptime features.
- Keep rule changes test‑backed; add/adjust tests under `modules/comptime/shared/src/test/scala/comptime`.
- Performance changes should be gated by measurement; see `modules/comptime/PERF_PLAN.md`.
