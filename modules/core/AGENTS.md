# Neotype Core (modules/core) Agent Notes

- Core macros live in `modules/core/shared/src/main/scala/neotype/Macros.scala` and delegate to `comptime`.
- Do not re‑introduce the old `neotype.eval` implementation; comptime is the supported path.
- Update tests under `modules/core/shared/src/test/scala/neotype` when behavior changes.
- Keep error message changes in sync with `modules/core/shared/src/main/scala/neotype/ErrorMessages.scala`.
