# Rule Indexing + Perf Measurement Plan

## Goals
- Reduce `compileCall` rule selection from *linear scan of all rules* to a *small candidate set*.
- Preserve rule ordering semantics (first matching rule wins).
- Avoid behavior changes; only speed up rule lookup.

## Plan
### 1) Build a rule index (name → ordered rules)
**Why:** Most calls only need rules for a single method name. Grouping by name avoids scanning unrelated rules.

**Key constraints:**
- Rule order must be preserved exactly (the first rule in the original list that matches should still win).

**Design**
- Build a `RuleIndex` once in `RuleEngine` from the `List[CallRule]`.
- `RuleIndex` contains:
  - `byName: Map[String, Vector[CallRule]]` for quick lookup
  - `anyName: Vector[CallRule]` for fallback (rules using `AnyName`)
- While building, append rules in **original order** to every relevant bucket so order is preserved.
  - For `NameIs` / `NameIn`: append to those name buckets.
  - For `AnyName`: append to **all** known name buckets and the `anyName` list.

**Call path change**
- `compileCall` now uses `index.candidates(call.name)` instead of scanning all rules.
- The candidate list is still filtered by `CallMatcher` (recv + arity), but on a much smaller set.

### 2) Optional next-level indexing (future)
If needed, further reduce candidates by indexing on:
- receiver fullName (`TypeRecv` / `UnionRecv`) and
- arity class (`A0`/`A1`/…)

This is not required for the first perf win and can be layered later without changing semantics.

### 3) Tests / correctness
- Run full `sbt test` to confirm no behavioral drift.
- Add a small debug check (optional) to compare rule counts per call before/after indexing if needed.

## Performance Measurement
We can measure compile-time impact with simple, repeatable shell timing (no extra tooling needed):

1) Baseline compile (main branch):
```
(time sbt "clean; comptimeJVM/compile")
(time sbt "clean; neotypeJVM/test")
```

2) Same commands on this branch.

3) Compare median of 3 runs (first run often includes extra JVM warm-up).

If we need more detail later, we can add a tiny perf harness that compiles a macro-heavy test file repeatedly.

## Success Criteria
- Tests still green.
- For macro-heavy compile steps, rule selection should be measurably faster (even if modest).
- No change in error messages or rule match ordering.
