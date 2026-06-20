//! Invalid-case fixtures for the auto-`KdlPartial` generator: each proves the
//! generator yields a *descriptive compile error* rather than silently wrong
//! merge semantics (kdl-config plan, Change 3 negative stopping criterion).
//!
//! Covered here (stable `syn::Error` diagnostics): registry-as-`Vec`, `modifier`.
//! The "nested type missing `#[derive(KdlPartial)]`" case is the standard rustc
//! `cannot find type \`<T>Partial\`` error (documented in the README); it is not
//! stderr-locked here because rustc's surrounding suggestions vary by toolchain.

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/partial_*.rs");
}
