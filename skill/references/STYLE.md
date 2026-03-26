# First-Principles KDL Style Guide

Use this reference when designing a new KDL surface, canonicalizing an existing one, or reviewing whether a config language feels deliberate instead of parser-shaped.

## Core Idea

Design the language, not the parser. A good config should look like it was authored by a human with intent:

- Give each node a clear role.
- Make similar concepts look similar.
- Avoid multiple equal-status shapes for the same information.
- Prefer stable, canonical rendering.

The parser should serve the language.

## Mental Model

Make each node one of these:

- A declaration of a named thing.
- A group of related settings.
- A selector over some domain.
- An action or policy.
- A leaf setting.

Avoid mixing these roles casually inside the same node shape.

## Canonical Rules

### 1. Use positional arguments for identity, not for meaning

Use positional args when they read naturally as the subject of the node:

```kdl
source repo
extract rust
index local
pool code
```

Use keyed properties for operational meaning:

```kdl
vectors local backend=qdrant
embedding default backend=ollama
execution mode=hybrid
```

### 2. Use keyed properties for important semantics

Key anything that answers questions like:

- what kind is it?
- what backend does it use?
- what provider is active?
- what mode is selected?
- what threshold, limit, or timeout applies?
- what path, URL, or model is configured?

### 3. Use child nodes for conceptual grouping

Lift a set of fields into a child node when they:

- share a prefix,
- are configured together,
- form a semantic subsystem, or
- are likely to grow.

#### Shared-prefix rule

If multiple properties share a prefix, lift that prefix into a child node.

Prefer:

```kdl
quality {
    low-signal token-threshold=8 lexical-threshold=4 max-chunks=4
}
```

Over:

```kdl
quality {
    low-signal-token-threshold=8
    low-signal-lexical-threshold=4
    low-signal-max-chunks=4
}
```

### 4. Inline only what truly belongs together

Keep small cohesive settings inline:

```kdl
cli color=always
lsp timeout-ms=8000 max-concurrency=4
chunks max-tokens=1200 overlap-tokens=150 min-tokens=5
fusion method=rrf rrf-k=60
```

Use a block when the concept has subgroups, repeated structured entries, or materially better readability.

### 5. Use repeated args for simple lists

When the data is just a list, use repeated arguments:

```kdl
exclude "**/.git" "**/target"
format rs ts tsx
pass definitions references tests
style keywordify add-paths
```

### 6. Use repeated child nodes for structured repeated entries

When each entry carries its own fields, use repeated nodes:

```kdl
pool code { ... }
pool prose { ... }

collection code name=stag_code_3584
collection prose name=stag_prose_768
```

### 7. Use flags only for switch-like author intent

Flags are good for choices that read naturally as toggles:

```kdl
enabled
disabled
fail-fast
require-citations
refuse-if-no-evidence
mmr-enabled
symbol-exact
graph-walk
```

Prefer explicit keyed values when the field is not naturally a toggle.

### 8. Prefer one canonical way to say something

Pick one canonical form for:

- declarations,
- list values,
- structured repeated entries,
- selectors,
- classification,
- nested groups.

Render that form consistently.

### 9. Keep selectors compact, local, and composable

Keep selection syntax close to what it selects. Prefer self-contained selectors instead of inventing a mini query language too early:

```kdl
path "tokio:**/*.rs"
path "sub-*:**/*.rs"
path "**/*.md"
```

### 10. Separate declaration from applicability

A declaration should define a reusable thing. Applicability should define where it applies.

For example:

- `extract` defines extraction behavior.
- `extract.filter` defines where it applies.
- `index` assembles active sources and extracts.

### 11. Order nodes semantically

Render in the order a human wants to read:

1. identity and scope
2. matching and filtering
3. classification
4. core behavior
5. advanced behavior
6. policy and overrides
7. metadata

### 12. Make rendering stable

Make canonical rendering:

- preserve the intended shape,
- use stable ordering,
- eliminate redundant noise,
- prefer chosen canonical forms,
- look authored instead of dumped.

### 13. Use names for declarations and values for taxonomy

Use declarations for reusable behavior and reusable config objects.

Use canonical values for stable shared vocabulary when that is clearer than expanding into many small fields.

### 14. Keep the file layered

Prefer a top-down order like:

1. global defaults or exclusions
2. vocabulary or taxonomy
3. inputs or sources
4. processing profiles
5. backends or infrastructure
6. composition or assembly
7. runtime behavior

Move from what exists, to how it is assembled, to how it runs.

## Canonical Example

```kdl
exclude
  "**/.git"
  "**/target"
  "**/.stag"

source repo kind=local path=. {
    files
      "**/*"
      "!**/searchindex*.js"
}

extract rust {
    filter {
        path "**/*.rs"
    }

    assign profile="logic/code/code"
    chunks max-tokens=1200 overlap-tokens=150 min-tokens=5
    pass definitions implementations references tests topology
}

embedding default backend=ollama {
    endpoint "http://localhost:11434"

    pool code {
        match kind=code
        model "nomic-embed-code-safe" dim=3584
        budget max-tokens=1200 batch-size=32 timeout="120s"
    }
}

vectors local backend=qdrant {
    url "http://localhost:6334"
    collection code name=stag_code_3584
}

index local {
    source repo
    extract rust
    embedding default
    vectors local
}

search index=local {
    execution mode=hybrid
    fusion method=rrf rrf-k=60

    quality {
        low-signal
          token-threshold=8
          lexical-threshold=4
          max-chunks=4
    }
}
```

## Implementation Rules

Hand these rules to an implementation agent when building or reviewing a KDL language:

1. Use `<node> <name>` for named declarations.
2. Use keyed properties for operational meaning.
3. Use child groups whenever fields share a prefix or are configured together.
4. Keep small leaf settings inline.
5. Use repeated args for simple lists.
6. Use repeated child nodes for structured repeated entries.
7. Put applicability selectors on the thing being applied.
8. Prefer canonical vocabulary values over verbose expanded forms when the vocabulary is stable.
9. Render in semantic order, not parser field order.
10. Emit one canonical form only.
