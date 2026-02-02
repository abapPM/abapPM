Plan for load_actual_tree

- Survey arborist/pacote/package-json APIs and existing data structures.
- Define tree-building flow: installed packages -> pacote manifests -> nodes/edges.
- Add node/edge helpers to store nodes, edges, errors, and validation.
- Implement load_actual_tree with recursion, dep resolution, and loop guard.
- Verify with lints and commit in logical steps.
