# Arborist Implementation Plan

## Overview

Implement "load actual tree" for the `/apmg/cl_apm_arborist` class. This builds a dependency tree of all installed packages in the apm system.

Unlike npm which has a project-local tree, **apm has a single global tree** containing all packages.

## References

- https://www.npmjs.com/package/@npmcli/arborist
- https://github.com/npm/cli/tree/latest/workspaces/arborist

## Implementation Steps

### Step 1: Extend Arborist Interface
Add types and constants needed for the tree structure.

- Add `ty_tree` structure to hold the complete tree
- Add logging/warning table type for issues

### Step 2: Implement Node Class
Enhance `#apmg#cl_apm_arborist_node` with:

- Factory method to create nodes from package.json
- Constructor accepting manifest data
- Getter/setter methods for node properties
- Method to add edges (dependencies)
- Method to check if a dependency is satisfied
- Static tree storage (global tree)

### Step 3: Implement Edge Class
Enhance `#apmg#cl_apm_arborist_edge`:

- Complete the constructor to resolve the `to` node
- Validate if edge satisfies the spec using semver
- Track error states (MISSING, INVALID, etc.)

### Step 4: Implement load_actual_tree
In `#apmg#cl_apm_arborist`:

1. Get all installed packages via `/apmg/cl_apm_package_json=>list()`
2. For each installed package:
   - Get package.json via pacote or local storage
   - Create a node for the package
   - Add to global tree
3. For each node, process dependencies:
   - Create edges for each dependency type (prod, dev, optional, peer)
   - Link edge to target node if installed
   - Mark uninstalled dependencies
4. Recursively process all dependencies
5. Detect and log circular dependencies
6. Return the complete tree

### Step 5: Add Logging/Error Tracking
- Track circular dependencies
- Track missing dependencies
- Track version mismatches
- Store warnings/errors in tree structure

## Key Differences from npm

| npm | apm |
|-----|-----|
| Project-local node_modules | Global system-wide packages |
| Deep nesting possible | Flat structure |
| Multiple versions of same package | One version per package |
| File-system based | Database persistence |
| package-lock.json | Future: package-lock.abap.json |

## Classes Modified

1. `/apmg/if_apm_arborist` - Add tree types
2. `/apmg/cl_apm_arborist_node` - Full implementation
3. `/apmg/cl_apm_arborist_edge` - Complete edge resolution
4. `/apmg/cl_apm_arborist` - Implement load_actual_tree

## Commit Plan

1. Step 1-2: Interface and Node class updates
2. Step 3: Edge class implementation
3. Step 4-5: load_actual_tree implementation
