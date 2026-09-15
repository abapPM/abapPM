# apm Install Flow Implementation Plan

## Scope

This plan covers the remaining work in the apm repository after the Arborist changes delivered in commit `868f858`.

Arborist is now responsible for building and validating the actual tree, ideal tree, and dependency diff. This repository remains responsible for:

- collecting the initial install request;
- invoking Arborist;
- presenting an Install Preview dialog;
- collecting a target SAP package for every `ADD` action;
- validating SAP packages, transports, and authorizations;
- passing the confirmed diff to the install command;
- executing every `ADD`, `CHANGE`, and `REMOVE` action; and
- updating apm persistence only after successful SAP object changes.

No further Arborist implementation changes are part of this plan unless integration testing identifies a defect in its published contract.

## Target Flow

```text
Install Package dialog
  |
  | name, exact version, transport, options
  v
Arborist planning
  |
  | actual tree -> ideal tree -> validated diff
  v
Install Preview dialog
  |
  | show ADD / CHANGE / REMOVE
  | collect SAP package for every ADD
  v
Install command preflight
  |
  | validate the complete confirmed plan before mutation
  v
Install command execution
  |
  | install/change dependencies before dependents
  | remove obsolete packages after the new graph is installed
  v
Installed root package page
```

## Confirmed Arborist Contract

The pulled Arborist implementation now provides the contracts the apm flow needs:

- `load_actual_tree` loads current installed state.
- `build_ideal_tree` reuses an explicitly loaded actual tree.
- `get_diff` returns the actual-to-ideal diff.
- `is_executable` reports whether planning produced errors.
- `get_log` returns categorized planning diagnostics.
- `get_changes( root_name )` returns each reachable change once, deepest dependency first, with deterministic name ordering at equal depth.
- Ideal nodes retain the complete exact registry manifest as `ty_manifest`.
- `ideal_node->get_manifest( )-sap_package-default` provides the preview default.
- Actual nodes retain their installed SAP package in `actual_node->package`.

The apm code must consume these contracts instead of reimplementing dependency resolution.

## 1. Define the Confirmed-Plan Data Contract

### Package assignments

Add public types to `/apmg/cl_apm_command_install` or, if preferred for reuse, a small new `/apmg/if_apm_install` interface:

```abap
TYPES:
  BEGIN OF ty_package_assignment,
    name    TYPE /apmg/if_apm_types=>ty_name,
    package TYPE devclass,
  END OF ty_package_assignment,
  ty_package_assignments TYPE HASHED TABLE OF ty_package_assignment
    WITH UNIQUE KEY name.
```

The assignments table contains entries only for `ADD` actions. Existing packages keep the SAP package from their actual Arborist node:

- `ADD`: target SAP package comes from `assignments[ name ]-package`.
- `CHANGE`: target SAP package comes from `diff->get_actual( )->package`.
- `REMOVE`: target SAP package comes from `diff->get_actual( )->package`.

Do not mutate Arborist ideal nodes to store preview selections. The diff must remain an immutable description of registry package changes, while SAP package placement remains explicit consumer input.

### Install command signature

Replace the current single-package contract with a confirmed-plan contract:

```abap
/apmg/cl_apm_command_install=>run(
  registry    = registry
  root_name   = root_name
  diff        = diff
  assignments = assignments
  transport   = transport
  is_force    = is_force
  is_dry_run  = is_dry_run ).
```

Recommended parameter changes:

- Remove `package`; the root package comes from the assignment for `root_name`.
- Remove `package_json`; exact target manifests come from ideal diff nodes.
- Remove `is_production`; it affects Arborist planning, not execution.
- Retain `is_force` only for explicitly bypassable environment/prerequisite checks. It must never override an Arborist planning error or a structurally invalid diff.
- Retain and implement `is_dry_run`.

Only the install dialog currently calls this command, so the API can be changed atomically with that call site.

## 2. Convert the Existing Install Dialog into the Request Step

Update `/apmg/cl_apm_gui_dlg_install` so it gathers a request but does not mutate the system.

### Fields

Keep:

- registry package name;
- exact version;
- transport;
- production/development dependency option, if this is intended to be user-configurable;
- force option, if this is intended to remain user-configurable.

Remove the single SAP package field. The root package is an `ADD` action and must be assigned alongside every newly added dependency in the preview.

If the UI does not expose a production option, explicitly use `is_production = abap_true`. Do not rely on different defaults across layers.

### Initial validation

Before planning, validate only request-level data:

- package name syntax;
- exact semantic version syntax;
- nonempty registry;
- transport format, when supplied; and
- registry login/access.

Do not perform SAP package authorization or transport-required validation yet because the target SAP packages are not known until the preview.

### Planning action

Replace the current direct call to `cl_apm_command_install=>run` with:

```abap
DATA(arborist) = /apmg/cl_apm_arborist=>factory(
  registry                 = registry
  with_bundle_dependencies = abap_false ).

arborist->load_actual_tree( ).

arborist->build_ideal_tree(
  add_packages = VALUE #(
    ( name = params-name version = params-version ) )
  is_production = params-is_production ).

IF arborist->is_executable( ) = abap_false.
  " Convert error diagnostics into a user-facing planning error
ENDIF.

DATA(diff) = arborist->get_diff( ).
DATA(changes) = diff->get_changes( params-name ).
```

Reject the plan when:

- Arborist reports it is not executable;
- the diff is unbound;
- the requested root does not appear as an `ADD`;
- no changes are returned; or
- any change has an invalid action/node combination.

Planning errors should show all Arborist error diagnostics together, including category, package name, version/specification, and message. Warnings should be passed to the preview for display.

### Navigation

Open the Install Preview as the next dialog rather than executing the command.

Preserve enough request data for Back to return to a populated request form. After successful installation, navigation must replace the complete dialog flow with the installed root package page so Back does not return to a stale install form.

Choose one of these implementations and apply it consistently:

1. Keep request and preview as two internal states of one dialog component and re-render between states; or
2. Use a separate preview page and explicitly reconstruct the populated request page on Back.

The preferred implementation is a separate preview component because planning, form validation, and execution remain easier to test and reason about.

## 3. Add the Install Preview Dialog

Create:

- `src/ui/dialogs/#apmg#cl_apm_gui_dlg_inst_prev.clas.abap`
- `src/ui/dialogs/#apmg#cl_apm_gui_dlg_inst_prev.clas.xml`

The XML metadata must follow the repository convention of placing a newline after every closing tag.

### Constructor inputs/state

The preview component should retain:

- normalized registry URL;
- requested root name and version;
- Arborist diff reference;
- ordered changes from `diff->get_changes( root_name )`;
- Arborist warning diagnostics;
- transport;
- force/dry-run options where applicable;
- form and form-data objects; and
- validation log.

Do not recalculate the tree or fetch newer manifests when the user edits SAP package assignments. The displayed preview and executed command must use the same confirmed diff.

### Preview layout

Render three sections.

#### Added packages

Use the existing HTML form table support with columns:

| Column | Behavior |
| --- | --- |
| Action | Readonly, always `ADD` |
| Registry package | Readonly |
| Target version | Readonly, from the ideal node manifest |
| SAP package | Editable and required |

Initialize every SAP package cell from:

```abap
change->get_ideal( )->get_manifest( )-sap_package-default
```

The root package is included in this table and is not treated specially except when choosing the destination page after execution.

#### Changed packages

Readonly columns:

- action;
- registry package;
- installed version;
- target version; and
- existing SAP package.

#### Removed packages

Readonly columns:

- action;
- registry package;
- installed version; and
- existing SAP package.

Also render Arborist warnings above the confirmation controls. Escape every registry-sourced string before rendering it as HTML.

### Form mapping

Keep a stable row-to-package-name mapping in component state. Do not trust readonly posted values as execution data. On submit, read only the editable SAP package cells and join them to the retained ADD nodes by the stored row index.

Normalize SAP package values to uppercase before validation.

### Package selection and creation

At minimum, users must be able to type or accept the default SAP package.

To preserve current functionality, add a `Create Missing SAP Packages` action:

1. Normalize the current form payload.
2. Validate package names and duplicates.
3. For each distinct ADD assignment whose SAP package does not exist, call `/apmg/cl_apm_popup_utils=>create_package` using the entered/default name.
4. Update the form values with the returned names.
5. Re-render the preview without rebuilding the Arborist plan.

Optional row-specific search help can be added later. It is not required for the first complete implementation because users can enter an existing package directly.

### Preview validation

Validate all ADD assignments before calling the command:

- every ADD has exactly one assignment;
- every assigned SAP package is nonempty;
- `/apmg/cl_apm_package_json_vali=>is_valid_sap_package` succeeds;
- `/apmg/cl_apm_auth=>check_package_allowed` succeeds;
- no two added registry packages map to the same SAP package;
- the SAP package is not already registered to another apm package;
- the package contains no existing application objects, matching the existing install safety rule;
- create and change authorization checks succeed; and
- the selected transport satisfies `check_transport_required` for every affected SAP package.

Validate transport requirements for ADD, CHANGE, and REMOVE targets, not only the root package.

The preview Confirm action must call the install command only when the entire validation log is empty.

## 4. Rewrite the Install Command Around Diff Actions

Remove the legacy shallow-dependency implementation from `/apmg/cl_apm_command_install`:

- `ty_action` and `ty_actions`;
- `check_dependencies`;
- `check_dependency`;
- `collect_actions`;
- `check_actions`;
- `take_actions`; and
- recursive calls to `run`.

Arborist is now the sole dependency resolver.

### Normalize changes

At command entry:

```abap
DATA(changes) = diff->get_changes( root_name ).
```

Convert interface references into an internal action table containing:

- action;
- registry package name;
- actual node;
- ideal node;
- actual version;
- target version;
- target SAP package;
- exact target manifest; and
- dependency depth/order index.

Resolve the target SAP package according to action type. Reject duplicate names and malformed action/node combinations.

### Structural action validation

Require:

- `ADD`: actual unbound, ideal bound, exact ideal manifest, assignment present;
- `CHANGE`: actual and ideal bound, equal package names, actual SAP package present, different exact versions;
- `REMOVE`: actual bound, ideal unbound, actual SAP package present.

The command must not accept an initial/unknown action.

### Complete preflight

Run all checks for all actions before performing the first mutation:

- registry authentication;
- assignment completeness and uniqueness;
- SAP package ownership/emptiness for additions;
- create/change/delete authorization as applicable;
- transport requirements for every affected SAP package;
- engine, ABAP release, database, OS, and CPU compatibility for every ADD/CHANGE manifest supported by current environment APIs;
- nonempty exact target versions;
- complete distribution metadata for every ADD/CHANGE;
- tarball availability and integrity, preferably fetched and verified before mutation; and
- confirmation that the root action exists and has a target SAP package.

`is_force` may bypass only checks explicitly classified as warnings or environmental compatibility overrides. It must not bypass missing assignments, authorization, corrupt tarballs, invalid diff structure, or transport requirements.

### Dry run

When `is_dry_run = abap_true`:

- perform normalization and complete preflight;
- do not create/delete SAP objects;
- do not save/delete package JSON;
- do not save/delete README data;
- do not update settings; and
- return or log the actions that would be executed.

The preview itself is not a substitute for correct dry-run semantics in the command API.

## 5. Execute in Dependency-Safe Order

Arborist returns changes deepest dependency first. Split the normalized action table into mutation phases.

### Phase A: ADD and CHANGE

Process `ADD` and `CHANGE` together in Arborist order so dependencies are available before packages that depend on them.

#### ADD

For each `ADD`:

1. Use the exact manifest stored on the ideal node.
2. Install its tarball into the confirmed assigned SAP package.
3. Persist its package JSON and README only after the installer reports success.

#### CHANGE

For each `CHANGE`:

1. Keep the SAP package from the actual node.
2. Replace the installed version with the exact ideal manifest version.
3. Ensure objects removed from the new artifact are actually deleted.
4. Upsert package JSON and README only after replacement succeeds.

The current deserializer ignores objects deleted remotely, so merely deserializing the new tarball over the old package is not a complete `CHANGE`. Implement one of:

- a dedicated `replace_package` operation that removes the old managed content and installs the target artifact; or
- synchronization logic that explicitly deletes old managed objects absent from the new artifact before/after deserialization.

Prefer a dedicated replacement operation initially because its semantics are explicit and testable. Fetch and verify the target artifact before uninstalling the old version.

### Phase B: REMOVE

After the new ideal graph has been installed, remove obsolete packages.

- Reverse/regroup removal ordering when necessary so dependents are removed before dependencies.
- Uninstall SAP objects from the actual node's SAP package.
- Delete package JSON and README only after successful object removal.
- Never use a preview assignment for `REMOVE`.

### Root completion

After all actions succeed:

- obtain the root SAP package from the root ADD assignment;
- display one aggregate success message; and
- navigate to `/apmg/cl_apm_gui_page_package=>create( root_package )`.

Avoid emitting one success popup/message per dependency.

## 6. Make Installer Failures Observable

The low-level `/apmg/cl_apm_installer` currently catches `cx_root`, adds it to a log, and can return normally. That prevents the install command from knowing whether an action failed.

Change the installer boundary so `install`, `uninstall`, and the new replacement operation either:

- raise `/apmg/cx_apm_error` when the log contains a failure; or
- return an explicit result with success flag and diagnostic log which the command must check.

Raising is preferred because the existing public signatures already declare `/apmg/cx_apm_error`.

Requirements:

- always restore transport/tool-message state in cleanup logic;
- preserve the original exception as `previous` where possible;
- do not continue to persistence after a failed object operation; and
- include package name, version, SAP package, and action in the raised error context.

This change applies to:

- `/apmg/cl_apm_installer`;
- `/apmg/cl_apm_command_installer`; and
- command-level ADD/CHANGE/REMOVE handlers.

## 7. Consolidate Manifest Persistence

Create a private install-command helper or reusable service with two explicit operations:

```abap
persist_manifest(
  package  = target_package
  manifest = target_manifest ).

delete_manifest(
  package = actual_package ).
```

### Persist behavior

- Convert `ty_manifest` to `ty_package_json`.
- Store README separately, as the existing init command does.
- Clear README from the package JSON record before saving it.
- Create the record for ADD.
- Replace the record and README for CHANGE.
- Do not use `cl_apm_command_init=>run` for CHANGE because it rejects an already initialized package.

### Delete behavior

- Delete package JSON.
- Delete README.
- Run only after successful SAP object removal.

Keep persistence helpers free of UI messages so the bulk install command can emit one final result.

## 8. Error and Partial-Execution Behavior

ABAP object installation is not fully transactional, so the command must minimize partial state.

Before mutation:

- validate the entire diff;
- resolve all SAP packages;
- fetch every required tarball;
- verify every integrity hash; and
- check all authorizations/transports.

During mutation:

- stop immediately on the first failed action;
- report which actions completed and which action failed;
- do not claim overall success;
- do not persist metadata for the failed action; and
- retain accurate metadata for every action that did complete.

A later enhancement may add compensating rollback, but rollback is not required for the first implementation. The command must nevertheless produce a precise partial-execution error.

## 9. File-Level Change List

### Modify

- `src/ui/dialogs/#apmg#cl_apm_gui_dlg_install.clas.abap`
  - remove root SAP package collection;
  - build the Arborist plan;
  - handle diagnostics;
  - open the preview.
- `src/commands/#apmg#cl_apm_command_install.clas.abap`
  - replace shallow dependency checks with diff execution;
  - define/import assignments;
  - implement preflight, dry run, ordering, persistence, and aggregate results.
- `src/commands/utils/#apmg#cl_apm_command_installer.clas.abap`
  - support prepared artifacts/replacement where needed;
  - propagate failures.
- `src/installer/#apmg#cl_apm_installer.clas.abap`
  - make failures observable;
  - implement or support complete replacement semantics.

### Add

- `src/ui/dialogs/#apmg#cl_apm_gui_dlg_inst_prev.clas.abap`
- `src/ui/dialogs/#apmg#cl_apm_gui_dlg_inst_prev.clas.xml`
- ABAP Unit test include/file for install planning-to-preview mapping.
- ABAP Unit test include/file for command action normalization and ordering.

### Possibly modify

- `src/ui/lib/#apmg#cl_apm_html_form.clas.abap`
  - only if preview table behavior cannot be implemented with existing readonly/editable column support.
- `src/commands/#apmg#cl_apm_command_update.clas.abap`
  - not required for this install change, but it should eventually reuse the same replacement and manifest-persistence helpers.
- UI CSS
  - only if action badges or preview tables need styling not provided by existing dialog/table classes.

Do not edit the imported Arborist classes as part of this implementation.

## 10. Test Plan

### Pure command tests

Introduce a narrow injectable executor around registry artifact access, SAP installation, and persistence so command orchestration can be tested without changing a SAP system.

Test:

- one root ADD;
- root plus multiple transitive ADDs;
- ADD and CHANGE interleaving in dependency order;
- obsolete REMOVE after successful additions/changes;
- malformed action/node combinations;
- missing root assignment;
- missing dependency assignment;
- duplicate SAP package assignments;
- authorization failure during preflight;
- transport-required failure on a dependency package;
- dry run invokes no mutation methods;
- installer failure prevents persistence;
- partial execution reports completed and failed actions; and
- root SAP package is returned correctly.

### Preview tests

Test:

- every ADD produces one editable assignment row;
- root and dependencies use manifest SAP package defaults;
- missing defaults produce required blank inputs;
- CHANGE and REMOVE rows are readonly;
- posted readonly values cannot alter the retained diff;
- form normalization uppercases SAP package names;
- Arborist warnings render safely;
- invalid and duplicate assignments map to useful validation messages;
- Back preserves the original request; and
- Confirm calls the command exactly once with the retained diff.

### SAP integration tests

Exercise at least:

1. A package with no dependencies.
2. A package with two levels of transitive dependencies.
3. A dependency already installed at a compatible version.
4. A dependency requiring a version change.
5. A target version that removes an obsolete dependency.
6. A manifest without `sapPackage.default`.
7. Local packages with no transport and transport-managed packages with a selected request.
8. A failed import demonstrating that package metadata is not falsely saved.

### Static validation

Run:

```text
abaplint
```

The baseline before implementation is zero issues across 456 files.

## 11. Recommended Implementation Sequence

Implement in this order to keep each step reviewable:

1. Define assignment types and the new install-command signature.
2. Add pure diff normalization and structural validation.
3. Add command preflight and dry-run behavior with injected mutation seams.
4. Make installer failures observable.
5. Implement ADD, CHANGE, and REMOVE execution plus manifest persistence.
6. Add the preview component and its unit tests.
7. Change the initial install dialog to invoke Arborist and open the preview.
8. Complete navigation and aggregate success/error reporting.
9. Run ABAP Unit, `abaplint`, and SAP integration scenarios.
10. Only after the install flow is stable, consider refactoring the update command to reuse the new execution helpers.

## Acceptance Criteria

The remaining apm work is complete when:

- submitting the initial dialog performs no SAP object mutation;
- the preview lists every Arborist `ADD`, `CHANGE`, and `REMOVE` action;
- every ADD has an editable SAP package initialized from the exact ideal manifest default;
- invalid, missing, occupied, unauthorized, or duplicate package assignments block execution;
- Arborist planning errors block preview confirmation;
- the exact previewed diff is passed to the command without recalculation;
- additions and changes execute dependency-first;
- obsolete removals occur only after the new graph is installed;
- changed artifacts do not leave objects that were removed from the target version;
- dry run performs no mutations;
- failed object operations do not produce false package metadata or success messages;
- successful actions persist the exact installed manifest and README;
- the final page is the installed root package;
- all new ABAP Unit tests pass;
- `abaplint` reports zero issues; and
- the imported Arborist implementation remains unchanged.
