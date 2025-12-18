# Filter state snapshot management

Capture and restore snapshots of the global (app) filter state.

## Usage

``` r
ui_snapshot_manager_panel(id)

srv_snapshot_manager_panel(id, slices_global)

ui_snapshot_manager(id)

srv_snapshot_manager(id, slices_global)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- slices_global:

  (`reactiveVal`) that contains a `teal_slices` object containing all
  `teal_slice`s existing in the app, both active and inactive.

## Value

`list` containing the snapshot history, where each element is an
unlisted `teal_slices` object.

## Details

This module introduces snapshots: stored descriptions of the filter
state of the entire application. Snapshots allow the user to save the
current filter state of the application for later use in the session, as
well as to save it to file in order to share it with an app developer or
other users, who in turn can upload it to their own session.

The snapshot manager is accessed with the camera icon in the tabset bar.
At the beginning of a session it presents three icons: a camera, an
upload, and an circular arrow. Clicking the camera captures a snapshot,
clicking the upload adds a snapshot from a file and applies the filter
states therein, and clicking the arrow resets initial application state.
As snapshots are added, they will show up as rows in a table and each
will have a select button and a save button.

## Server logic

Snapshots are basically `teal_slices` objects, however, since each
module is served by a separate instance of `FilteredData` and these
objects require shared state, `teal_slice` is a `reactiveVal` so
`teal_slices` cannot be stored as is. Therefore, `teal_slices` are
reversibly converted to a list of lists representation (attributes are
maintained).

Snapshots are stored in a `reactiveVal` as a named list. The first
snapshot is the initial state of the application and the user can add a
snapshot whenever they see fit.

For every snapshot except the initial one, a piece of UI is generated
that contains the snapshot name, a select button to restore that
snapshot, and a save button to save it to a file. The initial snapshot
is restored by a separate "reset" button. It cannot be saved directly
but a user is welcome to capture the initial state as a snapshot and
save that.

## Snapshot mechanics

When a snapshot is captured, the user is prompted to name it. Names are
displayed as is but since they are used to create button ids, under the
hood they are converted to syntactically valid strings. New snapshot
names are validated so that their valid versions are unique. Leading and
trailing white space is trimmed.

The module can read the global state of the application from
`slices_global` and `mapping_matrix`. The former provides a list of all
existing `teal_slice`s and the latter says which slice is active in
which module. Once a name has been accepted, `slices_global` is
converted to a list of lists - a snapshot. The snapshot contains the
`mapping` attribute of the initial application state (or one that has
been restored), which may not reflect the current one, so
`mapping_matrix` is transformed to obtain the current mapping, i.e. a
list that, when passed to the `mapping` argument of
[`teal_slices()`](https://insightsengineering.github.io/teal/reference/teal_slices.md),
would result in the current mapping. This is substituted as the
snapshot's `mapping` attribute and the snapshot is added to the snapshot
list.

To restore app state, a snapshot is retrieved from storage and rebuilt
into a `teal_slices` object. Then state of all `FilteredData` objects
(provided in `datasets`) is cleared and set anew according to the
`mapping` attribute of the snapshot. The snapshot is then set as the
current content of `slices_global`.

To save a snapshot, the snapshot is retrieved and reassembled just like
for restoring, and then saved to file with
[`slices_store()`](https://insightsengineering.github.io/teal/reference/slices_store.md).

When a snapshot is uploaded, it will first be added to storage just like
a newly created one, and then used to restore app state much like a
snapshot taken from storage. Upon clicking the upload icon the user will
be prompted for a file to upload and may choose to name the new
snapshot. The name defaults to the name of the file (the extension is
dropped) and normal naming rules apply. Loading the file yields a
`teal_slices` object, which is disassembled for storage and used
directly for restoring app state.

## Transferring snapshots

Snapshots uploaded from disk should only be used in the same application
they come from, *i.e.* an application that uses the same data and the
same modules. To ensure this is the case, `init` stamps `teal_slices`
with an app id that is stored in the `app_id` attribute of a
`teal_slices` object. When a snapshot is restored from file, its
`app_id` is compared to that of the current app state and only if the
match is the snapshot admitted to the session.

## Bookmarks

An `onBookmark` callback creates a snapshot of the current filter state.
This is done on the app session, not the module session. (The snapshot
will be retrieved by `module_teal` in order to set initial app state in
a restored app.) Then that snapshot, and the previous snapshot history
are dumped into the `values.rds` file in `<bookmark_dir>`.

## Author

Aleksander Chlebowski
