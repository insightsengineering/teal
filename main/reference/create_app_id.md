# Application ID

Creates App ID used to match filter snapshots to application.

## Usage

``` r
create_app_id(data, modules)
```

## Arguments

- data:

  (`teal_data` or `teal_data_module`) as accepted by `init`

- modules:

  (`teal_modules`) object as accepted by `init`

## Value

A single character string.

## Details

Calculate app ID that will be used to stamp filter state snapshots. App
ID is a hash of the app's data and modules. See "transferring snapshots"
section in ?snapshot.
