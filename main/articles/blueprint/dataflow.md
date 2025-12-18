# Data Flow

``` mermaid
%% This is a mermaid diagram, if you see this the plot failed to render. Sorry.
sequenceDiagram;
    autonumber
    participant data
    participant filters
    participant filtered data
    participant teal module
    data->filters: gets
    filters->>filtered data: becomes
    filtered data->>teal module: sent to
```

The sequence diagram above illustrates the different stages that data
goes through within the `teal` framework, supported by the `teal.slice`
package:

1.  Data is created and loaded into `teal` app;
    - Data sets are wrapped in a `teal_data` before being passed to the
      app;
    - The [`teal_data`
      class](https://insightsengineering.github.io/teal/articles/blueprint/input_data.md)
      facilitates reproducibility;
2.  Data is passed to the filter panel;
    - Users *(or app developers)* can specify filters to apply;
    - Filters can be specified globally, for the whole app, or for
      specific modules;
    - Filtering code is appended to the data;
    - See the [Filter panel
      vignette](https://insightsengineering.github.io/teal/articles/blueprint/filter_panel.md)
      for details;
3.  Filtered data is sent to `teal` modules for analysis;
    - Each module receives a `teal_data` object so analysis code applied
      to the data is tracked *(and can be used to reproduce the whole
      analysis)*;

Whenever filters are added or removed, the data coming into modules is
re-computed, providing the `teal` module with new filtered data to
conduct the required analysis.
