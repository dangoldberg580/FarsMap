# FarsMap

The goal of FarsMap is to be able to summarize and map traffic fatalities in the US for the
years 2013 through 2015.

## Example

There are two functions that are available. fars_summarize_years takes as input the year (or years) in 
question and returns a data frame that show number of traffic fatalities by month in that year.

```{R}
 fars_summarize_years(c(2013, 2014))
```

The second function (fars_map_state), takes a state number from 1 to 56 and one year (either 2013, 2014 or 2015) 
and plots the number of traffic fatalities in a map of that state.

```{R}
 fars_map_state(12, 2015)
```

https://travis-ci.org/dangoldberg580/FarsMap.svg?branch=master
