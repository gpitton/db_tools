## Database tools in racket

# Introduction
The purpose of this repository is to provide some automation for repetitive tasks that often appear when working with structured data.

I often run some computations that generate new data that I later need to insert into an existing database.
Typically this data is saved in text files, so my ideal toolchain would involve the following steps:

- Define the path where the new data can be found.
- Define how to interpret the data (the file format and the type of each entry).
- Define how to update an existing database or how to create a new table.

Optionally, it would be nice to have some basic data analysis functions e.g. to look at a summary of the new data and at the correlations between the new data and the existing one.

It would also be nice to have a tool to generate some python code that plots some specified data in one of the few formats I typically use (histograms, scatter plots, ...).


For more information, read the documentation available in scribble format inside this repository.

