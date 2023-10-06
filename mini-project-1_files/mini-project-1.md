Mini Data-Analysis Deliverable 1
================

# Welcome to your (maybe) first-ever data analysis project!

And hopefully the first of many. Let’s get started:

1.  Install the [`datateachr`](https://github.com/UBC-MDS/datateachr)
    package by typing the following into your **R terminal**:

<!-- -->

    install.packages("devtools")
    devtools::install_github("UBC-MDS/datateachr")

2.  Load the packages below.

``` r
library(datateachr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

3.  Make a repository in the <https://github.com/stat545ubc-2023>
    Organization. You can do this by following the steps found on canvas
    in the entry called [MDA: Create a
    repository](https://canvas.ubc.ca/courses/126199/pages/mda-create-a-repository).
    One completed, your repository should automatically be listed as
    part of the stat545ubc-2023 Organization.

# Instructions

## For Both Milestones

- Each milestone has explicit tasks. Tasks that are more challenging
  will often be allocated more points.

- Each milestone will be also graded for reproducibility, cleanliness,
  and coherence of the overall Github submission.

- While the two milestones will be submitted as independent
  deliverables, the analysis itself is a continuum - think of it as two
  chapters to a story. Each chapter, or in this case, portion of your
  analysis, should be easily followed through by someone unfamiliar with
  the content.
  [Here](https://swcarpentry.github.io/r-novice-inflammation/06-best-practices-R/)
  is a good resource for what constitutes “good code”. Learning good
  coding practices early in your career will save you hassle later on!

- The milestones will be equally weighted.

## For Milestone 1

**To complete this milestone**, edit [this very `.Rmd`
file](https://raw.githubusercontent.com/UBC-STAT/stat545.stat.ubc.ca/master/content/mini-project/mini-project-1.Rmd)
directly. Fill in the sections that are tagged with
`<!--- start your work below --->`.

**To submit this milestone**, make sure to knit this `.Rmd` file to an
`.md` file by changing the YAML output settings from
`output: html_document` to `output: github_document`. Commit and push
all of your work to the mini-analysis GitHub repository you made
earlier, and tag a release on GitHub. Then, submit a link to your tagged
release on canvas.

**Points**: This milestone is worth 36 points: 30 for your analysis, and
6 for overall reproducibility, cleanliness, and coherence of the Github
submission.

# Learning Objectives

By the end of this milestone, you should:

- Become familiar with your dataset of choosing
- Select 4 questions that you would like to answer with your data
- Generate a reproducible and clear report using R Markdown
- Become familiar with manipulating and summarizing your data in tibbles
  using `dplyr`, with a research question in mind.

# Task 1: Choose your favorite dataset

The `datateachr` package by Hayley Boyce and Jordan Bourak currently
composed of 7 semi-tidy datasets for educational purposes. Here is a
brief description of each dataset:

- *apt_buildings*: Acquired courtesy of The City of Toronto’s Open Data
  Portal. It currently has 3455 rows and 37 columns.

- *building_permits*: Acquired courtesy of The City of Vancouver’s Open
  Data Portal. It currently has 20680 rows and 14 columns.

- *cancer_sample*: Acquired courtesy of UCI Machine Learning Repository.
  It currently has 569 rows and 32 columns.

- *flow_sample*: Acquired courtesy of The Government of Canada’s
  Historical Hydrometric Database. It currently has 218 rows and 7
  columns.

- *parking_meters*: Acquired courtesy of The City of Vancouver’s Open
  Data Portal. It currently has 10032 rows and 22 columns.

- *steam_games*: Acquired courtesy of Kaggle. It currently has 40833
  rows and 21 columns.

- *vancouver_trees*: Acquired courtesy of The City of Vancouver’s Open
  Data Portal. It currently has 146611 rows and 20 columns.

**Things to keep in mind**

- We hope that this project will serve as practice for carrying our your
  own *independent* data analysis. Remember to comment your code, be
  explicit about what you are doing, and write notes in this markdown
  document when you feel that context is required. As you advance in the
  project, prompts and hints to do this will be diminished - it’ll be up
  to you!

- Before choosing a dataset, you should always keep in mind **your
  goal**, or in other ways, *what you wish to achieve with this data*.
  This mini data-analysis project focuses on *data wrangling*,
  *tidying*, and *visualization*. In short, it’s a way for you to get
  your feet wet with exploring data on your own.

And that is exactly the first thing that you will do!

1.1 **(1 point)** Out of the 7 datasets available in the `datateachr`
package, choose **4** that appeal to you based on their description.
Write your choices below:

**Note**: We encourage you to use the ones in the `datateachr` package,
but if you have a dataset that you’d really like to use, you can include
it here. But, please check with a member of the teaching team to see
whether the dataset is of appropriate complexity. Also, include a
**brief** description of the dataset here to help the teaching team
understand your data.

<!-------------------------- Start your work below ---------------------------->

1: *steam_games*  
2: *vancouver_trees*  
3: *apt_buildings*  
4: *building_permits*

<!----------------------------------------------------------------------------->

1.2 **(6 points)** One way to narrowing down your selection is to
*explore* the datasets. Use your knowledge of dplyr to find out at least
*3* attributes about each of these datasets (an attribute is something
such as number of rows, variables, class type…). The goal here is to
have an idea of *what the data looks like*.

*Hint:* This is one of those times when you should think about the
cleanliness of your analysis. I added a single code chunk for you below,
but do you want to use more than one? Would you like to write more
comments outside of the code chunk?

<!-------------------------- Start your work below ---------------------------->

``` r
### EXPLORE HERE ###
library(dplyr)

# Explore attributes of each dataset
steam_games_attributes <- steam_games %>%
  summarise(
    num_rows = n(),
    num_columns = ncol(.),
    columns = colnames(.),
    col_class_type = map_chr(., class)
  )
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
vancouver_trees_attributes <- vancouver_trees %>%
  summarise(
    num_rows = n(),
    num_columns = ncol(.),
    columns = colnames(.),
    col_class_type = map_chr(., class)
  )
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
apt_buildings_attributes <- apt_buildings %>%
  summarise(
    num_rows = n(),
    num_columns = ncol(.),
    columns = colnames(.),
    col_class_type = map_chr(., class)
  )
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
building_permits_attributes <- building_permits %>%
  summarise(
    num_rows = n(),
    num_columns = ncol(.),
    columns = colnames(.),
    col_class_type = map_chr(., class)
  )
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
# Display the results
steam_games_attributes
```

    ## # A tibble: 21 × 4
    ##    num_rows num_columns columns        col_class_type
    ##       <int>       <int> <chr>          <chr>         
    ##  1    40833          21 id             numeric       
    ##  2    40833          21 url            character     
    ##  3    40833          21 types          character     
    ##  4    40833          21 name           character     
    ##  5    40833          21 desc_snippet   character     
    ##  6    40833          21 recent_reviews character     
    ##  7    40833          21 all_reviews    character     
    ##  8    40833          21 release_date   character     
    ##  9    40833          21 developer      character     
    ## 10    40833          21 publisher      character     
    ## # ℹ 11 more rows

``` r
vancouver_trees_attributes
```

    ## # A tibble: 20 × 4
    ##    num_rows num_columns columns            col_class_type
    ##       <int>       <int> <chr>              <chr>         
    ##  1   146611          20 tree_id            numeric       
    ##  2   146611          20 civic_number       numeric       
    ##  3   146611          20 std_street         character     
    ##  4   146611          20 genus_name         character     
    ##  5   146611          20 species_name       character     
    ##  6   146611          20 cultivar_name      character     
    ##  7   146611          20 common_name        character     
    ##  8   146611          20 assigned           character     
    ##  9   146611          20 root_barrier       character     
    ## 10   146611          20 plant_area         character     
    ## 11   146611          20 on_street_block    numeric       
    ## 12   146611          20 on_street          character     
    ## 13   146611          20 neighbourhood_name character     
    ## 14   146611          20 street_side_name   character     
    ## 15   146611          20 height_range_id    numeric       
    ## 16   146611          20 diameter           numeric       
    ## 17   146611          20 curb               character     
    ## 18   146611          20 date_planted       Date          
    ## 19   146611          20 longitude          numeric       
    ## 20   146611          20 latitude           numeric

``` r
apt_buildings_attributes
```

    ## # A tibble: 37 × 4
    ##    num_rows num_columns columns                        col_class_type
    ##       <int>       <int> <chr>                          <chr>         
    ##  1     3455          37 id                             numeric       
    ##  2     3455          37 air_conditioning               character     
    ##  3     3455          37 amenities                      character     
    ##  4     3455          37 balconies                      character     
    ##  5     3455          37 barrier_free_accessibilty_entr character     
    ##  6     3455          37 bike_parking                   character     
    ##  7     3455          37 exterior_fire_escape           character     
    ##  8     3455          37 fire_alarm                     character     
    ##  9     3455          37 garbage_chutes                 character     
    ## 10     3455          37 heating_type                   character     
    ## # ℹ 27 more rows

``` r
building_permits_attributes
```

    ## # A tibble: 14 × 4
    ##    num_rows num_columns columns                     col_class_type
    ##       <int>       <int> <chr>                       <chr>         
    ##  1    20680          14 permit_number               character     
    ##  2    20680          14 issue_date                  Date          
    ##  3    20680          14 project_value               numeric       
    ##  4    20680          14 type_of_work                character     
    ##  5    20680          14 address                     character     
    ##  6    20680          14 project_description         character     
    ##  7    20680          14 building_contractor         character     
    ##  8    20680          14 building_contractor_address character     
    ##  9    20680          14 applicant                   character     
    ## 10    20680          14 applicant_address           character     
    ## 11    20680          14 property_use                character     
    ## 12    20680          14 specific_use_category       character     
    ## 13    20680          14 year                        numeric       
    ## 14    20680          14 bi_id                       numeric

<!----------------------------------------------------------------------------->

1.3 **(1 point)** Now that you’ve explored the 4 datasets that you were
initially most interested in, let’s narrow it down to 1. What lead you
to choose this one? Briefly explain your choice below.

<!-------------------------- Start your work below ---------------------------->

I choose the apt_buildings dataset. The reason is that I am particularly
interested in houses because it is a necessity for us as ordinary people
to survive. At the same time, I am familiar with almost all variables in
the database.

<!----------------------------------------------------------------------------->

1.4 **(2 points)** Time for a final decision! Going back to the
beginning, it’s important to have an *end goal* in mind. For example, if
I had chosen the `titanic` dataset for my project, I might’ve wanted to
explore the relationship between survival and other variables. Try to
think of 1 research question that you would want to answer with your
dataset. Note it down below.

<!-------------------------- Start your work below ---------------------------->

I want to research the relationship between the age of a house and the
floors it was built on

<!----------------------------------------------------------------------------->

# Important note

Read Tasks 2 and 3 *fully* before starting to complete either of them.
Probably also a good point to grab a coffee to get ready for the fun
part!

This project is semi-guided, but meant to be *independent*. For this
reason, you will complete tasks 2 and 3 below (under the **START HERE**
mark) as if you were writing your own exploratory data analysis report,
and this guidance never existed! Feel free to add a brief introduction
section to your project, format the document with markdown syntax as you
deem appropriate, and structure the analysis as you deem appropriate. If
you feel lost, you can find a sample data analysis
[here](https://www.kaggle.com/headsortails/tidy-titarnic) to have a
better idea. However, bear in mind that it is **just an example** and
you will not be required to have that level of complexity in your
project.

# Task 2: Exploring your dataset

If we rewind and go back to the learning objectives, you’ll see that by
the end of this deliverable, you should have formulated *4* research
questions about your data that you may want to answer during your
project. However, it may be handy to do some more exploration on your
dataset of choice before creating these questions - by looking at the
data, you may get more ideas. **Before you start this task, read all
instructions carefully until you reach START HERE under Task 3**.

2.1 **(12 points)** Complete *4 out of the following 8 exercises* to
dive deeper into your data. All datasets are different and therefore,
not all of these tasks may make sense for your data - which is why you
should only answer *4*.

Make sure that you’re using dplyr and ggplot2 rather than base R for
this task. Outside of this project, you may find that you prefer using
base R functions for certain tasks, and that’s just fine! But part of
this project is for you to practice the tools we learned in class, which
is dplyr and ggplot2.

1.  Plot the distribution of a numeric variable.
2.  Create a new variable based on other variables in your data (only if
    it makes sense)
3.  Investigate how many missing values there are per variable. Can you
    find a way to plot this?
4.  Explore the relationship between 2 variables in a plot.
5.  Filter observations in your data according to your own criteria.
    Think of what you’d like to explore - again, if this was the
    `titanic` dataset, I may want to narrow my search down to passengers
    born in a particular year…
6.  Use a boxplot to look at the frequency of different observations
    within a single variable. You can do this for more than one variable
    if you wish!
7.  Make a new tibble with a subset of your data, with variables and
    observations that you are interested in exploring.
8.  Use a density plot to explore any of your variables (that are
    suitable for this type of plot).

2.2 **(4 points)** For each of the 4 exercises that you complete,
provide a *brief explanation* of why you chose that exercise in relation
to your data (in other words, why does it make sense to do that?), and
sufficient comments for a reader to understand your reasoning and code.

<!-------------------------- Start your work below ---------------------------->

## 1. Plot the distribution of a numeric variable.

Plot the distribution of year_built

In order to study the relationship between the age of house construction
and the floors built, first look at the distribution of the year of
house construction.

``` r
library(ggplot2)

ggplot(apt_buildings, aes(x = year_built)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribution of Built Year",
       x = "Built Year",
       y = "Frequency")
```

    ## Warning: Removed 13 rows containing non-finite values (`stat_bin()`).

![](mini-project-1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## 2. Create a new variable based on other variables in your data (only if it makes sense)

Create the new variable ‘years_since_built’, which is 2023-year_built.

Convert the year the house was built to the year since the house was
built.

``` r
apt_buildings <- apt_buildings %>%
  mutate(years_since_built = 2023 - year_built)
```

## 3. Investigate how many missing values there are per variable. Can you find a way to plot this?

Understanding which variables have missing values and how many missing
values there are is very important for subsequent research. This
captures an important basic piece of information in the database.

``` r
# Investigate missing values per variable
missing_summary <- apt_buildings %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "variable", value = "missing_count")

# Plot the missing values
ggplot(missing_summary, aes(x = missing_count, y = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Missing Values of each Variable",
       x = "Number of Missing Values",
       y = "Variable") +
  theme_minimal()
```

![](mini-project-1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## 4. Explore the relationship between 2 variables in a plot.

Explore the relationship between years_since_built and no_of_storeys

Completed the small goal of the research: the relationship between the
construction year of the house and the number of floors

``` r
ggplot(apt_buildings, aes(x = years_since_built, y = no_of_storeys)) +
  geom_point() +
  labs(title = "Relationship between Years Since Built and No. of Storeys",
       x = "Years Since Built",
       y = "No. of Storeys") +
  theme_minimal()
```

    ## Warning: Removed 13 rows containing missing values (`geom_point()`).

![](mini-project-1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

<!----------------------------------------------------------------------------->

# Task 3: Choose research questions

**(4 points)** So far, you have chosen a dataset and gotten familiar
with it through exploring the data. You have also brainstormed one
research question that interested you (Task 1.4). Now it’s time to pick
4 research questions that you would like to explore in Milestone 2!
Write the 4 questions and any additional comments below.

<!--- *****START HERE***** --->

1.  Is there a correlation between the year a building was built
    (year_built) and the availability of energy-efficient features such
    as separate gas/hydro meters, sprinkler systems, or thermal windows?

2.  How does the availability of parking amenities (bike parking, tenant
    parking, visitor parking) relate to the size of the building (number
    of storeys or number of units)?

3.  What is the distribution of barrier-free accessibility features in
    the dataset, and how does it vary across different types of
    buildings (e.g., privately owned vs. social housing)?

4.  Is there any association between the property management company
    (prop_management_company_name) and building characteristics such as
    the presence of amenities, year built, or the number of elevators?

<!----------------------------->

# Overall reproducibility/Cleanliness/Coherence Checklist

## Coherence (0.5 points)

The document should read sensibly from top to bottom, with no major
continuity errors. An example of a major continuity error is having a
data set listed for Task 3 that is not part of one of the data sets
listed in Task 1.

## Error-free code (3 points)

For full marks, all code in the document should run without error. 1
point deduction if most code runs without error, and 2 points deduction
if more than 50% of the code throws an error.

## Main README (1 point)

There should be a file named `README.md` at the top level of your
repository. Its contents should automatically appear when you visit the
repository on GitHub.

Minimum contents of the README file:

- In a sentence or two, explains what this repository is, so that
  future-you or someone else stumbling on your repository can be
  oriented to the repository.
- In a sentence or two (or more??), briefly explains how to engage with
  the repository. You can assume the person reading knows the material
  from STAT 545A. Basically, if a visitor to your repository wants to
  explore your project, what should they know?

Once you get in the habit of making README files, and seeing more README
files in other projects, you’ll wonder how you ever got by without them!
They are tremendously helpful.

## Output (1 point)

All output is readable, recent and relevant:

- All Rmd files have been `knit`ted to their output md files.
- All knitted md files are viewable without errors on Github. Examples
  of errors: Missing plots, “Sorry about that, but we can’t show files
  that are this big right now” messages, error messages from broken R
  code
- All of these output files are up-to-date – that is, they haven’t
  fallen behind after the source (Rmd) files have been updated.
- There should be no relic output files. For example, if you were
  knitting an Rmd to html, but then changed the output to be only a
  markdown file, then the html file is a relic and should be deleted.

(0.5 point deduction if any of the above criteria are not met. 1 point
deduction if most or all of the above criteria are not met.)

Our recommendation: right before submission, delete all output files,
and re-knit each milestone’s Rmd file, so that everything is up to date
and relevant. Then, after your final commit and push to Github, CHECK on
Github to make sure that everything looks the way you intended!

## Tagged release (0.5 points)

You’ve tagged a release for Milestone 1.

### Attribution

Thanks to Icíar Fernández Boyano for mostly putting this together, and
Vincenzo Coia for launching.
