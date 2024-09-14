---
format: md
---

# `biocmask`

`biocmask` provides efficient abstractions to the *SummarizedExperiment* such
that using common dplyr functions feels as natural to operating on a
*data.frame* or *tibble*. `biocmask` uses 
[data-masking](https://rlang.r-lib.org/reference/topic-data-mask-programming.html) 
from the `rlang` package in order to connect dplyr functions to
*SummarizedExperiment* slots in a manner that aims to be intuitive and avoiding
ambiguity in outcomes.

***Note:***  This package is still under active development as of Fall 2024.

# Feedback

We would love to hear your feedback. Please post to 
[Bioconductor support site](https://support.bioconductor.org)
or the 
`#tidiness_in_bioc` Slack channel on community-bioc
for software usage help, 
or post an 
[Issue on GitHub](https://github.com/jtlandis/biocmask/issues),
for software development questions.

# Funding

`biocmask` was supported by a EOSS cycle 6 grant from The Wellcome Trust.
