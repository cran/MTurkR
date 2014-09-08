# R Client for the MTurk Requester API #

**MTurkR** provides programmatic access to the crowdsourcing functionality of the [Amazon Mechanical Turk](https://requester.mturk.com/) (MTurk) [Requester API](http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/Welcome.html).

Possible applications of the package include:
 * Survey or experimental research using MTurk workers as human subjects
 * Large-scale human coding or categorization of text, images, video, audio, or other files
 * Collection of unstructured data from the web
 * Audio and video transcription
 * Preprocessing of "messy" data prior to statistical procedures


---
## Installation ##

[![Build Status](https://travis-ci.org/leeper/MTurkR.png?branch=master)](https://travis-ci.org/leeper/MTurkR)

To install the latest development version of **MTurkR** from GitHub:

    if(!require("devtools")){
        install.packages("devtools")
        library("devtools")
    }
    install_github("leeper/MTurkR")

The GitHub [master branch](https://github.com/leeper/MTurkR) should be considered stable, while [the develop branch](https://github.com/leeper/MTurkR/tree/develop) reflects current code that is a work in progress. Major releases are periodically pushed to CRAN. To install the latest version from CRAN, simply use:

    install.packages("MTurkR")


## Using **MTurkR** ##

The MTurkR documentation files contain minimal examples for all functions. Further examples of how to use MTurkR are provided in [the MTurkR GitHub wiki](https://github.com/leeper/MTurkR/wiki). Users can contribute their own examples or further documentation there, or via pull requests to the GitHub repository.

If you experience problems using MTurkR, you can:
  
  - [Report issues on Github](https://github.com/leeper/MTurkR/issues)
  - Check the status of the MTurk Requester API on the [AWS Service Health Dashboard](http://status.aws.amazon.com/)
  - Contact the package maintainer [via email](mailto:thosjleeper@gmail.com) or on [Twitter](https://twitter.com/thosjleeper)

