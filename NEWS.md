# CHANGES TO MTurkR 0.8

## SIGNIFICANT USER-VISIBLE CHANGES

* `GetAssignments()` now attempts to convert non-character variables to an appropriate R vector type via `type.convert(, as.is = TRUE)`.
* Some statistics have been deprecated for the `GetStatistic()` and `RequesterReport()`, including "NumberAssignmentsAvailable", "NumberAssignmentsAccepted", "NumberAssignmentsReturned", "TotalFeePayout", and "TotalRewardAndFeePayout". This is reflected in the return value of `ListStatistics()`.

## DOCUMENTATION

* Added two papers (one from *The Political Methodologist* and one from *The R Journal*) as supplemental PDF documentation, accessible via `? MTurkR`. (#93)
* Documentation now indicates that `GrantBonus()` allows the `reason` argument to contain newlines and tabs. (#109)

## BUG FIXES

* Question identifiers used in a Qualification AnswerKey are only checked if `CreateQualificationType()` is called with `validate.answerkey = TRUE`. (#117, h/t Darrell Penta)
* Fixed a bug in `BulkCreateFromHITLayout()` that caused the function to fail. (#114, h/t Tyler Scott)
* Fixed a bug in trying to parse already parsed response for `GetBonuses()`. (#113, h/t Andrew Brown)
* The simple wizard now correctly respects AWS credentials specified as environment variables instead of the deprecated `credentials()` function. (h/t Kim Gross)
* Functions should now all return data frames with character class columns (unless otherwise noted) with sequentially numbered rownames. This corrects potentially some confusing behavior introduced during XML parsing. (#112)


# CHANGES TO MTurkR 0.7

## SIGNIFICANT USER-VISIBLE CHANGES

* `GenerateExternalQuestion` now sets a default value of `frame.height = 400`.
* The AWS Access Key ID used for requests is now redacted from the error message printing when an API request fails to reduce the risk of accidentally disclosing the credential. (#105)
* AWS removed the Categorization and Photo Moderation QualificationTypes on December 18th, 2015. Use of these QualificationTypeIds will trigger warnings in some cases and the "generic" Masters QualificationTypeId will be substituted automatically. Users should not need to update any code, though they may be receive warnings. These have also been removed from `ListQualificationTypes()`. (#104)
* The use of `credentials()`, and `options("MTurkR.keypair")` are completely deprecated. A warning will now be issued if trying to supply credentials in this way. AWS credentials should be specified in environment variables `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`. Messages are triggered on package load if environment variables are not set. (#102)
* `HITStatus()` will now display any non-empty RequesterAnnotation field for a HIT. (#101)
* The deprecated argument `print`, which was removed in v0.5, has been fully removed. Use `verbose` instead. (#97)
* The deprecated functions `mturkrhelp` and `APIReference` have been removed. (#96)
* `MTurkR.Wizard` has been removed from MTurkR and has been recreated as a separate package called MTurkRGUI. (#95)
* `HITStatus` now reports condensed column names better suited for narrow displays.
* Correctly handle multiple `response.group` arguments passed to HIT-related functions. (#89)
* `authenticate`, which was deprecated in v0.5, is removed.
* Use of RCurl has been replaced by curl. This necessitated a new dependency, base64enc. (#86)
* In lieu of `credentials`, AWS credentials should be specified via environment variables `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY`. `credentials` will be deprecated.
* `SufficientFunds()` reflects the new commission structure effective July 21, 2015. (#87)
* `BulkCreateFromTemplate` and `BulkCreateFromURLs` now have a `frame.height` argument with a default of 450.

## DOCUMENTATION

* A small test suite has been added to help ensure correct package functionality. It is only executed in the sandbox in the presence of valid AWS credentials. (#100)
* The package now includes pages describing common use cases for MTurk (surveys, categorization, sentiment analysis, and webscraping). (#98)
* Descriptions of global package options have been added to MTurkR-package.Rd. (#103)
* The package now includes additional HTMLQuestion templates including examples for image categorization and sentiment rating. (#94)
* Documentation for `GenerateHITReviewPolicy` and `GenerateAssignmentReviewPolicy` was expanded and tested, and a related HTMLQuestion HIT template is now installed.

## BUG FIXES

* Fixed a bug in checking for a valid AWS keypair in `request()`. (h/t Sean Murphy, #106)
* When ttk widgets were added to the graphical wizard in v0.6, scrollbar calls retained tk-only options. Those have been removed. (h/t Justin Koch)
* State-level locales are now correctly supported in QualificationRequirements, including in complex combinations with country locales with no subdivision (so mixing U.S. states with other countries is possible). This feature was added in v0.6.5 but had a critical bug. (h/t Ricky Bilakhia)
* Closing the graphical wizard crashed RStudio due to `bringToTop(-1)`. This is fixed. (h/t Carolina Kuepper-Tetzel)


# CHANGES TO MTurkR 0.6.5

## SIGNIFICANT USER-VISIBILE CHANGES

* Added `BulkCreate`, `BulkCreateFromTemplate`, `BulkCreateFromURLs`, and `BulkCreateFromHITLayout` functions that can be used to create multiple HITs of the same HITType with identical properties but different question content. The `hitsfromtemplate` function has been removed. (#76)
* The graphical wizard now supports creating HIT- and Assignment-level ReviewPolicies during HIT creation. (#55)
* `CreateHIT` now only warns about duplicated arguments (passing both `hit.type` and arguments for `RegisterHITType`) if `verbose = TRUE`.
* The graphical wizard now allows users to create an Qualification AnswerKey Template (via `AnswerKeyTemplate`) from a manually entered Qualification Test. (#73)
* `GrantBonus` now accepts a `unique.request.token` argument. (#75)
* The unused `format` argument to `GenerateNotification` has been removed. (#71)

## DOCUMENTATION

* Examples have been added to demonstrate functionality of `AnswerKeyTemplate`.
* Documentation for `GrantQualification` and `RejectQualification` has been merged to clarify the use of these functions. Documentation has been expanded with complete examples of both functions. (#72)
* `GenerateQualificationRequirement` now notes that U.S. state locale values (following ISO 3166-2 format) are allows. (#74)
* `GenerateHITsFromTemplate` now has a more complete example that includes an installed template HTML file. (#72)
* Several small changes have been made to correct documentation and fix bad formatting in the PDF version of the manual. (#72)

## BUG FIXES

* Fixed a bug in the CreateQualification dialog of the graphical wizard related to destroying the dialog. (h/t Sean Murphy)
* Fixed a bug in the AssignQualification dialog of the graphical wizard. (h/t Carolina Kuepper-Tetzel)
* Loading MTurkR no longer overwrites existing MRurkR `options`. (#83)
* `GetStatistic` now consistently returns a three-column data frame containing the name of the requested statistic, its value, and the date of the statistic. If the request fails, this data frame has zero rows. Printing of request information (if `verbose = TRUE`) has further been fixed to prevent incorrect printing when `count` was specified. (#78)
* Fixed a bug in `ExtendHIT` wherein specifying both `add.assignments` and `add.seconds` caused only assignments to be incremented. (#79)


# CHANGES TO MTurkR 0.6

## SIGNIFICANT USER-VISIBLE CHANGES

* `GetReviewResults` should now be able to return all results. This is untested due to the large page size of ReviewResults (65,535). (#54)
* The graphical wizard now uses "themed" tk (ttk) widgets where possible, in order to improve the appearance. This creates a dependency on tcl >= 8.5.
* The graphical wizard now allows retrieval of assignments by requester annotation (or batch) field.
* The graphical and text-based wizards now allow you to preview the email that will be sent to workers using the `ContactWorkers` function. (#53)
* The graphical and text-based wizards now allow users to assign qualifications to workers. (#61)
* The graphical user interface (`mturkr.wizard`) has a significantly cleaned up code base aimed at simplifying future bug fixes. A user-visible side effect of this is modified aesthetics that should make the interface more attractive and more uniform. (#56)
* `mturkr.wizard` now allows users to enter multiple WorkerIds for several functions by simply pasting one WorkerId per line into a multi-line textbox. (#52, h/t Justin Weigand)
* `GetReviewResultsForHIT` now has `pagenumber` and `pagesize` arguments to control pagination of results. (#54)

## DOCUMENTATION

* Documentation for `CreateHIT` and `CreateQualificationType` now include examples using template files installed with the package. (#40)
* Documentation of generating a Masters QualificationRequirement has been slightly clarified.
* Examples of Amazon Simple Queue Service (SQS) notifications have been added, along with a detailed tutorial on the MTurk wiki. (#60)

## BUG FIXES

* Fixed a bug in the wizard that prevented the user from clearing and viewing QualificationRequirements that had been added to a HITType.
* Numerous related bug fixes in the wizard related to populating HITId, HITTypeId, and QualificationTypeId fields were fixed. (#69)
* Wizard balance checking message box now reports balance correctly. (#67)
* Wizard dialogs for adding question data structures to a HIT did not close on accepting "OK". (#65)
* `GetAssignments` only returned the first answer option when multiple selection answers were chosen by a worker. (#63, h/t Kyle Harms)
* Fixed a possible bug in `ContactWorkers` batch mode that sent multiple emails to the same worker(s). (#58, h/t Andy Guess)
* Fixed a few XML-related bugs in `as.data frame.QuestionForm` and `as.data frame.AnswerKey`.


# CHANGES TO MTurkR 0.5.5

## SIGNIFICANT USER-VISIBLE CHANGES

* `GenerateReviewPolicy` is removed in lieu of two new functions: `GenerateHITReviewPolicy` and `GenerateAssignmentReviewPolicy`, both of which have a simplified function API and include extensive examples in the documentation. The previous code never worked correctly, so this change should be unimportant. The use of `CreateHIT` remains unchanged. (#47)
* `HITStatus`, `ApproveAllAssignments`, `ChangeHITType`, `DisposeHIT`, `DisableHIT`, `ExpireHIT`, `ExtendHIT`, `GetAssignment`, `GetBonuses`, `SetHITAsReviewing` now include an optional `annotation` argument that can be used to apply the function to all HITs of a given "batch" created in the online Requester User Interface. (#51)
* `GetFileUpload` no longer has a `file.ext` argument. Instead the file extension is taken from the uploaded file itself. The format of the filename for the downloaded file has also been clarified. (#45)

## DOCUMENTATION

* Examples for all functions have been substantially expanded and checked for accuracy.
* The MTurkR wiki (https://github.com/leeper/MTurkR/wiki) includes numerous updated code examples, including full documentation for both `wizard.simple` and `mturkr.wizard`, the MTurkR interactive interfaces.

## BUG FIXES
* Fixed a bug in `SearchHITs` that was not passing `...` arguments ot `GetQualificationType`
* Fixed a small bug in the batch (pagination) of `SearchQualificationTypes`.
* Functions for QualificationTypes now return a data frame containing character variables rather than factor variables, as this could cause unintended coercion when using `GenerateQualifiationType`. (h/t Haotian Zhou)
* Corrected several related bugs (API version number, LocaleValue specification) that prevented the "In" comparator in a QualificationRequirement from working with multiple LocaleValues in API version 2013-11-15. (#50, h/t Haotian Zhou)


# CHANGES TO MTurkR 0.5

## SIGNIFICANT USER-VISIBLE CHANGES
* `GenerateQualificationRequirement` now supports new comparators (given MTurk API update on 2014-07-17): "DoesNotExist", "In", and "NotIn". The "DoesNotExist" comparator allows requesters to effectively block workers who do not have, e.g., a requester-defined qualification. The "In" and "NotIn" comparators allow a boolean OR logical in QualificationRequirements (e.g., allowing a worker to be from the US or the UK) rather than having to create two separate HITs with different Locale restrictions.
* The `qual.number` argument to `GenerateQualificationRequirement` has been removed.
* The `return.hit.dataframe` argument to `GetHITsForQualificationType` has been removed.
* Functions of the form `*ToDataFrame` have been replaced with `as.data frame` S3 methods, which are handled by a global `as.data frame.MTurkResponse` function. (#36)
* `ParseErrorCodes` is removed (now used internally in `request`).
* `options('MTurkR.print')` is deleted and replaced by `options('MTurkR.verbose')`. All functions will, temporarily, accept `print` arguments. All code should be updated to reflect the new terminology moving forward.
* When `validation.test=TRUE` or `browser=TRUE`, functions now return an object of class "MTurkResponse" for the purpose of debugging.
* The ability to execute requests in a web browser, via the `options('MTurkR.browser')` is removed from all functions except `request`. It can, however, still be passed via `...` to `request`. (#38)
* `APIReference`, `mturkhelp`, and `genericmturkr` are deprecated.
* `GetBonuses` no longer supports the `return.bonus.dataframe` logical. All bonuses are automatically returned. To just retrieve details on number of bonuses per HIT, request only the first page with `page.number=1` and `verbose=TRUE`.
* `MTurkR.Wizard` now allows batch emails using `ContactWorkers`. WorkerIds can be specified as a comma-separated list. (#4)
* The `credentials` function has now been changed to create and/or retrieve a global option `MTurkR.keypair`. This is a backwards compatible change that allows users to specify `options(MTurkR.keypair=c(accesskey, secretkey))` directly without using `credentials`. All functions now point directly to the global option, so that `credentials` can be deprecated in a future release.
* In many functions that return a data frame with a `Valid` column (e.g., `ContactWorkers`, etc.), `Valid` is now a factor. (#28)
* Using `ContactWorkers` in batch mode now returns a data frame identical in structure to using it in regular mode, with one row per worker. (#32)
* The `signature` argument to `request` now defaults to `NULL` and is generated automatically.
* `HITsToDataFrame` (now `as.data frame.HITs`) now returns, for the QualificationRequirements list element, an empty data frame rather a single `NA` missing value if a given HIT does not have any QualificationRequirements attached to it.
* `request` now generates the API authentication signature automatically, eliminating the need for `authenticate`, which is deprecated. `request` no longer accepts `signature` or `timestamp` arguments and has a new argument order. Arguments can be passed to `request` via a new `...` argument in all functions.
* `request` now uses `curlPerform` instead of `getURL` to gain finer control over API requests.
* `request` now attempts to `tryCatch` any errors on writing the MTurkR log file. (#9)
* `request` now returns an object of S3 class "MTurkResponse" and an associated print method has been added.

## BUG FIXES
* `GenerateQualificationRequirement` now correctly handles the "Exists" comparator by requiring an empty character string for the corresponding `value` argument.
* `HITStatus` now prints correctly when only one HIT is retrieved.
* `ApproveAllAssignments` now only attempts to approved assignments with AssignmentStatus "Submitted".
* `as.data frame.QuestionForm` now returns "Overview" elements and "Question" elements as one data frame, distinguished by a new column: `Element`. (#8)
* `GetQualifications` and `GetQualificationRequests` now respect the `return.all` argument.
* The `return.qual.dataframe` argument in `GetQualificationType` and `GetQualifications` is removed.
* Fixed a bug in `GetBonuses` whereby the default `return.all=TRUE` argument overwrote manually specified `pagenumber` and `pagesize` arguments. When `GetBonuses` is called with a `hit.type`, `return.all` is used recursively, such that `return.all=FALSE` returns the requested page for each HIT of the requested HITType. (h/t Dane Wendell, #37)
* Corrected the API operation name in `SendTestEventNotification`.
* Fixed a bug in `GetBonuses` using a HITTypeId that caused the function to fail catastrophically.

## DOCUMENTATION
* The `return.qual.dataframe` argument in `GetQualificationType` and `GetQualifications` is removed.
* All functions now reflect the new `...` argument, the contents of which are passed to `request`. (#38)
* All references to the `browser` options are removed given removal of the feature from all functions.
* Documentation for `mturkhelp` and `APIReference` now reflect deprecated status.
* Documentation file for `authenticate` now reflect its deprecated status.
* Documentation file for `request` has been updated to reflect its now non-exported status. `request` notes that it no longer accepts `signature` or `timestamp` arguments, which are now generated internally. Documentation for `ParseErrorCodes` is removed.
* Work on the MTurkR wiki (https://github.com/leeper/MTurkR/wiki) has begun, with contributions from Kyle Hamilton.
* The `add.seconds` parameter was incorrectly called `add.time` in the `ExtendHIT` documentation.


# CHANGES TO MTurkR 0.4

## SIGNIFICANT USER-VISIBLE CHANGES
* Functions that accept a `hit.type` argument can now generally accept a vector of HITTypeIds rather than just a single character string. See documentation.
* `GetBonuses` now allows requests to be opened in the default browser, like most other API functions.
* From version 0.3.7, global options (`MTurkR.log`,`MTurkR.test`,`MTurkR.browser`,and `MTurkR.print`) have been added to all functions. Defaults for these remain the same in all places.
* From version 0.3.65, `SearchHITs` returns a named list of QualificationRequirements. This is a consequence of some significant changes to now the names of user-defined QualificationTypes are added to this list (moving that functionality from `QualificationRequirementsToDataFrame` to `SearchHITs`.
* From version 0.3.6, a global option `options('MTurkR.sandbox'=FALSE)` has been added to allow easier reuse of code between testing in the sandbox and the live server. The default behavior remains to use the live server, but using an option, which is called by all functions, allows one to toggle the global option and then recycle code verbatim.
* Per API changes on April 8, 2013, several built-in QualificationTypes were removed. See `ListQualificationTypes` for the available types.
* Improved error, warning, and message handling by converting calls to `cat` to `warning` and `message`, where appropriate.
* Most API query functions now include a `validation.test` parameter that, when TRUE, runs pre-query checks on the MTurkR call and returns the full API query URL. The call is not executed. This can be used for pre- or post-request debugging.
* The 'MTurkRlog.tsv' file is now located according to a global option, specified by `options(MTurkR.logdir = getwd())`. The previous default behavior (to store the file in the working directory) is preserved but can now be globally modified for all requests. This might be useful for storing all requests (from all MTurk projects) in a single directory.

## BUG FIXES
* `ContactWorkers` now supplies additional information when attempting to contact ineligible workers (i.e., those who have not worked for the requester previously). These workers are indicated by a value of `HardFailure` in the `Valid` column of the response data frame. (#29)
* All functions now explicitly convert factor arguments (e.g., for `worker`, `qual`, `hit`, etc.) to character in order to prevent some unintended side effects. (#26)
* New versions of libcurl (>7.28) deprecate boolean values for SSL_VERIFYPEER and SSL_VERIFYHOST options. `request` was returning errors due to this and has been updated. Everything is backwards compatible to earlier versions of libcurl. (#31, h/t James Ben Taylor)
* `GenerateHITLayoutParameter` now replaces any non-XML ampersands with `&amp;`, which had previously caused failures when creating HITs. (#30, h/t Eric Lin)
* In `GetAssignment`, made a small change to prevent a warning and coercion of a one-assignment vector to a list. (h/t Eric Lin)
* Two HITReviewPolicy names weres changed per a discussion on the AWS developer forum. Details here: https://forums.aws.amazon.com/thread.jspa?threadID=135285.
* In `HITStatus` and `HITsToDataFrame` the correct capitalization of "Of" (from lower to upper) has been corrected throughout.
* A series of related changes were made to `GetAssignment`, `AssignmentsToDataFrame` and `QuestionFormAnswersToDataFrame` to handle Answer data structures with differing QuestionIdentifiers. (h/t Robert Vesco and Solomon Messing)
* Eliminated several `response.group` options from `GetHITsForQualificationType` due to personal communication with AWS.
* Fixed bug in `GetReviewableHITs` that correctly queried the API but returned an incomplete data frame. (h/t Eric Lin)
* Fixed bug in `CreateHIT`, saying `type` was not defined, that occurred when `hit.type` was non-NULL. (h/t Eric Lin)
* Fixed bug in `GetHIT`, which trickled down through `HITsToDataFrame` and `QualificationRequirementsToDataFrame` that caused error when retrieving HITs from the sandbox with user-defined qualifications. (h/t Eric Lin)
* Fixed bug in `GenerateHTMLQuestion` related to XML encoding. (h/t Alex Neustädter)
* In `request` and several convenience functions, corrected `shell.exec` to `browseURL` to be platform-independent.
* Added filters to `request` to handle a variety of problematic characters (that might emerge from FreeText responses in assignments). (h/t and contributions from Solomon Messing)
* removed references to `shell.exec` and replaced with `browseURL`.

## DOCUMENTATION
* Where appropriate, functions using `hit.type` parameters are now described (correctly) as allowing vectors of HITTypeIds rather than just a single HITTypeId. See documentation.
* Documentation for `ContactWorkers` now describes in greater detail what happens when attempting to contact ineligible workers. (#29)

# CHANGES TO MTurkR 0.3.5

## SIGNIFICANT USER-VISIBLE CHANGES
* `CreateHIT` now provides an option to validate the 'question' parameter for HTMLQuestion, ExternalQuestion, and QuestionForm HIT structures.
* `CreateQualificationType` now provides options to validate the 'test' and/or 'answerkey' parameters for QuestionForm and AnswerKey structures, respectively.
* `GenerateAnswerKey` was modified to accept slightly different inputs for the 'questions' parameter. A new function, `AnswerKeyTemplate`, produces a user-modifiable template (as a list) for that parameter based on a QuestionForm data structure for which an AnswerKey is desired.
* When internal functions (described in the documentation of `XMLToDataFrame`) that normally return named lists have nothing to translate to a data frame, they now return named lists with NULL values rather than simply a single NULL.

## BUG FIXES
* Fixed bug in `GetAssignments` that prevented the return of assignments for an entire HITType or for multiple HITs. Specifically, previous version was retrieving assignments via the API but not returning them to the user. (h/t Robert Vesco)


# CHANGES TO MTurkR 0.3

## SIGNIFICANT USER-VISIBLE CHANGES
* To improve speed and reduce the number of API calls, `HITsToDataFrame`, which is called by `SearchHITs`, `GetHIT`, etc., now uses the convenience function `ListQualificationTypes` to return the name of QualificationRequirements when applicable.
* Expanded the functionality of `ContactWorkers` to allow a 'batch' mode, wherein workers are contacted with generic (i.e., not customized) email messages and subject lines in batches of 100.
* Text sent to console when `print=TRUE` modified slightly, so that iteration numbers are printed for each iteration (to monitor progress of mutli-item requests).
* ResponseGroups updated to current API specifications for `CreateHIT`, `GetAssignments`, `GetHIT`, `GetStatistic`, `GetWorkerStatistic`, and `SearchHITs`. These now include an optional "Request" ResponseGroup to simply return information about the API call without executing that call.
* `AssignmentsToDataFrame` now returns a "RequesterFeedback" variable, providing access to feedback supplied to workers when `GetAssignments` is called with the "AssignmentFeedback" ResponseGroup.
* Updates were made to `GenerateAnswerKey` (see documentation) and a new function `AnswerKeyToDataFrame` was released.
* `QuestionFormToDataFrame` created. See documentation for details.

## BUG FIXES
* MTurk API operation 'ChangeHITTypeofHIT' typo was fixed to 'ChangeHITTypeOfHIT'.
* A small change was made to `BlockWorkers` to allow a single reason to be used for multiple workers, as originally intended.
* Fixed bug in `GetAssignments` to allow retrieval of assignments by status, as originally intended.
* Fixed bug in `AssignmentsToDataFrame` to correctly store 'ApprovalTime' or 'RejectionTime' in 'ApprovalRejectionTime'
* Bug fixes to `CreateQualificationType` related to parsing of XML in 'test' and 'answerkey' parameters.
* Modified `credentials` to pass CRAN checks.

## DOCUMENTATION
* Documentation for `CreateQualificationType` notes web browser-dependent constraints on URL length that may produce unintended behavior when 'browser=TRUE' and a 'test' parameter is specified.
* Added documentation for `AnswerKeyToDataFrame`.
* Various copyedits.


# CHANGES TO MTurkR 0.2

## SIGNIFICANT USER-VISIBLE CHANGES
* A completely new GUI interface for managing MTurkR, which now suggests library(tcltk), has been added. The previous, text-based wizard remains available by calling `mturkr.wizard("simple")`.
* Support for ReviewPolicies has been added, both creating them using `GenerateReviewPolicy` and retrieving their results using `GetReviewResultsForHIT`.
* Support for AnswerKey data structures has been added, for use in `CreateQualificationType`.
* Added simple checks for 'sortproperty', 'sortdirection', 'pagesize', and 'pagenumber' parameters in relevant functions.
* Expanded support for QuestionForm data structures is currently under development and should be available in the next release.

## BUG FIXES
* A number of bug fixes have been corrected throughout the package. Most were minor and related to output, but some like the 'return.all' parameter in `GetAssignments`, were producing unintended behavior.
* Various mislabeled function parameters (used within functions) have been corrected (some were creating notes on R CMD check).
* `request` was modified to remove problematic whitespace characters when writing log entries (which made reading the log file into R problematic).

## DOCUMENTATION
* Fixed a number of minor errors in documentation.
* Expanded documentation for functions associated with AnswerKey and ReviewPolicy data structures.
* Expanded documentation for 'sortproperty' and 'sortdirection' parameters in relevant functions.
