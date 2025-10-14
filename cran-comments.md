## Test environments
- Local: R 4.5.1 on Fedora
- GitHub Actions: macOS (release), Ubuntu (devel, release, oldrel)
- Windows: win-builder (R-release/R-devel)

## R CMD check results

0 errors ✔ | 4 warnings ✖ | 3 notes ✖

* This is a new release.

## Submission notes
- Initial CRAN submission of **kbretrieveR** (AWS Bedrock Knowledge Base retrieval for R).
- Examples and tests are offline and fast (<1s); no network or file system writes outside tempdir().
- No non-CRAN packages in Imports; Suggests are optional and not required for examples/tests.