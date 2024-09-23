# Test environments

- Local R 4.4.1 installation on macOS 15.0 (intel chip)
- macOS 14.6.1, R 4.4.1 (via Github Actions)
- M1mac, R 4.4.1 (via mac-builder)
- Microsoft Windows Server 2022, R 4.4.1 (via Github Actions)
- Microsoft Windows Server 2022, R-devel (via win-builder)
- Ubuntu 22.04.5, R-devel (via Github Actions)
- Ubuntu 22.04.5, R 4.4.1 (via Github Actions)
- Ubuntu 22.04.5, R 4.3.3 (via Github Actions)

# R CMD check results

0 errors ✔ \| 0 warnings ✔ \| 1 note

## Comments on check results

The note is that this is considered a new submission because the package was previously archived.
This is because paleopop's dependency, poems, was archived. poems is now back on CRAN so paleopop
can also go back up.