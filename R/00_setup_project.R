# R/00_setup_project.R

# One-time project setup
usethis::use_git_ignore(c(".Rhistory", ".RData", ".DS_Store", ".targets",
                          "data/processed/"))
usethis::use_readme_md()
usethis::use_directory("R")
usethis::use_directory("data/raw")
usethis::use_directory("data/processed")
usethis::use_directory("notebooks")
usethis::use_directory("outputs/figures")
usethis::use_directory("outputs/tables")
usethis::use_directory("tests")

# Optional placeholder files so folders show up
file.create("R/.keep", "data/raw/.keep", "data/processed/.keep",
            "notebooks/.keep", "outputs/figures/.keep",
            "outputs/tables/.keep", "tests/.keep")
