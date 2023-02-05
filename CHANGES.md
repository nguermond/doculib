# Changes

## v1.3.4
* Remove tag column from metadata search
* bug fix: existing metadata was overwritten when metadata was searched

## v1.3.3
* add ability to search for and copy BibTex from DOI
* add field for notes to each entry
* updates library version from 3.0 to 3.1
* bug fix: year was not properly saved after retrieving metadata
* add menu for relaxed or compact view
* updated help formatting

## v1.3.2
* fix bug preventing moving and deleting of multiple entries

## v1.3.1
* loading speed of large libraries is now linear in number of duplicates
* add error dialog when trying to open missing file
* renamed libraries are now saved on exit
* add tooltip message to list locations of duplicates
* tabs can now be reordered
* remapping missing entries now attempts to merge

## v1.3.0
* major refactoring of JSON database
* library configuration is now stored in `$XDG_CONFIG_HOME/doculib` (or `$HOME/.config/doculib` if the former does not exist) instead of `$HOME/.doculib`
* library metadata is now stored relative to each library in `/path/to/library/.metadata/`
* searching for moved/renamed/duplicate files is now $O(n)$ instead of $O(n^2)$, and search will span across all libraries
* add "copy file name" and "copy file path" to the context menu options
* add option to remove library without removing metadata
* highlight or mark duplicate and missing entries
* can only migrate metadata from v1.2.1. Upgrade to v1.2.1 before upgrading to this version

## v1.2.1
* icon is now encoded in source (no longer needs to be installed)
* reorganized new library dialog
* add about dialog
* add help dialog
* remove edit-entry dialog

## v1.2.0
* add md5 hash to each file
* detect file moving/renaming within the same library
* selected files can be dragged and dropped between libraries
* allow renaming libraries
* files are no longer individually imported -> all files in library root path are automatically added to library
* cell sizes should now properly render on MacOS

## v1.1.0
* add library manager to add and remove libraries

## v1.0.1

## v1.0.0
