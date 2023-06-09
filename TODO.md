### Known bugs
- remapped files need to be updated in the notebook
  - remapped files are still marked as duplicate until restart
- renaming files may leave behind empty directories

### Very high priority
- make file paths system independent (using FileUtils)
- build snap package
- copy dragged files from system file manager
- DnD icon is off

### High priority
- tag management (subtags, aliases, retagging)
- abstract sublibraries (a library defined by a tag predicate)

### Medium priority
- Search for file by md5 on libgen:
  - https://libgen.li/json.php?object=f&md5=...
    -> editions -> ... -> e_id

### Low priority
- add Mathematical subject classification (MSC2020)
- add entry completions to editor for tags and authors
- Figure out searching zbMath database

### Very low priority
- add progress bar to load
- trees for multiple works ie. volumes/editions/appendices
- trees for subdirectories
- save metadata to pdf/djvu/etc file
