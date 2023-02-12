let help_text=
"<big><b>Libraries</b></big>

• Each library points to a unique directory. Each library must 
  have a unique document type, which indicates:
	- Whether to keep track of DOI (for articles) or ISBN (for books)
	- The default database to search for metadata
• Libraries can be managed by selecting the <b>Library</b> menu:
	- A library may be renamed by selecting the name field
	- Removing a library will never remove the physical files in 
	  the library. If <i>Delete metadata</i> is selected, the metadata 
	  will be removed, but not the files
	- A new library may be created by selecting an existing 
	  directory. All files in that directory are automatically 
	  added to that library
• Selected files may be moved between libraries by dragging them 
  to a library tab
• Libraries may be rearranged by dragging tabs

<big><b>Entries</b></big>

• Right click on an entry for the following options:
	- <b>Open</b> -- opens the file
	- <b>Search Metadata</b> -- Search for metadata on either 
	  <a href=\"https://openlibrary.org\">openlibrary.org</a> (for books) or <a href=\"https://semanticscholar.org\">semanticscholar.org</a> 
	  (for articles). The default search query is either the 
	  title (if nonempty) or the name of the file. It typically 
	  works best to use the full title as search query, 
	  without authors, date, etc.
	- <b>Open DOI</b> -- if the file has a DOI, open the doi address 
	  in a web browser
	- <b>Copy BibTex</b> -- if the file has a DOI, search for the BibTex 
	  entry at <a href=\"https://crossref.org\">crossref.org</a>. Currently, this does not work for
	  entries without DOI
	- <b>Edit Notes</b> -- add and edit notes for an entry;
	  notes will appear in tooltip
	- <b>Copy File Name</b> -- copy file name to clipboard
	- <b>Copy File Path</b> -- copy absolute file path to clipboard
	- <b>Delete File</b> -- delete physical file and its metadata
• Left click on an entry's field to edit
	- Authors and tags must be separated by semicolons

<big><b>Selecting and Moving Entries</b></big>

• Multiple entries may be selected by holding <span foreground=\"green\">SHIFT</span> or <span foreground=\"green\">CONTROL</span>
• Entries may be moved to another library by dragging the
  selected entries to another tab. This will move both the files and
  their associated metadata. If an entry is a duplicate of an existing
  entry in the library it is moved to, they will be merged if possible

<big><b>Missing/Duplicate Files</b></big>

• If multiple entries refer to identical files (irrespective of 
  their file names), they will be marked as duplicates
• If an entry refers to a file which is missing, and the file cannot 
  be found, it will be marked as missing. If the missing file 
  is found, the entry will be appropriately remapped
"
