# Upcoming release notes

* NOTE: Unicode viewer (TipHTMLPanel) does not correctly support multiple character Unicode sequences under Windows. These are needed to fully support CP777/778. This control will be replaced with another or custom viewer in the next release.
* Minor adjustment to DOS Font for better support of alternative hyphens in CP777.
* Automatic importing of older version config file. (not Session Data or Display Settings)
* When locale is unknown and no codepage is detected or preferred, CP437 is selected.
* Option to Auto Select codepage switches between: Enabled = Preferred, Disabled = Detected.
* Full support for codepages 777 & 778 with "unmappable" characters.
* Export to Unicode and Codepage for "unmappable" characters.
* Visual support for "unmappable" characters in in Unicode View.
* Unmappable codepage character expansion and contraction.
* Add CP437 when filtered and no compatible CP is found.

