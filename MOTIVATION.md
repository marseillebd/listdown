LIST APP

There are lots of things that are (hierarchical) lists:
- project action breakdowns
- checklists (e.g. packing)
- inventories of consumables (e.g. foods, wiring)
  - should be convertible into shopping lists
- conversations with tangents (i.e. Antar)
- recipes and other instructions
  - what items are ingredients can be extracted by detecting number + unit + name of ingredient + optional preparation
  - if you have a list of known kitchen tools, those are easily extractible from the instruction text
  - variations can be listed as a sub-list headed by `(.+, ?)either:`
- material sourcing (what grocery stores have the best ingredients? what produce is in-season? &c)
- flexible databases like Simply Plural, or a lexicon
- curation (i.e. collect lists of other files, e.g. which recipes are untested/in-progress/known-good/&c)
- &relational possibly relational data, like my relationships field in Simply Plural, but better:
  - the nodes of the graph would be documents (things you can wikilink to, see below)
  - the edges would be given by a list item of a given format
  - data associated with the edges would be given as sub-items of the edge-defining list item


There are a few key improvements over markdown that I see being useful:
- WikiLinks-style links `[[SomeFile]]` to find a document on the filesystem without having to write `[Some File](SomeFile.html)`
  - along with that, a way to specify relative files `[[../cook/orange-chicken]]`, or start from some root directory `[[~/source/grapes]]`
- status indicators in a box at the start of a list item `- [ ]`, `- [x]`,
  and others beyond GFM like `- [?]`, `- [1], `- [in review]`, or anything else you want to throw in the box, like `[✓], [✗]` for fanciness
- gives names/labels to list items (e.g. `- some field:`, `- note: blahblah`) that can identify the type of a node or turn (part of) the list into an asociative (multi-)map
- metadata that drives the interpretation of the file (or all files in a directory) (i.e. what are admissible statuses?, required fields?)
- links to specific list items, which would also require creating identifiers for list items
  (e.g. `- &ident` to define, `[[somefile&ident]]` to reference, where the filename is optional if in same file)

At the start, I want to just build some scripts (bash, Haskell, as long as it startsup fast) that can do more complex edits to the files:
- change the name of a file/directory and compensatorily update wikilinks
- extract information from natural -language text
- check for structural oddities:
  - list item labels or statuses so one can identify unexpected names
  - report locations of specific unexpected names (to correct them in the source)
  - rename names consistently across the document
- attach date(time)s to nodes when they are inserted/edited
- duplicate lists (now I've packed), possibly removing sections
- set up some lists to be removed after a (somewhere) configured date

The first big sea-change in the app is putting it online (editable with a simple text box) so I've got access from anywhere.
Later, there are a number of features that would be very nice:
- reading features:
  - a file browser
  - collapse sub-lists
  - solo/mute sublists (i.e. display-wise remove any display of an item or sublist, or of all sister sublists because e.g. you've selected one option from many)
  - insert linked lists (e.g. where a leaf list item is a wikilink (or perhaps contains a wikilink, display-wise insert the linked document)
  - fancy rendering of statuses? (e.g. write `- [Q]`, but render as "Question")
  - mute/solo items with a given status or label
  - if [[&relational]] can work, then some way to graphically render the (local) connection network
- editing features:
  - edit individual list items as a markdown file separate from display
  - drag-and-drop reordering
  - add/delete items anywhere
  - status dropdown and alter status by click
  - access scripts that perform analyses
  - a file selector that can create wikilinks
- secure, collaborative multi-tenancy
  (this is like the last thing I need, but it could be handy to share house-chores with Mom, recipes with Leslie &c):
  - separate user accounts
  - authentication
  - sharing documents among multiple accounts
  - plugin for plurality
- UX:
  - dark mode
  - feedback when you edit something:
    - the change notification is sent to a scheduler
    - the scheduler batches changes while they're still coming in
    - different icons attached to the item for:
      - update unsaved
      - saving (attempting server communication)
      - saved successfully (which then disappears reasonably quickly)
      - there was an error while save-attempting
    - confirm leaving page if not everything is saved