# Revision history for TaggerGUI

## 0.1.0.0 -- 2022-04-23

* First version. Released on an unsuspecting world.

### 0.1.1.0 -- 2022-04-25

* Added a new configuration table and field: 
  * [descriptor_tree]
    * main_request
      * A text field with the name of a descriptor for the main descriptor tree widget 
      to request when the refresh button is pushed.
* Also added a 'Descriptor' configuration page.
  * Can change the value of descriptor_tree.main_request there.
  * Can also view a similar version of the main descriptor tree widget there.
* Stopped the descriptors in the #UNRELATED# tree widget from sending request events
to the main tree widget when clicked.

### 0.1.2.0 -- 2022-04-26

* Adjusted the Cmd so that, if Solo Tagging Mode is enabled, the shell cmd
will take the file currently previewed as the command's only argument.
If Solo Tagging Mode is not enabled or there is no file previewed then all files in the
selection are arguments to the shell cmd.
  * The argument substitution keyword is still '`%file`'

#### 0.1.2.1 -- 2022-04-27

* Added a command line option `-v, --version` to see the program version.

### 0.1.3.0 -- 2022-04-27

* Added a field to rename descriptors.
  * In the Descriptor configuration page.

### 0.1.4.0 -- 2022-04-30

* Added the FileSelection widget back in.
  * It uses a lazy buffer that can be loaded or flushed for a more responsive GUI.
  * Uses a new config field ```selection.buffer_size = int``` to determine how many elements to load.
  * Added buttons to load, load all, flush, and shuffle the file selection.
* Improved styling of the ImagePreview widget.
  * Added a filename to the top left.
  * Made it a drop target for FileWithTags.
    * Dropping a file from the fileSelection Widget into the image preview widget will open a preview of that file.
      * Does not change the order in which images are cycled via Ctrl-i or -k.
  * Made it a draggable FileWithTags.
    * There aren't currently any other widgets that are drop targets for this message type.

#### 0.1.4.1 -- 2022-04-30

* Fixed a bug with the lazy selection that would flush the buffer when an image was tagged.
* Fixed a bug where shuffling an empty selection would result in an infinite hang.

------

## 0.2.0.0 -- 2022-05-01

![image](Doc/doc_tan.jpg)
* Added a new Representative File feature:
  * Added a new table `Representative`
    * This table creates links between a descriptor and an image. The purpose of creating
    Representative links is to provide a quintessential example of what a descriptor is meant
    to convey when it tags a file. It is meant to be used as an aid for tagging, rather than querying.
    Shell commands cannot be performed on a Representative file that is being previewed, nor can it be tagged.
  * Representative files are viewed in the `Descriptor` config page.
    * From the Descriptor tree structure of the database, drag a descriptor into the Representative box.
    If there is a representative file for a given Descriptor it will be displayed, otherwise nothing will happen.
  * To create a Representative file, in the main page of the application, drag an image or filename from either the active image preview or the image selection buffer.
    Then drop it onto the descriptor, in the Main descriptor widget on the bottom left, that you want to create a representative link for.
    A box will appear around a given descriptor as you drag a file over it to show you which descriptor a link will be created for.
* Minor fixes involving the lazy selection buffer. When unioning, intersecting, or diffing a new query.
The buffer will not flush but rather be unioned, intersected, or diffed appropriately with the contents of the new query selection.
* Added new fields to the config file: `style`, `style.font`, and `style.window`.
  * `style.font` takes three keys, `regular`, `thin`, and `bold`. These are paths to a font for tagger to use.
  * `style.window` takes three keys. `maximize`, `window_size_x`, and `window_size_y`.
    * `maximize` is a boolean, if true the window is maximized on start-up.
    The other two keys are the x and y sizes to use for the window if false.
* Moved the `Shell Command` used on startup to the config file under the key `shell_command`.
  * Exporting the config will now export the current shell command as well.

#### 0.2.0.1 -- 2022-05-02

 * Fixed a bug with unioning results into a BufferList. Should hopefully no longer union duplicate files into the selection BufferList.

## 0.3.0.0 -- 2022-05-10

* Introduced Subtagging!
  * Using the image featured in 0.2.0.0, I will demonstrate how subtags may be used.
  * Subtags are added to an image using curly brackets in the Tag textfield. For instance,
  I want to tag the character Momo Chiyoda in the above image but I want to also tag the image with
  descriptors that may only apply to her specifically. For that I will use subtags.
  ```
  Chiyoda_Momo{tanned}
  ```
  With this, we can see that the tanned keyword is only applied to the Chiyoda_Momo tag.
* Queries using Subtags.
  * Querying with subtags can be done normally, as in any file with a queried tag will be part of the new selection.
  * Querying using subtag notation will fetch a selection that contains only files that have that specific sub-tagging combination.
    * If I search `tanned` I will get all images with tanned characters. However, if I search `Chiyoda_Momo{tanned}`
    I will get only the images with that specific combination.
* Draggable tag association.
  * In the Image details pane, for tags on a single image only, each tag is draggable.
    * A new zone has been designated `untag` for these tags. When a tag is drag-and-dropped into that zone,
    that specific tag will be deleted.
      * Untagging can still be done via the Tag text field but is less precise and may end up deleting more than you wanted to.
  * New subtags can be made from existing tags by dragging one tag on top of another, this will place the dragged tag as a 
  sub tag of the target.
* Breaking changes:
  * Any databases created on versions 2.x or lower need to run the Migrate0_2_XTo0_3_X.sql
  script. Any desired subtags will have to be created manually.

### 0.3.1.0 -- 2022-05-12

* Migrated to Monomer 1.4.0.0-dc848e8ddb76ba6d8339b47f8bc3a0ff3982950c.
* Introduced a new config option, `style.window.window_scaling_factor`.
  * A double value that controls the relative size of text and widgets in the window.
    * Values will most likely be in-between 1.0 and 2.0.
* Introduced an optional config option `style.window.window_icon`.
  * Takes a path to .bmp images and uses it as the window icon.
##### Minor Changes
* Added a dispose event to close the database connection.

### 0.3.2.0 -- 2022-05-14

##### Breaking Changes

* Changed some column names in Tag.
* Added a new TaggerDBInfo table for database-level version tracking.

To upgrade, run the script `Migrate0_3_1_0To0_3_2_0.sql` on a database that is up-to-date with tagger v0.3.1.0

#### Non-breaking changes:

* Implemented a new SDL query language to use in the Query textfield!
  * Normal queries as they have been previously written still work.
  * With the new language, you can fit many unions, intersections, and differences in to one query
  * You can also query by Descriptor, Relation, and Pattern in one query.

### Tagger Query Language:

Previously, file patterns, descriptor patterns, and descriptor relation trees could be queried all by text input
in the Query textfield. But if you wanted to search for a tag relationship then difference that by a file pattern query, you had to run two separate queries and interact with the GUI dropdowns to change set arithmetic. With the Tagger Query Language, you can now combine many queries into one.

The rules are simple: Tag search, Relation search, or Pattern search can now specified per-token by prepending a specific operator to the front of the token. For instance, if you wanted to search for files that contain the string "roughdraft" in them, you would normally select the Pattern option from the dropdown then query with the token "%roughdraft%". But with the Tagger Query Language, this can be written as "p.%roughdraft%" and tagger will search by file pattern, regardless of what the current dropdown selection is.

The tokens for these searches are: 
```
Tag      -> t.
Relation -> r.
Pattern  -> p.
```

Unions, intersections, and differences work similary. These tokens are placed inbetween query sections to describe how they are combined together.

The tokens for these operations are:
```
union      -> u|
intersect  -> i|
difference -> d|
```

Here are some examples.

I want to find all files that are tagged with 'Rough_Draft' and are in a folder called 'documents'
```
t.Rough_Draft i| p.%documents%
```

I want to find all files that are related to the tag 'Work' but not the files tagged with 'Side_Project'
```
r.Work d| t.Side_Project
```

I want to find find all images of Yui Otsuki that are subtagged with 'dress' or 'cute' that are not seasonal and do not have white backgrounds
```
t.otsuki_yui {dress r.cute} d| r.seasonal d| image_background{white}
```

From these examples, you can see that the search tokens are optional, if they are not specified, then Tagger uses whatever is selected in the relevant dropdown.

Some restrictions:
* You can not precede a subtag search with a pattern query.
  * `p.%filepattern% {something}` will fail to parse and output a parse error.
* You can not use pattern queries inside subtag searches.
  * `something {p.%filepattern%}`
* You can not do set arithmetic inside a subtag search, though this is planned as a future feature. By default, all results from individual subtags are unioned together.
  * `something {this d| that}` fails to parse and prints a parse error.