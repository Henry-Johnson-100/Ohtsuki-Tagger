# Revision history for TaggerGUI

## 0.1.0.0 -- 2022-04-23

* First version. Released on an unsuspecting world.

### 0.1.1.0 -- 2022-04-25

* Added a new configuration table and field: 
  * [descriptor_tree]
    * main_request
      * A text field with the name of a descriptor for the main descriptor tree widget 
      to request when the refresh button is pushed.
  * So now a sample minimum config file (keeping mind that selection.display_parents is still not used)
  for this project to run is:
```
[database]
  auto_connect = true
  backup = ":memory:"
  init = "./taggerSchemaDef.sql"
  path = ":memory:"

[descriptor_tree]
  main_request = "#ALL#"

[selection]
  display_parents = 1
```
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