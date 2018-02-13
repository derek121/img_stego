# Overview
Embed data within image files, a common form of steganography. Currently only supports PNG image files. The embedded data can be anything (but less that 65536 bytes in size).

# Building
This assumes that `rebar` is in your path.

```
$ make deps
$ make
$ make run
```

# Using
## Embedding a File
```
img_png:add_message_from_file(PathToMessageFile, PathToInputPng, PathToOutputPng).
```
Or, using the separate init/add/write functions in `img_png`.

## Extracting a file
```
img_png:read_and_write_message(PathToFileToExtractFrom, PathToOutputFile).
```

Note that the extracted bytes in `PathToOutputFile` could be anything. The `file` utility can be used to determine its type, if it's not known.

The `img_png` module contains additional functions to embed data referenced by a variable (as opposed to a file), extract into a variable, among others.

# Embedding options
An Options list parameter may be passed to the `add_message*` functions, containing some or all of the following.

* `offset_pct`: The percentage of the total image at which the embedded data should start. This allows placing the data in parts of the source image that are noisier in appearance than others. Default is 0.
* `spacing`: How many bytes apart the message data is placed. This allows spreading out the noise for a smaller visual difference. Default: 1.
* `bits_per_byte`: How many low-order bits of the image should be used to hold the bits of the message. A higher number allows for larger messages, but increases the visual noise. Default: 1.

**Example:**  
`[{offset_pct, 10}, {bits_per_byte, 2}]`

# Possible improvements
* Increasing the maximum supported message length. Currently it's 65535 due to the length being stored in 16 bits.
* Support for other image types.
* Fix/change the module name prefixes.


