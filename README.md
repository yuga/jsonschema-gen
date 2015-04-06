# jsonschema-gen

`jsonschema-gen` is a Haskell library to generate JSON Schema that is based on your existing Haskell datatype.
Generated schema can be extracted in JSON format. You can use it with any tools that support JSON Schema file.

This library provides

* Functions generating JSON Schema that conforms to the draft v4 schema in json format.
* Generated schema can be used with any tools that support for JSON Schema in order to validate json data
  that are converted from and to [aeson](http://hackage.haskell.org/package/aeson) value.
