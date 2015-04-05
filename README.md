# jsonschema-gen

`jsonschema-gen` is a Haskell library to generate JSON Schema that is based on your existing Haskell datatype.
You can use generated schema to validate data in json format with the Haskell datatype.

This package provides

* An AST type `Schema` defining the structure of a JSON object.
* A type class `JSONSchemaGen` to generate a schema for a type.
* Functions generating JSON Schema in json format as a draft v4 schema.
* Generated schema can be used for validating json data that are from and to [aeson](http://hackage.haskell.org/package/aeson) value.

