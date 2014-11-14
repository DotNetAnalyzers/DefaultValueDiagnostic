# Default Parameter Value Diagnostic

This analyzer extends the .NET Compiler Platform to analyze the default values assigned to method parameters for certain
errors which are likely to affect users. The following diagnostic messages are currently reported.

| Diagnostic | Description |
| --- | --- |
| `MissingDefaultValue` (with code fix) | A method overrides or implements a method which contains a default value for a parameter `x`, but the override does not specify a default value for the same parameter `x`. |
