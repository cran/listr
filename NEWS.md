# Version 0.0.2 (2022-04-21)
* Added `list_bind_all()`, providing a short-cut for the pattern
```R
l |> 
    list_bind(everything()) |> 
    list_extract(1)
```
* Added `list_is_same_class()` and `list_is_compatible_class()` allowing to
test whether all elements of a list are the same or potentially compatible
classes
