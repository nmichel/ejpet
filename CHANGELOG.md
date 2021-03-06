# 0.8.0

Matching object key again regex.
Bugfixes and some changes to help in crafting compile-time matcher generation in Exjpet.

### Bug fix(es)
* Fix key of `<>` (any iterable) as it collided with key of `<_>` (iterable with one any value)
* Fix parsing of object entries matchers. Generators modified accordingly.

### New feature(s)
* Allow for matching object key with a regex.
* Retrieve all cached entries from the cache server.
* Clear all entries of the cache server.

### Breaking change(s)
* `epm()` type changes for `{ejpet, backend(), {gen(), key()}`}.
 
 
    where

    * `backend()` is the name of the backend upon which the name of the generator module has been crafted,
    * `gen()` is the opaque type of the value returnde by the generator,
    * `key()` is the key for the whole pattern. 

    Function `ejpet:run/3` implementation has been changed so that API user has nothing to change.

# 0.7.1

Bugfix version to help in crafting Exjpet.

### New feature(s)
* Make value `nil` matches pattern expression `null`.
* Use maps to store collect captures in `jsone` backend generator.

# 0.7.0

### New feature(s)
* Published as [hex](https://www.hex.pm "hex package manager") package. 

### Breaking change(s)
* ```ejpet``` does not have any JSON decoder/encoder in its dependencies anymore.
  User must ensure he/she has one of the supported JSON codecs in its BEAM load path.

# 0.6.0

### Bug fix(es)
* Something was wrong when no (k,v) pairs of an object were matching a (k,v) pair matching pattern.

### New feature(s)
* Add support for ```jsone```

# 0.5.0

### Bug fix(es)
* Empty list ```[]``` used to match pattern ```[_]```. Shouldn't. (#5)
* mochijson backend did not return captures in the required order.
* jsx backend was flawed when it came to match objects with pattern ```[*]``` (any list) : it shouldn't match but it did.

### Breaking change(s)

* Captures are returned in backend's format for JSON objects.
* Captures order may differ because of the fix done in backends.

### New feature(s)
* Add helper functions to manipulate result of the capturing process:
    * ```ejpet:get_status/1```
    * ```ejpet:get_captures/1```
    * ```ejpet:get_capture/2```, ```ejpet:get_capture/3```
    * ```ejpet:empty_capture_set/0```, ```ejpet:empty_capture_set/1```
* Add types and functions specifications in module ```ejpet```. (#3)

# 0.4.0

### Bug fix(es)
* Fix generator for list matching : matching functions produced for patterns like ```'[*, 42, "foo"]'``` were flawed.

### Breaking change(s)
* Remove shorten pattern forms ```*/``` and ```**/``` because they lead to ambiguous cases when combined with global search marker. E.g. ```*/**/42/g``` is ambiguous when it comes to know to which between ```*/``` and ```**/``` the ```/g``` tag applies to.

### New feature(s)
* Add new pattern form ```<! c1, c2, ... cn !>[/g]```, enhanced version of now deprecated shorten form ```**/c``` with possibly several conditions.

# 0.3.0

### Bug fix(es)
* Fix ambiguous global search marker positionning in iterable patterns.

### Breaking change(s)
* Remove positionnal capturing
* Global search marker ```'/g'``` in "iterable" pattern must now appear after the trailing ```>```.
  Was ```'<.../g>'```, now is ```'<...>/g'```

### New feature(s)
* Memoization

# 0.2.0

###  New feature(s)
* Add global capture

### Breaking change(s)
* Captures are now returned as {name, capture_list} pairs
