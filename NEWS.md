
# gsim 0.1.0.9000

* New functions to retrieve objects from a container: retrieve and
  collect. `retrieve()` returns the objects untouched, while
  `collect()` returns a tidy data.frame (it may need a more suggestive
  name). The non-NSE versions of these functions have names ending
  with an underscore, e.g., `collect_()`.

  The `$` and `[[` operators are shortcuts for `retrieve()` and
  `retrieve_()` respectively. Now, objects can be retrieved as follows:
  `sims$beta`.

* New helper functions for posterior predictive checking. Fixme:
  documentation


# gsim 0.1

Initial release
