library(R6)


SomeClass = R6Class(
  # Private Attribute:
  private = list(attr = NULL),
  public some class= list(
    # Offentliche Methoden
    # Konstruktor:
    # @param attr string
    # @return some class
    # @export
    initialize = function(attr = 'attr') {
      private$attr = attr
    }
  ),
  active = list(
    # Properties
    # @return string of attr.
    # @export
    toString = function() private$attr
  )
)
