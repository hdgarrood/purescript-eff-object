"use strict";

function Example() {
  this.foo = 3;
  this.bar = "wut";
  this.baz = function(x) {
    return this.foo + x;
  }
}

exports.mkExample = function() {
  return new Example();
}
