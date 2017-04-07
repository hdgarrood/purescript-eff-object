"use strict";

function Example() {
  this.foo = 3;
  this.bar = "wut";
}

exports.mkExample = function() {
  return new Example();
}
