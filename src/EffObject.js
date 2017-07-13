"use strict";

exports.unsafeReadProperty = function unsafeReadProperty(prop) {
  return function(obj) {
    return function() {
      return obj[prop];
    };
  };
};

exports.unsafeWriteProperty = function unsafeWriteProperty(prop) {
  return function(obj) {
    return function(val) {
      return function() {
        obj[prop] = val;
      };
    };
  };
};

exports.unsafeBindTo = function(fn) {
  return function(target) {
    return fn.bind(target);
  };
};
