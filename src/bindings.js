"use strict";

exports.toArray = function (list) {
  return function () {
    //still not working, perhaps need to check how map is implmented on this
    //also tried return Array.prototype.slice.call(list);
    return Array.from(list);
  };
};
