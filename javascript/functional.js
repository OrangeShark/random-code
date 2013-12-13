/**
 * Example functional programming from
 * Eloquent JavaScript
 * http://eloquentjavascript.net/
 */

/**
 * forEach iterates through an array performing
 * an action on each element
 */
function forEach(array, action){
  for(var i = 0; i < array.length; i++){
    action(array[i]);
  }
}

/**
 * map applies a function to every element
 * in an array
 */
function map(func, array){
  var result = [];
  forEach(array, function(element){
    result.push(func(element));
  });
  return result;
}

/**
 * reduce combines an array to a single value
 * using the combine function
 */
function reduce(combine, base, array){
  forEach(array, function(element){
    base = combine(base, element);
  });
  return base;
}
