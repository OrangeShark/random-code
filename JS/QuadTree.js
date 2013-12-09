/**
 * QuadTree implementation in JavaScript
 * Based off of http://en.wikipedia.org/wiki/Quadtree
 */

function QuadTree(bbox) {
  this.quad = {
    NW: null,
    NE: null,
    SW: null,
    SE: null
  }
  this.bbox = bbox;
  this.points = [];
}

QuadTree.prototype.CAPACITY = 4;

QuadTree.prototype.insert = function( point ) {
  if( !this.bbox.containsPoint(point.key) ) {
    return false;
  }

  if( this.quad.NW === null ) {
    if( this.points.length < this.CAPACITY ) {
      this.points.push(point);
      return true;
    } else {
      this.subdivide();

      for( var i = 0; i < this.points.length; i++) {
        var p = this.points[i];
        this.quad.NW.insert(p);
        this.quad.NE.insert(p);
        this.quad.SW.insert(p);
        this.quad.SE.insert(p);
      }
      this.points = [];

    }
  }

  if(this.quad.NW.insert(point) ) return true;
  if(this.quad.NE.insert(point) ) return true;
  if(this.quad.SW.insert(point) ) return true;
  if(this.quad.SE.insert(point) ) return true;

  return false;
};

QuadTree.prototype.subdivide = function() {
  var halfwidth = ((this.bbox.br[0] - this.bbox.tl[0]) / 2),
      halfheight = ((this.bbox.tl[1] - this.bbox.br[1]) / 2);

  var center = [this.bbox.tl[0] + halfwidth, this.bbox.tl[1] - halfheight];
  this.quad.NW = new QuadTree(new BBox(this.bbox.tl, center));
  this.quad.NE = new QuadTree(new BBox([center[0], this.bbox.tl[1]], [this.bbox.br[0], center[1]]));
  this.quad.SW = new QuadTree(new BBox([this.bbox.tl[0], center[1] ] , [ center[0], this.bbox.br[1] ]));
  this.quad.SE = new QuadTree(new BBox(center, this.bbox.br));
};

QuadTree.prototype.queryRange = function(range) {
  var results = [];

  if( !this.bbox.intersectBBox(range) ) {
    return results;
  }

  for( var i = 0; i < this.points.length; i++ ) {
    if( range.containsPoint(this.points[i].key) ) {
      results.push(this.points[i]);
    }
  }

  if( this.quad.NW == null ) {
    return results;
  }

  results = results.concat(this.quad.NW.queryRange(range));
  results = results.concat(this.quad.NE.queryRange(range));
  results = results.concat(this.quad.SW.queryRange(range));
  results = results.concat(this.quad.SE.queryRange(range));

  return results;
};

function BBox(tl, br) {
  this.tl = tl;
  this.br = br;
};

BBox.prototype.containsPoint = function( point ) {
  if( point[0] >= this.tl[0] && point[0] <= this.br[0] ) {
    if( point[1] <= this.tl[1] && point[1] >= this.br[1] ) {
      return true;
    }
  }
  return false;
};

BBox.prototype.intersectBBox = function( other ) {
  return (!( this.br[0] < other.tl[0] ||
        this.tl[0] > other.br[0] ||
        this.br[1] > other.tl[1] ||
        this.tl[1] < other.br[1]));
};
