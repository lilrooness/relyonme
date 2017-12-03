var env = {
    friction: 0.01,
    gravity: {
        x: 0,
        y: 0
    }
};

var Entity = function(x, y) {
      this.x = x;
      this.y = y;

      this.xacc = 0;
      this.yacc = 0;

      this.xvel = 0;
      this.yvel = 0;

};

Entity.prototype.apply_force = function (force) {
      this.xacc += force.x;
      this.yacc += force.y;

};

Entity.prototype.updatex = function() {
      this.xvel += this.xacc;
      this.x += this.xvel;
      this.xacc = 0;

};

Entity.prototype.updatey = function() {
      this.yvel += this.yacc;
      this.y += this.yvel;
      this.yacc = 0;

};

