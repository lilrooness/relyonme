var Particle = function(x, y, ttl) {
	this.entity = new Entity(x, y);
	this.colour = {
		"r": 255,
		"g": 255,
		"b": 255
	}; 
}

Particle.prototype.update = function() {
	this.entity.updatex();
	this.entity.updatey();
}

Particle.prototype.render = function(context) {
	context.fillRect(this.x, this.y, 10, 10);
}

var Explosion = function(x, y, size) {
	this.particles = [];

	for(var i=0; i<size; i++) {
		var p = new Particle(x, y);
		p.entity.apply_force({
			x: (Math.random() * 2) - 1,
			y: (Math.random() * 2) - 1
		})
		this.particles.push()
	}
}

Explosion.prototype.render = function(context) {
	for(var i=0; i<particles.length; i++) {
		particles[i].render(context);
	}
}