var Particle = function(x, y, ttl) {
	this.entity = new Entity(x, y);
}

Particle.prototype.update = function() {
	this.entity.updatex();
	this.entity.updatey();
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