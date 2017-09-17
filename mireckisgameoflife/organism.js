var VEGETATION_SPREAD = 0.002;
var TURN_SPEED_MS = 10;

var SCALE = 10;
var LAKE_NUMBER = 1;
var LAKE_SIZE_MIN = 3;
var LAKE_SIZE_MAX = 10;
var RIVER_TURN_RATE = 0.1;
var RIVER_NUMBER = 1;
var MATING_SUCCESS_RATE = 0.8;

// Note: Don't call this yourself! Use one of the global directions _always_!
function Direction(x, y) {
    this.x = x;
    this.y = y;
}

Direction.none  = new Direction(0, 0);
Direction.north = new Direction(0, -1);
Direction.south = new Direction(0, 1);
Direction.east  = new Direction(1, 0);
Direction.west  = new Direction(-1, 0);

Direction.random = function() {
    return ([Direction.north, Direction.south, Direction.east, Direction.west]
            [Math.floor(Math.random() * 4)]);
}

Direction.prototype.randomTurn = function() {
    return (this.y == 0)
        ? (Math.random() < 0.5 ? Direction.north : Direction.south)
        : (Math.random() < 0.5 ? Direction.east  : Direction.west);
}

Direction.none.randomTurn = function() {
    return Direction.random();
}

Direction.none.turnLeft = function() {
    return Direction.random();
}
Direction.north.turnLeft = function() {
    return Direction.west;
}
Direction.west.turnLeft = function() {
    return Direction.south;
}
Direction.south.turnLeft = function() {
    return Direction.east;
}
Direction.east.turnLeft = function() {
    return Direction.north;
}

/**
 * Random percentege between min and max (inclusive).
 */
function randomPercentInRange(min, max) {
    var minInt = Math.floor(min * 100);
    var maxInt = Math.floor(max * 100);
    return Math.round((Math.random() * (maxInt - minInt)) + minInt) / 100;
}

function Tile(x, y) {
    this.x             = x;
    this.y             = y;
    this.hasVegetation = true;
    this.hasWater      = false;
    this.organism      = null;
}

Tile.prototype.update = function(board) {
    if (this.hasVegetation) {
        if (this.x > 0 && Math.random() < VEGETATION_SPREAD) {
            board.spreadVegetation(this.x - 1, this.y);
        }
        if (this.y > 0 && Math.random() < VEGETATION_SPREAD) {
            board.spreadVegetation(this.x, this.y - 1);
        }
        if (this.x < board.width - 1 && Math.random() < VEGETATION_SPREAD) {
            board.spreadVegetation(this.x + 1, this.y);
        }
        if (this.y < board.height - 1 && Math.random() < VEGETATION_SPREAD) {
            board.spreadVegetation(this.x, this.y + 1);
        }
    }
}

Tile.prototype.render = function(ctx) {
    ctx.beginPath();
    ctx.rect(this.x * SCALE, this.y * SCALE, SCALE, SCALE);
    ctx.fillStyle = this.getColor();
    ctx.fill();
}

Tile.prototype.getColor = function() {
    if (this.hasWater) {
        return this.hasVegetation ? '#0f0' : '#ff0';
    } else {
        return this.hasVegetation ? '#070' : '#770';
    }
}

function Organism(parent1, parent2) {
    // Coordinates are set at birth
    this.x             = undefined;
    this.y             = undefined;
    this.sex           = Math.random() < 0.5 ? 'm' : 'f';
    this.age           = 0;
    this.hunger        = 0.0;
    this.direction     = Direction.random();
    this.childOrganism = null;
    this.pregnantTimer = 0;
    this.matingTimer   = 0;

    // Default base stats - percentages
    // Size is restrained to [1%, 100%], the other can be [0%, 100%]
    // I set the default to be all identical to speed up initial mating
    this.sizeStat        = 0.5;
    this.aquaticStat     = 0.5;
    this.carnivorousStat = 0.9;

    // The parents are optional - we manually generate some misc organisms
    if (parent1 === undefined) {
        return;
    }

    // FIXME Add the aquatic stat eventually
    //var stats = ['sizeStat', 'aquaticStat', 'carnivorousStat'];
    var stats = ['sizeStat', 'carnivorousStat'];
    for (var i = 0; i < stats.length; i++) {
        var name = stats[i];
        var min = Math.max(Math.min(parent1[name], parent2[name]) - 0.01, 0);
        if (name == 'sizeStat' && min == 0) {
            min = 0.01;
        }
        var max = Math.min(Math.max(parent1[name], parent2[name]) + 0.01, 1);
        this[name] = randomPercentInRange(min, max);
    }
}

Organism.prototype.getColor = function() {
    return tinycolor({
        // Hue between blue and red (240 deg and 360 deg)
        h: 240 + (360 - 240) * (1 - this.aquaticStat),
        s: 0.3 + 0.7 * this.sizeStat,
        // Never have pure black, which hides the hue & saturation
        v: 0.3 + 0.7 * this.carnivorousStat,
    }).toHexString();
}

Organism.prototype.getAggression = function() {
    return this.hunger * this.carnivorousStat;
}

Organism.prototype.isMating = function() {
    return this.matingTimer > 0;
}

Organism.prototype.isPregnant = function() {
    return this.pregnantTimer > 0;
}

Organism.prototype.isMature = function() {
    return this.age > this.sizeStat * 100;
}

Organism.prototype.isFertile = function() {
    return this.pregnantTimer == 0 && this.isMature();
}

Organism.prototype.canMoveTo = function(board, x, y) {
    if (x < 0 || x >= board.width || y < 0 || y >= board.height) {
        return false;
    }
    if (board.tiles[y][x].organism != null) {
        return false;
    }
    return true;
}

Organism.prototype.tryMove = function(board, direction) {
    var newX = this.x + direction.x;
    var newY = this.y + direction.y;

    if ( ! this.canMoveTo(board, newX, newY)) {
        return false;
    }

    board.moveOrganism(this, direction);
    return true;
}

// If the organism has nothing better to do, it will wander semi-randomly
Organism.prototype.wander = function(board) {
    var direction = this.direction;

    if (Math.random() < 0.1) {
        direction = this.direction.randomTurn();
    }

    // Try finding a valid direction
    for (var i = 0; i < 4; i++) {
        if (this.tryMove(board, direction)) {
            return;
        }
        direction = Direction.random();
    }
}

Organism.prototype.canMate = function(organism) {
    if ( ! this.isFertile() || ! organism.isFertile()) {
        return false;
    }
    if (organism.sex == this.sex) {
        return false;
    }
    if (organism.isMating()) {
        return false;
    }
    
    var meanDiff = (Math.abs(organism.sizeStat - this.sizeStat) +
                    Math.abs(organism.carnivorousStat - this.carnivorousStat) +
                    Math.abs(organism.aquaticStat - this.aquaticStat)) / 3;
    
    return meanDiff < 0.02;
}

Organism.prototype.tryMating = function(board, organism) {
    if (Math.random() < MATING_SUCCESS_RATE) {
        // Wait 5 turns
        this.matingTimer = organism.matingTimer = 5;

        if (this.sex == 'f') {
            this.setPregnant(organism);
        } else {
            organism.setPregnant(this);
        }

        return true;
    }
    return false;
}

Organism.prototype.setPregnant = function(father) {
    this.childOrganism = new Organism(this, father);
    this.pregnantTimer = 5 + Math.round(this.sizeStat * 100);
}

Organism.prototype.giveBirth = function(board, adjacentTiles) {
    var child = this.childOrganism;
    this.childOrganism = null;

    // If there is no empty adjacent, the child will die at birth
    for (var i = 0; i < adjacentTiles.length; i++) {
        var tile = adjacentTiles[i];
        if (tile.organism == null) {
            board.addOrganismToTile(child, tile);
            return;
        }
    }
}

Organism.prototype.directionOfTile = function(tile) {
    if (this.x < tile.x) {
        return Direction.east;
    } else if (this.x > tile.x) {
        return Direction.west;
    } else if (this.y < tile.y) {
        return Direction.south;
    } else if (this.y > tile.y) {
        return Direction.north;
    }
    return Direction.none;
}

Organism.prototype.tryAttacking = function(board, adjacentTiles) {
    for (var i = 0; i < adjacentTiles.length; i++) {
        var tile = adjacentTiles[i];
        // FIXME use the defence/attack probability stuff
        // Eat another organism!
        if (tile.organism != null) {
            var direction = this.directionOfTile(tile);
            board.removeOrganism(tile.organism); // Eat it
            board.moveOrganism(this, direction);

            this.hunger -= this.carnivorousStat / this.sizeStat;
            return true;
        }
    }
    return false;
}

Organism.prototype.tryGrazing = function(board, adjacentTiles) {
    var tile = board.tiles[this.y][this.x];

    if (tile.hasVegetation) {
        tile.hasVegetation = false;
        this.hunger -= (1 - this.carnivorousStat) / this.sizeStat;
        return true;
    }

    for (var i = 0; i < adjacentTiles.length; i++) {
        var tile = adjacentTiles[i];
        if (tile.hasVegetation && this.canMoveTo(board, tile.x, tile.y)) {
            var direction = this.directionOfTile(tile);
            board.moveOrganism(this, direction);
            return true;
        }
    }

    return false;
}

Organism.prototype.move = function(board) {
    if (this.age == 0) {
        return;
    }

    if (this.isMating()) {
        this.matingTimer--;
        return;
    }

    var adjacentTiles = board.getAdjacentTiles(this.x, this.y);
    var adjacentOrganisms = [];

    for (var i = 0; i < adjacentTiles.length; i++) {
        var organism = adjacentTiles[i].organism;
        if (organism !== null) {
            adjacentOrganisms.push(organism);
        }
    }

    if (Math.random() < this.hunger) {
        if (Math.random() < this.getAggression()) {
            if (this.tryAttacking(board, adjacentTiles)) {
                return;
            }
        } else {
            if (this.tryGrazing(board, adjacentTiles)) {
                return;
            }
        }
    }

    if (this.isPregnant()) {
        this.pregnantTimer--;
        if (this.pregnantTimer == 0) {
            this.giveBirth(board, adjacentTiles);
        }
    } else {
        // The way of the wild
        for (var i = 0; i < adjacentOrganisms.length; i++) {
            var organism = adjacentOrganisms[i];
            if (this.canMate(organism)) {
                if (this.tryMating(board, organism)) {
                    return;
                } else {
                    break;
                }
            }
        }
    }

    this.wander(board);
}

Organism.prototype.update = function(board) {
    board.tiles[this.y][this.x].organism = null;
    this.move(board);
    board.tiles[this.y][this.x].organism = this;

    this.age++;

    // Sadness... death...
    if (this.hunger >= 1) {
        board.removeOrganism(this);
    }

    if (this.age > this.sizeStat * 100 * 20) {
        board.removeOrganism(this);
    }

    this.hunger += 0.02 * this.sizeStat;
}

Organism.prototype.render = function(ctx) {
    ctx.beginPath();

    ctx.moveTo(this.x * SCALE, this.y * SCALE + SCALE/2);

    // Top two corners, which indicate male / female
    if (this.sex == 'f') {
        ctx.arc(this.x * SCALE + SCALE/2, this.y * SCALE + SCALE/2,
                SCALE/2, Math.PI, 1.5 * Math.PI);
        ctx.lineTo(this.x * SCALE + SCALE, this.y * SCALE);
        ctx.lineTo(this.x * SCALE + SCALE, this.y * SCALE + SCALE/2);
    } else {
        ctx.lineTo(this.x * SCALE, this.y * SCALE);
        ctx.lineTo(this.x * SCALE + SCALE/2, this.y * SCALE);
        ctx.arc(this.x * SCALE + SCALE/2, this.y * SCALE + SCALE/2,
                SCALE/2, 1.5 * Math.PI, 0);
    }

    ctx.lineTo(this.x * SCALE + SCALE, this.y * SCALE + SCALE);
    ctx.lineTo(this.x * SCALE + SCALE/2, this.y * SCALE + SCALE);

    if (this.isPregnant()) {
        ctx.arc(this.x * SCALE + SCALE/2, this.y * SCALE + SCALE/2,
                SCALE/2, 0.5 * Math.PI, Math.PI);
    } else {
        ctx.lineTo(this.x * SCALE, this.y * SCALE + SCALE);
        ctx.lineTo(this.x * SCALE, this.y * SCALE + SCALE/2);
    }

    ctx.fillStyle = this.getColor();
    ctx.fill();

    ctx.strokeStyle = "black";
    ctx.lineWidth   = 1;
    ctx.stroke();
}

function Board($container, width, height) {
    this.width = width;
    this.height = height;
    this.$container = $container;
    this.tiles = [];
    this.organisms = [];

    for (var y = 0; y < height; y++) {
        var row = [];
        for (var x = 0; x < width; x++) {
            var tile = new Tile(x, y);
            row.push(tile);
        }
        this.tiles.push(row);
    }

    for (var i = 0; i < LAKE_NUMBER; i++) {
        this.generateNewLake();
    }
    for (var i = 0; i < RIVER_NUMBER; i++) {
        this.generateNewRiver();
    }

    for (var i = 0; i < 100; i++) {
        this.addOrganismToTile(new Organism(),
                               this.tiles[Math.floor(i / 10)][i % 10])
    }

    this.update();
}

Board.prototype.removeOrganism = function(organism) {
    this.tiles[organism.y][organism.x].organism = null;
    this.organisms.splice(this.organisms.indexOf(organism), 1);
}

Board.prototype.addOrganismToTile = function(organism, tile) {
    organism.x = tile.x;
    organism.y = tile.y;
    tile.organism = organism;
    this.organisms.push(organism);
}

// Assumes it is a valid movement.
Board.prototype.moveOrganism = function(organism, direction) {
    var newX = organism.x + direction.x;
    var newY = organism.y + direction.y;

    this.tiles[organism.y][organism.x].organism = null;
    this.tiles[newY][newX].organism = organism;

    organism.x = newX;
    organism.y = newY;
    organism.direction = direction;
}

Board.prototype.generateNewRiver = function() {
    var x = Math.floor(Math.random() * this.width);
    var y = Math.floor(Math.random() * this.height);

    // Randomly select an edge to start at
    var direction = Direction.random();
    if (direction == Direction.east) {
        x = 0;
    } else if (direction == Direction.west) {
        x = this.width - 1;
    } else if (direction == Direction.south) {
        y = 0;
    } else {
        y = this.height - 1;
    }

    while (x >= 0 && y >= 0 && x < this.width && y < this.height) {
        var tile = this.tiles[y][x];
        if (tile.hasWater) {
            // Don't allow crossing water!
            break;
        }
        this.tiles[y][x].hasWater = true;

        if (Math.random() < RIVER_TURN_RATE) {
            direction = direction.randomTurn();
        }

        x += direction.x;
        y += direction.y;
    }
}

Board.prototype.generateNewLake = function() {
    var originX = Math.floor(Math.random() * this.width);
    var originY = Math.floor(Math.random() * this.height);

    // TODO: Randomize the shape and size of the lake a bit more nicely
    // NOTE: I made hacky square for now, just to get the ball rolling

    var width = Math.floor(Math.random() * (LAKE_SIZE_MAX - LAKE_SIZE_MIN)) +
        LAKE_SIZE_MIN;
    var height = Math.floor(Math.random() * (LAKE_SIZE_MAX - LAKE_SIZE_MIN)) +
        LAKE_SIZE_MIN;

    for (var y = originY; y < originY + height; y++) {
        if (y >= this.height) {
            break;
        }
        for (var x = originX; x < originX + width; x++) {
            if (x >= this.width) {
                break;
            }
            
            this.tiles[y][x].hasWater = true;
        }
    }
}

Board.prototype.update = function() {
    for (var y = 0; y < this.height; y++) {
        for (var x = 0; x < this.width; x++) {
            this.tiles[y][x].update(this);
        }
    }
    
    for (var i = 0; i < this.organisms.length; i++) {
        this.organisms[i].update(this);
    }
}

Board.prototype.render = function(ctx) {
    for (var y = 0; y < this.height; y++) {
        for (var x = 0; x < this.width; x++) {
            this.tiles[y][x].render(ctx);
        }
    }

    for (var i = 0; i < this.organisms.length; i++) {
        this.organisms[i].render(ctx);
    }
}

Board.prototype.spreadVegetation = function(x, y) {
    this.tiles[y][x].hasVegetation = true;
}

Board.prototype.getAdjacentTiles = function(x, y) {
    var tiles = [];
    if (x > 0) {
        tiles.push(this.tiles[y][x - 1]);
    }
    if (x < this.width - 1) {
        tiles.push(this.tiles[y][x + 1]);
    }
    if (y > 0) {
        tiles.push(this.tiles[y - 1][x]);
    }
    if (y < this.height - 1) {
        tiles.push(this.tiles[y + 1][x]);
    }
    return tiles;
}

function init($container) {
    var board = new Board($container,
                          $container.width() / SCALE,
                          $container.height() / SCALE);
    var canvas = document.getElementById("board");
    canvas.width  = $container.width();
    canvas.height = $container.height();

    var ctx = canvas.getContext("2d");

    var tick = 0;

    var isUpdating = false;
    setInterval(function() {
        if (isUpdating) {
            return;
        }
        isUpdating = true;

        board.update();
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        board.render(ctx);
        isUpdating = false;

        if (++tick == 100) {
            var carnivorousStatSum = 0;
            for (var i = 0; i < board.organisms.length; i++) {
                var organism = board.organisms[i];
                carnivorousStatSum += organism.carnivorousStat;
            }
            console.log("Average carnivorous stat: " +
                        carnivorousStatSum / board.organisms.length);
            tick = 0;
        }
    }, TURN_SPEED_MS);
}
