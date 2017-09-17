var VEGETATION_SPAWN = 0.01;
var VEGETATION_SPREAD = 0.005;
var TURN_SPEED_MS = 100;
var SCALE = 10;
var LAKE_NUMBER = 1;
var LAKE_SIZE_MIN = 3;
var LAKE_SIZE_MAX = 10;
var RIVER_TURN_RATE = 0.1;
var RIVER_NUMBER = 1;

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
    this.hasVegetation = false;
    this.hasWater      = false;

    this.$element = $('<div/>')
        .addClass('block')
        .width(SCALE)
        .height(SCALE)
        .offset({
            top: this.y * SCALE,
            left: this.x * SCALE,
        });
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

    this.$element
        .css('background-color', this.getColor())
        .offset({
            top: this.y * SCALE,
            left: this.x * SCALE,
        });
}

Tile.prototype.getColor = function() {
    if (this.hasWater) {
        return this.hasVegetation ? '#0f0' : '#ff0';
    } else {
        return this.hasVegetation ? '#070' : '#770';
    }
}

function Organism(x, y, parent1, parent2) {
    this.x   = x;
    this.y   = y;
    this.sex = Math.random() < 0.5 ? 'm' : 'f';
    this.age = 0;
    this.pregnent = false;

    // Default base stats - percentages
    // Size is restrained to [1%, 100%], the other can be [0%, 100%]
    this.sizeStat        = randomPercentInRange(0.01, 1);
    this.aquaticStat     = randomPercentInRange(0, 1);
    this.carnivorousStat = randomPercentInRange(0, 1);

    this.$element = $('<div/>')
        .addClass('organism')
        .addClass(this.sex == 'm' ? 'male' : 'female')
        .width(SCALE)
        .height(SCALE)
        .offset({
            top: this.y * SCALE,
            left: this.x * SCALE,
        });

    // The parents are optional - we manually generate some misc organisms
    if (parent1 === undefined) {
        return;
    }

    var stats = ['sizeStat', 'aquaticStat', 'carnivorousStat'];
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

Organism.prototype.update = function(board) {
    if (Math.random() < 0.5) {
        if (Math.random() < 0.5) {
            this.x = Math.max(this.x - 1, 0);
        } else {
            this.x = Math.min(this.x + 1, board.width - 1);
        }
    } else {
        if (Math.random() < 0.5) {
            this.y = Math.max(this.y - 1, 0);
        } else {
            this.y = Math.min(this.y + 1, board.height - 1);
        }
    }

    this.$element
        .css('background-color', this.getColor())
        .offset({
            top: this.y * SCALE,
            left: this.x * SCALE,
        });
}

function Board($container, width, height) {
    this.width = width;
    this.height = height;
    this.$container = $container;
    this.isUpdating = false;
    this.tiles = [];
    this.organisms = [];

    for (var y = 0; y < height; y++) {
        var row = [];
        for (var x = 0; x < width; x++) {
            var tile = new Tile(x, y);
            if (Math.random() < VEGETATION_SPAWN) {
                tile.hasVegetation = true;
            }
            row.push(tile);
            $container.append(tile.$element);
        }
        this.tiles.push(row);
    }

    for (var i = 0; i < LAKE_NUMBER; i++) {
        this.generateNewLake();
    }
    for (var i = 0; i < RIVER_NUMBER; i++) {
        this.generateNewRiver();
    }

    for (var i = 0; i < 10; i++) {
        var organism = new Organism(10, i);
        $container.append(organism.$element);
        this.organisms.push(organism);
    }

    this.update();
}

Board.prototype.generateNewRiver = function() {
    var x = Math.floor(Math.random() * this.width);
    var y = Math.floor(Math.random() * this.height);
    var direction;

    // 25/25/25/25 chance on which edge to start at
    if (Math.random() < 0.5) {
        if (Math.random() < 0.5) {
            x = 0;
            direction = 'e';
        } else {
            x = this.width - 1;
            direction = 'w';
        }
    } else {
        if (Math.random() < 0.5) {
            y = 0;
            direction = 's';
        } else {
            y = this.height - 1;
            direction = 'n';
        }
    }

    while (x >= 0 && y >= 0 && x < this.width && y < this.height) {
        var tile = this.tiles[y][x];
        if (tile.hasWater) {
            // Don't allow crossing water!
            break;
        }
        this.tiles[y][x].hasWater = true;

        if (direction == 'e' || direction == 'w') {
            if (Math.random() < RIVER_TURN_RATE) {
                if (Math.random() < 0.5) {
                    direction = 'n';
                } else {
                    direction = 's';
                }
            }
        } else {
            if (Math.random() < RIVER_TURN_RATE) {
                if (Math.random() < 0.5) {
                    direction = 'e';
                } else {
                    direction = 'w';
                }
            }
        }

        if (direction == 'e') {
            x++;
        } else if (direction == 'w') {
            x--;
        } else if (direction == 'n') {
            y--;
        } else if (direction == 's') {
            y++;
        }
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
    if (this.isUpdating) {
        return;
    }
    this.isUpdating = true;
    
    for (var y = 0; y < this.height; y++) {
        for (var x = 0; x < this.width; x++) {
            this.tiles[y][x].update(this);
        }
    }
    
    for (var i = 0; i < this.organisms.length; i++) {
        this.organisms[i].update(this);
    }
    
    this.isUpdating = false;
}

Board.prototype.spreadVegetation = function(x, y) {
    this.tiles[y][x].hasVegetation = true;
}

function init($container, width, height) {
    var board = new Board($container, width, height);

    setInterval(function() {
        board.update();
    }, TURN_SPEED_MS);
}
