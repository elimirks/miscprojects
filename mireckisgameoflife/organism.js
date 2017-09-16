var VEGETATION_SPAWN = 0.01;
var VEGETATION_SPREAD = 0.005;
var TURN_SPEED_MS = 100;
var SCALE = 10;
var LAKE_NUMBER = 1;
var LAKE_SIZE_MIN = 3;
var LAKE_SIZE_MAX = 10;
var RIVER_TURN_RATE = 0.1;
var RIVER_NUMBER = 1;

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

function Board($container, width, height) {
    this.width = width;
    this.height = height;
    this.$container = $container;
    this.isUpdating = false;

    this.tiles = [];
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
