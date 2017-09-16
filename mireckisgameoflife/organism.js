var VEGETATION_SPAWN = 0.01;
var VEGETATION_SPREAD = 0.005;
var TURN_SPEED_MS = 100;
var SCALE = 10;

function Tile(x, y) {
    this.x = x;
    this.y = y;

    this.$element = $('<div/>')
        .addClass('block')
        .width(SCALE)
        .height(SCALE)
        .offset({
            top: this.y * SCALE,
            left: this.x * SCALE,
        });
}

Tile.prototype.getColor = function() {
    throw "getColor must be implemented in subclasses!";
}

Tile.prototype.update = function(board) {
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

    this.tiles = [];
    for (var y = 0; y < height; y++) {
        var row = [];
        for (var x = 0; x < width; x++) {
            var tile = new Earth(x, y, Math.random() < VEGETATION_SPAWN);
            row.push(tile);
            $container.append(tile.$element);
            console.log("Adding " + x + "," + y);
        }
        this.tiles.push(row);
    }

    this.update();
}

Board.prototype.update = function() {
    for (var y = 0; y < this.height; y++) {
        for (var x = 0; x < this.width; x++) {
            this.tiles[y][x].update(this);
        }
    }
}

Board.prototype.spreadVegetation = function(x, y) {
    this.tiles[y][x].hasVegetation = true;
}

function Earth(x, y, hasVegetation) {
    Tile.call(this, x, y);
    this.hasVegetation = hasVegetation;
}

Earth.prototype.getColor = function() {
    return this.hasVegetation ? '#070' : '#770';
}

Earth.prototype.update = function(board) {
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

    Tile.prototype.update.call(this, board);
}

function init($container, width, height) {
    var board = new Board($container, width, height);

    setInterval(function() {
        board.update();
    }, TURN_SPEED_MS);
}
