var VEGETATION_SPREAD = 0.01;
var TURN_SPEED_MS = 500;
var SCALE = 2;

function Tile(x, y) {
    this.x = x;
    this.y = y;

    this.$element = $('<div/>')
        .addclass('block')
        .width(SCALE)
        .height(SCALE)
        .offset({
            top: this.y * SCALE,
            left: this.x * SCALE,
        });
}

Tile.prototype.getColor() {
    throw "getColor must be implemented in subclasses!";
}

Tile.prototype.update(board) {
    this.$element
        .backgroundColor(this.getColor())
        .offset({
            top: this.y * SCALE,
            left: this.x * SCALE,
        });
}

function Board($container, width, height) {
}

Board.prototype.update() {
}

function Earth(x, y, hasVegetation) {
    Tile.call(this, x, y);
    this.hasVegetation = hasVegetation;
}

Earth.prototype.getColor() {
    return this.hasVegetation ? '#070' : '#770';
}

Earth.prototype.update(board) {
    if (this.hasVegetation) {
        if (this.x > 0 && Math.random() < VEGETATION_SPREAD) {
            board.spreadVegetation(this.x - 1, this.y, board);
        }
        if (this.y > 0 && Math.random() < VEGETATION_SPREAD) {
            board.spreadVegetation(this.x, this.y - 1, board);
        }
        if (this.x < board.width - 1 && Math.random() < VEGETATION_SPREAD) {
            board.spreadVegetation(this.x + 1, this.y, board);
        }
        if (this.y < board.height - 1 && Math.random() < VEGETATION_SPREAD) {
            board.spreadVegetation(this.x, this.y + 1, board);
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
