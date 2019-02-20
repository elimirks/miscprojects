// Used in the game, but not defined in the JS API
var DORDER_NONE = 0;

var baseRadius = 3;

/**
 * Debug event - for testing
 */
function debugPickle() {
    var trucks = enumTrucks();

    if (trucks.length < 4) {
        buildTruck();
    }

    buildBaseWalls();
}

function hasStructAtLocation(structs, x, y) {
    for (var i = 0; i < structs.length; i++) {
        if (structs[i].x == x && structs[i].y == y) {
            return true;
        }
    }
    return false;
}

/**
 * Enumerate where walls should be built, forming a nice cozy base.
 */
function enumBaseWalls() {
    var startPosition = getStartPosition();

    var baseWalls = [];

    var startX = Math.max(startPosition.x - baseRadius, 0);
    var startY = Math.max(startPosition.y - baseRadius, 0);
    var endX   = Math.min(startPosition.x + baseRadius, mapWidth);
    var endY   = Math.min(startPosition.y + baseRadius, mapHeight);

    var myStructs = enumStruct();

    var baseHeight = endY - startY;
    var baseWidth  = endX - startX;
    var perimiterLength = 2 * (baseWidth - 1) + 2 * (baseHeight - 1);

    // Trace the base perimeter
    for (var i = 0; i < perimiterLength; i++) {
        var x = 0;
        var y = 0;

        // Top wall
        if (i < baseWidth) {
            x = startX + i;
            y = startY;
        // Right wall
        } else if (i < baseWidth + baseHeight) {
            x = endX;
            y = startY + i - baseWidth;
        }
        // TODO: Bottom and left walls

        // Something already exists here!
        if (hasStructAtLocation(myStructs, x, y)) {
            continue;
        }

        baseWalls.push({
            x: x,
            y: y,
        });
    }

    return baseWalls;
}

/**
 * Event to run at the beginning of the game.
 */
function eventStartLevel() {
    setTimer('debugPickle', 1000);
}

/**
 * Return available trucks
 */
function enumTrucks() {
    return enumDroid(me, DROID_CONSTRUCT);
}

function enumAvailableTrucks() {
    var trucks = enumTrucks();
    var available = [];

    for (var i = 0; i < trucks.length; i++) {
        if (trucks[i].order == DORDER_NONE) {
            available.push(trucks[i]);
        }
    }

    return available;
}

function findAvailableFactory() {
    var factories = enumStruct(me, FACTORY);

    for (var i = 0; i < factories.length; i++) {
        if (factories[i].status == BUILT && structureIdle(factories[i])) {
            return factories[i];
        }
    }

    return null;
}

function buildTruck() {
    var factory = findAvailableFactory();

    if (factory == null) {
        return;
    }

    debug('Building a new truck.');
    buildDroid(factory, "Truck", ['Body2SUP', 'Body4ABT', 'Body1REC'],
               ['hover01', 'wheeled01'], "", DROID_CONSTRUCT, "Spade1Mk1");
}

function positionToString(position) {
    return '' + position.x + ',' + position.y;
}

function getStartPosition() {
    var position = startPositions[playerData[me].position];

    // FIXME: hack!
    return {
        x: position.x - 5,
        y: position.y - 5,
    };

    return position;
}

function buildBaseWalls() {
    var trucks = enumAvailableTrucks();

    if (trucks.length == 0) {
        debug('No trucks available');
        return;
    }

    var truck = trucks[0];
    var walls = enumBaseWalls();

    var startPosition = getStartPosition();
    debug('Truck pos: ' + positionToString(truck));
    debug('Start pos: ' + positionToString(startPosition));

    for (var i = 0; i < walls.length; i++) {
        if ( ! droidCanReach(truck, walls[i].x, walls[i].y)) {
            continue;
        }

        orderDroidBuild(truck, DORDER_BUILD, 'A0HardcreteMk1Wall',
                        walls[i].x, walls[i].y);
    }

    // Build a square section walled in
    /*
    for (var x = baseRegionStartX; x <= baseRegionEndX; x++) {
        for (var y = baseRegionStartY; y <= baseRegionEndX; y++) {
            if ( ! droidCanReach(trucks[0], x, y )) {
                continue;
            }

            debug('Buliding wall at ' + x + ',' + y);

            // Return after buliding the wall.
            if (orderDroidBuild(trucks[0], DORDER_BUILD, 'A0HardcreteMk1Wall', x, y)) {
                return;
            }
        }    
    }
    */
    /*
    orderDroidBuild(trucks[0], DORDER_BUILD, 'A0HardcreteMk1Wall',
                    trucks[0].x, trucks[0].y + 1);
    */
}
