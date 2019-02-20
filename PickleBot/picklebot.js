// Used in the game, but not defined in the JS API
var DORDER_NONE = 0;

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
    buildDroid(factory, "Truck", ['Body2SUP','Body4ABT','Body1REC'],
               ['hover01','wheeled01'], "", DROID_CONSTRUCT, "Spade1Mk1");
}

function buildBaseWalls() {
    var trucks = enumAvailableTrucks();

    if (trucks.length == 0) {
        debug('No trucks available');
        return;
    }

    var startPosition = playerData[me].position;

    // Naively pick a base diameter... for now!
    // TODO: make it "intelligently" decide which region to wall off
    var baseDiameter = 5;

    var baseRegionStartX = Math.max(0, startPosition.x - baseDiameter);
    var baseRegionEndX   = Math.min(0, baseRegionStartX + 2 * baseDiameter);
    var baseRegionStartY = Math.max(0, startPosition.y - baseDiameter);
    var baseRegionEndY   = Math.min(0, baseRegionStartY + 2 * baseDiameter);

    var baseTiles = enumArea(baseRegionStartX, baseRegionStartY,
                             baseRegionEndX, baseRegionEndY);

    // Build a square section walled in
    for (var x = baseRegionStartX; x <= baseRegionEndX; x++) {
        for (var y = baseRegionStartY; y <= baseRegionEndX; y++) {
            debug('Buliding wall');

            // Return after buliding the wall.
            if (orderDroidBuild(trucks[0], DORDER_BUILD, 'A0HardcreteMk1Wall', x, y)) {
                return;
            }
        }    
    }
}
