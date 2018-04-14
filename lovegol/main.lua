local grid, gridWidth, gridHeight

local time = 0
local TICK_PERIOD = 0.5
local DEAD_FADE_SCALE = 2
-- Used for fancy flashy animations.
-- Stores tuples of x,y coordinate pairs and elapsed times.
local deadCells = {}

local SCREEN_WIDTH  = 512
local SCREEN_HEIGHT = 512

local SCALE = 8

local MAX_GRID_WIDTH  = SCREEN_WIDTH / SCALE
local MAX_GRID_HEIGHT = SCREEN_HEIGHT / SCALE

function gridHasCellAt(x, y)
   if x < 0 or y < 0 or x >= gridWidth or y >= gridHeight then
      return false
   end

   return grid[x + y * gridWidth + 1]
end

function getCellNeighborCount(x, y)
   return (gridHasCellAt(x - 1, y - 1) and 1 or 0) +
      (gridHasCellAt(x - 1, y)     and 1 or 0) +
      (gridHasCellAt(x - 1, y + 1) and 1 or 0) +
      (gridHasCellAt(x, y - 1)     and 1 or 0) +
      (gridHasCellAt(x, y + 1)     and 1 or 0) +
      (gridHasCellAt(x + 1, y - 1) and 1 or 0) +
      (gridHasCellAt(x + 1, y)     and 1 or 0) +
      (gridHasCellAt(x + 1, y + 1) and 1 or 0)
end

function updateGrid()
   local tempGrid = {}

   for i=0,(gridWidth * gridHeight - 1) do
      local x = i % gridWidth
      local y = (i - x) / gridWidth

      local neighborCount = getCellNeighborCount(x, y)

      -- Apply the rules of CGOL
      tempGrid[#tempGrid + 1] = ((not gridHasCellAt(x, y)) and (neighborCount == 3)) or
         (gridHasCellAt(x, y) and (neighborCount >= 2 and neighborCount <= 3))

      -- Start tracking this cell as a sad, dead cell (for animations)
      if (not tempGrid[#tempGrid]) and grid[#tempGrid] then
         deadCells[#deadCells + 1] = {
            x = x,
            y = y,
            time = TICK_PERIOD,
         }
      end
   end

   grid = tempGrid
end

-- Pads the grid with empty cells
function padGrid(amount)
   for i=1,amount do
      grid[#grid + 1] = false
   end
end

-- Loads a map from the given file
function loadMap(fileName)
   local f = assert(io.open(fileName, 'r'))

   local heightString = f:read()
   local widthString  = f:read()
   assert(heightString ~= nil and widthString ~= nil)

   gridHeight = tonumber(heightString)
   gridWidth  = tonumber(widthString)

   assert(gridWidth > 0 and gridWidth <= MAX_GRID_WIDTH)
   assert(gridHeight > 0 and gridHeight <= MAX_GRID_HEIGHT)

   grid = {}

   for y=0,(gridHeight - 1) do
      local line = f:read()

      -- If we've reached the end of the file, assume the rest of the grid is blank.
      if line == nil then
         padGrid((gridHeight - y) * gridWidth)
         break
      end

      for x=0,(#line - 1) do
         local c = line:sub(x + 1, x + 1)
         grid[#grid + 1] = (c == 'X')
      end

      -- In case the user didn't specify all the characters in the line
      if #line < gridWidth then
        padGrid(gridWidth - #line)
      end
   end
end

function love.load(args)
   assert(#args == 1)
   loadMap(args[1])
   time = 0
end

function love.draw()
   love.graphics.setBackgroundColor(230/255/4, 88/255/4, 160/255/4)

   local boardWidth  = gridWidth * SCALE;
   local boardHeight = gridHeight * SCALE;

   local xOffset = (SCREEN_WIDTH - boardWidth) / 2
   local yOffset = (SCREEN_HEIGHT - boardHeight) / 2

   love.graphics.setColor(230/255, 88/255, 160/255)
   love.graphics.rectangle('fill', xOffset, yOffset, boardWidth, boardHeight)

   love.graphics.setColor(131/255, 192/255, 240/255)

   for i=0,(gridWidth * gridHeight - 1) do
      local x = i % gridWidth
      local y = (i - x) / gridWidth

      if grid[i + 1] then
         love.graphics.rectangle('fill', xOffset + x * SCALE,
                                 yOffset + y * SCALE, SCALE, SCALE)
      end
   end

   for i=1,#deadCells do
      local cell = deadCells[i]
      local alpha = cell.time / TICK_PERIOD

      love.graphics.setColor(131/255, 192/255, 240/255, alpha)
      love.graphics.rectangle('fill', xOffset + cell.x * SCALE,
                              yOffset + cell.y * SCALE, SCALE, SCALE)
   end
end

function love.update(dt)
   time = time + dt

   for i=#deadCells,1,-1 do
      local cell = deadCells[i]
      cell.time = cell.time - dt * DEAD_FADE_SCALE

      if cell.time <= 0 then
         table.remove(deadCells, i)
      end
   end

   if time >= TICK_PERIOD then
      time = time - TICK_PERIOD
      updateGrid()
   end
end
