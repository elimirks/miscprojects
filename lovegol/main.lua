local time, grid, gridWidth, gridHeight

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

-- TODO make it load from the _same_ format as cgol! (for simplicity)

function updateGrid()
   local tempGrid = {}

   for i=0,(gridWidth * gridHeight - 1) do
      local x = i % gridWidth
      local y = (i - x) / gridWidth

      local neighborCount = getCellNeighborCount(x, y)

      -- Apply the rules of CGOL
      tempGrid[#tempGrid + 1] = ((not gridHasCellAt(x, y)) and (neighborCount == 3)) or
         (gridHasCellAt(x, y) and (neighborCount >= 2 and neighborCount <= 3))
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
   if #args ~= 1 then
      -- FIXME show errors nicely :)
      -- ... or maybe use a file picker widget instead, to learn about Love2D
      error('Please specify the map to load!')
   end

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
end

function love.update(dt)
   time = time + dt

   if time >= 1 then
      time = time - 1
      updateGrid()
   end
end
