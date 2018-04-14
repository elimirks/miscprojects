require 'class'

local objects = {}

-- 60 FPS
local TICK_PERIOD = 1/60
local GRAVITY     = 9.8 * 40

-- Note: They are all 120x87 images
-- 40 width, 45 height (hitbox)
local playerImages = {}

Tile = class(function(o, x, y)
   o.x = x
   o.y = y
end)

function Tile:draw()
   assert(false)
end

function Tile:update(dt)
end



Ground = class(Tile, function(o, x, y, width, height)
   Tile.init(o, x, y)
   o.width = width
   o.height = height
end)

function Ground:draw()
  love.graphics.setColor(72 / 255, 160 / 255, 14 / 255)
  love.graphics.rectangle("fill", self.x, self.y, self.width, self.height)
end


Player = class(Tile, function(o, x, y)
   Tile.init(o, x, y)
   o.state = playerImages.jump0
   o.time = 0

   o.xVel = 12
   o.yVel = -512

   -- The player center, relative to the image sizes (constants)
   o.centerX = 30
   o.centerY = 30
end)

function Player:update(dt)
   self.time = self.time + dt

   if self.time > 0.1 then
      self.time = self.time - 0.1
   end

   self.x = self.x + self.xVel * dt
   self.y = self.y + self.yVel * dt
   self.yVel = self.yVel + GRAVITY * dt

   if self.yVel < -64 then
      self.state = playerImages.jump0
   elseif self.yVel > 64 then
      self.state = playerImages.jump1
   else
      self.state = playerImages.jump3
   end
end

function Player:draw()
   love.graphics.setColor(1, 1, 1) -- No color filter
   love.graphics.draw(self.state, self.x - self.centerX, self.y - self.centerY)
end

function love.load(args)
   playerImages.jump0 = love.graphics.newImage('img/jump_0.png')
   playerImages.jump1 = love.graphics.newImage('img/jump_1.png')
   playerImages.jump2 = love.graphics.newImage('img/jump_2.png')
   playerImages.jump3 = love.graphics.newImage('img/jump_3.png')
   
   objects[#objects + 1] = Ground(0, 480, 512, 32)
   objects[#objects + 1] = Player(30, 435)

   love.graphics.setBackgroundColor(104, 136, 248)
end

function love.draw()
   for i=1,#objects do
      objects[i]:draw()
   end
end

function love.update(dt)
   for i=1,#objects do
      objects[i]:update(dt)
   end
end
