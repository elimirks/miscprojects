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

function Tile:isGround()
   return false
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

function Ground:isGround()
   return true
end


Player = class(Tile, function(o, x, y)
   Tile.init(o, x, y)
   o.state = 'jumping'
   o.subState = 0
   o.stateTimer = 0

   o.xVel = 12
   o.yVel = -512

   -- The player center, relative to the image sizes (constants)
   o.centerX = 30
   o.centerY = 30

   -- Hitbox
   o.width = 40
   o.height = 45
end)

function Player:collidsWithGround(g)
   return self.x + self.width > g.x and self.x < g.x + g.width and
      self.y + self.height > g.y and self.y < g.y + g.height
end

function Player:handleHitGround(g)
   -- ...YES, IT'S PRIMITIVE
   self.yVel = 0
   self.y = g.y - self.height

   if self.state == 'jumping' then
      self.xVel = 0
      self:setNewState('landing')
   end
end

function Player:handleMovement(dt)
   for i=1,#objects do
      local o = objects[i]

      -- ... primitive! TODO: You need to check the range of the velocity!
      if o:isGround() and self:collidsWithGround(o) then
         self:handleHitGround(o)
         break
      end
   end

   self.x = self.x + self.xVel * dt
   self.y = self.y + self.yVel * dt
   self.yVel = self.yVel + GRAVITY * dt
end

function Player:setNewState(state, subState)
   self.state = state
   self.subState = subState or 0
   self.stateTimer = 0
end

function Player:handleStateAnimations(dt)
   if self.state == 'landing' then
      if self.subState == 0 then
         if self.stateTimer > 0.1 then
            self:setNewState('landing', 1)
         end
      else
         if self.stateTimer > 0.1 then
            self:setNewState('idle')
         end
      end
   elseif self.state == 'idle' then
      if self.subState == 0 or self.subState == 2 then
         if self.stateTimer > 5.0 then
            self:setNewState('idle', self.subState == 0 and 1 or 3)
         end
      elseif self.stateTimer > 1.0 then
         self:setNewState('idle', self.subState == 1 and 2 or 0)
      end
   end

   self.stateTimer = self.stateTimer + dt
end

function Player:update(dt)
   self:handleMovement(dt)
   self:handleStateAnimations(dt)
end

function Player:draw()
   love.graphics.setColor(1, 1, 1) -- No color filter

   local image = nil

   if self.state == 'jumping' then
      if self.yVel < -64 then
         image = playerImages.jump0
      elseif self.yVel > 64 then
         image = playerImages.jump1
      else
         image = playerImages.jump3
      end
   elseif self.state == 'landing' then
      if self.subState == 0 then
         -- The landing position
         image = playerImages.jump2
      else
         -- "getting up" position
         image = playerImages.run5
      end
   elseif self.state == 'idle' then
      if self.subState == 0 then
         image = playerImages.idle0
      elseif self.subState == 2 then
         image = playerImages.idle1
      else
         image = playerImages.idle2
      end
   end

   love.graphics.draw(image, self.x - self.centerX, self.y - self.centerY)
end

function love.load(args)
   playerImages.idle0 = love.graphics.newImage('img/idle_0.png')
   playerImages.idle1 = love.graphics.newImage('img/idle_1.png')
   playerImages.idle2 = love.graphics.newImage('img/idle_2.png')
   playerImages.jump0 = love.graphics.newImage('img/jump_0.png')
   playerImages.jump1 = love.graphics.newImage('img/jump_1.png')
   playerImages.jump2 = love.graphics.newImage('img/jump_2.png')
   playerImages.jump3 = love.graphics.newImage('img/jump_3.png')
   playerImages.run5  = love.graphics.newImage('img/run_5.png')
   
   objects[#objects + 1] = Ground(0, 480, 512, 32)
   objects[#objects + 1] = Player(30, 350)

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
