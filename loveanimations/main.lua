require 'class'

local objects = {}

-- 60 FPS
local TICK_PERIOD = 1/60
local GRAVITY     = 9.8 * 40
local PLAYER_MAX_VEL = 256

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

   o.direction = 'right'

   o.xVel = 0
   o.yVel = 0

   -- The player center, relative to the image sizes (constants)
   o.centerX = 30
   o.centerY = 30

   -- Hitbox
   o.width = 40
   o.height = 45
end)

function Player:collidsWithGround(g)
   return self.x + self.width > g.x and self.x < g.x + g.width and
      self.y + self.height + 1 > g.y and self.y < g.y + g.height
end

function Player:handleHitGround(g)
   -- ...YES, IT'S PRIMITIVE
   self.yVel = 0
   self.y = g.y - self.height

   if self.state == 'jumping' then
      self:setNewState('landing')
   end
end

function Player:getActiveGround()
   for i=1,#objects do
      local o = objects[i]

      -- ... primitive! TODO: You need to check the range of the velocity!
      if o:isGround() and self:collidsWithGround(o) then
         return o
      end
   end

   return nil
end

function Player:handleMovement(dt)
   local ground = self:getActiveGround()
   if ground ~= nil then
      self:handleHitGround(ground)
   elseif self.state ~= 'jumping' then
      -- Well, falling, really.
      self:setNewState('jumping')
   end

   self.x = self.x + self.xVel * dt
   self.y = self.y + self.yVel * dt
   self.yVel = self.yVel + GRAVITY * dt

   if self.state == 'idle' or self.state == 'running' then
      if love.keyboard.isDown('right') then
         local accMultiplier = self.direction == 'left' and 2 or 1
         self.xVel = math.min(self.xVel + accMultiplier * PLAYER_MAX_VEL * dt, PLAYER_MAX_VEL)

         if self.state ~= 'running' then
            self:setNewState('running')
         end
      elseif love.keyboard.isDown('left') then
         local accMultiplier = self.direction == 'right' and 2 or 1
         self.xVel = math.max(self.xVel - accMultiplier * PLAYER_MAX_VEL * dt, -PLAYER_MAX_VEL)
         
         if self.state ~= 'running' then
            self:setNewState('running')
         end
      elseif self.state == 'running' then
         if self.xVel > 0 then
            self.xVel = math.max(0, self.xVel - 2 * PLAYER_MAX_VEL * dt)
         elseif self.xVel < 0 then
            self.xVel = math.min(0, self.xVel + 2 * PLAYER_MAX_VEL * dt)
         else
            self:setNewState('idle')
         end
      end

      if love.keyboard.isDown('up') then
         self.yVel = -180
         self.y = self.y - 1 -- To avoid collision issues
         self:setNewState('jumping')
      end
   end
end

function Player:setNewState(state, subState)
   self.state = state
   self.subState = subState or 0
   self.stateTimer = 0
end

function Player:handleStateAnimations(dt)
   if self.xVel > 0 then
      self.direction = 'right'
   elseif self.xVel < 0 then
      self.direction = 'left'
   end

   if self.state == 'landing' then
      if self.subState == 0 then
         if math.abs(self.xVel) > PLAYER_MAX_VEL / 2 then
            self:setNewState('rolling')
         else
            self.xVel = 0
         end

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
   elseif self.state == 'running' then
      -- TODO: Make him animate faster when his xVel is higher!
      if self.stateTimer > 0.1 then
         self:setNewState('running', (self.subState + 1) % 6)
      end
   elseif self.state == 'rolling' then
      if self.xVel > 0 then
         self.xVel = math.max(0, self.xVel - 2 * PLAYER_MAX_VEL * dt)
      elseif self.xVel < 0 then
         self.xVel = math.min(0, self.xVel + 2 * PLAYER_MAX_VEL * dt)
      else
         self:setNewState('landing')
      end
   end

   self.stateTimer = self.stateTimer + dt
end

function Player:update(dt)
   self:handleMovement(dt)
   self:handleStateAnimations(dt)
end

function Player:draw()
   local image = nil

   -- Flip the images if going left
   local xScale = self.direction == 'right' and 1 or -1
   -- FIXME: Be rid of these evil magic numbers!
   local xOrigin = self.direction == 'right' and self.centerX or 70
   local yOrigin = self.centerY

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
   elseif self.state == 'running' then
      image = playerImages['run' .. self.subState]
   elseif self.state == 'rolling' then
      -- For now, the rolling animation will just be a sliding animation
      image = playerImages.swim5
      yOrigin = yOrigin - 15
   end

   love.graphics.setColor(1, 1, 1) -- No color filter
   love.graphics.draw(image, self.x, self.y, 0,
                      xScale, 1, xOrigin, yOrigin)
end

function love.load(args)
   playerImages.idle0 = love.graphics.newImage('img/idle_0.png')
   playerImages.idle1 = love.graphics.newImage('img/idle_1.png')
   playerImages.idle2 = love.graphics.newImage('img/idle_2.png')
   playerImages.jump0 = love.graphics.newImage('img/jump_0.png')
   playerImages.jump1 = love.graphics.newImage('img/jump_1.png')
   playerImages.jump2 = love.graphics.newImage('img/jump_2.png')
   playerImages.jump3 = love.graphics.newImage('img/jump_3.png')
   playerImages.run0  = love.graphics.newImage('img/run_0.png')
   playerImages.run1  = love.graphics.newImage('img/run_1.png')
   playerImages.run2  = love.graphics.newImage('img/run_2.png')
   playerImages.run3  = love.graphics.newImage('img/run_3.png')
   playerImages.run4  = love.graphics.newImage('img/run_4.png')
   playerImages.run5  = love.graphics.newImage('img/run_5.png')
   playerImages.swim5 = love.graphics.newImage('img/swim_5.png')
   
   objects[#objects + 1] = Ground(0, 300, 100, 32)
   objects[#objects + 1] = Ground(0, 480, 512, 32)
   objects[#objects + 1] = Player(30, 200)

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
