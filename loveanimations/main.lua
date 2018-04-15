require 'class'

local objects

-- 60 FPS
local TICK_PERIOD = 1/60
local GRAVITY     = 9.8 * 40
local PLAYER_MAX_VEL = 300
local PLAYER_CRAWL_VEL = 20
local MIN_ROLL_VEL = 50
local MAX_STEP_HEIGHT = 8

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

function Tile:isFloor()
   return false
end

function Tile:isWall()
   return false
end

function Tile:isRamp()
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

function Ground:isFloor()
   return true
end

function Ground:isWall()
   return true
end

Ramp = class(Tile, function(o, x, y, width, height, direction)
   Tile.init(o, x, y)
   o.width = width
   o.height = height
   -- The direction of incline
   o.direction = direction
end)

function Ramp:draw()
  love.graphics.setColor(72 / 255, 160 / 255, 14 / 255)

  if self.direction == 'left' then
     love.graphics.polygon("fill", {
                              self.x, self.y,
                              self.x, self.y + self.height,
                              self.x + self.width, self.y + self.height})
  else
     love.graphics.polygon("fill", {
                              self.x + self.width, self.y,
                              self.x + self.width, self.y + self.height,
                              self.x, self.y + self.height})
  end
end

function Ramp:isRamp()
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

   -- Full hitbox size
   o.width = 40
   o.height = 45
end)

function Player:getFullHitbox()
   return {
      x = self.x,
      y = self.y,
      width = self.width,
      height = self.height,
   }
end

function Player:getLowHitbox()
   local CRAWL_HEIGHT_DIFF = 12

   return {
      x = self.x,
      y = self.y + CRAWL_HEIGHT_DIFF,
      width = self.width,
      height = self.height - CRAWL_HEIGHT_DIFF,
   }
end

function Player:getHitbox()
   if self.state == 'rolling' or self.state == 'crawling' then
      return self:getLowHitbox()
   end
   return self:getFullHitbox()
end

function Player:collidesWithRamp(r, dt)
   local hitPointX = self.x + self.width / 2

   if hitPointX > r.x + r.width or hitPointX < r.x then
      return false
   end
   if self.y > r.y + r.height or self.y + self.height < r.y then
      return false
   end
   
   -- It _potentially_ has hit at this point.
   
   local hitPointY = self.y + self.height
   local slope = r.height / r.width

   local deltaX = hitPointX - r.x
   local deltaY = hitPointY - r.y

   if r.direction == 'right' then
      return deltaY + 1 >= (r.width - deltaX) * slope
   else
      return deltaY + 1 >= deltaX * slope
   end
end

function Player:handleHitRamp(o)
   if self.yVel < 0 then
      return
   end

   self:handleLanding()
   
   self.yVel = 0
   
   local hitPointX = self.x + self.width / 2
   local deltaX    = hitPointX - o.x
   local slope     = o.height / o.width
   
   if o.direction == 'right' then
      self.y = o.y - self.height + (o.width - deltaX) * slope
   else
      self.y = o.y - self.height + deltaX * slope
   end
end

function Player:hasHandledRampCollider(dt)
   for i=1,#objects do
      local o = objects[i]

      if o:isRamp() and self:collidesWithRamp(o, dt) then
         self:handleHitRamp(o)
         return true
      end
   end

   return false
end

function Player:handleLanding()
   local HARD_Y_VEL = 180

   if self.state == 'jumping' then
      if math.abs(self.xVel) > PLAYER_MAX_VEL / 2 and self.yVel > HARD_Y_VEL then
         self:setNewState('rolling')
      else
         local wantsToMove = (self.direction == 'right' and love.keyboard.isDown('right')) or
            (self.direction == 'left' and love.keyboard.isDown('left')) or
            love.keyboard.isDown('down')
         
         if self.yVel > HARD_Y_VEL or not wantsToMove then
            self.xVel = 0
            self:setNewState('landing')
         else
            self:setNewState('running')
         end
      end
   end

end

function Player:handleHitFloor(g)
   self:handleLanding()

   self.yVel = 0
   self.y = g.y - self.height
end

function Player:collidesWithFloor(g, dt)
   local travel = self.yVel * dt
   local hitbox = self:getHitbox()

   return hitbox.x + hitbox.width > g.x and hitbox.x < g.x + g.width and
      hitbox.y + hitbox.height + travel > g.y and hitbox.y + travel < g.y + g.height
end

function Player:hasHandledFloorCollider(dt)
   local handledFloor = false

   for i=1,#objects do
      local o = objects[i]

      if o:isFloor() and self:collidesWithFloor(o, dt) then
         self:handleHitFloor(o)
         -- We only have to care about at most one floor hit.
         handledFloor = true
         break
      end
   end

   -- We should always check ramps _after_ handling floors
   if self:hasHandledRampCollider(dt) then
      handledFloor = true
   end

   return handledFloor
end

function Player:collidesWithWall(g, dt)
   local hitbox = self:getHitbox()
   local travel = self.xVel * dt

   return hitbox.x + hitbox.width + travel > g.x and hitbox.x + travel < g.x + g.width and
      hitbox.y + hitbox.height > g.y and hitbox.y < g.y + g.height
end

function Player:handleHitWall(g)
   local hitbox = self:getHitbox()

   if self.state ~= 'jumping' and hitbox.y + hitbox.height - g.y <= MAX_STEP_HEIGHT then
      self.y = g.y - self.height

      if self.xVel > 0 then
         self.x = self.x + 1
      else
         self.x = self.x - 1
      end

      return
   end

   if self.xVel > 0 then
      self.x = g.x - self.width
   else
      self.x = g.x + g.width
   end

   self.xVel = 0

   if self.state == 'running' then
      self:setNewState('idle')
   end
end

function Player:handleWallCollisions(dt)
   for i=1,#objects do
      local o = objects[i]

      if o:isWall() and self:collidesWithWall(o, dt) then
         self:handleHitWall(o)
         -- We only have to care about at most one wall hit.
         break
      end
   end
end

function Player:handleHitCeiling(o)
   self.yVel = 0
end

function Player:collidesWithCeiling(g, dt, hitbox)
   local travel = self.yVel * dt

   hitbox = hitbox or self:getHitbox()
   local headHeight = self.height / 2

   return hitbox.x + hitbox.width > g.x and hitbox.x < g.x + g.width and
      hitbox.y + headHeight + travel > g.y and hitbox.y + travel < g.y + g.height
end

function Player:handleCeilingCollider(dt)
   for i=1,#objects do
      local o = objects[i]

      if o:isFloor() and self:collidesWithCeiling(o, dt) then
         self:handleHitCeiling(o)
         break
      end
   end
end

function Player:canStandUp(dt)
   for i=1,#objects do
      local o = objects[i]

      if o:isFloor() and self:collidesWithCeiling(o, dt, self:getFullHitbox()) then
         return false
      end
   end

   return true
end

function Player:handleMovement(dt)
   self:handleCeilingCollider(dt)

   if not self:hasHandledFloorCollider(dt) then
      -- Well, falling, really.
      self:setNewState('jumping')
   end

   self.x = self.x + self.xVel * dt
   self.y = self.y + self.yVel * dt
   self.yVel = self.yVel + GRAVITY * dt

   if self.state == 'idle' or self.state == 'running' then
      if love.keyboard.isDown('up') then
         self.yVel = -180
         self.y = self.y - 1 -- To avoid collision issues
         self:setNewState('jumping')
      elseif love.keyboard.isDown('down') then
         if self.state == 'running' then
            self:setNewState('rolling')
            return
         else
            -- Crawling substate 10 will crouch down etc
            self:setNewState('crawling', 10)
            return
         end
      end

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
   end

   if self.state == 'crawling' then
      if not love.keyboard.isDown('down') and self:canStandUp(dt) then
         self.xVel = 0
         self:setNewState('landing')
      else
         if self.direction == 'right' and love.keyboard.isDown('right') then
            self.xVel = PLAYER_CRAWL_VEL
         elseif self.direction == 'left' and love.keyboard.isDown('left') then
            self.xVel = -PLAYER_CRAWL_VEL
         else
            self.xVel = 0
         end
      end
   end

   if self.state == 'rolling' then
      local wantsToRoll = (self.direction == 'right' and love.keyboard.isDown('right')) or
            (self.direction == 'left' and love.keyboard.isDown('left'))
      local canRoll = math.abs(self.xVel) > MIN_ROLL_VEL and self.subState ~= 1

      local friction = (wantsToRoll and canRoll) and 0.3 or 2

      if self.xVel > 0 then
         self.xVel = math.max(0, self.xVel - friction * PLAYER_MAX_VEL * dt)
      elseif self.xVel < 0 then
         self.xVel = math.min(0, self.xVel + friction * PLAYER_MAX_VEL * dt)
      elseif self.subState == 1 then
         self:setNewState('crawling')
      end
   end

   self:handleWallCollisions(dt)
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
      if self.stateTimer > (0.2 - 0.12 * math.abs(self.xVel / PLAYER_MAX_VEL)) then
         self:setNewState('running', (self.subState + 1) % 6)
      end
   elseif self.state == 'rolling' then
      -- If moving slowly, slide animation!
      if math.abs(self.xVel) > MIN_ROLL_VEL or self.subState ~= 1 then
         if self.stateTimer > (0.1 - 0.04 * math.abs(self.xVel / PLAYER_MAX_VEL)) then
            self:setNewState('rolling', (self.subState + 1) % 4)
         end
      end
   elseif self.state == 'crawling' then
      if self.subState < 10 then
         if math.abs(self.xVel) > 0 and self.stateTimer > 0.2 then
            self:setNewState('crawling', (self.subState + 1) % 4)
         end

         -- Don't stop in state 1! It should only be for animations
         if self.subState ~= 0 and math.abs(self.xVel) == 0 and self.stateTimer > 0.2 then
            self:setNewState('crawling', (self.subState + 1) % 4)
         end
      end

      if self.subState == 10 and self.stateTimer > 0.1 then
         self:setNewState('crawling', 11)
      elseif self.subState == 11 and self.stateTimer > 0.1 then
         self:setNewState('crawling', 0)
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
   local rotation = 0

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
      image = playerImages['roll' .. self.subState]

      if self.subState == 1 or self.subState == 3 then
         yOrigin = yOrigin - 15
      end

      if self.subState == 1 then
         xOrigin = xOrigin + 2
      end
   elseif self.state == 'crawling' then
      if self.subState <= 3 then
         image = playerImages['crawl' .. self.subState]
         yOrigin = yOrigin - 15
         xOrigin = xOrigin + 2
      elseif self.subState == 10 then
         -- Crouching down
         image = playerImages.run5
      elseif self.subState == 11 then
         -- Crouching down lower
         image = playerImages.jump2
      end
   end

   love.graphics.setColor(1, 1, 1) -- No color filter
   love.graphics.draw(image, self.x, self.y, rotation,
                      xScale, 1, xOrigin, yOrigin)
end

function love.load(args)
   playerImages = {}
   playerImages.idle0  = love.graphics.newImage('img/idle_0.png')
   playerImages.idle1  = love.graphics.newImage('img/idle_1.png')
   playerImages.idle2  = love.graphics.newImage('img/idle_2.png')
   playerImages.jump0  = love.graphics.newImage('img/jump_0.png')
   playerImages.jump1  = love.graphics.newImage('img/jump_1.png')
   playerImages.jump2  = love.graphics.newImage('img/jump_2.png')
   playerImages.jump3  = love.graphics.newImage('img/jump_3.png')
   playerImages.run0   = love.graphics.newImage('img/run_0.png')
   playerImages.run1   = love.graphics.newImage('img/run_1.png')
   playerImages.run2   = love.graphics.newImage('img/run_2.png')
   playerImages.run3   = love.graphics.newImage('img/run_3.png')
   playerImages.run4   = love.graphics.newImage('img/run_4.png')
   playerImages.run5   = love.graphics.newImage('img/run_5.png')
   playerImages.crawl0 = love.graphics.newImage('img/crawl_0.png')
   playerImages.crawl1 = love.graphics.newImage('img/crawl_1.png')
   playerImages.crawl2 = love.graphics.newImage('img/crawl_2.png')
   playerImages.crawl3 = love.graphics.newImage('img/crawl_3.png')
   playerImages.roll0  = love.graphics.newImage('img/roll_0.png')
   playerImages.roll1  = playerImages.crawl0
   playerImages.roll2  = love.graphics.newImage('img/roll_2.png')
   playerImages.roll3  = love.graphics.newImage('img/roll_3.png')
   
   objects = {}
   objects[#objects + 1] = Ground(0, 200, 200, 32)
   objects[#objects + 1] = Ground(0, 100, 32, 700)
   objects[#objects + 1] = Ground(0, 100, 80, 32)
   objects[#objects + 1] = Ground(270, 230, 50, 32)
   objects[#objects + 1] = Ground(500, 380, 32, 32)
   objects[#objects + 1] = Ground(100, 448, 540, 32)
   objects[#objects + 1] = Ground(100, 416, 100, 32)
   objects[#objects + 1] = Ramp(200, 416, 100, 32, 'left')
   objects[#objects + 1] = Ground(608, 0, 32, 800)
   objects[#objects + 1] = Ground(0, 768, 640, 32)
   objects[#objects + 1] = Player(35, 150)

   love.graphics.setBackgroundColor(200/255, 220/255, 255/255)
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

   if love.keyboard.isDown('r') then
      love.load()
   end
end
