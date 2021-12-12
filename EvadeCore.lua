
--[[

    Author: Ark223
    Version: 1.0.3b
    Date: 4 December 2021
    Copyright (c) 2021 Arkadiusz Kwiatkowski

    [License]
    Use, modification & distribution is subject to Boost Software License Ver 1.
    https://www.boost.org/LICENSE_1_0.txt

    [Geometry API]
    Vector - represents two numeric values:
    Vector:New(...) - initial call, returns instance
        overloads:
            () [null vector] => Vector(0, 0)
            (50, 25) [number, number] => Vector(50, 25)
            ({x = 50, y = 25}) [xy-table] => Vector(50, 25)
            ({x = 50, z = 25}) [xz-table] => Vector(50, 25)
            ({x = 50, y = 25, z = 10}) [xyz-table] => Vector(50, 10)
        properties:
            .x - the x value (floating-point number or integer)
            .y - the y value (floating-point number or integer)
        metamethods:
            -> Vector == Vector (vector equality comparison)
            -> Vector + Vector (sum of two vectors)
            -> Vector - Vector (difference of two vectors)
            -> Vector * number or number * Vector (scalar multiplication)
            -> Vector / number or number / Vector (scalar division)
        functions:
            :AngleBetween(Vector, Vector) - returns the angle formed from a point to both input points
            :Append(Vector, number dist) - creates a new vector, appends it by set distance and returns it
            :Clone() - clones a vector and returns it
            :ClosestOnSegment(Vector, Vector) - returns the closest point on a line segment
                + two input vectors are representing a line segment
            :CrossProduct(Vector) - returns cross product of two vectors
            :Distance(Vector) - returns distance to another vector
            :DistanceSquared(Vector) - returns squared distance to another vector
            :DotProduct(Vector) - returns dot product of two vectors
            :Extend(Vector, number dist) - creates a new vector, extends it by set distance and returns it
            :IsZero() - indicates if vector contains its properties set to zero
            :LengthSquared(Vector) - returns squared length between two vectors or of just one vector
            :Length(Vector) - returns length between two vectors or of just one vector
            :LineIntersection(Vector, Vector, Vector) - returns a line-line intersection point
                + first two vectors are representing first line, the last ones - second line
            :Negate() - negates prioperties of a vector and returns a new one
            :Normalize() - normalizes a vector and returns it
            :Perpendicular() - creates a new vector that is rotated 90° left
            :Perpendicular2() - creates a new vector that is rotated 90° right
            :Rotate(number phi, Vector) - rotates vector by phi (in radians) around another vector and returns it
                + if input vector is not set it will take a null vector into account as default value
            :Round() - rounds properties of a vector and returns a new one
            :SegmentIntersection(Vector, Vector, Vector) - returns a segment-segment intersection point
                + first two vectors are representing first segment, the last ones - second segment

    Polygon - represents a closed polygonal chain described by line segments created from points
    Polygon:New(...) - initial call, returns instance
        overloads:
            () [empty polygon] => Polygon({points = Linq()})
            (Linq()) [empty polygon] => Polygon({points = Linq()})
            (Linq({Vector, Vector, ...})) => Polygon({points = Linq({...})})
        properties:
            .points - connected points to form a closed polygon
            .size - a finite number of points
        functions:
            :Add(Vector) - inserts a new point to polygon
            :Area() - calculates an area of a polygon and returns it
            :Clear() - cleares all points making a polygon empty
            :Draw(number color, number height) - draws a polygon on screen with ARGB color
                + height - defines a height of drawn segments (used in 3D perspective)
            :Get(number index) - returns a specific point at input index
            :IsInside(Vector) - indicates if input point is inside of a polygon
            :IsOutside(Vector) - indicates if input point is outside of a polygon
            :Offset(number delta, number step) - offsets a polygon by input delta and returns it
                + step - defines a length step in arc generation for vertex offsetting
            :Orientation() - returns a boolean based on a polygon's orientation
            :PathIntersection(Vector, Vector) - returns a path-polygon intersection points
            :Remove(number index) - removes a specific point from polygon at input index
            :Set(number index, Vector) - overrides a point in a polygon with different one

    Path - represents a route for a travel from starting to ending point
    Path:New(speed, delay, delta, startPos, endPos) - initial call, returns instance
        properties:
            .speed - pathing movement speed (default value: huge number)
            .delay - delay before a game object starts pathing (default value: 0)
            .delta - error propagation to enhance collision time calculations (default value: 0)
            .startPos - starting position of a path (default value: null vector)
            .endPos - ending position of a path (default value: null vector)

--]]

--------------------------------------------------------
-- Customizable directives for platform functionality --

local atan2 = math.atan -- Lua 5.3+
local myHero = game.local_player

local directives = {
    mousePos = function() return game.mouse_pos end,
    timer = function() return game.game_time end,
    hitbox = function() return myHero.bounding_radius end,
    position = function() return myHero.path.server_pos end,
    isWall = function(p) return nav_mesh:is_wall(p.x, 0, p.y) end,
    drawLine = function(p1, p2, height, a, r, g, b)
        local pa = game:world_to_screen_2(p1.x, height or 0, p1.y)
        local pb = game:world_to_screen_2(p2.x, height or 0, p2.y)
        renderer:draw_line(pa.x, pa.y, pb.x, pb.y, 1, r, g, b, a)
    end,
    drawCircle = function(pos, radius, height, a, r, g, b)
        local p = vec3.new(pos.x, height or 0, pos.y)
        renderer:draw_circle(p.x, p.y, p.z, radius, r, g, b, a)
    end
}

------------------------------------------------
-- Class constructor with inheritance support --

local Class = function(...)
    local cls, bases = {}, {...}
    for _, base in ipairs(bases) do
        for param, value in pairs(base) do
            cls[param] = value
        end
    end
    cls.__index = cls
    function cls:New(...)
        local instance = setmetatable({}, cls)
        cls.__init(instance, ...)
        return instance
    end
    cls.__call = function(_, ...) return cls:New(...) end
    return setmetatable(cls, {__call = cls.__call})
end

--------------------------------------
-- Language INtegrated Query (LINQ) --

local function ParseFunc(func)
    if func == nil then return function(x) return x end end
    if type(func) == "function" then return func end
    local index = string.find(func, "=>")
    local arg = string.sub(func, 1, index - 1)
    local func = string.sub(func, index + 2, #func)
    return load(string.format("return function"
        .. " %s return %s end", arg, func))()
end

local function Linq(source)
    return setmetatable(source or {}, {__index = table})
end

function table.AddRange(source, collection)
    local index = #source
    for _, value in ipairs(collection) do
        index = index + 1
        source[index] = value
    end
end

function table.Aggregate(source, func, seed)
    local result = seed or 0
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        result = func(result, value, index)
    end
    return result
end

function table.All(source, func)
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        if not func(value, index) then
            return false
        end
    end
    return true
end

function table.Any(source, func)
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        if func(value, index) then
            return true
        end
    end
    return false
end

function table.Clear(source)
    for index = 1, #source do
        source[index] = nil
    end
end

function table.Concat(source, collection)
    local result, index = Linq(), 0
    for _, value in ipairs(source) do
        index = index + 1
        result[index] = value
    end
    for _, value in ipairs(collection) do
        index = index + 1
        result[index] = value
    end
    return result
end

function table.Copy(source, index)
    local result, iteration = Linq(), 0
    for i = (index or 1), #source do
        iteration = iteration + 1
        result[iteration] = source[i]
    end
    return result
end

function table.Distinct(source)
    local result = Linq()
    local hash, index = {}, 0
    for _, value in ipairs(source) do
        if hash[value] == nil then
            index = index + 1
            result[index] = value
            hash[value] = true
        end
    end
    return result
end

function table.First(source, func)
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        if func(value, index) then
            return value
        end
    end
    return nil
end

function table.ForEach(source, func)
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        func(value, index)
    end
end

function table.Min(source, func)
    local result = math.huge
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        local num = func(value, index)
        if type(num) == "number" and num <
            result then result = num end
    end
    return result
end

function table.OrderBy(source, comparer)
    local result = source:Copy()
    table.sort(result, comparer or
        function(a, b) return a > b end)
    return result
end

function table.RemoveWhere(source, func)
    local size = #source
    local func = ParseFunc(func)
    for index = size, 1, -1 do
        local value = source[index]
        if func(value, index) then
            source:remove(index)
        end
    end
    return size ~= #source
end

function table.Reverse(source)
    local result, iteration = Linq(), 0
    for index = #source, 1, -1 do
        iteration = iteration + 1
        result[iteration] = source[index]
    end
    return result
end

function table.Select(source, func)
    local result = Linq()
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        result[index] = func(value, index)
    end
    return result
end

function table.Where(source, func)
    local result, iteration = Linq(), 0
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        if func(value, index) then
            iteration = iteration + 1
            result[iteration] = value
        end
    end
    return result
end

---------------
-- Utilities --

local function Argb2Hex(a, r, g, b)
    return tonumber(string.format(
        "0x%.2X%.2X%.2X%.2X", a, r, g, b))
end

local function Hex2Argb(value)
    local alpha = (value >> 24) & 0xFF
    local red = (value >> 16) & 0xFF
    local green = (value >> 8) & 0xFF
    local blue = value & 0xFF
    return alpha, red, green, blue
end

---------------------------------------
-- Vector class (two numeric values) --

local Vector = Class()

local function IsVector(v)
    return v and v.x and type(v.x) == "number"
        and v.y and type(v.y) == "number"
end

local function IsZero(v)
    return math.abs(v) < 1e-6
end

function Vector:__init(x, y)
    if x and y then
        self.x, self.y = x, y
    elseif x and not y then
        self.x, self.y = x.x, x.z or x.y
    else
        self.x, self.y = 0, 0
    end
end

function Vector:__type()
    return "Vector"
end

function Vector:__eq(p)
    return IsZero(self.x - p.x) and IsZero(self.y - p.y)
end

function Vector:__add(p)
    return Vector:New(self.x + p.x, self.y + p.y)
end

function Vector:__sub(p)
    return Vector:New(self.x - p.x, self.y - p.y)
end

function Vector.__mul(a, b)
    if type(a) == "number" and IsVector(b) then
        return Vector:New(b.x * a, b.y * a)
    elseif type(b) == "number" and IsVector(a) then
        return Vector:New(a.x * b, a.y * b)
    end
    error("Multiplication error!")
end

function Vector.__div(a, b)
    if type(a) == "number" and IsVector(b) then
        return Vector:New(a / b.x, a / b.y)
    elseif type(b) == "number" and IsVector(a) then
        return Vector:New(a.x / b, a.y / b)
    end
    error("Division error!")
end

function Vector:__tostring()
    return string.format("(%f, %f)", self.x, self.y)
end

function Vector:AngleBetween(p1, p2, fix)
    local angle = atan2(p2.y - self.y, p2.x - self.x)
        - atan2(p1.y - self.y, p1.x - self.x)
    if angle < 0 then angle = angle + math.pi * 2 end
    return fix and angle > math.pi and angle - math.pi or angle
end

function Vector:Append(v, dist)
    if IsZero(dist) then return v:Clone() end
    return v + (v - self):Normalize() * dist
end

function Vector:Clone()
    return Vector:New(self.x, self.y)
end

function Vector:ClosestOnSegment(s1, s2)
    local ap, ab = self - s1, s2 - s1
    local t = ap:DotProduct(ab) / ab:LengthSquared()
    return t < 0 and s1 or t > 1 and s2 or s1 + ab * t
end

function Vector:CrossProduct(v)
    return self.x * v.y - self.y * v.x
end

function Vector:Distance(v)
    return math.sqrt(self:DistanceSquared(v))
end

function Vector:DistanceSquared(v)
    local dx = v.x - self.x
    local dy = v.y - self.y
    return dx * dx + dy * dy
end

function Vector:DistSqrToSegment(s1, s2)
    local v = self:ClosestOnSegment(s1, s2)
    return self:DistanceSquared(v)
end

function Vector:DotProduct(v)
    return self.x * v.x + self.y * v.y
end

function Vector:Extend(v, dist)
    if IsZero(dist) then return self:Clone() end
    return self + (v - self):Normalize() * dist
end

function Vector:IsZero()
    return IsZero(self.x) and IsZero(self.y)
end

function Vector:LengthSquared(v)
    local v = v or self
    return v.x * v.x + v.y * v.y
end

function Vector:Length(p)
    return math.sqrt(self:LengthSquared(p))
end

function Vector:LineIntersection(a2, b1, b2)
    local a, b = a2 - self, b2 - b1
    local axb = a:CrossProduct(b)
    if IsZero(axb) then return nil end -- parallel
    local bsa = b1 - self
    local t = bsa:CrossProduct(b)
    return self + a * t / axb
end

function Vector:Negate()
    return Vector:New(-self.x, -self.y)
end

function Vector:Normalize()
    local len = self:Length()
    if IsZero(len) then return Vector:New() end
    return Vector:New(self.x / len, self.y / len)
end

function Vector:Perpendicular()
    return Vector:New(-self.y, self.x)
end

function Vector:Perpendicular2()
    return Vector:New(self.y, -self.x)
end

function Vector:Rotate(phi, p)
    if IsZero(phi) then return self:Clone() end
    local c, s = math.cos(phi), math.sin(phi)
    local p = p or Vector:New()
    local d = self - p
    local x = c * d.x - s * d.y + p.x
    local y = s * d.x + c * d.y + p.y
    return Vector:New(x, y)
end

function Vector:Round()
    local x = math.floor(self.x + 0.5)
    local y = math.floor(self.y + 0.5)
    return Vector:New(x, y)
end

function Vector:SegmentIntersection(a2, b1, b2)
    local a, b = a2 - self, b2 - b1
    local axb = a:CrossProduct(b)
    if IsZero(axb) then return nil end -- parallel
    local bsa = b1 - self
    local t1 = bsa:CrossProduct(b) / axb
    local t2 = bsa:CrossProduct(a) / axb
    return t1 >= 0 and t1 <= 1 and t2 >= 0
        and t2 <= 1 and self + a * t1 or nil
end

--------------------
-- Geometry class --

local Geometry = Class()

function Geometry:__init() end

function Geometry:Arc(p1, p2, phi, step)
    local angle, result = -phi * 0.5, {}
    local length = p1:Distance(p2) * phi
    if step > length then step = length end
    local steps = math.floor(length / step)
    for i = 1, steps + 1 do
        local rotated = p2:Rotate(angle, p1)
        table.insert(result, rotated)
        angle = angle + phi / steps
    end
    return result
end

function Geometry:CircleSegmentIntersection(p1, p2, circle, radius)
    local result = Linq()
    local dp, dc = p2 - p1, p1 - circle
    local a = dp:LengthSquared()
    local b = 2 * dc:DotProduct(dp)
    local c = dc:LengthSquared() - radius * radius
    local delta = b * b - 4 * a * c
    if delta < 0 then return result end
    local delta = math.sqrt(delta)
    local t1 = (-b - delta) / (2 * a)
    local t2 = (-b + delta) / (2 * a)
    if t1 >= 0 and t1 <= 1 then
        local pos = p1 + dp * t1
        table.insert(result, pos)
    end
    if t2 >= 0 and t2 <= 1 then
        local pos = p1 + dp * t2
        table.insert(result, pos)
    end
    return result
end

function Geometry:DrawCircle(pos, radius, color, height)
    local a, r, g, b = Hex2Argb(color)
    directives.drawCircle(pos, radius, height, a, r, g, b)
end

function Geometry:DrawLine(p1, p2, color, height)
    local a, r, g, b = Hex2Argb(color)
    directives.drawLine(p1, p2, height, a, r, g, b)
end

function Geometry:DrawPath(path, color, height)
    for index = 1, #path - 1 do
        local p1, p2 = path[index], path[index + 1]
        self:DrawLine(p1, p2, color, height)
    end
end

function Geometry:DynamicCollision(a1, a2, b1, b2, sa, sb, ra, rb)
    local dist = (ra + rb) * (ra + rb)
    local va = (a2 - a1):Normalize() * sa
    local vb = (b2 - b1):Normalize() * sb
    local dp, dv = a1 - b1, va - vb
    local a = dv:LengthSquared()
    local b = 2 * dp:DotProduct(dv)
    local c = dp:LengthSquared() - dist
    local delta = b * b - 4 * a * c
    if delta < 0 then return nil end
    local delta = math.sqrt(delta)
    local t1 = (-b - delta) / (2 * a)
    local t2 = (-b + delta) / (2 * a)
    return math.min(t1, t2)
end

-------------------
-- Polygon class --

local Polygon = Class()

function Polygon:__init(points)
    self.points = points or Linq()
    self.size = #self.points
end

function Polygon:Add(point)
    self.size = self.size + 1
    self.points[self.size] = point
end

function Polygon:Area()
    return -self.points:Aggregate(function(area, p1, index)
        local p2 = self.points[(index - 2) % self.size + 1]
        return area + (p2.x + p1.x) * (p2.y - p1.y) end) / 2
end

function Polygon:Clear()
    for index = 1, self.size do
        self.points[index] = nil
    end
    self.size = 0
end

function Polygon:Draw(color, height)
    for index = 1, self.size do
        local p1 = self.points[index]
        local p2 = self.points[index % self.size + 1]
        Geometry:DrawLine(p1, p2, color, height)
    end
end

function Polygon:Get(index)
    return self.points[index]
end

function Polygon:IsInside(point)
    -- Based on paper whose author is Jianqiang Hao:
    -- "Optimal Reliable Point-in-Polygon Test and
    -- Differential Coding Boolean Operations on Polygons"
    local result = 0
    for index = 1, self.size do
        local next = index == self.size and 1 or (index + 1)
        local a, b = self.points[index], self.points[next]
        local v1, v2 = a.y - point.y, b.y - point.y
        if not (v1 < 0 and v2 < 0 or v1 > 0 and v2 > 0) then
            local u1, u2 = a.x - point.x, b.x - point.x
            if v2 > 0 and v1 <= 0 then
                local crs = u1 * v2 - u2 * v1
                if crs > 0 then result = result + 1 end
                if crs == 0 then return true end
            elseif v1 > 0 and v2 <= 0 then
                local crs = u1 * v2 - u2 * v1
                if crs < 0 then result = result + 1 end
                if crs == 0 then return true end
            elseif v2 == 0 and v1 < 0 then
                local crs = u1 * v2 - u2 * v1
                if crs == 0 then return true end
            elseif v1 == 0 and v2 < 0 then
                local crs = u1 * v2 - u2 * v1
                if crs == 0 then return true end
            elseif v1 == 0 and v2 == 0 then
                if u2 <= 0 and u1 >= 0 then return true end
                if u1 <= 0 and u2 >= 0 then return true end
            end
        end
    end
    return result % 2 ~= 0
end

function Polygon:IsOutside(point)
    return not self:IsInside(point)
end

function Polygon:Offset(delta, step)
    local result = Linq()
    local poly = self:Orientation() and
        self.points or self.points:Reverse()
    for index = 1, self.size do
        local p1 = poly[index]
        local p2 = poly[(index - 2) % self.size + 1]
        local p3 = poly[(index - 3) % self.size + 1]
        local d1 = (p2 - p1):Normalize():Perpendicular() * delta
        local d2 = (p3 - p2):Normalize():Perpendicular() * delta
        local a, b, c, d = p1 + d1, p2 + d1, p2 + d2, p3 + d2
        local int = a:LineIntersection(b, c, d)
        if int == nil then goto continue end -- parallel
        local prev = #result -- previous size of result
        local phi = math.pi - p2:AngleBetween(p1, p3)
        if phi < 0 then table.insert(result, int) end
        if prev ~= #result then goto continue end
        local vertex = p2:Extend(int, delta)
        local arc = Geometry:Arc(p2, vertex, phi, step)
        result:AddRange(arc)
        ::continue::
    end
    return Polygon:New(result)
end

function Polygon:Orientation()
    return self:Area() >= 0
end

function Polygon:PathIntersection(p1, p2)
    local result = Linq()
    for index = 1, self.size do
        local a = self.points[index]
        local b = self.points[index % self.size + 1]
        local int = p1:SegmentIntersection(p2, a, b)
        if int ~= nil then table.insert(result, int) end
    end
    return result:Distinct()
end

function Polygon:Remove(index)
    self.size = self.size - 1
    table.remove(self.points, index)
end

function Polygon:Set(index, point)
    if index > self.size then
        self:Add(point) return end
    self.points[index] = point
end

------------------------------------------
-- Path structure (for IsPathDangerous) --

local Path = Class()

function Path:__init(speed, delay, delta, startPos, endPos)
    self.startPos = startPos or Vector:New()
    self.endPos = endPos or Vector:New()
    self.speed = speed or math.huge
    self.delay = delay or 0
    self.delta = delta or 0
end

--------------------------
-- Skillshot superclass --

_G.DetectionType = {
    ["UNDEFINED"] = 0,
    ["ON_ACTIVE_SPELL"] = 1,
    ["ON_NEW_PATH"] = 2,
    ["ON_OBJECT_CREATED"] = 3,
    ["ON_PROCESS_SPELL"] = 4,
    ["ON_WND_PROC"] = 5
}

_G.CollisionFlag = {
    ["CHAMPION"] = 0,
    ["MINION"] = 1,
    ["TERRAIN_WALL"] = 2,
    ["WIND_WALL"] = 3,
}

_G.SkillshotType = {
    ["CIRCLE"] = 0,
    ["CONE"] = 1,
    ["LINE"] = 2,
    ["POLYGON"] = 3,
    ["RING"] = 4
}

local uniqueId = 0
local Skillshot = Class()

function Skillshot:__init()
    self.id = uniqueId + 1
    self.arcStep = 0
    self.coneAngle = 0
    self.dangerLevel = 0
    self.extraDuration = 0
    self.height = 0
    self.offset = 0
    self.preDelay = 0
    self.rotAngle = 0
    self.radius = 0
    self.range = 0
    self.speed = 0
    self.startTime = 0
    self.dot = false
    self.fixedRange = false
    self.fogOfWar = false
    self.hitbox = false
    self.invert = false
    self.rotate90 = false
    self.caster = myHero
    self.name = ""
    self.collisions = {}
    self.offsetPolygon = Polygon:New()
    self.polygon = Polygon:New()
    self.geometry = Geometry:New()
    self.destPos = Vector:New()
    self.direction = Vector:New()
    self.endPos = Vector:New()
    self.perpendicular = Vector:New()
    self.position = Vector:New()
    self.startPos = Vector:New()
    self.detectionType = _G.DetectionType.UNDEFINED
    self.skillshotType = _G.SkillshotType.CIRCLE
end

function Skillshot:Draw(color1, color2)
    self.polygon:Draw(color1, self.height)
    if not self.hitbox then return end
    self.offsetPolygon:Draw(color2, self.height)
end

function Skillshot:FixOrigin()
    local unfixed = not self.fixedRange
    local dist = self.startPos:Distance(self.destPos)
    if unfixed and dist < self.range then self.range = dist end
    self.direction = (self.destPos - self.startPos):Normalize()
    self.endPos = self.startPos + self.direction * self.range
    self.startPos = self.startPos + self.direction * self.offset
    self.endPos = self.endPos:Rotate(self.rotAngle, self.startPos)
    self.direction = self.direction:Rotate(self.rotAngle)
    self.perpendicular = self.direction:Perpendicular()
    self.range = self.range - self.offset
    if self.rotate90 then -- its center appears on end position
        local distance = self.perpendicular * self.range * 0.5
        self.startPos = self.endPos - distance
        self.endPos = self.endPos + distance
        self.direction = self.perpendicular:Clone()
        self.perpendicular = self.direction:Perpendicular()
    end
    if not self.invert then return end
    self.direction = self.direction:Negate()
    self.perpendicular = self.perpendicular:Negate()
    local spos, epos = self.startPos, self.endPos
    self.startPos, self.endPos = epos, spos
    self.invert = false
end

function Skillshot:Initialise(data)
    self:LoadData(data)
    self:FixOrigin()
    self:Update()
end

function Skillshot:IsExpired()
    local timer = directives.timer()
    local elapsed = timer - self.startTime
    return elapsed >= self:TotalLifeTime()
end

function Skillshot:IsPathDangerousStatic(path)
    -- supports all skillshots with no displacement
    if self:IsDangerous(path.endPos) then return true end
    local timeToHit = self:TimeToHitStatic()
    local delay, pos = timeToHit - path.delay, {}
    if delay <= 0 then return self:IsDangerous(
        path.startPos) or #self:PathIntersection(
        path.startPos, path.endPos) > 0 end
    if path.speed == math.huge then return false end
    for i = -1, path.delta > 0 and 1 or 0, 2 do
        local index = i == -1 and 1 or 2
        local dist = path.speed * (delay + path.delta * i)
        pos[index] = path.startPos:Extend(path.endPos, dist)
        if self:IsDangerous(pos[index]) then return true end
    end
    return #pos > 1 and #self:PathIntersection(pos[1], pos[2]) > 0
end

function Skillshot:IsSafe(pos)
    return not self:IsDangerous(pos)
end

function Skillshot:LoadData(data)
    for parameter, value in pairs(data) do
        if self[parameter] ~= nil then
            self[parameter] = value
        end
    end
end

function Skillshot:TimeToHitStatic()
    local timer = directives.timer()
    return math.max(0, self.startTime - timer +
        self.range / self.speed + self.preDelay)
end

function Skillshot:TotalLifeTime()
    return self.range / self.speed +
        self.preDelay + self.extraDuration
end

-- Abstract methods

function Skillshot:IsDangerous(pos) end

function Skillshot:IsPathDangerous(path) end

function Skillshot:PathIntersection(p1, p2) end

function Skillshot:Position(delta) end

function Skillshot:TimeToHit(pos) end

function Skillshot:Update() end

---------------------------------
-- Circular skillshot subclass --

local Circle = Class(Skillshot)

function Circle:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.position = self.endPos
    self.skillshotType = _G.SkillshotType.CIRCLE
end

function Circle:Draw(color1, color2) -- @override
    self.geometry:DrawCircle(self.position,
        self.radius, color1, self.height)
    if not self.hitbox then return end
    local hitbox = directives.hitbox()
    self.geometry:DrawCircle(self.position,
        self.radius + hitbox, color2, self.height)
end

function Circle:IsDangerous(pos)
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local dist = pos:DistanceSquared(self.position)
    return dist <= (self.radius + hitbox) ^ 2
end

function Circle:IsPathDangerous(path)
    return self:IsPathDangerousStatic(path)
end

function Circle:PathIntersection(p1, p2)
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    return self.geometry:CircleSegmentIntersection(
        p1, p2, self.position, self.radius + hitbox)
end

function Circle:Position(delta)
    return self.position
end

function Circle:TimeToHit(pos)
    return self:IsDangerous(pos) and
        self:TimeToHitStatic() or -1
end

function Circle:Update() end

------------------------------
-- Conic skillshot subclass --

local Cone = Class(Skillshot)

function Cone:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.position = self.startPos
    self.skillshotType = _G.SkillshotType.CONE
end

function Cone:Draw(color) -- @override
    self.geometry:DrawLine(self.leftEdge,
        self.position, color, self.height)
    self.geometry:DrawLine(self.rightEdge,
        self.position, color, self.height)
    self.geometry:DrawPath(
        self.arc, color, self.height)
end

function Cone:IsDangerous(pos)
    local spos, epos = self.position, self.endPos
    local dist = pos:DistanceSquared(spos)
    if dist > self.range ^ 2 then return false end
    local angle = spos:AngleBetween(pos, epos)
    angle = math.min(angle, math.pi * 2 - angle)
    return angle <= self.coneAngle * 0.5
end

function Cone:IsPathDangerous(path)
    return self:IsPathDangerousStatic(path)
end

function Cone:PathIntersection(p1, p2)
    local pos, result = self.position, Linq()
    local left, right = self.leftEdge, self.rightEdge
    local seg1 = pos:SegmentIntersection(left, p1, p2)
    if seg1 ~= nil then table.insert(result, seg1) end
    local seg2 = pos:SegmentIntersection(right, p1, p2)
    if seg2 ~= nil then table.insert(result, seg2) end
    local ints = self.geometry:CircleSegmentIntersection(
        p1, p2, pos, self.range):Where(function(int)
        local angle = pos:AngleBetween(int, self.endPos)
        angle = math.min(angle, math.pi * 2 - angle)
        return angle < self.coneAngle * 0.5 end)
    result:AddRange(ints)
    return result:Distinct()
end

function Cone:Position(delta)
    return self.position
end

function Cone:TimeToHit(pos)
    return self:IsDangerous(pos) and
        self:TimeToHitStatic() or -1
end

function Cone:Update()
    if self.position:IsZero() then
        self.position = self.startPos end
    self.arc = self.geometry:Arc(self.position,
        self.endPos, self.coneAngle, self.arcStep)
    self.leftEdge = self.endPos:Rotate(
        self.coneAngle * 0.5, self.position)
    self.rightEdge = self.endPos:Rotate(
        -self.coneAngle * 0.5, self.position)
end

-------------------------------
-- Linear skillshot subclass --

local Line = Class(Skillshot)

function Line:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.position = self.startPos
    self.skillshotType = _G.SkillshotType.LINE
end

function Line:IsDangerous(pos)
    local inside = self.polygon:IsInside(pos)
    if not self.hitbox or inside then return inside end
    return self.polygon.points:Min(function(p1, index)
        local p2 = self.polygon:Get(index % 4 + 1)
        return pos:DistSqrToSegment(p1, p2)
    end) <= directives.hitbox() ^ 2
end

function Line:IsPathDangerous(path)
    if self:IsDangerous(path.endPos) then return true
    elseif self.speed == math.huge then return
        self:IsPathDangerousStatic(path) end
    local time = self:TimeToHit(path.startPos)
    if time >= 0 and time <=
        path.delay then return true end
    local infSpeed = path.speed == math.huge
    if infSpeed then return false end
    -- predict positions after delay
    local pos = self:Position(path.delay)
    local delay = math.max(0, self.preDelay -
        directives.timer() + self.startTime)
    local dir = path.endPos - path.startPos
    dir = dir:Normalize() * path.speed * delay
    local startPos = path.startPos + dir
    local endPos = path.endPos + dir
    -- dynamic collision test
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local time = self.geometry:DynamicCollision(
        pos, self.endPos, startPos, endPos,
        self.speed, path.speed, self.radius,
        hitbox + path.speed * path.delta)
    -- no collision detected, path is safe
    if time == nil then return false end
    -- collision detected, but if it will happen
    -- after skillshot expiration then return false
    return time <= self:TimeToHitStatic()
end

function Line:PathIntersection(p1, p2)
    local exclude, poly = not self.hitbox, self.polygon
    if exclude then return poly:PathIntersection(p1, p2) end
    local hitbox = directives.hitbox()
    local dir = self.direction * hitbox
    local perp = self.perpendicular * hitbox
    local segments, result = Linq(), Linq()
    segments[1] = Linq({poly:Get(4) - dir, poly:Get(1) - dir})
    segments[2] = Linq({poly:Get(1) + perp, poly:Get(2) + perp})
    segments[3] = Linq({poly:Get(2) + dir, poly:Get(3) + dir})
    segments[4] = Linq({poly:Get(3) - perp, poly:Get(4) - perp})
    local poly1 = Polygon:New(segments[1]:Concat(segments[3]))
    local poly2 = Polygon:New(segments[2]:Concat(segments[4]))
    for i = 1, 4 do
        local a, b = segments[i][1], segments[i][2]
        local int = a:SegmentIntersection(b, p1, p2)
        if int ~= nil then table.insert(result, int) end
        local ints = self.geometry:CircleSegmentIntersection(
            p1, p2, self.polygon:Get(i), hitbox):Where(function(p)
            return poly1:IsOutside(p) and poly2:IsOutside(p) end)
        result:AddRange(ints)
    end
    return result:Distinct()
end

function Line:Position(delta)
    local delta = delta or 0
    local infSpeed = self.speed == math.huge
    if infSpeed then return self.startPos end
    local t = math.max(0, delta - self.preDelay
        + directives.timer() - self.startTime)
    local x = math.min(self.speed * t, self.range)
    return self.startPos + self.direction * x
end

function Line:TimeToHit(pos)
    if self:IsSafe(pos) then return -1
    elseif self.speed == math.huge then
        return self:TimeToHitStatic() end
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local time = self.geometry:DynamicCollision(
        self.position, self.endPos, pos, pos,
        self.speed, 0, self.radius, hitbox)
    local delay = math.max(0, self.preDelay -
        directives.timer() + self.startTime)
    return delay + math.max(0, time or 0)
end

function Line:Update()
    self.position = self:Position()
    if self.position == self.endPos
        then self.offsetPolygon:Clear()
        self.polygon:Clear() return end
    local dist = self.perpendicular * self.radius
    self.polygon:Set(1, self.position + dist)
    self.polygon:Set(2, self.endPos + dist)
    self.polygon:Set(3, self.endPos - dist)
    self.polygon:Set(4, self.position - dist)
    if not self.hitbox then return end
    self.offsetPolygon = self.polygon:Offset(
        directives.hitbox(), self.arcStep)
end

----------------------------------
-- Polygonal skillshot subclass --

local Poly = Class(Skillshot)

function Poly:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.position = self.endPos
    self.skillshotType = _G.SkillshotType.POLYGON
end

function Poly:IsDangerous(pos)
    return self.offsetPolygon:IsInside(pos)
end

function Poly:IsPathDangerous(path)
    return self:IsPathDangerousStatic(path)
end

function Poly:PathIntersection(p1, p2)
    return self.offsetPolygon:PathIntersection(p1, p2)
end

function Poly:Position(delta)
    return self.position
end

function Poly:TimeToHit(pos)
    return self:IsDangerous(pos) and
        self:TimeToHitStatic() or -1
end

function Poly:Update()
    self.offsetPolygon = self.polygon
    if not self.hitbox then return end
    self.offsetPolygon = self.polygon:Offset(
        directives.hitbox(), self.arcStep)
end

-------------------------------
-- Ringed skillshot subclass --

local Ring = Class(Skillshot)

function Ring:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.innerRadius = data.innerRadius
    self.outerRadius = data.outerRadius
    self.position = self.endPos
    self.skillshotType = _G.SkillshotType.RING
end

function Ring:Draw(color1, color2) -- @override
    self.geometry:DrawCircle(self.position,
        self.innerRadius, color1, self.height)
    self.geometry:DrawCircle(self.position,
        self.outerRadius, color2, self.height)
end

function Ring:IsDangerous(pos)
    local ri, ro = self.innerRadius, self.outerRadius
    local dist = pos:DistanceSquared(self.position)
    return dist >= ri * ri and dist <= ro * ro
end

function Ring:IsPathDangerous(path)
    return self:IsPathDangerousStatic(path)
end

function Ring:PathIntersection(p1, p2)
    return self.geometry:CircleSegmentIntersection(
        p1, p2, self.position, self.innerRadius)
    :Concat(self.geometry:CircleSegmentIntersection(
        p1, p2, self.position, self.outerRadius))
end

function Ring:Position(delta)
    return self.position
end

function Ring:TimeToHit(pos)
    return self:IsDangerous(pos) and
        self:TimeToHitStatic() or -1
end

function Ring:Update() end

---------------------
-- Core algorithms --

_G.SortMode = {
    ["SHORTEST_PATH"] = 0,
    ["MOUSE_DIRECTION"] = 1
}

local Core = Class()

function Core:__init(skillshots, step)
    self.angleStep = step or 12
    self.skillshots = skillshots
    self.sortingModes = {
        [_G.SortMode.SHORTEST_PATH] = function(a, b)
            local hero = Vector:New(directives.position())
            return hero:DistanceSquared(a) <
                hero:DistanceSquared(b) end,
        [_G.SortMode.MOUSE_DIRECTION] = function(a, b)
            local mouse = Vector:New(directives.mousePos())
            local hero = Vector:New(directives.position())
            return hero:AngleBetween(a, mouse, true) <
                hero:AngleBetween(b, mouse, true) end
    }
end

function Core:IsCollidingWall(p1, p2)
    local dist, step = p1:Distance(p2), 0
    local hitbox = directives.hitbox()
    local dir = (p2 - p1):Normalize()
    for i = 1, math.ceil(dist / hitbox) do
        local pos = p1 + dir * step
        if directives.isWall(pos) then return true end
        step = step + math.min(dist - step, hitbox)
    end
    return false
end

function Core:IsDangerous(pos)
    return self.skillshots:Any(function(s)
        return s:IsDangerous(pos) end)
end

function Core:IsPathDangerous(path)
    return self.skillshots:Any(function(s)
        return s:IsPathDangerous(path) end)
end

function Core:IsSafe(pos)
    return not self:IsDangerous(pos)
end

function Core:FindSafeSpots(path, maxRange, wallCheck)
    local results = {}
    path.startPos = Vector:New(directives.position())
    for i = 0, 360 - self.angleStep, self.angleStep do
        -- rotate around hero and create a one-way path
        local rotated = Vector:New(0, 1):Rotate(math.rad(i))
        path.endPos = path.startPos + rotated * (maxRange or 1000)
        -- get and analyse the closest safe intersection point
        local intersection = self.skillshots:Select(function(s)
            return s:PathIntersection(path.startPos, path.endPos) end)
            :Aggregate(function(r, i) return r:Concat(i) end, Linq())
            :OrderBy(function(a, b) return a:DistanceSquared(
                path.startPos) < b:DistanceSquared(path.startPos) end)
            :First(function(i) local after = path.startPos:Append(i, 1)
                return not self:IsDangerous(after) end) or nil
        if intersection == nil then goto continue end
        path.endPos = path.startPos:Append(intersection, 1)
        if self:IsPathDangerous(path) or (wallCheck
            and self:IsCollidingWall(path.startPos,
            path.endPos)) then goto continue end
        table.insert(results, path.endPos:Clone())
        ::continue::
    end
    return results
end

function Core:GetEvadeSpot(path, maxRange, wallCheck)
    local delta = path.delta
    for index = 1, 0, -1 do
        path.delta = delta + 0.1 * index
        local spots = self:FindSafeSpots(
            path, maxRange or 1000, wallCheck)
        table.sort(spots, self.SortMode[index])
        if #spots > 0 then return spots[1] end
    end
    return nil
end

return {
    Linq = Linq,
    Vector = Vector,
    Polygon = Polygon,
    Path = Path,
    Circle = Circle,
    Cone = Cone,
    Line = Line,
    Poly = Poly,
    Ring = Ring,
    Core = Core
}

