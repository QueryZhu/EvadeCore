# EvadeCore

## Introduction

Geometry library for evade utility and skillshot simulation.

It has been a long time since people began inventing extensive solutions for problems with pathfinding around dynamic and static obstacles in 2D and 3D environments, yet no one has distributed an optimal way of finding the best path for the game object to get outside of a dangerous area.

This is an interesting subject of artificial intelligence capabilities that I have decided to provide to others in a core format as an easy-to-use library. The solution can be used on any sort of shooting games as well as MMORPG games like League of Legends or Dota, but it can have a purpose in different areas too.

The skillshot types that are supported are linear, circular, conic, polygonal, and annular. Skillshots are divided into two disparate forms depending on the type of movement. The first one follows a specific path from a fixed starting point to its destination, it contains a circular hitbox and a constant speed. The second form has no position displacement, so the skillshot is placed at a certain location - this form is handled by all skillshot types that have their own defined collision boxes, whereas both forms are supported by linear skillshots only.

The following documentation will help you understand how the library is supposed to work on your environment. The introduced API will mostly focus on a skillshot structure and core functions. More details about vectors, polygons and paths can be found inside library file.

## Skillshot API

Parameters:

* **id** [number] - an unique serial number id of an instantiated skillshot
* **arcStep** [number] - a step length needed to construct an arc in a vertex offsetting
    - offsetting process is needed to visualise a true hitbox area of a skillshot
* **coneAngle** [radians] - an angle formed by two rays of a cone structure
* **dangerLevel** [number] - a state of a vulnerability of a game object against skillshot
* **extraDuration** [number] - an extra amount of life time for a static skillshot
* **height** [number] - a vertical component for drawing in a 3D perspective
* **offset** [number] - an amount of a displacement of the starting position
* **preDelay** [number] - this parameter provides two disparate functions:
    - when a skillshot speed is huge, it determines a time left for skillshot to expire
    - otherwise it defines a delay before a skillshot begins a travel over a set distance
* **rotAngle** [radians] - an angle of rotation of the ending position around starting position
* **radius** [number] - a length of line extending from the center of a  
circle to the circumference or perpendicular offset to a line segment
* **range** [number] - a distance from the starting position to ending position
* **speed** [number] - a scalar quantity that refers to how fast a skillshot is moving
    - an immobile skillshot has a huge speed and covers area in amount of total life time
* **startTime** [number] - a runtime (in seconds) when a skillshot has been created
* **dot** [boolean] - indicates if a skillshot deals a persistent damage
* **fixedRange** [boolean] - indicates if a skillshot range is constantly fixed
* **fogOfWar** [boolean] - indicates if a skillshot was created in a fog of war
* **hitbox** [boolean] - considers a hitbox of a game object to the skillshot area
* **invert** [boolean] - indicates if a skillshot should be inverted
* **rotate90** [boolean] - a custom perpendicular rotation of a skillshot
    - besides being rotated, the middle point becomes an original end position
* **caster** [game_object] - a game object who has casted a skillshot
* **name** [string] - an original name of a instantiated skillshot
* **collisions** [table(CollisionFlag)] - a collection of collision flags
* **offsetPolygon** [Polygon] - a skillshot area offsetted by hitbox of a game object
* **polygon** [Polygon] - a skillshot area constructed from input parameters
    + both polygons are only used by linear and polygonal skillshots
* **geometry** [Geometry] - an instance of the geometry class providing useful methods
* **direction** [Vector] - a vector defining in which way the skillshot is pointing
* **endPos** [Vector] - the ending position of a skillshot on a travel path
* **perpendicular** [Vector] - a skillshot direction rotated perpendicularly
* **position** [Vector] - a current skillshot position (manually updated)
* **startPos** [Vector] - the starting position of a skillshot on a travel path
* **detectionType** [DetectionType] - defines a method used to detect a skillshot
* **skillshotType** [SkillshotType] - sets a skillshot type

Methods:

* **Draw(number color1, number color2)** [void] - draws a skillshot hitbox area
* **FixOrigin()** [void] - executes a fix for skillshot origin placements
* **LoadData(table)** [void] - overrides skillshot parameters with input table values
* **IsDangerous(Vector)** [boolean] - indicates if input point is inside of skillshot area
* **IsDangerousPath(Path)** [boolean] - indicates if input path will collide with a skillshot
* **IsExpired()** [boolean] - indicates if a skillshot has already expired
* **IsSafe(Vector)** [boolean] - indicates if input point is outside of skillshot area
* **PathIntersection(Vector, Vector)** [Linq(Vector)] - returns skillshot-path intersection points
* **Position(number)** [Vector] - returns a predicted skillshot position after input time delta
* **TimeToHit(Vector)** [number] - returns a time for skillshot to enter a collision with object
* **TotalLifeTime()** [number] - returns a total skillshot life time
* **Update()** [void] - updates the skillshot hitbox area

## Core API

Parameters:

* **skillshots** [Linq(Skillshot)] - a reference to the collection of active skillshot instances
* **angleStep** [degrees] - an angle step used in path generation around game object

Methods:

* **IsDangerous(Vector)** [boolean] - indicates if input point is inside of any skillshot area
* **IsDangerousPath(Path)** [boolean] - indicates if input path will collide with any skillshot
* **IsSafe(Vector)** [boolean] - indicates if input point is outside of all skillshot areas
* **FindSafeSpots(number speed, number delay, number range, number delta)** [Linq(Vector)]
    - returns safe destinations for game object within a limited range (default value is 1000)
* **GetEvadeSpot(number speed, number delay, number range, number delta)** [Vector]
    - based on generated spots it prioritises the ones which are closer to mouse position
    - in case the paths were too close to reach a collision, it takes the shortest path

## Demo

```lua
local lib = require "EvadeCore"
local Linq, Vector, Line, Core =
    lib.Linq, lib.Vector, lib.Line, lib.Core

local safeSpots = {}
local skillshots = Linq() -- initialise collection
local core = Core:New(skillshots) -- inject collection to core

client:set_event_callback("on_wnd_proc", function(msg, wparam)
    -- 'Z' button press detection
    if msg ~= 514 or wparam ~= 0 then return end
    local pos = Vector:New(myHero.origin)
    table.insert(skillshots, Line:New({
        arcStep = 10, preDelay = 0.25,
        postDelay = 0, radius = 70,
        range = 1000, height = 100,
        speed = 1800, fixedRange = true,
        hitbox = true, startTime = os.clock(),
        endPos = pos, placementPos = pos,
        startPos = Vector:New(2700, 1800)
    }))
end)

client:set_event_callback("on_tick", function()
    local pos = Vector:New(myHero.origin)
    if core:IsDangerous(pos) then
        local latency = game.ping / 2000
        safeSpots = core:FindSafeSpots(
            myHero.move_speed, latency, 600, 0.1)
    end
    skillshots:RemoveWhere(function(s) return s:IsExpired() end)
    skillshots:ForEach(function(s) s:Update() end)
end)

client:set_event_callback("on_draw", function()
    local hitbox = myHero.bounding_radius
    for _, point in ipairs(safeSpots) do
        renderer:draw_circle(point.x, 100,
            point.y, hitbox, 255, 192, 255, 255)
    end
    local c1, c2 = 0xFFFFFFFF, 0xC0FFFFFF
    skillshots:ForEach(function(s) s:Draw(c1, c2) end)
end)
```
