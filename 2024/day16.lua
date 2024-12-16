local Grid = {}
Grid.__index = Grid

local function posKey(x, y)
    return x .. "," .. y
end

function Grid.new(filename)
    local self = setmetatable({}, Grid)
    self.walls = {}
    self.width = 0
    self.height = 0
    self.start = nil
    self.finish = nil

    local file = assert(io.open(filename, "r"), "Could not open file: " .. filename)
    local y = 1

    for line in file:lines() do
        self.width = math.max(self.width, #line)

        for x = 1, #line do
            local char = line:sub(x, x)

            if char == '#' then
                self.walls[posKey(x, y)] = true
            elseif char == 'S' then
                self.start = {x = x, y = y}
            elseif char == 'E' then
                self.finish = {x = x, y = y}
            end
        end

        y = y + 1
    end

    self.height = y - 1
    file:close()

    return self
end

function Grid:isWall(x, y)
    return self.walls[posKey(x, y)] == true
end

function Grid:getStart()
    if not self.start then
        error("No start position (S) found in grid")
    end
    return self.start.x, self.start.y
end

function Grid:getEnd()
    if not self.finish then
        error("No end position (E) found in grid")
    end
    return self.finish.x, self.finish.y
end

function Grid:print()
    for y = 1, self.height do
        for x = 1, self.width do
            if self.walls[posKey(x, y)] then
                io.write('#')
            elseif self.start and self.start.x == x and self.start.y == y then
                io.write('S')
            elseif self.finish and self.finish.x == x and self.finish.y == y then
                io.write('E')
            else
                io.write('.')
            end
        end
        io.write("\n")
    end
end

local Reindeer = {}
Reindeer.__index = Reindeer

-- Initial directions
Reindeer.NORTH = {dx = 0, dy = -1}
Reindeer.EAST = {dx = 1, dy = 0}
Reindeer.SOUTH = {dx = 0, dy = 1}
Reindeer.WEST = {dx = -1, dy = 0}

function Reindeer.new(x, y, direction)
    local self = setmetatable({}, Reindeer)
    self.x = x or 1
    self.y = y or 1
    -- Default to facing north if no direction specified
    self.dx = direction and direction.dx or 0
    self.dy = direction and direction.dy or -1
    return self
end

function Reindeer:turnLeft()
    return Reindeer.new(self.x, self.y, {dx = -self.dy, dy = self.dx})
end

function Reindeer:turnRight()
    return Reindeer.new(self.x, self.y, {dx = self.dy, dy = -self.dx})
end

function Reindeer:getPosition()
    return self.x, self.y
end

function Reindeer:advance()
    return Reindeer.new(self.x + self.dx, self.y + self.dy, {dx = self.dx, dy = self.dy})
end

function Reindeer:asKey()
    return self.x .. "," .. self.y .. "," .. self.dx .. "," .. self.dy
end


local Queue = {}
Queue.__index = Queue

function Queue.new()
    local self = setmetatable({
        first = 0,
        last = -1,
        items = {}
    }, Queue)
    return self
end

function Queue:push(value)
    self.last = self.last + 1
    self.items[self.last] = value
end

function Queue:pop()
    if self:isEmpty() then
        error("Queue is empty")
    end
    
    local value = self.items[self.first]
    self.items[self.first] = nil
    self.first = self.first + 1
    return value
end

function Queue:isEmpty()
    return self.first > self.last
end

function copyTable(t)
    local t2 = {}
    for k,v in pairs(t) do
        t2[k] = v
    end
    return t2
end

function setSize(set)
    local count = 0
    for _ in pairs(set) do
        count = count + 1
    end
    return count
end

local function main()
    if #arg < 1 then
        print("Usage: lua day16.lua <filename>")
        os.exit(1)
    end

    local grid = Grid.new(arg[1])
    print("Grid dimensions:", grid.width, "x", grid.height)
    grid:print()

    local sx, sy = grid:getStart()
    local ex, ey = grid:getEnd()
    print("Start position:", sx, sy)
    print("End position:", ex, ey)

    local reindeer = Reindeer.new(sx, sy, Reindeer.EAST)


    local visited = {}
    local queue = Queue.new()
    queue:push({reindeer = reindeer, score = 0, path = {}})

    local bestScore = math.huge
    local bestPath = ""

    while not queue:isEmpty() do
        local next = queue:pop()
        local current = next.reindeer
        local score = next.score

        if score >= bestScore then
            goto continue
        end

        if visited[current:asKey()] and visited[current:asKey()] < score then
            goto continue
        end
        visited[current:asKey()] = score

        path = copyTable(next.path)
        path[posKey(current.x, current.y)] = true

        local x, y = current:getPosition()
        if x == ex and y == ey then
            if score < bestScore then
                bestScore = score
                bestPath = path
            end

            if score == bestScore then
                for k, _ in pairs(path) do
                    bestPath[k] = true
                end
            end
        end


        local nextReindeer = current:advance()
        local nx, ny = nextReindeer:getPosition()

        if not grid:isWall(nx, ny) then
            queue:push({reindeer = nextReindeer, score = score + 1, path = path})
        end

        queue:push({reindeer = current:turnRight(), score = score + 1000, path = path})
        queue:push({reindeer = current:turnLeft(), score = score + 1000, path = path})

        ::continue::
    end

    print("Best score:", bestScore)
    print("Tiles along a best path:", setSize(bestPath))
end

main()
