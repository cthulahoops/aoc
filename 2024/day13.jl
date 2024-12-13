function parse_coordinate(str)
    m = match(r"[+=](\d+)", str)
    return m !== nothing ? parse(Int, m[1]) : nothing
end

function parse_block(lines)
    ax = ay = bx = by = px = py = nothing
    
    for line in lines
        if startswith(line, "Button A:")
            coords = split(line[9:end], ",")
            ax = parse_coordinate(coords[1])  # X coordinate
            ay = parse_coordinate(coords[2])  # Y coordinate
        elseif startswith(line, "Button B:")
            coords = split(line[9:end], ",")
            bx = parse_coordinate(coords[1])
            by = parse_coordinate(coords[2])
        elseif startswith(line, "Prize:")
            coords = split(line[7:end], ",")
            px = parse_coordinate(coords[1])
            py = parse_coordinate(coords[2])
        end
    end
    
    return (ax, ay, bx, by, px, py)
end


function main()
    if length(ARGS) < 1
        println("Usage: julia script.jl input_file")
        exit(1)
    end
    
    content = readlines(ARGS[1])
    
    num_blocks = div(length(content), 4)
    
    cost = 0
    count = 0
    for i in 1:num_blocks
        start_idx = (i-1)*4 + 1
        block = content[start_idx:start_idx+2]

        ax, ay, bx, by, px, py = parse_block(block)
        # print("Block $i: A($ax, $ay), B($bx, $by), P($px, $py)\n")
        offset = 10000000000000 # BigInt(10000000000000) ?
        px += offset
        py += offset

        numerator = px * ay - py * ax
        denominator = ay * bx - by * ax

        # print("Numerator: $numerator, Denominator: $denominator\n")

        if numerator % denominator != 0
            # print("No solution\n")
            continue
        end
        b = div(numerator, denominator)

        if (px - b * bx) % ax != 0
            # print("No solution\n")
            continue
        end
        a = div(px - b * bx, ax)

        if a < 0 || b < 0
            print("Negative solution\n")
            continue
        end

        # print("Solution: $a * $ax + $b * $bx == $px\n")
        # print("Solution: $a * $ay + $b * $by == $py\n")
        # # println("Cost: $a + $b = $(a + b)")
        # println("$a $b")
        count += 1
        cost += 3 * a + b
    end

    println(cost)
    println("count: $count")
end

main()
