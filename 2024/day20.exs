defmodule Grid do
  def read_points(filename) do
    # Read the file and convert to list of lines
    points =
      filename
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(&process_line/1)

    # Extract special points and create final map
    {start, end_point, regular_points} =
      Enum.reduce(points, {nil, nil, MapSet.new()}, fn
        {:start, point}, {_, end_point, points} ->
          {point, end_point, MapSet.put(points, point)}
        {:end, point}, {start, _, points} ->
          {start, point, MapSet.put(points, point)}
        point, {start, end_point, points} ->
          {start, end_point, MapSet.put(points, point)}
      end)

    %{
      start: start,
      end: end_point,
      points: regular_points
    }
  end

  defp process_line({line, y}) do
    line
    |> String.graphemes()
    |> Enum.with_index()
    |> Enum.flat_map(fn
      {".", x} -> [{x, y}]
      {"S", x} -> [{:start, {x, y}}]
      {"E", x} -> [{:end, {x, y}}]
      _ -> []
    end)
  end

  def walk_path(grid) do
    walk_path(grid.start, grid, [])
  end

  def walk_path(point, grid, path) when point == grid.end do
    [grid.end | path]
  end

  def walk_path(start, grid, path) do
    walk_path(next_point(start, grid, path), grid, [start | path])
  end

  defp next_point({x, y}, grid, visited) do
    Enum.reduce(
      [{x, y - 1}, {x, y + 1}, {x - 1, y}, {x + 1, y}],
      nil,
      fn point, acc ->
        if MapSet.member?(grid.points, point) && !Enum.member?(visited, point) do
          point
        else
          acc
        end
      end
    )
  end

  def number_path(path) do
    path
    |> Enum.with_index()
    |> Map.new(fn {point, index} -> {point, index} end)
  end

  def neighbours({x, y}) do
    [{x, y - 1}, {x, y + 1}, {x - 1, y}, {x + 1, y}]
  end

  def two_cheat({x, y}) do
    [
      {x, y - 2},
      {x, y + 2},
      {x - 2, y},
      {x + 2, y}
    ]
  end

  def twenty_cheat({x, y}) do
    for dx <- -20..20, dy <- -20..20, dx + dy <= 20 do
      {x + dx, y + dy}
    end
  end

  def short_cuts_at(point, path_numbers) do
    for jump_point <- two_cheat(point),
        Map.has_key?(path_numbers, jump_point) do
      Map.get(path_numbers, jump_point) - Map.get(path_numbers, point) - 2
    end |> Enum.filter(&(&1 > 0))
  end

  def shortcuts(path_numbers) do
    Enum.flat_map(path_numbers, fn {point, _index} ->
      short_cuts_at(point, path_numbers)
    end)
  end
end

# Get filename from command line arguments
[filename] = System.argv()

# Read and display the results
result = Grid.read_points(filename)
IO.puts("Start point: #{inspect(result.start)}")
IO.puts("End point: #{inspect(result.end)}")
# IO.puts("All points: #{inspect(result.points)}")

path = Grid.walk_path(result)
# IO.puts("Path: #{inspect(path)}")
IO.puts("Path length: #{length(path)}")

path_numbers = Grid.number_path(path)
# IO.puts("Path numbers: #{inspect(path_numbers)}")

old_shortcuts = Grid.shortcuts(path_numbers) |> Enum.sort()
# IO.puts("Shortcuts: #{inspect(all_shortcuts)}")
good_shortcuts = Enum.filter(old_shortcuts, fn shortcut -> shortcut >= 100 end)

IO.puts("Part 1: #{Enum.count(good_shortcuts)}")


