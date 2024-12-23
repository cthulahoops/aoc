defmodule Grid do
  def read_points(filename) do
    points =
      filename
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(&process_line/1)

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

  def n_cheat(n, {x, y}) do
    for dx <- -n..n, dy <- -n..n, abs(dx) + abs(dy) <= n, abs(dx) > 1 || abs(dy) > 1 do
      {x + dx, y + dy}
    end
  end

  def short_cuts_at(point, path_numbers, cheat_length) do
    for jump_point <- n_cheat(cheat_length, point),
        Map.has_key?(path_numbers, jump_point) do
      Map.get(path_numbers, jump_point) - Map.get(path_numbers, point) - manhattan_distance(point, jump_point)
    end |> Enum.filter(&(&1 > 0))
  end

  defp manhattan_distance({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end

  def shortcuts(path_numbers, cheat_length) do
    Enum.flat_map(path_numbers, fn {point, _index} ->
      short_cuts_at(point, path_numbers, cheat_length)
    end)
  end

  def count_cheats([head|tail]) do
    count_cheats(tail, head, 1)
  end

  defp count_cheats([], last, count) do
    [{last, count}]
  end

  defp count_cheats([head|tail], last, count) do
    if head == last do
      count_cheats(tail, last, count + 1)
    else
      [{last, count} | count_cheats(tail, head, 1)]
    end
  end
end

[filename] = System.argv()

result = Grid.read_points(filename)
IO.puts("Start point: #{inspect(result.start)}")
IO.puts("End point: #{inspect(result.end)}")

path = Grid.walk_path(result)
IO.puts("Path length: #{length(path)}")

path_numbers = Grid.number_path(path)

old_shortcuts = path_numbers |> Grid.shortcuts(2)
good_shortcuts = Enum.filter(old_shortcuts, fn shortcut -> shortcut >= 100 end)

IO.puts("Part 1: #{Enum.count(good_shortcuts)}")

new_shortcuts = path_numbers |> Grid.shortcuts(20)

if filename == "input/20-example" do
  IO.puts("Shortcuts: #{inspect(new_shortcuts |> Enum.filter(&(&1 >= 50)) |> Enum.sort() |> Grid.count_cheats(), charlists: :as_lists, limit: :infinity)}")
end

part_2 = new_shortcuts |> Enum.filter(&(&1 >= 100)) |> Enum.count()
IO.puts("Part 2: #{part_2}")
