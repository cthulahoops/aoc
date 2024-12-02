.mode csv
create table day1 (first integer, second integer);
.import '| < inputs/1 sed "s/ \+/,/"' day1

select * from day1 limit 3;

select sum(abs(first - second)) from (
        select first, row_number() over(order by first) as first_id from day1
) join (
        select second, row_number() over(order by second) as second_id from day1
) where first_id = second_id;

select sum(c)
from (
        select a.first * count(a.first) as c from day1 a
        join day1 b on a.first = b.second group by a.first
) a;
