/*
 * Schema:
 * +-------------+---------+
 * | Column Name | Type    |
 * +-------------+---------+
 * | id          | int     |
 * | name        | varchar |
 * +-------------+---------+
 * id is the primary key column for this table.
 * Each row of this table indicates the name and the ID of a student.
 * id is a continuous increment.
 * https://leetcode.com/problems/exchange-seats/
 */
SELECT new_id as id, student FROM (
  SELECT
  IF(
    id = (SELECT MAX(id) FROM Seat) AND id % 2 = 1,
    id,
    IF(
      id % 2 = 1,
      id + 1,
      id - 1
    )
  ) AS new_id,
  student
  FROM Seat
) AS tmp
ORDER BY id
