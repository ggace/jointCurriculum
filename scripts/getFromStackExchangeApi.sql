-- url : https://data.stackexchange.com/stackoverflow/query/new
-- tag 이름에 따른 id 가져오기
SELECT id, tagname FROM tags WHERE tagname='jinja2';
-- tag에 따른 년도별 개수 가져오기
SELECT 
        CONVERT(CHAR(4), p.CreationDate, 112) AS year,
        COUNT(p.Id) AS count
    FROM 
        Posts p
        JOIN PostTags pt 
        ON p.Id = pt.PostId
        JOIN Tags t
        ON t.id = pt.TagId
    WHERE 
        pt.TagId = 20674
        GROUP BY CONVERT(CHAR(4), p.CreationDate, 112)
        ORDER BY CONVERT(CHAR(4), p.CreationDate, 112)